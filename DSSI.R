library(readxl)
library(magrittr)
library(XLConnect)
library(dplyr)
library(wbstats)
library(tidyr)


gdp <- wb(indicator = c("NY.GDP.MKTP.CD"), mrv = 20, gapfill = TRUE) %>% 
  filter(date==2019) %>% 
  select(value,iso3c) %>% 
  rename(GDP_Nom_WB_latest_usd=value) 


DSSI_countries<- wb_cachelist$countries %>% 
  filter(lending=="IDA" | lending=="Blend" | iso3c=="AGO" |  iso3c=="FJI") %>% 
  select(iso3c, country, region, lending) %>% 
  filter(iso3c!="FSM" & iso3c!="KIR" & iso3c!="MHL" & iso3c!="SSD" & iso3c!="TUV")%>% 
  left_join(gdp, by="iso3c")









tmp = tempfile(fileext = ".xlsx")
new_df= data.frame()

for (p in DSSI_countries$iso3c) {
  file<-paste0("https://datatopics.worldbank.org/debt/ids/DSSITables/DSSI-",p, ".xlsx")
  download.file(url = file, destfile = tmp, mode="wb")
  my_data <- readxl::read_excel(tmp, sheet =1, skip = 2) %>% 
  mutate(iso3c=p) %>% 
  left_join(DSSI_countries, by="iso3c") %>% 
  select(iso3c,country,everything())
  new_df <- bind_rows(new_df,  my_data)

}

clean_df2<-new_df %>% 
  rename(lending_agency=`...4`) %>% 
  rename(lender_type=`Data are in US$ thousands`) %>% 
  select(-`...10`) %>% 
  rename(DebtServiceDue2019=`2019`) %>% 
  rename_at(vars(starts_with('201')), funs(sub('20', 'DOD20', .))) %>% 
  rename_at(vars(starts_with('202')), funs(sub('20', 'DebtServiceDue20', .))) %>% 
  mutate(lender_type2=lender_type) %>% 
  fill(lender_type2) %>% 
  mutate(lender_type=ifelse(!is.na(lending_agency),lender_type2,lender_type)) %>% 
  select(-lender_type2) %>% 
  filter(lender_type!="Table: Debt Service Payments Due" & lender_type!="") %>% 
  filter(!grepl("Country:",lender_type)) %>% 
  group_by(iso3c) %>% 
  mutate(last_update=last(lender_type))%>%
  mutate(last_update=substring(last_update, 20, nchar(last_update))) %>% 
  ungroup() %>% 
  filter(!grepl("Last Upd",lender_type)) %>% 
  mutate(sum_row=ifelse(!is.na(lending_agency),0,1)) %>% 
  mutate(lending_agency=ifelse(is.na(lending_agency),lender_type,lending_agency)) %>% 
  select(iso3c,country,lending, region, GDP_Nom_WB_latest_usd, lender_type,lending_agency,sum_row, last_update, everything()) 
  

write.csv(clean_df2,"DSSI_debt_service_v2.csv")

plot1<-clean_df2 %>% 
  select(iso3c,country, region, lender_type,lending_agency,sum_row, DebtServiceDue2020, GDP_Nom_WB_latest_usd) %>% 
  group_by(iso3c, country, region) %>% 
  summarise(DSD2020China= sum(DebtServiceDue2020[lending_agency == "China"]), 
            DSD2020Bond= sum(DebtServiceDue2020[lending_agency == "Total Bondholders"]),
            DSD2020Total= sum(DebtServiceDue2020[lending_agency == "Total"]),  	
            GDP= max(GDP_Nom_WB_latest_usd)) %>% 
  mutate(DSD2020China_GDP=DSD2020China/GDP) %>% 
  mutate(DSD2020Bond_GDP=DSD2020China/GDP) %>% 
  mutate(DSD2020China_Total=round(DSD2020China/DSD2020Total,2)) %>% 
  mutate(DSD2020Bond_Total=round(DSD2020Bond/DSD2020Total,2)) %>% 
  mutate(DSD2020_Total_mUSD=round(DSD2020Total/10^6,2))
  
library(ggrepel)
library(scales)
library(ggplot2)

ggplot(plot1, aes(DSD2020Bond_Total,DSD2020China_Total, color=factor(region))) +
  geom_point(aes(size=DSD2020_Total_mUSD))  +
  scale_color_discrete(name = "Regions")+
  scale_size_continuous(guide = FALSE)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1))+
  labs(title=("Share of Total Debt Service due in 2020 owed to China/Bondholders/Others"), 
       subtitle=("Among the 72 eligible countries to 2020 Debt Service Suspension Initiative (DSSI)"),
       caption = paste0("All figures are  estimates by WB of debt service due by lender. \n Size of points represents value of total debt service due in 2020 in USD \n Data source: WB International Debt Statistics DSSI Data.\n By: @davidmihalyi")) + 
  xlab("Bondholders (%)") +
  ylab("China - Official+Non-official (%)") + 
  geom_text_repel(data = subset(plot1,  DSD2020Bond_Total>0.33 | DSD2020China_Total>0.33), aes(DSD2020Bond_Total,DSD2020China_Total, label = country))

ggsave(file="DSSI_2020_servicing.png")


