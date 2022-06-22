# Figure S1 Gage Comparison


library(tidyverse)
library(dplyr)
library(lubridate)
library(dataRetrieval)
library(patchwork)
library(scales)

Q<-readNWISdata(sites=c("05357205",  "05357206"), parameterCd =c("00060", "00010", "00095", "00300", "00400", "63680"), 
                startDate = "2020-04-01", endDate = "2021-10-01", service="iv")

# Parameter Codes 00010 temp, 00060 discharge, 00095 spcond (uS), 00300 do(mg/L), 00400 pH, 63680 turbidity (fnu)y

q <- Q %>% 
  mutate(Date=date(dateTime)) %>% 
  mutate(Year=year(Date)) %>% 
  mutate(Time=time(dateTime)) %>% 
  mutate(Temp=X_00010_00000) %>% 
  mutate(Discharge=X_00060_00000/35.315) %>% 
  mutate(DOMGL =X_00300_00000) %>% 
  select(Date, Year, site_no, Time,Temp, Discharge, DOMGL)
q_sum <- q %>% 
  group_by(Date, site_no) %>% 
  summarise(`Water Temperature (C)`=mean(Temp),
            `Discharge (m3/sec)`=mean(Discharge),
            `Dissolved Oxygen (mg/L)`= mean(DOMGL)) %>% 
  mutate(Year=as.factor(year(Date))) %>% 
  mutate(Day=yday(Date))

# Time Series ----
q_long <- pivot_longer(q_sum, cols = c(`Discharge (m3/sec)`, `Water Temperature (C)`, `Dissolved Oxygen (mg/L)`))

q_sum <- q_sum %>% 
  rename(`Site Number`=site_no)
ts <- ggplot(q_sum)+
  geom_line(aes(x=Date, y=`Discharge (m3/sec)`, colour=`Site Number`))+
  ylab(expression(Discharge~(m^3~sec^-1)))+
  xlab("Date")+
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y")+
  scale_color_manual(values=c("darkseagreen4", "sandybrown"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust=1))

# Linear Regression ----
q_sum <- q_sum %>% 
  filter(Day>152) %>% 
  filter(Day<272)
oh5 <- q_sum %>% 
  filter(`Site Number`=="05357205")
oh6 <- q_sum %>% 
  filter((`Site Number`=="05357206"))

oh5vsoh6 <- merge(oh5, oh6, by="Date") %>% 
  rename(discharge5=`Discharge (m3/sec).x`,
         discharge6=`Discharge (m3/sec).y` ) %>% 
  select(Date, discharge5, discharge6)
linearplot <- ggplot(oh5vsoh6)+
  geom_point(aes(x=log(discharge5), y=log(discharge6)))+
  geom_smooth(aes(x=log(discharge5), y=log(discharge6)), method = "lm")+
  theme_bw()+
  xlab(expression(05357205~Discharge~(m^3~sec^-1)))
  ylab(expression(05357206~Discharge~(m^3~sec^-1)))

lm <- lm(oh5vsoh6$discharge6~oh5vsoh6$discharge5)
summary(lm)


design <- "A
b"
ts+linearplot+plot_layout(design=design)
ggsave("Figures/FigureS1.png", plot=last_plot())