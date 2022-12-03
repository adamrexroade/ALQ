# Figure 3 Gage Data

library(tidyverse)
library(dplyr)
library(lubridate)
library(dataRetrieval)
library(patchwork)
library(scales)

Q<-readNWISdata(sites="05357205", parameterCd =c("00060", "00010", "00095", "00300", "00400", "63680"), 
                startDate = "2020-04-01", endDate = "2021-10-01", service="iv")

# Parameter Codes 00010 temp, 00060 discharge, 00095 spcond (uS), 00300 do(mg/L), 00400 pH, 63680 turbidity (fnu)y

q <- Q %>% 
  mutate(Date=date(dateTime)) %>% 
  mutate(Year=year(Date)) %>% 
  mutate(Time=time(dateTime)) %>% 
  mutate(Temp=X_00010_00000) %>% 
  mutate(Discharge=X_00060_00000/35.315) %>% 
  mutate(DOMGL =X_00300_00000) %>% 
  select(Date, Year, Time,Temp, Discharge, DOMGL)
q_sum <- q %>% 
  group_by(Date) %>% 
  summarise(`Water Temperature (C)`=mean(Temp),
            `Discharge (m3/sec)`=mean(Discharge),
            `Dissolved Oxygen (mg/L)`= mean(DOMGL)) %>% 
  mutate(Year=as.factor(year(Date))) %>% 
  mutate(Day=yday(Date))

dis <- ggplot(q_sum)+
  geom_line(aes(x=as.Date(Day, origin = as.Date("2020-01-01")), y=`Discharge (m3/sec)`, colour=Year))+
  #  facet_wrap(~name, nrow=3, scales = "free_y")+
  #  ylab("Discharge (m3/sec)")+
  ylab(expression(Discharge~(m^3~sec^-1)))+
  xlab("Date")+
  scale_color_manual(values=c("darkseagreen4", "sandybrown"))+
  theme_bw()+
  annotate(geom="rect", xmin=as.Date("2020-01-01"), xmax=as.Date("2020-05-31"), ymin=0, ymax=Inf, fill="grey", alpha=.3)+
  annotate(geom="rect", xmin=as.Date("2020-10-01"), xmax=as.Date("2020-12-31"), ymin=0, ymax=Inf, fill="grey", alpha=.3)+
  scale_x_date(labels = date_format("%b"), date_breaks = '1 month')

do <- ggplot(q_sum)+
  geom_line(aes(x=as.Date(Day, origin = as.Date("2020-01-01")), y=`Dissolved Oxygen (mg/L)`, colour=Year))+
  #  facet_wrap(~name, nrow=3, scales = "free_y")+
  #  ylab("Dissolved Oxygen (mg/L)")+
  ylab(expression(Dissolved~Oxygen~(mg~L^-1)))+
  xlab("Date")+
  scale_color_manual(values=c("darkseagreen4", "sandybrown"))+
  annotate(geom="rect", xmin=as.Date("2020-01-01"), xmax=as.Date("2020-05-31"), ymin=0, ymax=Inf, fill="grey", alpha=.3)+
  annotate(geom="rect", xmin=as.Date("2020-10-01"), xmax=as.Date("2020-12-31"), ymin=0, ymax=Inf, fill="grey", alpha=.3)+
  theme_bw()+
  scale_x_date(labels = date_format("%b"), date_breaks = '1 month')
tem <- ggplot(q_sum)+
  geom_line(aes(x=as.Date(Day, origin = as.Date("2020-01-01")), y=`Water Temperature (C)`, colour=Year))+
  #  facet_wrap(~name, nrow=3, scales = "free_y")+
  ylab("Water Temperature (ºC)")+
  xlab("Date")+
  scale_color_manual(values=c("darkseagreen4", "sandybrown"))+
  annotate(geom="rect", xmin=as.Date("2020-01-01"), xmax=as.Date("2020-05-31"), ymin=0, ymax=Inf, fill="grey", alpha=.3)+
  annotate(geom="rect", xmin=as.Date("2020-10-01"), xmax=as.Date("2020-12-31"), ymin=0, ymax=Inf, fill="grey", alpha=.3)+
  theme_bw()+
  scale_x_date(labels = date_format("%b"), date_breaks = '1 month')

design <- "
A
B
C"
dis+
  do+
  tem+
  plot_layout(design= design,guides = 'collect')+plot_annotation(tag_levels = 'A')


ggsave("Figures/Figure3.png", plot=last_plot())

# Basic Stats
qsummer <- q_sum %>% 
  filter(Day < 237) %>% 
  filter(Day > 152)

qsummer20 <- qsummer %>% 
  filter(Year=="2020")
qsummer21 <- qsummer %>% 
  filter(Year=="2021")

summary(qsummer20)
summary(qsummer21)