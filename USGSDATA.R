library(tidyverse)
library(dplyr)
library(lubridate)
library(patchwork)
library(dataRetrieval)

df <- read_csv("Data/USGSSynoptic.csv") %>% 
  mutate(YEAR=year(Date)) %>% 
  filter(!str_detect(Site, "Spring")) %>% 
  filter(!str_detect(Site, "Pond"))

#time Series
c <- ggplot(df, aes(Date, `CO2_umoles/L_syringe`, colour=Site))+
  geom_line()+
  ylim(50,220)+
  theme_bw()+
  theme(legend.position = "top")
  
d <- ggplot(df, aes(Date, `CH4_umoles/L_syringe`, colour=Site))+
  geom_line()+
  ylim(0,5)+
  theme_bw()+
  theme(legend.position = "top")

c+d+plot_layout(ncol=1)

# stie plots  
a <- ggplot(df, aes(Site, `CO2_umoles/L_syringe`))+
  geom_boxplot()+
  geom_jitter()+
  scale_x_discrete(limits=c("Allequash Creek near Sayner, WI", 
                            "Allequash Creek Synoptic 1",
                            "Allequash Creek Synoptic 2",
                            "Allequash Creek Synoptic 3",
                            "Allequash Wetland near tower",
                            "Allequash Creek Synoptic 4",
                            "ALLEQUASH CREEK SITE NO. 3 NR BOULDER JUNCTION, WI"))+
  ylim(0,210)+
  theme_bw()


b <- ggplot(df, aes(Site, `CH4_umoles/L_syringe`))+
  geom_boxplot()+
  geom_jitter()+
  scale_x_discrete(limits=c("Allequash Creek near Sayner, WI", 
                            "Allequash Creek Synoptic 1",
                            "Allequash Creek Synoptic 2",
                            "Allequash Creek Synoptic 3",
                            "Allequash Wetland near tower",
                            "Allequash Creek Synoptic 4",
                            "ALLEQUASH CREEK SITE NO. 3 NR BOULDER JUNCTION, WI"))+
  ylim(0,10)+
  theme_bw()

a+b+plot_layout(ncol = 1)

# look at discharge

Q<-readNWISdata(sites=c("05357205",  "05357206"), parameterCd =c("00060"), 
                startDate = "2018-10-01", endDate = "2021-11-10", service="iv") %>% 
  mutate(Date=date(dateTime)) %>% 
  mutate(Year=year(Date)) %>% 
  mutate(Time=time(dateTime)) %>% 
  mutate(Discharge=X_00060_00000/35.315) %>% 
  select(Date, Year, site_no, Time, Discharge)
q_sum <- Q %>% 
  group_by(Date, site_no) %>% 
  summarise(`Discharge (m3/sec)`=mean(Discharge)) %>% 
  mutate(Year=as.factor(year(Date))) %>% 
  mutate(Day=yday(Date))

q_long <- pivot_longer(q_sum, cols = c(`Discharge (m3/sec)`)


