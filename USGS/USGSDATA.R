library(tidyverse)
library(dplyr)
library(lubridate)
library(patchwork)
library(dataRetrieval)


# Expore USGS Data Release ----
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

# look at discharge ----

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

q_wide <- pivot_wider(q_sum, names_from = site_no, values_from = `Discharge (m3/sec)`) %>% 
  rename(up=`05357205`,
         down=`05357206`) %>% 
  mutate(qdif=down-up)


q_long <- pivot_longer(q_wide, cols = c("up","down","qdif"))

ggplot(q_long, aes(Date, value, colour=name))+
  geom_line()
  geom_ribbon(data=q_wide, aes(ymin=up, ymax=down))



# Excess saturation ----

df <- read_csv("Data/clean.v2.csv")
atsat <- read_csv("Data/seasonalsaturated.csv")

data <- left_join(df,atsat, by=c("Date","Site", "Depth")) %>% 
  mutate(excessCO2=dCO2-CO2,
         excessCH4=dCH4-CH4)

ggplot(data, aes(excessCO2, excessCH4, color=Site))+
  geom_point()+
  geom_abline(intercept = -20, slope =1 )+
  ylab("Excess CH4 (umol/L)")+
  xlab("Excess CO2 (umol/L)")

data1 <- left_join(data,q_wide, by="Date")


ggplot(data1, aes(qdif,excessCO2))+
  geom_point()+
  facet_wrap(~Site)
ggplot(data1, aes(qdif,excessCH4))+
   geom_point()+
  geom_smooth(method = lm)+
  facet_wrap(~Site)
  


