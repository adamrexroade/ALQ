library(tidyverse)
library(dplyr)
library(lubridate)
library(patchwork)

df <- read_csv("Data/USGSSynoptic.csv") %>% 
  mutate(YEAR=year(Date)) %>% 
  filter(!str_detect(Site, "Spring")) %>% 
  filter(!str_detect(Site, "Pond"))

#time Series
c <- ggplot(df, aes(Date, `CO2_umoles/L_syringe`, colour=Site))+
  geom_line()+
  theme(legend.position = "top")+
  ylim(50,220)
  
d <- ggplot(df, aes(Date, `CH4_umoles/L_syringe`, colour=Site))+
  geom_line()+
  theme(legend.position = "top")+
  ylim(0,5)

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
  ylim(0,210)


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
  ylim(0,10)

a+b+plot_layout(ncol = 1)

