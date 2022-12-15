library(tidyverse)
library(dplyr)
library(lubridate)
library(patchwork)
library(dataRetrieval)
library(broom)
library(ggpubr)


# Expore USGS Data Release ----
df <- read_csv("USGS/USGSSynoptic.csv") %>% 
  mutate(YEAR=year(Date)) %>% 
  filter(!str_detect(Site, "Spring")) %>% 
  filter(!str_detect(Site, "Pond")) %>% 
  filter(!str_detect(Site, "tower"))

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

# pull in gage data from q_wide and add to USGS data

data2 <- left_join(df, q_wide, by="Date") %>% 
  rename(CO2=`CO2_umoles/L_syringe`,
         CH4=`CH4_umoles/L_syringe`) %>% 
  mutate(AKA=NULL,
         YEAR=NULL)

data2_long <- data2 %>% pivot_longer(cols = c("down","up","qdif"), names_to = ("Location")) %>% 
  rename(Q=value)

data3<- data2_long %>% pivot_longer(cols = c("CO2","CH4"), names_to = "Gas") %>% 
  rename(Concentration=value)

data3 <- data3 %>% 
  filter(!str_detect(Site, "Synoptic"))
ggplot(data3, aes(Q, Concentration, colour=Site))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  facet_wrap(Gas~Location, scales = "free")+
  theme_bw()+
  theme(legend.position = "bottom")
ggsave("USGS/Figures/USGSDataVsDischarge.png", plot=last_plot())



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

# differnce in discharge between up and down
ggplot(q_long, aes(Date, value, colour=name))+
  geom_line()+
  theme_bw()
  geom_ribbon(data=q_wide, aes(ymin=up, ymax=down))



# Excess saturation ----

df <- read_csv("Data/clean.v2.csv")
atsat <- read_csv("Data/seasonalsaturated.csv")

data <- left_join(df,atsat, by=c("Date","Site", "Depth")) %>% 
  mutate(excessCO2=dCO2-CO2,
         excessCH4=dCH4-CH4)

ggplot(data, aes(excessCO2, excessCH4, color=Site))+
  geom_point()+
  geom_smooth(method = 'lm', se=FALSE)+
#  geom_abline(intercept = -20, slope =1 )+
  ylab("Excess CH4 (umol/L)")+
  xlab("Excess CO2 (umol/L)")+
  theme_bw()
ggsave("USGS/Figures/ExcessCO2vsExcessCH4.png", plot = last_plot())
data1 <- left_join(data,q_wide, by="Date")


## Excess gas vs discharge -----
a1 <- ggplot(data1, aes(qdif,excessCO2))+
  geom_point()+
  geom_smooth(method = lm)+
  stat_regline_equation(label.x.npc = "left")+
  xlab("Q Difference (m3/s)")+
  ylab("Excess CO2 (umol/L)")+
  facet_wrap(~Site)+
  theme_bw()
ggsave("USGS/Figures/DiscahrgeDiffvsExcessCO2.png", plot = last_plot())
a2 <- ggplot(data1, aes(qdif,excessCH4))+
   geom_point()+
  geom_smooth(method = lm)+
  stat_regline_equation(label.x.npc = "left")+
  xlab("Q Difference (m3/s)")+
  ylab("Excess CH4 (umol/L)")+
  facet_wrap(~Site)+
  theme_bw()
ggsave("USGS/Figures/DiscahrgeDiffvsExcessCH4.png", plot = last_plot())


a1+a2+plot_layout(ncol=1)
ggsave("USGS/Figures/DischargevsExcessGas.png", plot = last_plot())

# Q Conc Plot ----

a <- ggplot(data1, aes(fill=Site))+
  geom_point(aes(up,dCO2), shape=24)+
  theme_bw()+
  theme(legend.position="none")+
  xlab("")
b <- ggplot(data1, aes(fill=Site))+
  geom_point(aes(down,dCO2), shape=25)+
  theme_bw()+
  xlab("")+
  ylab("")

c <- ggplot(data1, aes(fill=Site))+
  geom_point(aes(up,dCH4), shape=24)+
  theme_bw()+
  theme(legend.position="none")
d <- ggplot(data1, aes(fill=Site))+
  geom_point(aes(down, dCH4), shape=25)+
  theme_bw()+
  ylab("")

a+b+c+d+plot_layout(nrow = 2)
ggsave("USGS/Figures/QConc.png", plot = last_plot())

# compare yearly average from both datasets to total discharge

gs <- read_csv("USGS/USGSSynoptic.csv") %>% 
  mutate(YEAR=year(Date)) %>% 
  filter(!str_detect(Site, "Spring")) %>% 
  filter(!str_detect(Site, "Pond")) %>% 
  filter(!str_detect(Site, "tower")) %>% 
  filter(!str_detect(Site, "Synoptic")) %>% 
  mutate(AKA=NULL) %>% 
  rename(dCO2=`CO2_umoles/L_syringe`,
         dCH4=`CH4_umoles/L_syringe`)

df <- read_csv("Data/clean.v2.csv") %>% 
  filter(Depth=="0") %>% 
  mutate(
    Time=NULL,
    AirPressure=NULL,
    AirTemp=NULL,
    Humidity=NULL,
    DOSat=NULL,
    DOMGL=NULL,
    WTemp=NULL,
    Q=NULL,
    DOC=NULL,
    fCO2=NULL,
    fCH4=NULL,
    Depth=NULL,
    YEAR=year(Date)
  )

alldata <- bind_rows(df,gs)



q_year_sum <- q_wide %>% 
  group_by(Year) %>% 
  summarise(Cum_Q_up=mean(up, na.rm=TRUE),
            Cum_Q_down=mean(down, na.rm=TRUE)) %>% 
  rename(YEAR=Year)
q_year_sum[1,1] <- 2018
q_year_sum[2,1] <- 2019
q_year_sum[3,1] <- 2020
q_year_sum[4,1] <- 2021



data <- left_join(alldata,q_year_sum, by="YEAR")
data$YEAR <- as.factor(data$YEAR)
ggplot(data, aes(YEAR, dCO2, colour=Site))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position = "bottom")