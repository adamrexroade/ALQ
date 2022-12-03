# Figure 6 Day Night Comparisons

library(tidyverse)
library(dplyr)
library(lubridate)
library(patchwork)
library(scales)


df <- read_csv("Data/clean.v2.csv")
df <- df %>% 
  filter(Site=="AL2") 

night <- df %>% 
  filter(Time=="Night")


#modifies some dates which were improperly recorded as the following date, rather than as the date which the night started
night[1,1] <- as.Date("2021-06-08")
night[2,1] <- as.Date("2021-06-08")
night[10,1] <- as.Date("2021-07-20")
night[11,1] <- as.Date("2021-07-20")

daynight <- left_join(night, df, by=c("Date", "Depth"))
daynight.1 <- daynight %>% 
  select(Date,Site.y, Depth, Time.y,AirPressure.y,AirTemp.y,Humidity.y,DOSat.y,DOMGL.y,WTemp.y,Q.y,DOC.y,dCO2.y,fCO2.y,dCH4.y,fCH4.y) %>% 
  filter(Time.y=="Day") %>% 
  rename(Site=Site.y,
         Time=Time.y,
         AirPressure=AirPressure.y,
         AirTemp=AirTemp.y,
         Humidity=Humidity.y,
         DOSat=DOSat.y,
         DOMGL=DOMGL.y,
         WTemp=WTemp.y,
         Q=Q.y,
         DOC=DOC.y,
         dCO2=dCO2.y,
         dCH4=dCH4.y,
         fCO2=fCO2.y,
         fCH4=fCH4.y
  )


dn <- bind_rows(daynight.1,night)
dn_long <- pivot_longer(dn, cols = c("dCO2","fCO2","dCH4","fCH4", "DOMGL"))
dn_long <- dn_long %>% 
  mutate(Gas=NA) %>% 
  mutate(Measurment = NA) %>% 
  filter(Depth !="0.5")

for(i in 1:nrow(dn_long)){
  if (dn_long$name[i] == "CO2Sat"){
    dn_long$Gas[i] =c("CO2")
  }else if (dn_long$name[i]=="fCO2"){
    dn_long$Gas[i]=c("CO2")
  }else if (dn_long$name[i]=="dCO2"){
    dn_long$Gas[i]=c("CO2")
  }else if (dn_long$name[i] =="CH4Sat"){
    dn_long$Gas[i]=c("CH4")
  }else if (dn_long$name[i] =="dCH4"){
    dn_long$Gas[i]=c("CH4")
  }else if (dn_long$name[i] =="fCH4"){
    dn_long$Gas[i]=c("CH4")
  } else{
    dn_long$Gas[i]=c("DO")
  }
}


for (i in 1:nrow(dn_long)) {
  if(dn_long$name[i]=="CO2Sat"){
    dn_long$Measurment[i]=c("Saturation")
  }else if(dn_long$name[i]=="dCO2"){
    dn_long$Measurment[i]=c("Concentration (µmol/L)")
  }else if(dn_long$name[i]=="fCO2"){
    dn_long$Measurment[i]=c("Flux (mol/m2/day)")
  }else if(dn_long$name[i]=="CH4Sat"){
    dn_long$Measurment[i]=c("Saturation")
  }else if(dn_long$name[i]=="dCH4"){
    dn_long$Measurment[i]=c("Concentration (µmol/L)")
  }else if(dn_long$name[i]=="fCH4"){
    dn_long$Measurment[i]=c("Flux (mol/m2/day)")
  }else {
    dn_long$Measurment[i]=c("Concentration (mg/L)")
  }
}

dn_long$Depth <- as.factor(dn_long$Depth)


p1 = ggplot(dn_long %>% filter(Measurment == 'Flux (mol/m2/day)')) +
  geom_boxplot(aes(x=Time, y=value, colour=Depth),position = position_dodge(preserve = "single"))+
  #  labs(title = expression("CH"[4])) +
  scale_color_manual(values=c("skyblue2", "indianred2"))+
  facet_wrap(~Gas, scales = 'free_y', ncol = 2)+
  theme_bw() +
  theme(legend.position = "none")+
  #  theme(strip.background = element_rect(fill=alpha("deeppink4",0.2)))+
  # ylab("Flux (mol/m2/day)")+
  ylab(expression(Flux~(mol~m^-2~day^-1)))
xlab("")

p2 = ggplot(dn_long %>% filter(Measurment == 'Concentration (µmol/L)')) +
  geom_boxplot(aes(x=Time, y=value, colour=Depth),position = position_dodge(preserve = "single"))+
  #  labs(title = expression("CO"[2])) +
  scale_color_manual(values=c("skyblue2", "indianred2"))+
  facet_wrap(~Gas, scales = 'free_y', ncol = 2)+
  theme_bw() +
  #  theme(strip.background = element_rect(fill=alpha("dodgerblue4",0.2)))+
  xlab("")+
  # ylab("Concentration (µmol/L)")
  ylab(expression(Concentration~(µmol~L^-1)))

p3 = ggplot(dn_long %>% filter(Measurment == 'Concentration (mg/L)')) +
  geom_boxplot(aes(x=Time, y=value, colour=Depth),position = position_dodge(preserve = "single"))+
  #  labs(title = expression("CO"[2])) +
  scale_color_manual(values=c("skyblue2", "indianred2"))+
  facet_wrap(~Gas, scales = 'free_y', ncol = 2)+
  theme_bw() +
  #  theme(strip.background = element_rect(fill=alpha("dodgerblue4",0.2)))+
  xlab("")+
  # ylab("Concentration (µmol/L)")
  ylab(expression(Concentration~(mg~L^-1)))

layout <- "
AA
BB
CD"
p1+p2+p3 + guide_area()+ plot_layout(guides = 'collect', design =  layout)+plot_annotation(tag_levels = 'A')
ggsave("Figures/Figure6.png", plot = last_plot())