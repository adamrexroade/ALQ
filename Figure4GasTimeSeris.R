# Figure 4 Gas Timeseries


library(tidyverse)
library(dplyr)
library(lubridate)
library(patchwork)
library(scales)

df <- read_csv("Data/clean.v2.csv")

df <- df %>% 
  filter(Depth=="0") %>% 
  filter(Time=="Day") %>% 
  mutate(Year=as.factor(year(Date))) %>% 
  mutate(Day=yday(Date)) %>% 
  select(Date,Site,Year,Day,fCO2,fCH4,dCO2,dCH4, Depth)

df_long <- pivot_longer(df, cols = c("fCO2","fCH4","dCO2","dCH4"))
df_long <- df_long[!duplicated(df_long),]


df_long <- df_long %>% 
  mutate(Gas=NA) %>% 
  mutate(Param=NA)

for (n in 1:nrow(df_long)) {
  if(df_long$name[n]=="fCO2"){
    df_long$Gas[n]="CO2"
    df_long$Param[n]="Flux (mol/m2/day)"
  } else if (df_long$name[n]=="dCO2"){
    df_long$Gas[n]="CO2"
    df_long$Param[n]="Concentration (µmol/L)"
  }else if(df_long$name[n]=="fCH4"){
    df_long$Gas[n]="CH4"
    df_long$Param[n]="Flux (mol/m2/day)"
  } else if (df_long$name[n]=="dCH4"){
    df_long$Gas[n]="CH4"
    df_long$Param[n]="Concentration (µmol/L)"
  }
}

df_long <- na.omit(df_long)

df_long_flux <- df_long %>% 
  filter(Param=="Flux (mol/m2/day)")

df_long_conc <- df_long %>% 
  filter(Param=="Concentration (µmol/L)")

flux <- ggplot(df_long_flux,aes(x=as.Date(Day, origin=as.Date("2020-01-01")),y=value))+
  geom_point(aes(colour=Year, shape=Site), size=2.5)+
  geom_line(aes(colour=Year, shape=Site))+
  scale_shape_manual(values = c(21,22,24))+
  scale_color_manual(values=c("darkseagreen4", "sandybrown"))+
  facet_wrap(~Gas, scales = "free", nrow = 1)+
  xlab("Date")+
  scale_x_date(labels = date_format("%b"), date_breaks = '1 month')+
  # ylab("Flux (mol/m2/day)")+
  ylab(expression(Flux~(mol~m^-2~day^-1)))+
  theme_bw() 
#  theme(strip.background = element_rect(fill=alpha("deeppink4",0.2)))
conc <- ggplot(df_long_conc,aes(x=as.Date(Day, origin=as.Date("2020-01-01")),y=value))+
  geom_point(aes(colour=Year, shape=Site), size=2.5)+
  geom_line(aes(colour=Year, shape=Site))+
  scale_shape_manual(values = c(21,22,24))+
  scale_color_manual(values=c("darkseagreen4", "sandybrown"))+
  #  labs(title = expression("CH"[4])) +
  facet_wrap(~Gas, scales = "free", nrow = 1)+
  xlab("Date")+
  scale_x_date(labels = date_format("%b"), date_breaks = '1 month')+
  # ylab("Concnentration (µmol/L)")+
  ylab(expression(Concentration~(µmol~L^-1)))+
  theme_bw() 
#  theme(strip.background = element_rect(fill=alpha("deeppink4",0.2)))

spatial <- ggplot(df_long_conc) +
  geom_boxplot(aes(x=Site, y=value, colour=Year))+
  facet_wrap(~Gas, scales = "free_y")+
  ylab(expression(Concentration~(µmol~L^-1)))+
  scale_color_manual(values=c("darkseagreen4", "sandybrown"))+
  theme_bw()

layout <- "
A
B
C"

conc+flux+spatial+plot_layout(design = layout, guides = "collect")+plot_annotation(tag_levels = 'A')
ggsave("Figures/Figure4.png", plot=last_plot())

#### Friedman Test ####

df <- read_csv("Data/clean.v2.csv")
df <- df %>% 
  filter(Depth=="0") %>% 
  filter(Time=="Day") %>% 
  mutate(Year=as.factor(year(Date))) %>% 
  mutate(Week=as.numeric(week(Date))) %>% 
  mutate(Day=yday(Date)) %>% 
  select(Date,Site,Year,Week,Day,fCO2,fCH4,dCO2,dCH4, Depth)


library(PMCMRplus)
df.na <- na.omit(df) %>% 
  mutate(ID= paste(Year,Site))
 df.na$Date <- as.factor(df.na$Date)
df.1 <-  as.data.frame(df.na) 

#co2
df.2 <- df.1 %>% 
  select(Date,ID, dCO2)
df.2_co2 <- pivot_wider(df.2, names_from = ID, values_from = dCO2) %>% 
  # na.omit() %>% 
  mutate(Week=week(as.Date(Date))) %>% 
  mutate(Date=NULL)

co2fried <- df.2_co2 %>% 
  group_by(Week) %>% 
  fill(`2020 AL1`, `2020 AL2`, `2020 AL3`, `2021 AL1`, `2021 AL2`, `2021 AL3`) %>% 
  fill(`2020 AL1`, `2020 AL2`, `2020 AL3`, `2021 AL1`, `2021 AL2`, `2021 AL3`, .direction = 'up') %>% 
  distinct
friedman.test(as.matrix(co2fried))

co2nem <- co2fried %>% 
  na.omit() %>% 
  pivot_longer(everything(), names_to = "ID", values_to = 'values' )

frdAllPairsNemenyiTest(as.matrix(co2fried$))

#ch4
df.3 <- df.1 %>% 
  select(Date,ID, dCH4)
df.3_ch4 <- pivot_wider(df.3, names_from = ID, values_from = dCH4) %>% 
  # na.omit() %>% 
  mutate(Week=week(as.Date(Date))) %>% 
  mutate(Date=NULL)

ch4fried <- df.3_ch4 %>% 
  group_by(Week) %>% 
  fill(`2020 AL1`, `2020 AL2`, `2020 AL3`, `2021 AL1`, `2021 AL2`, `2021 AL3`) %>% 
  fill(`2020 AL1`, `2020 AL2`, `2020 AL3`, `2021 AL1`, `2021 AL2`, `2021 AL3`, .direction = 'up') %>% 
  distinct
friedman.test(as.matrix(ch4fried))
frdAllPairsNemenyiTest(as.matrix(ch4fried))
