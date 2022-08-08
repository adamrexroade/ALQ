#Figure 5 Random Forest Results

library(tidyverse)
library(lubridate)
library(corrplot)
library(randomForest)
library(vip)
library(pdp)
library(patchwork)
library(dataRetrieval)


# Data Prep ----
al2 <- read_csv("Data/clean.v2.csv") %>% 
  filter(Site=="AL2")

## add turbidity and ph data ###

usgs <-readNWISdata(sites=c("05357205"), parameterCd =c("63680", "00400"), 
                    startDate = "2020-04-01", endDate = "2021-10-01", service="iv")
usgs.mod <- usgs %>% 
  mutate(Date=date(dateTime)) %>% 
  group_by(Date) %>% 
  summarize(TURBIDITYFNU=mean(X_63680_00000),
            pH=mean(X_00400_00000))

al2 <- left_join(al2, usgs.mod)


# subset numerical parameters only, depth is considered categorical due to only having two factors
al2_num <-  as.data.frame(al2) %>% 
  select(AirPressure, AirTemp, Humidity, DOSat, DOMGL, WTemp, Q, DOC, dCO2,fCO2,dCH4, fCH4, TURBIDITYFNU, pH)
al2_num_no.na <- na.omit(al2_num)


# Random Forest for CO2 Concentration ----
rf_dco2_df <- al2_num %>% 
  select(AirPressure, AirTemp, WTemp, Q, DOMGL, Humidity, TURBIDITYFNU, pH, dCO2) %>% 
  rename(Discharge=Q,
         DO= DOMGL,
         Turbidity=TURBIDITYFNU)
rf_dco2_df <- na.omit(rf_dco2_df)
co2conc_rf <- randomForest(dCO2~., rf_dco2_df, importance=TRUE)
co2conc_rf

## Partial Dependence Plots----
partial.list <- list()
var <- as.vector(colnames(rf_dco2_df))

for (i in 1:8) {
  varname = var[i]
  partial.list[[i]] = data.frame(pdp::partial(co2conc_rf, pred.var = varname))%>% 
    rename(var = varname) %>% mutate(pred = varname) 
  
}

partial.df = do.call(rbind, partial.list)

co2conc.pdp <- ggplot(partial.df)+
  geom_line(aes(x = var, y = yhat))+
  geom_point(aes(x = var, y = yhat))+
  facet_wrap(~pred, scales = "free_x")+
  theme_bw()+
  theme(strip.background = element_rect(fill=alpha("dodgerblue4",0.1)))+
  ylab("Feature Contribution")+
  xlab("")

## Variable Importance ----
vi <- vi(co2conc_rf) %>%   # consider different ways to asses importance
  mutate(group="CO2 Concentration Variable Importance")

co2conc.vi <- ggplot(vi) +
  geom_col(aes(x = reorder(Variable, -Importance), y = Importance)) +
  xlab('Variable')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  +
  facet_wrap(~group)+
  theme(strip.background = element_rect(fill=alpha("dodgerblue4",0.1))) 


## Observe vs predicted ----
dco2_ob_pr <- data.frame(predict(co2conc_rf), rf_dco2_df$dCO2)
co2conc.ob.vs.pr <- ggplot(dco2_ob_pr)+
  geom_point(aes(x=rf_dco2_df.dCO2, y=predict.co2conc_rf.))+
  geom_abline(slope = 1, intercept = 0, color="dodgerblue4", size=1 )+
  theme_bw()+
  xlab("Observed CO2 
       Concentration (µmol/L)")+
  ylab("Predicted CO2 
       Concentration (µmol/L)")


# Random Forest for CH4 Concentration ----
rf_dch4_df <- al2_num %>% 
  select(AirPressure, AirTemp, WTemp, Q, DOMGL, Humidity,TURBIDITYFNU,pH,dCO2, dCH4) %>% 
  rename(Discharge=Q,
         DO= DOMGL,
         Turbidity=TURBIDITYFNU,
         CO2Concentration=dCO2)
rf_dch4_df <- na.omit(rf_dch4_df)
ch4conc_rf <- randomForest(dCH4~., rf_dch4_df, importance=TRUE)
ch4conc_rf

## Partial Dependence Plots----
partial.list <- list()
var <- as.vector(colnames(rf_dch4_df))

for (i in 1:9) {
  varname = var[i]
  partial.list[[i]] = data.frame(pdp::partial(ch4conc_rf, pred.var = varname))%>% 
    rename(var = varname) %>% mutate(pred = varname) 
  
}

partial.df = do.call(rbind, partial.list)

ch4conc.pdp <- ggplot(partial.df)+
  geom_line(aes(x = var, y = yhat))+
  geom_point(aes(x = var, y = yhat))+
  facet_wrap(~pred, scales = "free_x")+
  theme_bw()+
  theme(strip.background = element_rect(fill=alpha("deeppink4",0.1))) +
  ylab("Feature Contribution")+
  xlab("")

## Variable Importance ----
vi_ch4conc <- vi(ch4conc_rf) %>% 
  mutate(group="CH4 Concentration Variable Importance")

ch4conc.vi <- ggplot(vi_ch4conc) +
  geom_col(aes(x = reorder(Variable, -Importance), y = Importance)) +
  xlab('Variable')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  +
  facet_wrap(~group)+
  theme(strip.background = element_rect(fill=alpha("deeppink4",0.1))) 



## Observe vs predicted ----
dch4.ob.pr <- data.frame(predict(ch4conc_rf), rf_dch4_df$dCH4)
ch4conc.ob.vs.pr <- ggplot(dch4.ob.pr)+
  geom_point(aes(x=rf_dch4_df.dCH4, y=predict.ch4conc_rf.))+
  geom_abline(slope = 1, intercept = 0, color="deeppink4", size=1 )+
  theme_bw()+
  xlab("Observed CH4 
       Concentration (µmol/L)")+
  ylab("Predicted CH4 
       Concentration (µmol/L)")


layout <- "
ABB
CBB
DEE
FEE
"
ch4conc.ob.vs.pr+ch4conc.pdp+
  ch4conc.vi+
  co2conc.ob.vs.pr+co2conc.pdp+
  co2conc.vi+
  plot_layout(design = layout)+
  plot_annotation(tag_levels = 'A')




ggsave("Figures/Figure5.png", plot=last_plot())