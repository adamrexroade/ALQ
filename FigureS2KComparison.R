# Figure S2 K validation

# Compare modeled k values to measured k values

#load libraries
library(tidyverse)
library(dplyr)
library(ggpubr)
library(patchwork)


kvalues  <- read_csv("Data/k.csv")


# make fake data for the legend
legenddata <- data.frame(x=c(5,5),
                         y=c(10,10),
                         Gas=c(4,2))
a <- ggplot(kvalues)+
  geom_point(aes(x=k.co2.model, y=k600co2chamber), color="dodgerblue4")+
  geom_smooth(aes(x=k.co2.model, y=k600co2chamber), color="dodgerblue4", method='lm')+
  stat_regline_equation(aes(x=k.co2.model, y=k600co2chamber), label.x=1.9, label.y=3, color="dodgerblue4", size=5)+
  stat_cor(aes(x=k.co2.model, y=k600co2chamber), label.x=1.9, label.y=2.8, color="dodgerblue4", size=5)+
  ylim(0,3.5)+
  ylab(expression(Measured~k~(m~day^-1)))+
  xlab(expression(Modeled~k~(m~day^-1)))+
  theme_bw(base_size = 14)
b <- ggplot(kvalues)+
  geom_point(aes(x=k.ch4.model, y=k600ch4chamber), color="deeppink4")+
  geom_smooth(aes(x=k.ch4.model, y=k600ch4chamber), color="deeppink4", method='lm')+
  stat_regline_equation(aes(x=k.ch4.model, y=k600ch4chamber), label.x=1.9, label.y=2.7, color="deeppink4", size=5)+
  stat_cor(aes(x=k.ch4.model, y=k600ch4chamber), label.x=1.9, label.y=2.5, color="deeppink4", size=5)+
  ylim(0,3.5)+
  ylab(expression(Measured~k~(m~day^-1)))+
  xlab(expression(Modeled~k~(m~day^-1)))+
  theme_bw(base_size = 14)


a+b+
  plot_layout(guides ='collect')+plot_annotation(tag_levels = 'A')

ggsave("Figures/FigureS2.png", plot=last_plot())
