rm(list=ls())
library(readxl)
library(tidyverse)
library(bbplot)
dat <- read_excel("data/vaccineff_plot.xlsx", sheet = "plot")


ggplot(data = dat) +
  geom_line(aes(x = days_since_vaccine, control_y), colour = "blue", size = 1) +
  geom_point(aes(x = days_since_vaccine, control_y), colour = "black", size = 6, shape = 22, fill = "deepskyblue") +

  geom_line(aes(x = days_since_vaccine, vaccine_y), colour = "red", size = 1) +
  geom_point(aes(x = days_since_vaccine, vaccine_y), colour = "black", size = 6, shape = 22, fill = "red") +
  theme_minimal(10) +
  xlab("") + ylab ("")
  # theme(axis.text.x=element_blank(), #remove x axis labels
  #       axis.ticks.x=element_blank(), #remove x axis ticks
  #       axis.text.y=element_blank(),  #remove y axis labels
  #       axis.ticks.y=element_blank()  #remove y axis ticks
  # )


