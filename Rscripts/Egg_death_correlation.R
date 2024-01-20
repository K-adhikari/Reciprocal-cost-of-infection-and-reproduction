library(ggplot2)
library(dplyr)

setwd()
Data<- read.csv("Egg_death_correlation_data.csv", header=T)

Data.long<- pivot_longer(Data, cols=3:6, names_to = "Day", values_to = "Eggs")

ggplot(Mated_later_new.long, aes(x= Day, y= Eggs, color=Died.on, shape=Died.on)) + ylim(0,100)+
  geom_jitter (position=position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5))+ theme_bw()+
  theme(axis.text = element_text(size = 13), axis.title.y = element_text(size=14), plot.title = element_text(hjust=0.5))+
  scale_x_discrete(labels = c('Day 1','Day 2','Day 3', 'Day 4'))+ xlab("")+ylab("Number of eggs")+
  labs(color="Flies dead on", shape="Flies dead on")  +ggtitle("I120M")
