library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotrix)
library(reshape2)
library(rstatix)


setwd()
Data<- read.csv("Egg_to_offspring_data.csv", header = T)
New_data<- Data %>% group_by(Treatment, Batch) %>% summarise_at (.vars=c("Eggs", "Offspring"), .funs =sum)
New_data$Females<- c(25, 20, 20, 24, 18, 14, 25, 25, 25, 23, 25, 28, 25, 25, 24, 25, 20, 20, 15, 15, 20, 25, 25, 25) #Numbers represent total females that were used for those treatment in each block
New_data$Mean_eggs <- New_data$Eggs/ New_data$Females
New_data$Mean_offspring <- New_data$Offspring/ New_data$Females
New_data$Egg_to_offspring <- New_data$Mean_offspring/New_data$Mean_eggs


#Plot mean egg and offspring numbers across three experimental blocks
New_data1<- New_data %>% select(Treatment, Mean_eggs, Mean_offspring) %>% group_by(Treatment) %>% summarise_each(funs(mean, sd, std.error))

New_data2<- data.frame(Treatment= c(rep(New_data1$Treatment,2)), variables = c(rep('Eggs', 8), rep('Offspring', 8)),
                       Mean = c(New_data1$Mean_eggs_mean, New_data1$Mean_offspring_mean), 
                       SE = c(New_data1$Mean_eggs_std.error, New_data1$Mean_offspring_std.error))

Plot<- ggplot(New_data2, aes(x= Treatment, y = Mean, fill = variables))+
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + theme_bw()

Plot

#Pairwise comparison for mean number of eggs between treatments
New_data<- ungroup(New_data)
pair <- New_data %>%
  pairwise_t_test(Mean_eggs ~ Treatment, p.adjust.method = "bonferroni")
pair


#Pairwise comparison for mean number of offspring between treatments
pair1 <- New_data %>%
  pairwise_t_test(Mean_offspring ~ Treatment, p.adjust.method = "bonferroni")
pair1


#Pairwise comparison for egg to offspring ratio between treatments
pair2 <- New_data %>%
  pairwise_t_test(Egg_to_offspring ~ Treatment, p.adjust.method = "bonferroni")
pair2


#Plot egg to offspring data
Ratio<- New_data %>% select(Treatment, Egg_to_offspring) %>% group_by(Treatment) %>% summarise_each(funs(mean, sd, std.error))
Plot1<- ggplot(Ratio, aes(x= Treatment, y = mean))+
  geom_bar(stat="identity", fill="white", colour = "black") +
  geom_errorbar(aes(ymin=mean-std.error, ymax=mean+std.error,
                    width=.2)) +theme_classic() + ylab("Egg to offspring ratio") +xlab("")

Plot1


# Testing for difference in mean number of eggs between treatments
Data.aov <- aov(Mean_eggs ~ Treatment, data = New_data)
Data.aov

# Testing for difference in mean number of offspring between treatments
Data1.aov <- aov(Mean_offspring ~ Treatment, data = New_data)
Data1.aov

# Testing for difference in mean egg to offspring ratio between treatments
Data2.aov <- aov(Egg_to_offspring ~ Treatment, data = New_data)
Data2.aov


