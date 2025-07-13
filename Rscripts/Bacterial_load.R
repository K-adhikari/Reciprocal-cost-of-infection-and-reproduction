library(ggplot2)
library(tidyverse)
library(lmerTest)
library(lme4)
library(car)


setwd()
Data<- read.csv("Bacterial_load.csv", header=T)

# Figuring out the highest number of CFU to replace the CFU value in rows with flag "T"
max(Data$Count.per.Frame, na.rm = TRUE)

# Assigning the numbers for rows with flag "T" i.e. too many colonies to count
Updated_data<- Data %>% mutate(Count.per.Frame = case_when(Flags %in% c('T') ~ '485',
                                                                       TRUE ~ as.character(Count.per.Frame)),
                                           Count.per.ml = case_when(Flags %in% c('T') ~ '225000',
                                                                    TRUE ~ as.character(Count.per.ml)),
                                           Count_per_fly = case_when (Flags %in% c('T') ~ '112500',
                                                                      TRUE~ as.character(Count_per_fly)),
                                           ln_count = case_when(Flags %in% c('T') ~ '11.630717',
                                                                TRUE~as.character(ln_count)))

Updated_data$Count.per.Frame<- as.numeric(Updated_data$Count.per.Frame)
Updated_data$Count.per.ml<- as.numeric(Updated_data$Count.per.ml)
Updated_data$Count_per_fly<- as.numeric(Updated_data$Count_per_fly)
Updated_data$ln_count<- as.numeric(Updated_data$ln_count)


#Plotting the data
Updated_data$Treatment<- factor(Updated_data$Treatment, levels=c("Control", "Unmated","Mated"))
Updated_data$Time<- as.factor(Updated_data$Time)

ggplot(Updated_data, aes(Time, ln_count, color=Treatment)) + geom_boxplot(width=0.5, position=position_dodge(0.8))+ geom_point(size=1, position = position_jitterdodge())+
  theme_bw()+
  ylab("ln(CFU/fly)") +xlab("Time post mating (in hours)") + stat_summary(fun.y = mean,
                                                                          geom = "point", shape = 18, size = 4,
                                                                          show.legend = FALSE)


#Mixed effect models to compare effect of experimental block
New_data<- Updated_data[!Updated_data$Treatment=="Control",]
New_data$Block<- as.factor(New_data$Block)

Model1<-lmer(ln_count~(1|Block) + Treatment, New_data)
Model2<-lm(ln_count~  Treatment, New_data)
anova(Model1, Model2)
summary(Model1)


#Levene's test to compare if the variances are significantly different from each other or not
leveneTest(ln_count ~ Treatment, New_data)

#Comparing variance between mated and unmated treatments
New_data %>% group_by(Treatment) %>% summarise(var=var(ln_count))



#Welch's t-est the difference in mean between two treatment groups as they have signifincant difference in their variance
t.test(ln_count~Treatment, New_data)

###Fisher's exact test on the number of dead individuals between mated and unmated groups. First numbers in matrix represent dead and second number represent alive individuals
A<- matrix(c(7,208, 107,247), nrow = 2)
B<- fisher.test(A)
B

