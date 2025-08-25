library(survival)
library(ggplot2)
library(ggfortify)
library(dplyr)
library(survminer)
library (ggpubr)
library(splitstackshape)
library(data.table)
library(coxme)
library(multcomp)
library(emmeans)

setwd()
Data<- read.csv("Survival_data.csv", header = T)
Data_all<- expandRows(Data, "Flies")
Data_all$Treatment<- as.factor(Data_all$Treatment)



##Survival plot

Km_fit <- survfit(Surv(Hours, Censor) ~ Treatment, data=Data_all)
survp<- ggsurvplot(Km_fit, data = Data_all, risk.table = TRUE, legend="right", risk.table.col="strata",
                   risk.table.y.text = FALSE,
                   risk.table.height=0.35, xlim=c(0,240), break.x.by=24)
survp

pdf("survplot_all.pdf", width = 10, height = 5)
print(survp$plot, newpage = FALSE)
dev.off()



##Cox mixed effect model

fit_m<- coxme(formula= Surv(Hours, Censor) ~ Treatment + (1|Block), data=Data_all)
summary(fit_m)



##Compare survival between treatments

A<- emmeans(fit_m, pairwise~Treatment)
A
A_means<- emmeans(fit_m, "Treatment")
A_means_cld<- cld(A_means, Letters= letters, alpha = 0.05)
A_means_cld
