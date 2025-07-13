library(ggpubr)
setwd()
Data<- read.csv("Correlation_data.csv", header = T)

#Correlation between egg numbers and bacterial load
Plot1<- ggscatter(Total, x = "Egg", y = "ln", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Egg", ylab = "ln(bacterial load))")
Plot1


#Correlation between egg to offspring ratio and bacterial load

Plot2<- ggscatter(Total, x = "Egg_offspring.ratio", y = "ln", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Egg to offspring ratio", ylab = "ln(bacterial load))")
Plot2
