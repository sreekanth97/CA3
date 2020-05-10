#importing required packages
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(maps)
library(mapdata)
library(sp)
library(maptools)
library(rgdal)
library(mapproj)
library(scales)
library(treemapify)
library(ckanr)
library(tidyverse)
library(jsonlite)
library(magrittr)
library(readr) 
library(dplyr)
library(openxlsx)
library(foreign)
library(knitr)



#Loading datasets 

df <- read.csv("Final_data.csv", header = TRUE, na.strings = c("","NA"))


Finaldata <- df
replaceCommas<-function(x){x<-as.numeric(gsub("\\,","",x))}
Finaldata$Price.... <- replaceCommas(Finaldata$Price....)

#Structure of the data


str(Finaldata)


#using mice
library(mice)
md.pattern(Finaldata)

#using VIM
library(VIM)
missing_values <- aggr(Finaldata, prop = FALSE, numbers = TRUE)
summary(missing_values)

matrixplot(Finaldata)




#install.packages("Hmisc")
#library("Hmisc")



library(corrplot)

#correlation plot
library(ggplot2) 
hist(Finaldata$temp, main = "Price vs Postal code vs Date", xlab = "Postal.Code")
plot <- ggplot(Finaldata, aes(x = Postal.Code, y = Price.... ))
plot <- plot + stat_smooth(method = "lm", col = "darkblue", se = FALSE)
plot <- plot + geom_point()
print(plot)

#subset numeric data
Finaldata1 <- subset(Finaldata, select=c(Postal.Code,Price....,Date.of.Sale..dd.mm.yyyy.))
colnames(Finaldata1)
str(Finaldata1)


#PCA
pca <- prcomp(Finaldata1, center = TRUE, scale. = TRUE)
summary(pca)
str(pca)
library("factoextra")
eig_values <- get_eigenvalue(pca)
eig_values
library("FactoMineR")
pca2 <- PCA(Finaldata1, graph = FALSE)
print(pca2)
pca2_eig_values <- get_eigenvalue(pca2)
pca2_eig_values
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50))
pca_for_variables <- get_pca_var(pca)
pca_for_variables
library("corrplot")
corrplot(pca_for_variables$cos2, is.corr = FALSE)
fviz_pca_var(pca, col.var = "black")
head(pca_for_variables$cos2, 10)
fviz_cos2(pca, choice = "var", axes = 1:2)
fviz_pca_var(pca, col.var = "cos2",
             gradient.cols = c("red", "Blue", "Green"), 
             repel = TRUE # Avoid text overlapping
)  
head(pca_for_variables$contrib, 20)
fviz_pca_var(pca, col.var = "contrib",
             gradient.cols = c("red", "Blue", "Green"),
)


#Q-Q plot
qqnorm(Finaldata1$Postal.Code)
qqline(Finaldata1$Postal.Code, col = 'blue')

#power analysis
#install.packages("pwr")
library(pwr)
effective_size <- cohen.ES(test = "r", size = "large")
effective_size

power_analysis <-pwr.t.test(d=0.5,n=NULL,sig.level=0.05,  power=0.95, type="one.sample",alternative="two.sided")
power_analysis
#plotting power analysis
plot(power_analysis)

#Hypothesis Testing
test <- cor.test(Finaldata$Price...., Finaldata$Postal.Code,
                 method = 'spearman', exact = FALSE) 

test




















