###################################
## Biodiversity Exploratories    ##
## Introduction to stats with R  ##
## code by Caterina Penone       ##
## caterina.penone@ips.unibe.ch  ##
## Long version                  ##
###################################

install.packages(c("ggplot2", "MASS", "effects", "car", "corrplot", "GGally"))

library(corrplot)
library(car)
library(GGally)
library(effects)

# The folder structure is assumed to be identical to the folder structure in GitHub,
#  see https://github.com/biodiversity-exploratories-synthesis/Synthesis_courses_workshops
#  the required dataset is saved in the same folder as this script.
getwd() # double-check if your working directory is where you expect it to be 
        #  --> wherever you store this script and the according dataset.
setwd("Help_desk_Fall_2021/")

##### Data exploration #####
#read arthropod - plant dataset
dat <- read.table("example_data_linear_models.txt", h=T, sep="\t")

#have a look at the data
dim(dat)
names(dat)
head(dat)
summary(dat)

# check data types in the data
str(dat)
# 3 columns are characters, but should be factors
# region
#recode the regions names (to avoid problems with special characters)
dat$Region <- sub("Schwäbische Alb", "ALB", dat$Region)
dat$Region <- sub("Schorfheide-Chorin", "SCH", dat$Region)
dat$Region <- sub("Hainich-Dün", "HAI", dat$Region)
class(dat$Region) #it's a character
dat$Region <- as.factor(dat$Region) # convert to factor
levels(dat$Region) <- c("HAI", "SCH", "ALB")
# PlotID
dat$PlotID <- as.factor(dat$PlotID)
# Lifestock
dat$Lifestock <- as.factor(dat$Lifestock)

#explore the dataset with simple plots
plot(dat$Region)
hist(dat$Plant_SpeciesRichness)

plot(Plant_SpeciesRichness~Grazing, data=dat)
plot(Plant_SpeciesRichness~Herbivore_SpeciesRichness, data=dat)
plot(Plant_SpeciesRichness~Mowing, data=dat)
plot(Plant_SpeciesRichness~Fertilization, data=dat)

#an outlier.. which plot is that?
text(Plant_SpeciesRichness~Fertilization, data=dat, labels=PlotID, cex=0.7, pos=3)

#check HEG01 in the dataset
dat[dat$PlotID == "HEG01",]
hist(dat$Fertilization)

#it does not seem realistic, it is an outlier that we can probably remove
dat <- dat[dat$PlotID != "HEG01",]
dim(dat) #one plot less

#we plot the data again
plot(Plant_SpeciesRichness~Fertilization, data=dat)

#two categorical variables
plot(dat$Region, dat$Lifestock)

#one categorical, one numerical variable
plot(Plant_SpeciesRichness~Region, data=dat)

#standardize the LUI components and calculate LUI
dat$Fstd <- sapply(dat$Fertilization, function(x)x/mean(dat$Fertilization,na.rm=T))
#sapply(dat$Fertilization, function(x)x/mean(dat$Fertilization,na.rm=T)) means: apply to each 
#element (x) of dat$Fertilization a function that divides x by the mean of dat$Fertilization and return a vector
#that will be a new column in my dataset called Fstd (dat$Fstd)
dat$Gstd <- sapply(dat$Grazing, function(x)x/mean(dat$Grazing,na.rm=T))
dat$Mstd <- sapply(dat$Mowing, function(x)x/mean(dat$Mowing,na.rm=T))

#LUI is the square root of the sum of the 3 components
dat$LUI <- sqrt(rowSums(dat[,c("Fstd","Gstd","Mstd")], na.rm=T))

plot(Plant_SpeciesRichness~LUI, data=dat)

#we want to know how correlated are the variables in our dataset
#correlation matrix, spearman correlations (all variables are not normally distributed)
M <- cor(dat[,c(3:12,14:17)], method="spearman") #we select the numerical ones
pairs(dat[,c(3,4,5,7,9,10,11,12)], pch = 21)
pairs(dat[,c(7,9,10,11)], pch = 21) #fewer
ggpairs(dat[,c(7,9,10,11)])
M <- cor(dat[,c(3,7,9,10,11)], method="spearman")
corrplot(M)
corrplot(M, addCoef.col = "black")

#Explore Livestock variable
plot(LUI~Lifestock, data=dat)
summary(dat$Lifestock) #this variable is very unbalanced, only one point for sheep/goat

dat$Lifestock <- gsub(pattern="cattle_horse", replacement="cattle", x=dat$Lifestock)
dat$Lifestock <- gsub(pattern="sheep/goat", replacement="sheep", x=dat$Lifestock)
dat$Lifestock <- as.factor(dat$Lifestock)
summary(dat$Lifestock) #now the categories are more balanced


##### Linear model #####

#simplest linear model
mod0 <- lm(Plant_SpeciesRichness ~ LUI, data=dat)

#explore the results
summary(mod0)

#calculate residual standard error
k <- length(mod0$coefficients)-1 #Subtract one to ignore intercept
SSE <- sum(mod0$residuals**2) 
n <- length(mod0$residuals)
sqrt(SSE/(n-(1+k))) #Residual Standard Error

#calculate multiple R-Squared
SSyy <- sum((dat$Plant_SpeciesRichness-mean(dat$Plant_SpeciesRichness))**2)
SSE <- sum(mod0$residuals**2)
(SSyy-SSE)/SSyy

#calculate adjusted R-squared
n <- length(dat$Plant_SpeciesRichness)
k <- length(mod0$coefficients)-1 #Subtract one to ignore intercept
1-(SSE/SSyy)*(n-1)/(n-(k+1))

#extract model information
coef(mod0)
fitted(mod0)
residuals(mod0)
predict(mod0, interval="confidence")

#### Check model assumptions ####
hist(residuals(mod0))
shapiro.test(residuals(mod0))
# interpretation of p-values form shapiro.test : 
# p > 0.05 : data distribution does not differ from normality
#   --> assume normality.
# p < 0.05 : deviance from normality --> don't assume normality
# http://www.sthda.com/english/wiki/normality-test-in-r
qqnorm(residuals(mod0))
qqline(residuals(mod0))

par(mfrow=c(2,2)) #with this all graphs will be in the same plot (see plot course on Friday)
plot(mod0)
dev.off() # make plots disappear and have graphs in their own's windows again

#influential Observations, Cook's D plot - identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(dat)-length(mod0$coefficients)-2)) # cutoff at 0.03361345
plot(mod0, which=4, cook.levels=cutoff)

#### Transformation of the response variable ####
#for this example we will use the richness of predators as response
plot(dat$Predator_SpeciesRichness ~ dat$Herbivore_biomass)
mod0.1 <- lm(Predator_SpeciesRichness ~ Herbivore_biomass, data=dat)
summary(mod0.1)
par(mfrow=c(2,2))
plot(mod0.1)
plot(lm(log(Predator_SpeciesRichness+1)~LUI,data=dat))
hist(dat$Predator_SpeciesRichness)
hist(log(dat$Predator_SpeciesRichness))

#### Check outliers ###
dat[22,] #it seems that species richness is very high, but it's normal
dat[100,] #any reason for this plot to be an outlier?
hist(dat$Plant_SpeciesRichness)

#### Visualise results ####
#visualise results with the effects package
ef <- effect("LUI", mod0)
plot(ef)

#extract effects to plot them with ggplot
efdf <- data.frame(ef)


#### Partial linear regression ####
#Two continuous variables
mod1 <- lm(Plant_SpeciesRichness ~ LUI + Herbivore_SpeciesRichness, data=dat)
summary(mod1)
anova(mod1)
plot(effect("LUI", mod1))
plot(allEffects(mod1))

mod1 <- lm(Plant_SpeciesRichness ~ Herbivore_SpeciesRichness + LUI, data=dat)
summary(mod1)
anova(mod1)

#Continuous and categorical
mod2 <- lm(Plant_SpeciesRichness ~ Region + LUI + Herbivore_SpeciesRichness, data=dat)
summary(mod2)
levels(dat$Region)

anova(mod2)

#Interactions
mod3 <- lm(Plant_SpeciesRichness ~ Region*LUI, data=dat)
summary(mod3)

anova(mod3)

plot(allEffects(mod3))

#Exercice: create a model with an interaction and plot it

#### Variables standardization ####
mod4 <- lm(Plant_SpeciesRichness ~ LUI + Herbivore_SpeciesRichness, data=dat)
summary(mod4)

#center variables
dat$LUI_s <- scale(dat$LUI, scale=F)
dat$Herbivore_SpeciesRichness_s <- scale(dat$Herbivore_SpeciesRichness, scale=F)
mod4 <- lm(Plant_SpeciesRichness ~ LUI_s + Herbivore_SpeciesRichness_s, data=dat)
summary(mod4)

#scale variables
dat$LUI_s <- scale(dat$LUI, center=F)
dat$Herbivore_SpeciesRichness_s <- scale(dat$Herbivore_SpeciesRichness, center=F)
mod4 <- lm(Plant_SpeciesRichness ~ LUI_s + Herbivore_SpeciesRichness_s, data=dat)
summary(mod4)

#standardize variables
dat$LUI_s <- scale(dat$LUI)
dat$Herbivore_SpeciesRichness_s <- scale(dat$Herbivore_SpeciesRichness)
mod4 <- lm(Plant_SpeciesRichness ~ LUI_s + Herbivore_SpeciesRichness_s, data=dat)
summary(mod4)


#### Polynomial ####
#quadratic term
mod5 <- lm(Plant_SpeciesRichness ~ Region + LUI + I(LUI^2), data=dat)
summary(mod5)
plot(effect("LUI", mod5))
plot(mod5)

#cubic term
mod5 <- lm(Plant_SpeciesRichness ~ Region + LUI + I(LUI^2) + I(LUI^3), data=dat)
summary(mod5)

#### Potential problems with models ####
#what about the effects of single LUI components?
mod6 <- lm(Plant_biomass ~ Fstd + Gstd + Mstd, data=dat)
summary(mod6)
plot(mod6)

#check correlations between x variables
pairs(dat[,c("Fstd","Gstd", "Mstd")])
cor(dat[,c("Fstd","Gstd", "Mstd")])

#check multicollinearity with variance inflation factors
vif(mod6)

#mowing is higly correlated with both fertilization (and grazing) -> they should not be together in the model
mod7 <- lm(Plant_biomass ~ Fstd + Gstd, data=dat)
summary(mod7)
par(mfrow=c(2,2))
plot(mod7)

#### Model predictions ####
mod8 <- lm(Plant_biomass ~ Region + LUI, data=dat)
summary(mod8)
plot(mod8) #the model meets the assumptions, we can use it to make predictions

#create new LUI (new plots in each region)
summary(dat$LUI) #to have an idea of actual LUI values
summary(dat[dat$Region=="ALB","LUI"]) #and only in the ALB
newLUI_ALB <- data.frame(Region=rep("ALB",10),
                         LUI=c(0.1,0.2,0.3,0.4,0.7,2,3,3.5,4,5))
predict(mod8, newLUI_ALB)

preds <- data.frame(predict(mod8, newLUI_ALB, interval = "confidence"))

# plot
plot(Plant_biomass~LUI, data= dat, type="n")
# add fill
polygon(c(rev(newLUI_ALB$LUI), newLUI_ALB$LUI), c(rev(preds[ ,3]), preds[ ,2]), col = 'grey80', border = NA)
# model
coefficients(mod8)
abline(mod8$coef[1]+mod8$coef[3], mod8$coef[4])
# intervals
lines(newLUI_ALB$LUI, preds[ ,3], lty = 'dashed', col = 'red')
lines(newLUI_ALB$LUI, preds[ ,2], lty = 'dashed', col = 'red')
#add observed and predicted dots
points(preds$fit ~ newLUI_ALB$LUI, data= dat, col= "red")
points(Plant_biomass~LUI, data=dat, col="blue")

