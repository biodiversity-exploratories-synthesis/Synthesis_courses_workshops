### import data skip if you already did this ##########

setwd("C:\\Users\\ericallan\\Desktop\\Explo stats")
dat <- read.table("04_SEMData_2008.txt", header =T)

names(dat)[which(names(dat)=="PlotID")] <- "Plot"

#recode the regions names (to avoid problems with special characters)
class(dat$Region) #it's a factor
levels(dat$Region)
levels(dat$Region) <- c("HAI", "SCH", "ALB")

#it does not seem realistic, it is an outlier that we can probably remove
dat <- dat[dat$Plot != "HEG01",]

#standardize the LUI components and calculate LUI
dat$Fstd <- sapply(dat$Fertilization, function(x)x/mean(dat$Fertilization,na.rm=T))
dat$Gstd <- sapply(dat$Grazing, function(x)x/mean(dat$Grazing,na.rm=T))
dat$Mstd <- sapply(dat$Mowing, function(x)x/mean(dat$Mowing,na.rm=T))

#LUI is the square root of the sum of the 3 components
dat$LUI <- sqrt(rowSums(dat[,c("Fstd","Gstd","Mstd")], na.rm=T))

########################################################
############## code for the polynomial example #########
########################################################


x <- seq(1,10, len=100)
y <- x^2 + rnorm(100, 0,20)
par(las=1)
plot(x, y)

m <- lm(y~x)
y2 <- predict(m)
lines(x, y2, typ="l", col ="black", lwd =2)
legend("topleft", legend=paste("R2 = ", round(summary(m)[[8]],2)), bty="n")

m2 <- lm(y~poly(x,2))
y3 <- predict(m2)
lines(x, y3, typ="l", col ="indianred2", lwd =2)
legend("topleft", legend=paste("R2 = ", round(summary(m2)[[8]],2)), bty="n", text.col ="indianred2",inset=c(0,0.1))

m3 <- lm(y~poly(x,3))
y4 <- predict(m3)
lines(x, y4, typ="l", col ="dodgerblue", lwd =2)
legend("topleft", legend = paste("R2 = ", round(summary(m3)[[8]],2)), bty="n", text.col ="dodgerblue",inset=c(0,0.2))

mx <- lm(y~poly(x,25))
yx <- predict(mx)
lines(x, yx, typ="l", col ="forestgreen", lwd =2)
legend("topleft", legend = paste("R2 = ", round(summary(mx)[[8]],2)), bty="n", text.col ="forestgreen",inset=c(0,0.3))

################################################################################################
############################## model simplification ############################################
################################################################################################

## fit a complex model

m <- lm(Predator_SpeciesRichness  ~ Region + Fstd + Gstd + Mstd + Plant_SpeciesRichness + Plant_biomass + Herbivore_SpeciesRichness + Herbivore_biomass +
           +Mstd:Herbivore_SpeciesRichness + Plant_biomass:Herbivore_SpeciesRichness +Mstd:Herbivore_biomass + Plant_biomass:Herbivore_biomass, data = dat)
summary(m)

## drop the interactions first, starting with the one with highest p-value

m2 <- update(m, ~.-Plant_biomass:Herbivore_SpeciesRichness)
anova(m, m2)

summary(m2)

## now drop the rest...

## if the interactions are out you can move onto the main effect with highest p-value

m4 <- update(m3, ~.- Fstd)
anova(m4, m3)

## keep going until you have only significant terms left...


#### demonstrate marginality
summary(mplant <- lm(Plant_SpeciesRichness~Region*(LUI + Plant_biomass), data=dat))

## interactions are significant
mplant2 <- update(mplant, ~.- Region:Plant_biomass)
anova(mplant, mplant2)

## plot with each region coloured differently
par(las=1)
with(dat, plot(Plant_SpeciesRichness~Plant_biomass, subset = Region =="ALB", 
pch = 16, col ="indianred", ylim = c(0,60), xlim = c(0,1500)))
with(dat, points(Plant_SpeciesRichness~Plant_biomass, subset = Region =="HAI",
col="dodgerblue",pch=16))
with(dat, points(Plant_SpeciesRichness~Plant_biomass, subset = Region =="SCH",
col="forestgreen",pch=16))

## new data for predictions, set LUI to mean value
pdat <- expand.grid("Region" = c("HAI","SCH","ALB"), 
"Plant_biomass" = seq(min(dat$Plant_biomass), max(dat$Plant_biomass), len =100),
"LUI" = mean(dat$LUI))

## do the predictions
yp <- predict(mplant, newdata=pdat)

## add the lines
lines(pdat$Plant_biomass[pdat$Region=="ALB"], yp[pdat$Region=="ALB"], col ="indianred", lwd =2)
lines(pdat$Plant_biomass[pdat$Region=="HAI"], yp[pdat$Region=="HAI"], col ="dodgerblue")
lines(pdat$Plant_biomass[pdat$Region=="SCH"], yp[pdat$Region=="SCH"], col ="forestgreen")


################################################################################################
############################## AIC approach  ###################################################
################################################################################################

library(MuMIn)

mlist <- list()

## only land use
mlist[[1]] <- lm(Predator_SpeciesRichness  ~ Region + Fstd + Gstd + Mstd, data = dat)

## land use and plants
mlist[[2]] <- lm(Predator_SpeciesRichness  ~ Region + Fstd + Gstd + Mstd + Plant_SpeciesRichness + Plant_biomass, data = dat)

## land use plants and herbivore diversity
mlist[[3]] <- lm(Predator_SpeciesRichness  ~ Region + Fstd + Gstd + Mstd + Plant_SpeciesRichness + Plant_biomass  + Herbivore_SpeciesRichness, data = dat)

## land use plants and herbivore biomass
mlist[[4]] <- lm(Predator_SpeciesRichness  ~ Region + Fstd + Gstd + Mstd + Plant_SpeciesRichness + Plant_biomass  + Herbivore_biomass, data = dat)

## land use plants and herbivore biomass
mlist[[5]] <- lm(Predator_SpeciesRichness  ~ Region + Fstd + Gstd + Mstd + Plant_SpeciesRichness + Plant_biomass  + Herbivore_biomass + Herbivore_SpeciesRichness, data = dat)

names(mlist) <- c("lui.only", "lui.plants", "herbivore.rich", "herbivore.abun", "herbivore.rich.abun")
aic.ranks <- model.sel(mlist)
aic.ranks

## try all possible models
## here we fit the model without interactions to make things a bit simpler
m2 <- lm(Predator_SpeciesRichness  ~ Region + Fstd + Gstd + Mstd + Plant_SpeciesRichness + Plant_biomass + Herbivore_SpeciesRichness + Herbivore_biomass, 
        data = dat, na.action = na.fail) ## need to set na.action to na.fail, otherwise submodels could be fitted to different datasets

all.mods2 <- dredge(m2)
all.mods2

## get the model averaged parameters
ma <- model.avg(all.mods2, fit =TRUE) 
## fit = TRUE means it fits models again, necessary to use with predict, below, but time consuming with many models..
summary(ma)

ii <- importance(ma)

par(mar=c(8,4,1,1))   ## large bottom margin for names
x <- barplot(as.vector(ii), col ="indianred4", ylim=c(0,1))
 text(x, -0.02, names(ii), srt = 45,xpd=NA, adj=1)

### predict herbivore biomass effect on predator species richness
### set all other effects to their means, except for region where you have to predict for each
pdat.hb <- expand.grid("Region" = c("HAI","SCH","ALB"), 
"Herbivore_biomass" = seq(min(dat$Herbivore_biomass), max(dat$Herbivore_biomass), len =100),
"Fstd" = mean(dat$Fstd), "Mstd" = mean(dat$Mstd), "Gstd" = mean(dat$Gstd),
"Plant_SpeciesRichness" = mean(dat$Plant_SpeciesRichness), "Plant_biomass" = mean(dat$Plant_biomass),
"Herbivore_SpeciesRichness" = mean(dat$Herbivore_SpeciesRichness))

### predict from model averaged parameters, full =TRUE uses all models, FALSE would use subset
y.hb <- predict(ma, newdata=pdat.hb, full =TRUE)

## predict from original model for comparison
y.hb.raw <- predict(m2, newdata=pdat.hb)

## produce average across regions
y.hb2 <- tapply(y.hb, pdat.hb$Herbivore_biomass, mean)
y.hb.raw2 <- tapply(y.hb.raw, pdat.hb$Herbivore_biomass, mean)
x.hb2 <- seq(min(dat$Herbivore_biomass), max(dat$Herbivore_biomass), len =100) ## vector the same length

### predict mowing effect on predator species richness
### set all other effects to their means, except for region where you have to predict for each
pdat.mw <- expand.grid("Region" = c("HAI","SCH","ALB"), 
"Mstd" = seq(min(dat$Mstd), max(dat$Mstd), len =100),
"Fstd" = mean(dat$Fstd), "Herbivore_biomass" = mean(dat$Herbivore_biomass), "Gstd" = mean(dat$Gstd),
"Plant_SpeciesRichness" = mean(dat$Plant_SpeciesRichness), "Plant_biomass" = mean(dat$Plant_biomass),
"Herbivore_SpeciesRichness" = mean(dat$Herbivore_SpeciesRichness))

### predict from model averaged parameters, full =TRUE uses all models, FALSE would use subset
y.mw <- predict(ma, newdata=pdat.mw, full =TRUE)

## predict from original model for comparison
y.mw.raw <- predict(m2, newdata=pdat.mw)

## produce average across regions
y.mw2 <- tapply(y.mw, pdat.mw$Mstd, mean)
y.mw.raw2 <- tapply(y.mw.raw, pdat.mw$Mstd, mean)
x.mw2 <- seq(min(dat$Mstd), max(dat$Mstd), len =100) ## vector the same length

### plot the effects
par(mar=c(5,4,1,1)) ## margins back to normal
par(mfrow=c(1,2))
with(dat, plot(Predator_SpeciesRichness ~ Herbivore_biomass, pch =16))
lines(x.hb2, y.hb2, col ="indianred", lwd =2)
lines(x.hb2, y.hb.raw2, col ="dodgerblue", lwd=2)

with(dat, plot(Predator_SpeciesRichness ~ Mstd, pch =16))
lines(x.mw2, y.mw2, col ="indianred", lwd =2)
lines(x.mw2, y.mw.raw2, col ="dodgerblue", lwd=2)


#################################################################################
###################### mixed models #############################################
#################################################################################
library(lme4)
library(gplots)

intra <- read.table("Plant_intra_assembled.txt", header =T)

## add LUI
intra2 <- merge(dat[,match(c("Plot", "LUI", "Mstd", "Fstd", "Gstd"), names(dat))], intra, by="Plot")

## log biomass because distribution is very skewed
intra2$logbio <- log(intra2$Biom_tot_g)

## fit a model with no random effects, this is WRONG!
wrong.mod <- lm(logbio ~ LUI * FG, data = intra2)
summary(wrong.mod)

## fit a simple model for one species
mixm1sp <- lmer(logbio ~ Region + LUI + (1|Plot), subset = Species=="A_elatius", data = intra2)
summary(mixm1sp)

### fit a mixed model with all species together and test whether functional groups respond differently
mixm <- lmer(logbio ~ Region + LUI * FG + (1|Plot) + (1|Species) + (1|Species:Plot), data = intra2)
summary(mixm)

### fit a second mixed model with random slopes
mixm2 <- lmer(logbio ~ Region + LUI * FG + (1|Plot) + (1|Species) + (0+LUI|Species) + (1|Species:Plot), data = intra2)
summary(mixm2)

## test if the random slopes are needes

anova(mixm, mixm2, refit = FALSE)

## simplifying the model
mixmr1 <- update(mixm2, ~.- LUI:FG)
anova(mixmr1, mixm2)

### check model assumptions

RM <- resid(mixmr1)
F1 <- fitted(mixmr1)
RE <- ranef(mixmr1)

## vector with complete.cases for biomass for subsetting
bc <- which(complete.cases(intra2$logbio))

par(mfrow=c(2,4))
par(mar=c(5,4,2,2))

plot(RM ~ F1, xlab = "Fitted values", ylab ="Residuals")
plot(RM ~ intra2$LUI[bc], xlab = "LUI", ylab ="Residuals")
plot(RM ~ jitter(as.numeric(intra2$Species[bc])), xlab = "Species", ylab ="Residuals")
qqnorm(F1, main = "QQ plot residuals")
qqline(F1)
qqnorm(unlist(RE[[1]]), main = "QQ species:plot")
qqline(unlist(RE[[1]]))
qqnorm(unlist(RE[[2]]), main = "QQ Plot")
qqline(unlist(RE[[2]]))
qqnorm(unlist(RE[[3]][,1]), main = "QQ species slopes")
qqline(unlist(RE[[3]][,1]))
qqnorm(unlist(RE[[3]][,2]), main = "QQ species intercepts")
qqline(unlist(RE[[3]][,2]))

## confidence intervals

## for categorical effects
newdata <- expand.grid("Region" = c("ALB", "HAI", "SCH"), "FG" = c("grass", "herb"), "LUI" = mean(intra2$LUI))

newdata$logbio <- predict(mixmr1, newdata, re.form=NA)
mm <- model.matrix(terms(mixmr1), newdata)

pvar1 <- diag(mm %*% tcrossprod(vcov(mixmr1),mm)) ## variance of predictions

cmult <- 2 ## could use 1.96
newdata2 <- data.frame(
  newdata
  , plo = newdata$logbio-cmult*sqrt(pvar1)
  , phi = newdata$logbio+cmult*sqrt(pvar1)
)

xvals <- c(seq(1,3,1), seq(1.2,3.2,1))

par(mfrow=c(1,1))

plotCI(xvals, newdata2$logbio, li=newdata2$plo, ui=newdata2$phi, gap = 0, pch = 16, xaxt ="n", xlab = "Region", 
       col = c("indianred", "dodgerblue"), ylab = "Log(biomass)")
axis(side = 1, at = seq(1.1, 3.1, 1), labels = c("ALB", "HAI", "SCH"))


## for a continuous effect, go back to Arrhenatherum model, so you can see something...

mixm1sp <- lmer(logbio ~ Region + LUI + (1|Plot), subset = Species=="A_elatius", data = intra2)
newdata <- expand.grid("Region" = c("ALB", "HAI", "SCH"), "LUI" = seq(min(intra2$LUI), max(intra2$LUI), len =100))

newdata$logbio <- predict(mixm1sp, newdata, re.form=NA)
mm <- model.matrix(terms(mixm1sp), newdata)

pvar1 <- diag(mm %*% tcrossprod(vcov(mixm1sp),mm)) ## variance of predictions

cmult <- 2 ## could use 1.96
newdata2 <- data.frame(
  newdata
  , plo = newdata$logbio-cmult*sqrt(pvar1)
  , phi = newdata$logbio+cmult*sqrt(pvar1)
)

## average across FGs and regions
newdata3 <- aggregate(newdata2[,-c(1)], list(newdata2$LUI), mean)

names(newdata3)[1] <- "LUI"

par(mfrow=c(1,1))

plot(newdata3$LUI, newdata3$logbio, ylab = "Log(biomass)", type ="l", ylim = c(0, 4), lwd = 2, xlab = "LUI", col = "indianred2")
lines(newdata3$LUI, newdata3$plo, lty = 2, col ="indianred")
lines(newdata3$LUI, newdata3$phi, lty = 2, col ="indianred")

### calculate pseudo r2

pr2 <- r.squaredGLMM(mixmr1)
