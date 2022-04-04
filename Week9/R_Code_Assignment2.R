install.packages("flux")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("moments")
install.packages("car")
install.packages("kableExtra")
install.packages("zeallot")

library(flux)
library(ggplot2)
library(gridExtra)
library(moments)
# library(rockchalk) # base R code replaces requirement for this package
library(car)

library(zeallot)
library(kableExtra)
dp<-dpois(0,lambda = .05*100)
pp<-ppois(0,lambda =  .05*100)
db<-dbinom(0,100,.05)
pb<-pbinom(0,100,.05)
results <-data.frame(c("dpois","ppois","dbinom","pbinom"),c(dp,pp,db,pb))
names(results) <- c("Method","Result")
kable(results) %>%
  kable_styling()

mydata <- read.csv("mydata.csv", sep = ",")
# mydata <- read.csv(file.path("c:...", "mydata.csv"), sep = ",")
# mydata <- read.csv(file.path("c:/Rabalone/", "mydata.csv"), sep = ",")

str(mydata)

## transform VOLUME using log2
mydata$L_VOLUME <- log2(mydata$VOLUME)
## aov() allows you to fit an analysis of variance model
summary(aov(L_VOLUME ~ CLASS + VOLUME + CLASS:VOLUME, data = mydata))

# Creating Levels here we show how to define the new variable TYPE using only base R functions (no need for outside 
#packages)
mydata$TYPE <- character(nrow(mydata))  # initialize the TYPE column as all blanks
for (i in seq(along = mydata$SEX)) {
  mydata$TYPE[i] <- 'I'
  if (mydata$SEX[i] == 'M' || mydata$SEX[i] == 'F') mydata$TYPE[i] <- 'ADULT'
}
mydata$TYPE <- factor(mydata$TYPE)
cat('\nCheck on definition of TYPE object (should be an integer): ', typeof(mydata$TYPE))
cat('\nmydata$TYPE is treated as a factor: ', is.factor(mydata$TYPE), '\n')
table(mydata$SEX, mydata$TYPE)

## Alternative way of Creating levels using rockchalk
mydata$TYPE <- rockchalk::combineLevels(mydata$SEX, levs = c("F", "M"), "ADULT")
table(mydata$SEX, mydata$TYPE)


##########Assignment 2

install.packages("flux")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("moments")
install.packages("car")
install.packages("kableExtra")
install.packages("zeallot")

library(flux)
library(ggplot2)
library(gridExtra)
library(moments)
# library(rockchalk) # base R code replaces requirement for this package
library(car)
library(zeallot)
library(kableExtra)

mydata <- read.csv("mydata.csv", sep = ",")

#1a Form a histogram and QQ plot using RATIO. Calculate skewness and kurtosis using 'rockchalk.' Be aware that with 
# 'rockchalk', the kurtosis value has 3.0 subtracted from it which differs from the 'moments' package. 

hist(mydata$RATIO, col = "red", main = "Histogram of Ratio", xlab = "Ratio")
qqnorm(mydata$RATIO, col = "red", pch = 16, main = "QQ Plot of Ratio")
qqline(mydata$RATIO, col = "green", lty = 2, lwd = 2)

skewness(mydata$RATIO)
kurtosis(mydata$RATIO)

#1b Tranform RATIO using *log10()* to create L_RATIO (Kabacoff Section 8.5.2, p. 199-200). Form a histogram and QQ 
#plot using L_RATIO. Calculate the skewness and kurtosis. Create a boxplot of L_RATIO differentiated by CLASS.

mydata$L_RATIO <- log10(mydata$RATIO)
hist(mydata$L_RATIO, col = "red", main = "Histogram of Ratio", xlab = "L_Ratio")
qqnorm(mydata$L_RATIO, col = "red", pch = 16, main = "QQ Plot of L_Ratio")
qqline(mydata$L_RATIO, col = "green", lty = 2, lwd = 2)

skewness(mydata$L_RATIO)
kurtosis(mydata$L_RATIO)

L_RationClassPlot = ggplot(mydata, aes(x = CLASS, y = L_RATIO)) + geom_boxplot()
L_RationClassPlot

#1c Test the homogeneity of variance across classes using *bartlett.test()* (Kabacoff Section 9.2.2, p. 222). 
bartlett.test(RATIO ~ CLASS, data = mydata)

bartlett.test(L_RATIO ~ CLASS, data = mydata)


#2a Perform an analysis of variance with *aov()* on L_RATIO using CLASS and SEX as the independent variables (Kabacoff
#chapter 9, p. 212-229). Assume equal variances. Perform two analyses. First, fit a model with the interaction term 
#CLASS:SEX. Then, fit a model without CLASS:SEX. Use *summary()* to obtain the analysis of variance tables (Kabacoff 
#chapter 9, p. 227).

ANOVA1 <- aov(formula = L_RATIO ~ CLASS + SEX + CLASS:SEX, data = mydata)
ANOVA1
summary(ANOVA1)

ANOVA2 <- aov(formula = L_RATIO ~ CLASS + SEX, data = mydata)
ANOVA2
summary(ANOVA2)

#2b For the model without CLASS:SEX (i.e. an interaction term), obtain multiple comparisons with the *TukeyHSD()* 
#function. Interpret the results at the 95% confidence level (*TukeyHSD()* will adjust for unequal sample sizes). 
TukeyHSD(ANOVA2)



#3a1 We combine "M" and "F" into a new level, "ADULT". (While this could be accomplished using *combineLevels()* from 
#the 'rockchalk' package, we use base R code because many students do not have access to the rockchalk package.) This 
#necessitated defining a new variable, TYPE, in mydata which had two levels:  "I" and "ADULT".

mydata$TYPE <- character(nrow(mydata))  # initialize the TYPE column as all blanks
for (i in seq(along = mydata$SEX)) {
  mydata$TYPE[i] <- 'I'
  if (mydata$SEX[i] == 'M' || mydata$SEX[i] == 'F') mydata$TYPE[i] <- 'ADULT'
}
mydata$TYPE <- factor(mydata$TYPE)
cat('\nCheck on definition of TYPE object (should be an integer): ', typeof(mydata$TYPE))
cat('\nmydata$TYPE is treated as a factor: ', is.factor(mydata$TYPE), '\n')
table(mydata$SEX, mydata$TYPE)

#3a2 Present side-by-side histograms of VOLUME. One should display infant volumes and, the other, adult volumes.
mydataInfant <- mydata[ which(mydata$TYPE=='I'),]
mydataAdult <- mydata[ which(mydata$TYPE=='ADULT'),]
par(mfrow = c(1,2)) 
hist(mydataInfant$VOLUME, main="Infant Volume",col = 'red')
hist(mydataAdult$VOLUME, main="Adult Volume",col = 'blue')


#3b Create a scatterplot of SHUCK versus VOLUME and a scatterplot of their base ten logarithms, labeling the variables 
#as L_SHUCK and L_VOLUME. Please be aware the variables, L_SHUCK and L_VOLUME, present the data as orders of magnitude
#(i.e. VOLUME = 100 = 10^2 becomes L_VOLUME = 2). Use color to differentiate CLASS in the plots. Repeat using color to
#differentiate by TYPE. 

mydata$L_VOLUME <- log10(mydata$VOLUME)
mydata$L_SHUCK <- log10(mydata$SHUCK)

vsc <- ggplot(data = mydata,aes(x=VOLUME, y=SHUCK, color=CLASS))+ geom_point()
Lvsc <- ggplot(data = mydata,aes(x=L_VOLUME, y=L_SHUCK, color=CLASS))+ geom_point()
vst <- ggplot(data = mydata,aes(x=VOLUME, y=SHUCK, color=TYPE))+ geom_point()
Lvst <- ggplot(data = mydata,aes(x=L_VOLUME, y=L_SHUCK, color=TYPE))+ geom_point()
grid.arrange(vsc, Lvsc, vst, Lvst, ncol=2)

#4a1 Since abalone growth slows after class A3, infants in classes A4 and A5 are considered mature and candidates for 
#harvest. Reclassify the infants in classes A4 and A5 as ADULTS. This reclassification could have been achieved using 
#*combineLevels()*, but only on the abalones in classes A4 and A5. We will do this recoding of the TYPE variable using
#base R functions. We will use this recoded TYPE variable, in which the infants in A4 and A5 are reclassified as 
#ADULTS, for the remainder of this data analysis assignment. 

for (i in seq(along = mydata$TYPE)) {
  if (mydata$CLASS[i] == 'A4' || mydata$CLASS[i] == 'A5') mydata$TYPE[i] <- 'ADULT'
}
mydata$TYPE <- factor(mydata$TYPE)
cat('\nCheck on redefinition of TYPE object (should be an integer): ', typeof(mydata$TYPE))
cat('\nmydata$TYPE is treated as a factor: ', is.factor(mydata$TYPE), '\n')
cat('\nThree-way contingency table for SEX, CLASS, and TYPE:\n')
print(table(mydata$SEX, mydata$CLASS, mydata$TYPE))

#4a2 Regress L_SHUCK as the dependent variable on L_VOLUME, CLASS and TYPE (Kabacoff Section 8.2.4, p. 178-186, the 
#Data Analysis Video #2 and Black Section 14.2). Use the multiple regression model: L_SHUCK ~ L_VOLUME + CLASS + TYPE. 
#Apply *summary()* to the model object to produce results.

RModel <- lm(formula = L_SHUCK ~ L_VOLUME + CLASS + TYPE, data = mydata)
summary(RModel)

#5a If "model" is the regression object, use model$residuals and construct a histogram and QQ plot. Compute the 
#skewness and kurtosis. Be aware that with 'rockchalk,' the kurtosis value has 3.0 subtracted from it which differs 
#from the 'moments' package.
r <- residuals(RModel)

hist(r, col = "red", main = "Histogram of Residuals", xlab = "Residual")
qqnorm(r, col = "red", pch = 16, main = "QQ Plot of Residuals")
qqline(r, col = "green", lty = 2, lwd = 2)

skewness(r)
kurtosis(r)

#5b Plot the residuals versus L_VOLUME, coloring the data points by CLASS and, a second time, coloring the data points
#by TYPE. Keep in mind the y-axis and x-axis may be disproportionate which will amplify the variability in the 
#residuals. Present boxplots of the residuals differentiated by CLASS and TYPE (These four plots can be conveniently 
#presented on one page using *par(mfrow..)* or *grid.arrange()*. Test the homogeneity of variance of the residuals 
#across classes using *bartlett.test()* (Kabacoff Section 9.3.2, p. 222).  

rvc <- ggplot(data = mydata, aes(x=L_VOLUME, y=r, color=CLASS))+ geom_point()
rvt <- ggplot(data = mydata, aes(x=L_VOLUME, y=r, color=TYPE))+ geom_point()   

rvcB <- ggplot(data = mydata, aes(x=L_VOLUME, y=r, color=CLASS))+ geom_boxplot()
rvtB <- ggplot(data = mydata, aes(x=L_VOLUME, y=r, color=TYPE))+ geom_boxplot() 

grid.arrange(rvc, rvt, rvcB, rvtB, ncol=2)

bartlett.test(r ~ CLASS, data = mydata)

#6a A series of volumes covering the range from minimum to maximum abalone volume will be used in a "for loop" to 
#determine how the harvest proportions change as the "cutoff" changes. Code for doing this is provided.

idxi <- mydata$TYPE == "I"
idxa <- mydata$TYPE == "ADULT"

max.v <- max(mydata$VOLUME)
min.v <- min(mydata$VOLUME)
delta <- (max.v - min.v)/10000
prop.infants <- numeric(10000)
prop.adults <- numeric(10000)
volume.value <- numeric(10000)

total.infants <- sum(idxi)  
total.adults <- sum(idxa)

for (k in 1:10000) { 
  value <- min.v + k*delta
  volume.value[k] <- value
  prop.infants[k] <- sum(mydata$VOLUME[idxi] <= value)/total.infants
  prop.adults[k] <-  sum(mydata$VOLUME[idxa] <= value)/total.adults
}

# prop.infants shows the impact of increasing the volume cutoff for
# harvesting. The following code shows how to "split" the population at
# a 50% harvest of infants.

n.infants <- sum(prop.infants <= 0.5)
split.infants <- min.v + (n.infants + 0.5)*delta  # This estimates the desired volume.
split.infants

n.adults <- sum(prop.adults <= 0.5)
split.adults <- min.v + (n.adults + 0.5)*delta
split.adults



num_infants <- sum(prop_infants <= 0.5)
split_infants <- min_vol + (num_infants + 0.5) * delta  
num_adults <- sum(prop_adults <= 0.5)
split_adults <- min_vol + (num_adults + 0.5) * delta
#6b Present a plot showing the infant proportions and the adult proportions versus volume.value. Compute the 50% 
#"split" volume.value for each and show on the plot.  


ggplot() + 
  geom_line(aes(volume.value, prop.infants), color = "red") +
  geom_line(aes(volume.value, prop.adults), color = "blue") + 
  geom_vline(xintercept = split.infants) +
  annotate("text", label = paste(round(split.infants, 2), "\nInfants"), x = split.infants + 50, y = 0.45, color = "red") +
  geom_vline(xintercept = split.adults) +
  annotate("text", label = paste(round(split.adults, 2), "\nAdults"),x = split.adults + 50, y = 0.45, color = "blue") +
  geom_hline(yintercept = 0.5) + labs(x = "VOLUME", y = "PROPORTION", title = "Adults and Infants Proportions vs Volume")


#7a Evaluate a plot of the difference ((1 - prop.adults) - (1 - prop.infants)) versus volume.value. Compare to the 50%
#"split" points determined in (6)(a). There is considerable variability present in the peak area of this plot. The 
#observed "peak" difference may not be the best representation of the data. One solution is to smooth the data to 
#determine a more representative estimate of the maximum difference.

ggplot() + 
  geom_line(aes(volume.value, ((1 - prop.adults) - (1 - prop.infants))), color = "red") +
  geom_vline(xintercept = split.infants) +
  annotate("text", label = paste(round(split.infants, 2), "\nInfants"), x = split.infants + 50, y = 0.45, color = "red") +
  geom_vline(xintercept = split.adults) +
  annotate("text", label = paste(round(split.adults, 2), "\nAdults"),x = split.adults + 50, y = 0.45, color = "blue") +
  geom_hline(yintercept = 0.5) + labs(x = "VOLUME", y = "PROPORTION", title = "Adults and Infants Proportions vs Volume")

#(7)(b) Since curve smoothing is not studied in this course, code is supplied below. Execute the following code to 
#create a smoothed curve to append to the plot in (a). The procedure is to individually smooth (1-prop.adults) and 
#(1-prop.infants) before determining an estimate of the maximum difference.

y.loess.a <- loess(1 - prop.adults ~ volume.value, span = 0.25,
                   family = c("symmetric"))
y.loess.i <- loess(1 - prop.infants ~ volume.value, span = 0.25,
                   family = c("symmetric"))
smooth.difference <- predict(y.loess.a) - predict(y.loess.i)

#(7)(c) Present a plot of the difference ((1 - prop.adults) - (1 - prop.infants)) versus volume.value with the 
#variable smooth.difference superimposed. Determine the volume.value corresponding to the maximum smoothed difference 
#(Hint:  use *which.max()*). Show the estimated peak location corresponding to the cutoff determined.


maxSmoothDif <- volume.value[which.max(smooth.difference)]

ggplot() +
  geom_line(aes(volume.value, ((1 - prop.adults) - (1 - prop.infants))), color = "red") +
  geom_line(aes(volume.value, smooth.difference), color = "blue") +
  geom_vline(xintercept = volume.value[maxSmoothDif]) +
  annotate("text", label = paste('Volume =', round(maxSmoothDif, 3)), x = maxSmoothDif + 20, y = 0.4, angle = 90) +
  labs(x = "VOLUME", y = "Difference in Proportions Harvested", title = "Difference in Harvest Proportions")

#7d What separate harvest proportions for infants and adults would result if this cutoff is used? Show the separate 
#harvest proportions (NOTE:  the adult harvest proportion is the "true positive rate" and the infant harvest 
#proportion is the "false positive rate").

(1 - prop.adults)[which.max(smooth.difference)] 
(1 - prop.infants)[which.max(smooth.difference)] 

#8a Harvesting of infants in CLASS "A1" must be minimized. The smallest volume.value cutoff that produces a zero 
#harvest of infants from CLASS "A1" may be used as a baseline for comparison with larger cutoffs. Any smaller cutoff 
#would result in harvesting infants from CLASS "A1."  

#Compute this cutoff, and the proportions of infants and adults with VOLUME exceeding this cutoff. Code for 
#determining this cutoff is provided. Show these proportions.

cut <- volume.value[volume.value > max(mydata[mydata$CLASS == "A1" &
                                         mydata$TYPE == "I", "VOLUME"])][1]
cut
(1 - prop.adults)[which(volume.value == cut)]
(1 - prop.infants)[which(volume.value == cut)]

#8b Another cutoff is one for which the proportion of adults not harvested equals the proportion of infants 
#harvested. This cutoff would equate these rates; effectively, our two errors:  'missed' adults and wrongly-harvested 
#infants. This leaves for discussion which is the greater loss:  a larger proportion of adults not harvested or 
#infants harvested?  This cutoff is 237.7383. Calculate the separate harvest proportions for infants and adults 
#using this cutoff. Show these proportions.  Code for determining this cutoff is provided. 

cut2 <- volume.value[which.min(abs(prop.adults - (1-prop.infants)))]
cut2
(1 - prop.adults)[which(volume.value == cut2)]
(1 - prop.infants)[which(volume.value == cut2)]


#9a Construct an ROC curve by plotting (1 - prop.adults) versus (1 - prop.infants). Each point which appears 
#corresponds to a particular volume.value. Show the location of the cutoffs determined in (7) and (8) on this plot and
#label each. 

cuts <- which(volume.value %in% c(maxSmoothDif, cut, cut2))
ggplot(mapping = aes((1 - prop.infants), (1 - prop.adults))) + 
  geom_line(color = 'blue', size = 1) + 
  geom_abline(intercept = 0, slope = 1, color = 'red', linetype = 2) +
  geom_point(mapping = aes((1 - prop.infants[cuts]), (1 - prop.adults[cuts])),
             shape = 21, size = 3) +
  annotate("text", label = paste('equal harvest \nvol =', round(cut2, 1)), 
           x = 0.2, y = 0.85, size = 3) +
  annotate("text", label = paste('zero A1 infants \nvol =', round(cut, 1)), 
           x = 0.35, y = 0.8, size = 3) +
  annotate("text", label = paste('max. difference \nvol =', round(maxSmoothDif, 1)), 
           x = 0.25, y = 0.7, size = 3) +
  labs(title = "ROC curve of adult and infant harvest proportions",
       x = "Infant harvest proportion", y = "Adult harvest proportion") + 
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5))


#9b Numerically integrate the area under the ROC curve and report your result. This is most easily done with the 
#*auc()* function from the "flux" package.   Areas-under-curve, or AUCs, greater than 0.8 are taken to indicate good 
#discrimination potential. 

flux::auc(x = (1 - prop.infants), y = (1 - prop.adults))

#10a Prepare a table showing each cutoff along with the following:
  #1)true positive rate (1-prop.adults,
  #2) false positive rate (1-prop.infants),
  #3) harvest proportion of the total population



sum(mydata$TYPE == "I")
sum(mydata$TYPE == "ADULT")

tpr <- (1 - prop.adults)[cuts]
fpr <- (1 - prop.infants)[cuts]
yld <- (fpr * sum(mydata$TYPE == "I") + tpr * sum(mydata$TYPE == "ADULT")) /
  (sum(mydata$TYPE == "I") + sum(mydata$TYPE == "ADULT"))
tpr
fpr
yld


