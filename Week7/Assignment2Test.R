#(1)(a) (6 points) The Poisson distribution may be used to approximate the binomial 
#distribution if n > 20 and np < 7. Estimate the following binomial probabilities using 
#*dpois()* and *ppois()* with probability p = 0.05, and n = 100. Then, estimate the same 
#probabilities using *dbinom()* and *pbinom()*. 

#i Prob of  0 successes
print(dpois(0,lambda = 5))

print(ppois(0,lambda = 5))

print(dbinom(0,100,0.05))

print(pbinom(0,100,0.05))

#ii Prob fewer than 6

print(sum(dpois(0:5,lambda = 5)))

print(ppois(5,lambda = 5))

print(sum(dbinom(0:5,100,0.05)))

print(pbinom(5,100,0.05))

#(1)(b) (3 points) Generate side-by-side barplots using *par(mfrow = c(1,2))* or *grid.arrange()*. 
#The left barplot will show Poisson probabilties for outcomes ranging from 0 to 10. The right barplot
#will show binomial probabilities for outcomes ranging from 0 to 10. Use p = 0.05 and n = 100. 
#Title each plot,  present in color and assign names to the bar; i.e. x-axis value labels.

Pprob1b <- (dpois(0:10,lambda = 5))
Bprob1b <- (dbinom(0:10,100,0.05))

par(mfrow=c(1,2))
barplot(Pprob1b,main ="Poisson probabilties",col="blue",xlab = "Outcome",ylab ="Probabilities")
barplot(Bprob1b,main = "Binomial probabilities",col = "red",xlab = "Outcome",ylab = "Probabilities")


#(1)(c) For this problem, refer to Sections 5.2 of Business Statistics. A discrete random variable 
#has outcomes:  0, 1, 2, 3, 4, 5, 6.  The corresponding probabilities in sequence with the outcomes 
#are: 0.215, 0.230, 0.240, 0.182, 0.130, 0.003, 0.001.  In other words, the probabilty of obtaining 
#"0" is 0.215. 

#(i) (3 points) Calculate the expected value and variance for this distribution using the general 
#formula for mean and variance of a discrete distribution. To do this, you will need to use integer 
#values from 0 to 6 as outcomes along with the corresponding probabilities. Round your answer to 2 
#decimal places. 

x <- 0:6
probx <- c(0.215, 0.230, 0.240, 0.182, 0.130, 0.003, 0.001)
Ex <- round(sum(x*probx),digits=2)
Ex

Varx <- round(((sum((x^2)*probx))-(Ex^2)),digits=2)
Varx



#(ii) (3 points) Use the *cumsum()* function and plot the cumulative probabilties versus the 
#corresponding outcomes.  Detemine the value of the median for this distribution and show on this 
#plot.

CumulativeProb<-cumsum(probx)
CumulativeProb

plot(x,CumulativeProb,main = "Cumulative Probabilties vs Corresponding Outcomes",xlab="Outcomes",ylab="Cumulative Probabilties")
text(1.6,0.50,"The Median is 1.8")

#(2)(a) (3 points) Load the "faithful" dataset and present summary statistics and a histogram of 
#waiting times.  Additionally, compute the empirical conditional probability of an eruption less 
#than 3.0 minutes, if the waiting time exceeds 70 minutes.

data("faithful")

summary(faithful)

hist(faithful$waiting)

WaitMoreThan70 <- subset(faithful,waiting>70)
print(WaitMoreThan70)

Less3 <- subset(WaitMoreThan70,eruptions<3)
print(Less3)

ProbEruptionLessThan3GivenWaitingGreaterThan70 <- nrow(Less3)/nrow(WaitMoreThan70)
print(ProbEruptionLessThan3GivenWaitingGreaterThan70)

#(i) (3 points) Identify any observations in "faithful" for which the waiting time exceeds 70 
#minutes and the eruptions are less than 3.0 minutes.  List and show any such observations in a 
#distinct color on a scatterplot of all eruption (vertical axis) and waiting times (horizontal axis). 
#Include a horizontal line at eruption = 3.0, and a vertical line at waiting time = 70.  Add a title 
#and appropriate text. 

library(ggplot2)
data(faithful)
str(faithful)

longwaits <- subset(faithful, waiting>70 & eruptions<3)
scatter1 <- ggplot(faithful, aes(x=waiting, y=eruptions,col=ifelse(waiting>70 & eruptions<3, "TRUE", "FALSE"))) + geom_point() + 
  geom_vline(xintercept=70) + geom_hline(yintercept=3) + labs(x='Waiting', 
                                                              y='Eruptions', title='Faith Observations')
print(scatter1)



#(ii)What does the plot suggest about the relationship between eruption time and waiting time?
We see a positive relationship with regards to waiting time and eruption time, we see when the waiting time increases we see a corresponding increase in the eruption times

#2b 
#One way to do this is to pass the vector of waiting times - faithful$waiting - to *matrix()*, 
#specifying 2 columns for our matrix, with values organized by row; i.e. byrow = TRUE.

waiting <- faithful$waiting

waitingMatrix<-matrix(waiting,ncol=2,byrow=TRUE)

print(waitingMatrix)

plot(y=waitingMatrix[,2],x=waitingMatrix[,1],xlab="First Waiting Time",ylab = "Second Waiting Time")

#2c
library(Kendall)
cor.test(waitingMatrix[,1],waitingMatrix[,2],alternative = "two.sided",method = "kendall", conf.level = 0.95)

#Null hypothesis not independent; Reject Null hypothesis

# (3)  Performing hypothesis tests using random samples is fundamental to statistical inference.
#The first part of this problem involves comparing two different diets. Using "ChickWeight" data 
#available in the base R, "datasets" package, execute the following code to prepare a data frame 
#for analysis.

# load "ChickWeight" dataset
data(ChickWeight)

# Create T | F vector indicating observations with Time == 21 and Diet == "1" OR "3"
index <- ChickWeight$Time == 21 & (ChickWeight$Diet == "1" | ChickWeight$Diet == "3")

# Create data frame, "result," with the weight and Diet of those observations with "TRUE" "index"" values
result <- subset(ChickWeight[index, ], select = c(weight, Diet))

# Encode "Diet" as a factor
result$Diet <- factor(result$Diet)
str(result) 

## The data frame, "result", has chick weights for two diets, identified as diet "1" and "3". 
#Use the data frame, "result," to complete the following item.

#(3)(a) Display two side-by-side vertical boxplots using par(mfrow = c(1,2)).  One boxplot would 
# display diet "1" and the other diet "3".

result
Diet1 <- subset(result,Diet == "1")
Diet3 <- subset(result,Diet == "3")

par(mfrow=c(1,2))
boxplot(Diet1$weight)
boxplot(Diet3$weight)

#(3)(b) Use the "weight" data for the two diets to test the null hypothesis of equal population mean
#weights for the two diets. Test at the 95% confidence level with a two-sided t-test. This can be 
#done using *t.test()* in R. Assume equal variances. Display the results of t.test().

t.test(Diet1$weight,Diet3$weight,alternative = "two.sided",conf.level = 0.95)


#Working with paired data is another common statistical activity. The "ChickWeight" data will be 
#used to illustrate how the weight gain from day 20 to 21 may be analyzed. Use the following code to
#prepare pre- and post-data from Diet == "3" for analysis.

# load "ChickWeight" dataset
data(ChickWeight)

# Create T | F vector indicating observations with Diet == "3"
index <- ChickWeight$Diet == "3"

# Create vector of "weight" for observations where Diet == "3" and Time == 20
pre <- subset(ChickWeight[index, ], Time == 20, select = weight)$weight

# Create vector of "weight" for observations where Diet == "3" and Time == 21
post <- subset(ChickWeight[index, ], Time == 21, select = weight)$weight

# The pre and post values are paired, each pair corresponding to an individual chick.
cbind(pre, post)

#(3)(c)Present a scatterplot of the variable "post" as a function of the variable "pre".  Include a 
#diagonal line with zero intercept and slope equal to one. Title and label the variables in this 
#scatterplot. 

plot(pre,post,col="blue",xlab="Variable Pre",ylab="Variable Post",
     main="Scatterplot Pre-Post")
abline(0,1,col="red")

#(3)(d) Calculate and present a one-sided, 95% confidence interval for the average weight gain from 
#day 20 to day 21. Write the code for the paired t-test and for determination of the confidence 
#interval endpoints. **Do not use *t.test()**, although you may check your answers using this 
#function. Present the resulting test statistic value, critical value, p-value and confidence 
#interval.

PreMean <- mean(pre)
PreMean

PostMean <- mean(post)
PostMean

AvgWeightGain <- PostMean-PreMean
AvgWeightGain

t.test(pre,post,alternate="two.sided",conf.level = 0.95)

df <- length(pre)+length(post) - 2 
df

PooledVar <- (sum(pre - PreMean)^2 + sum(post - PostMean)^2)/df

t <- (PreMean-PostMean)/sqrt((PooledVar/length(pre))+(PooledVar/length(pre)))
t

p <- abs(t)
p

data(Nile)
m <- mean(Nile)
std <- sd(Nile)

x <- seq(from = 400, to = 1400, by = 1)
hist(Nile, freq = FALSE, col = "darkblue", xlab = "Flow",
     main = "Histogram of Nile River Flows, 1871 to 1970")
curve(dnorm(x, mean = m, sd = std), col = "orange", lwd = 2, add = TRUE)

#4aUsing Nile River flow data and the "moments" package, calculate skewness and kurtosis. Present a 
#QQ plot and boxplot of the flow data side-by-side using *qqnorm()*, *qqline()* and *boxplot()*; 
#*par(mfrow = c(1, 2))*

library(moments) 
NileSkewness <- skewness(Nile)
NileSkewness

NileKurtosis <- kurtosis(Nile)
NileKurtosis

par(mfrow=c(1,2))
qqnorm(Nile,main = "QQ PLOT")
qqline(Nile)
boxplot(Nile,main="Boxplot")

#4b
set.seed(124)
sample1 <- rep(1:1000,0)
for(i in 1:1000) {
  sample1[i]<-mean(sample(Nile,16,replace=TRUE))
}

set.seed(127)
sample2 <- rep(1:1000,0)

for(i in 1:1000) {
  sample2[i]<-mean(sample(Nile,64,replace = TRUE))
}

row <- c("sample1","sample2")
col <- c("mean","sample standard deviation","sample variance")
matrix(c(mean(sample1),mean(sample2),sd(sample1),sd(sample2),var(sample1),var(sample2)),nrow = 2,ncol = 3,dimnames = list(row, col))

#4c
par(mfrow=c(1,2))
hist(sample1,freq = FALSE,xlim = c(750,1050),ylim = c(0,0.025))
curve(dnorm(x,mean=mean(sample1),sd=sd(sample1)),add=TRUE,col="blue",lty=3,lwd=3)

hist(sample2,freq = FALSE,xlim = c(750,1050),ylim = c(0,0.025))
curve(dnorm(x,mean=mean(sample2),sd=sd(sample2)),add=TRUE,col="green",lty=3,lwd=3)


#5a

data("warpbreaks")
str(warpbreaks)

median_breaks<-median(warpbreaks$breaks)
median_breaks

hist(warpbreaks$breaks)
abline(v = median(warpbreaks$breaks), col = "red",lwd = 3)



number <- ifelse(warpbreaks$breaks < median_breaks, "below", "above")
number

warpbreaks2 <- cbind(warpbreaks,number)
warpbreaks2

summary(warpbreaks2)

contingency <- table(warpbreaks2$tension,warpbreaks2$number)
contingency


#5b
chisq.test(contingency)

#Reject the null hypothesis 

#5c

chi <- function(x) {
  # To be used with 3x2 contingency tables that have margins added.
  # Expected values are calculated.
  e11 <- x[4,1]*x[1,3]/x[4,3]
  e12 <- x[4,2]*x[1,3]/x[4,3]
  e21 <- x[4,1]*x[2,3]/x[4,3]
  e22 <- x[4,2]*x[2,3]/x[4,3]
  e31 <- x[4,1]*x[3,3]/x[4,3]
  e32 <- x[4,2]*x[3,3]/x[4,3]
  
  # Value of chi square statistic is calculated.
  chisqStat <- (x[1,1] - e11)^2/e11 + (x[1,2] - e12)^2/e12 + (x[2,1] - e21)^2/e21 + 
    (x[2,2] - e22)^2/e22 + (x[3,1] - e31)^2/e31 + (x[3,2] - e32)^2/e32
  return(list("chi-squared" = chisqStat, "p-value" = pchisq(chisqStat, 2, lower.tail = F)))
}

x<-addmargins(contingency)
chi(x)

chisqfun <- function(t) {
  x <- addmargins(t)
  e <- matrix(0, nrow = nrow(t), ncol = ncol(t), byrow = T)
  r <- matrix(0, nrow = nrow(t), ncol = ncol(t), byrow = T)
  for (i in 1:3) {
    for (j in 1:2) {
      e[i,j] = x[nrow(x),j] * x[i,ncol(x)]/x[nrow(x), ncol(x)]
      r[i,j] = ((x[i,j] - e[i,j])^2)/e[i,j]
    }
  }
  chi <- sum(r)
  xdf <- nrow(t) - 1
  pv <- pchisq(chi, df = xdf, lower.tail = FALSE) 
  return(cat("Pearson's Chi-squared test \\n","Chi sq: ", chi, "; 
            Degree of Freedom :",xdf," ; P-value :",pv))
}

chisqfun(contingency)
