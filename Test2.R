
#1 Find Poison prob where lambda =2  and X is greater than 4 
print(1-ppois(4,2))

#2 What is the probability that the person has the disease given that the test result is positive

P_Disease_and_pos<- 0.939*0.058
P_no_Disease_and_pos<- 0.041*0.942

Bayes_Theorem<-(P_Disease_and_pos/(P_Disease_and_pos+P_no_Disease_and_pos))
print(Bayes_Theorem)

#3 
x <- c(0,1,2,3)
p <- c(0.749,0.225,0.024,0.002)
pTimesx <- x*p
mean <- round(sum(pTimesx),digits=2)
xSquared <- x^2
pTimesxSquared <- xSquared*p
Almost <- sum(pTimesxSquared)
Variance <- Almost - mean^2
print(Variance)

#4 find the 33% quantile for the sample b using type 7

b <- c(1.3,2.2,2.7,3.1,3.3,3.7)
quantile(b, c(.3333333333333), FALSE, TRUE, 7) 

#5
#A study of the amount of time it takes a mechanic to rebuild the transmission for a 2005 Chevrolet 
#Cavalier shows that the mean is 8.4 hours and the standard deviation is 1.8 hours. If a random 
#sample of 36 mechanics is selected, find the probability that their mean rebuild time exceeds 
#8.7 hours.  Assume the mean rebuild time has a normal distribution.  (Hint, interpolate in the 
#tables or use pnorm().)


#Find z(8.7)
# z=(8.7-8.4)÷(1.8÷???36) = 1

pnorm(1, lower.tail=FALSE)

#6
n <- 76
p <- .7
meanSix <- n*p
meanSix
SDSix <- sqrt(n*p*(1-p))
pnorm(q = 50.5, mean = meanSix, sd = SDSix) - pnorm(q = 49.5, mean = meanSix, sd = SDSix)

help(quantile)

#9
print((1/3)^4)

#10
print((40/50)*(39/49)*(38/48))