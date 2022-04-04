
#1a
a <- c((0:4),13,rep(c(2,-5.1,-23),3),c(7/42+3+35/42))

#1b
print(sort(a))
L <- length(a)
print(L)
b <- seq(from = L, to =1)
c <- sort(a) + b

#1c
print(d <- c(c[1],c[16]))
print(e <- c[2:15])

#1d
f <- append(d,e, after = 1)
print(f)
round(sum(f),digits=2)


#2a
function1<-function(x){
  y<- sin(x/2) + cos(x/2) 
  return(y)
}

#2b
x<- seq(from = -2, to =2, length=4001)
y<-function1(x)
max(y)
print(round(max(y),digits=3))
which.max(y)
findx <- x[3572]
print(round(findx,digits=3))

#2c
plot(x,y,col="red",main="Max Y and Corresponding X")
text(1.414,1.3,"(1.571,1.414)")

#3

x<-seq(-2,2,length.out = 4001)
fun1 <- function(x) y <- cos(x/2)*sin(x/2)
fun2 <- function(x) -(x/2)**3

plot(fun1, -2,2,col="red")
par(new=TRUE)
plot(fun2,-2,2,col="blue")
text(0,0,"(0,0)")

#4a
data(trees)
print(apply(trees,2,median))
print(trees[ which(trees$Girth ==12.9),])

#4b
radius <- (trees$Girth/2)
print(radius)

csa<-function(x){
  area<-(pi*(x^2))
  return(area)
}
print(area <- csa(radius))

stem(radius)
hist(radius,col="blue")

plot(radius,area,main="Area vs. Radius",col="blue")

#4c

boxplot(area,main="Cross-Sectional Area ",horizontal=TRUE,notch=TRUE,col="Blue",xlab="Area")

#4d
boxplot(trees$Volume)
boxplot(trees$Volume,range =3)
boxplot.stats(trees$Volume,coef = 3, do.conf = TRUE, do.out = TRUE)

print(max(trees$Girth))
print(trees[ which(trees$Girth ==20.6),])

#5a

set.seed(124)
y<-rexp(n=100,rate=5.5)

set.seed(127)
x<-rnorm(n=100,mean=0,sd=0.15)

Object1<-cbind(x,y)
interqrange<-apply(Object1,2,IQR)
round(interqrange,digits=4)

#5b
par(mfrow=c(2,2))
hist(x,col="blue")
boxplot(x,horizontal=TRUE,col="blue")
hist(y,col="red")
boxplot(y,horizontal=TRUE,col="red")

#5c

par(mfrow=c(2,2))
qqnorm(x,main="X Sample Normal Q-Q Plot",col="red")
qqline(x,distribution = qnorm)
qqnorm(y,main="Y Sample Exponential Q-Q Plot",col="blue")
qqline(y,distribution =qnorm)
boxplot(x,range=3)
boxplot(y,range=3)


boxplot.stats(x,coef = 3, do.conf = TRUE, do.out = TRUE)
boxplot.stats(y,coef = 3, do.conf = TRUE, do.out = TRUE)

