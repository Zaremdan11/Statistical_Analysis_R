library(ggplot2)
library(gridExtra)
library(knitr)

# b) Use read.csv() to read the abalones.csv into R, assigning the data frame to "mydata."

mydata <- read.csv("abalones.csv", sep = ",")


# c) Use the str() function to verify the structure of "mydata." You should have 1036 observations
# of eight variables.

str(mydata)

# d) Define two new variables, VOLUME and RATIO. Use the following statements to define VOLUME and
# RATIO as variables appended to the data frame "mydata."

mydata$VOLUME <- mydata$LENGTH * mydata$DIAM * mydata$HEIGHT
mydata$RATIO <- mydata$SHUCK / mydata$VOLUME

#1a Use *summary()* to obtain and present descriptive statistics from mydata.  
#Use table() to present a frequency table using CLASS and RINGS. There should be 115 cells 
#in the table you present.  
summary(mydata)
out <- table(mydata$CLASS,mydata$RINGS)
print(out)

#1b Generate a table of counts using SEX and CLASS

table_counts <- table(mydata$SEX, mydata$CLASS)
table_counts ## without column and row totals
addmargins(table_counts)  ## with column and row totals

barplot(table_counts,
        main = "Class membership, Sex-differentiated",
        xlab = "Class",
        col = c("red","blue","yellow"), beside = TRUE)

legend("topleft",
       c("Female","Infant","Male"),
       fill = c("red","blue","yellow")
)       

#1c
set.seed(123)
work <- mydata[sample(1:nrow(mydata),200,replace=FALSE),] 
plot(work[, 2:6])

#2a

p <- ggplot(data = mydata, 
            aes(x=VOLUME, y=WHOLE, color=CLASS))+
  geom_point()
p

#2b
mydata$RATIO2 <- mydata$SHUCK / mydata$WHOLE
MaxShuckWholeRatio <- max(mydata$RATIO2)

p2 <- ggplot(data = mydata, 
            aes(x=WHOLE, y=SHUCK, color=CLASS))+
  geom_point() 
p2 + geom_abline(intercept = 0, slope = MaxShuckWholeRatio)

#3a
mydataI <- mydata[ which(mydata$SEX=='I'),]
mydataF <- mydata[ which(mydata$SEX=='F'),]
mydataM <- mydata[ which(mydata$SEX=='M'),]
     
par(mfrow = c(3,3)) 
hist(mydataI$RATIO, main="Infant Ratio",col = 'red')
hist(mydataF$RATIO, main="Female Ratio",col = 'blue')
hist(mydataM$RATIO,main="Male Ratio",col = 'yellow')

boxplot(mydataI$RATIO,main="Infant Ratio",col = 'red')
boxplot(mydataF$RATIO,main="Female Ratio",col = 'blue')
boxplot(mydataM$RATIO,main="Male Ratio",col = 'yellow')

qqnorm(mydataI$RATIO,main="Infant Ratio",col = 'red') 
qqnorm(mydataF$RATIO,main="Female Ratio",col = 'blue') 
qqnorm(mydataM$RATIO,main="Male Ratio",col = 'yellow')


#3b
boxplot.stats(mydataI$RATIO)
boxplot.stats(mydataI$RATIO,coef = 3)

boxplot.stats(mydataF$RATIO)
boxplot.stats(mydataF$RATIO,coef = 3)

boxplot.stats(mydataM$RATIO)
boxplot.stats(mydataM$RATIO,coef = 3)


#4a
t<-ggplot(data = mydata, aes(x=CLASS, y=VOLUME)) + geom_boxplot(aes(fill=CLASS))
r<-ggplot(data = mydata, aes(x=CLASS, y=WHOLE)) + geom_boxplot(aes(fill=CLASS))
s1 <- ggplot(data = mydata,aes(x=RINGS, y=VOLUME, color=RINGS))+ geom_point()
s2 <- ggplot(data = mydata,aes(x=RINGS, y=WHOLE, color=RINGS))+ geom_point()
grid.arrange(t, r, s1, s2, ncol=2)

#5a compute the mean values of VOLUME, SHUCK and RATIO for each combination of SEX and CLASS.
#Then, using *matrix()*, create matrices of the mean values. Using the "dimnames" argument within 
#*matrix()* or the *rownames()* and *colnames()* functions on the matrices, label the rows by SEX 
#and columns by CLASS.
VolumeM <- aggregate(mydata$VOLUME, by = list(mydata$SEX, mydata$CLASS), mean)
matrix(VolumeM$x,nrow=3, ncol = 5, byrow = FALSE, dimnames = list(c("F","I","M"), c("A1","A2","A3","A4","A5")))

ShuckM <- aggregate(mydata$SHUCK, by = list(mydata$SEX, mydata$CLASS), mean)
matrix(ShuckM$x,nrow=3, ncol = 5, byrow = FALSE, dimnames = list(c("F","I","M"), c("A1","A2","A3","A4","A5")))

RatioM <- aggregate(mydata$RATIO, by = list(mydata$SEX, mydata$CLASS), mean)
matrix(RatioM$x,nrow=3, ncol = 5, byrow = FALSE, dimnames = list(c("F","I","M"), c("A1","A2","A3","A4","A5")))

#5b
interaction.plot(x.factor = mydata$CLASS,    # variable to plot on x-axis
                 trace.factor = mydata$SEX, # variable to specify "traces"; here, lines
                 response = mydata$RATIO,    # variable to plot on y-axis
                 fun = mean,  # summary statistic to be plotted for response variable
                 type = "l",     # type of plot, here "l" for lines
                 ylab = "Ratio",
                 xlab = "Class",
                 col = c("red4", "blue4","yellow4"),
                 lty = 1,  # line type
                 lwd = 2,  # line width
                 trace.label = "Sex",  # label for legend
                 xpd = FALSE) #,  # 'clip' legend at border

interaction.plot(x.factor = mydata$CLASS,    # variable to plot on x-axis
                 trace.factor = mydata$SEX, # variable to specify "traces"; here, lines
                 response = mydata$VOLUME,    # variable to plot on y-axis
                 fun = mean,  # summary statistic to be plotted for response variable
                 type = "l",     # type of plot, here "l" for lines
                 ylab = "Volume",
                 xlab = "Class",
                 col = c("red4", "blue4","yellow4"),
                 lty = 1,  # line type
                 lwd = 2,  # line width
                 trace.label = "Sex",  # label for legend
                 xpd = FALSE) #,  # 'clip' legend at border

interaction.plot(x.factor = mydata$CLASS,    # variable to plot on x-axis
                 trace.factor = mydata$SEX, # variable to specify "traces"; here, lines
                 response = mydata$SHUCK,    # variable to plot on y-axis
                 fun = mean,  # summary statistic to be plotted for response variable
                 type = "l",     # type of plot, here "l" for lines
                 ylab = "Shuck",
                 xlab = "Class",
                 col = c("red4", "blue4","yellow4"),
                 lty = 1,  # line type
                 lwd = 2,  # line width
                 trace.label = "Sex",  # label for legend
                 xpd = FALSE) #,  # 'clip' legend at border

#5c
mydataI2 <- mydata[mydata$SEX =='I' & mydata$RINGS < 16,]
mydataA <- mydata[mydata$SEX !='I' & mydata$RINGS < 16,]

v<-ggplot(data = mydataI2, aes(x=RINGS, y=VOLUME)) + geom_boxplot(aes(fill=RINGS))
v
w<-ggplot(data = mydataA, aes(x=RINGS, y=VOLUME)) + geom_boxplot(aes(fill=RINGS))
x<-ggplot(data = mydataI2, aes(x=RINGS, y=WHOLE)) + geom_boxplot(aes(fill=RINGS))
y<-ggplot(data = mydataA, aes(x=RINGS, y=WHOLE)) + geom_boxplot(aes(fill=RINGS))

grid.arrange(v, w, x, y, ncol=2)
