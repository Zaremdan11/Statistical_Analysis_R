install.packages("knitr")
install.packages("rmarkdown")

install.packages('rpart')
install.packages('rpart.plot')
install.packages('RColorBrewer')

library(rpart)
library(rpart.plot)
library(RColorBrewer)



mydata <-  read.csv("titanic.csv") 


binary.model <- rpart(Survived ~ Pclass + Sex + Age + Cabin + Embarked, data = mydata, cp = .02) 

rpart.plot(binary.model)


