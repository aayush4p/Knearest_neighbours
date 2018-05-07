library(ISLR)
head(iris)


###feature scaling


scaled.data <- scale(iris[1:4])
##scalled first 4 columns of the data set
print(var(scaled.data))

final.data <- cbind(scaled.data,iris[5])


###splitting into training and testing

set.seed(101)
library(caTools)


sample <- sample.split(final.data$Species ,SplitRatio = 0.7)

train <- subset(final.data, sample == T)
test <- subset(final.data , sample == F)



###knn model
library(class)


predicted.species <- knn(train[1:4],test[1:4],train$Species,k=1)
print(predicted.species)

####calculating the misclassification rate

mis.rate <- mean(test$Species != predicted.species)
print(mis.rate)


###choosing the best k value for the model 
######by elbow curve plot

predicted.species <- NULL
error.rate <-  NULL

for (i in 1:10) {
  
  set.seed(101)
  predicted.species <- knn(train[1:4],test[1:4],train$Species,k=i)
  error.rate[i]= mean(test$Species != predicted.species)
}

 ###getting error rates in a data frame so that we may plot them


k.values <- 1:10
error.dataframe <- data.frame(error.rate,k.values)

library(ggplot2)

pl <- ggplot(error.dataframe,aes(x=k.values,y=error.rate)) + geom_point()

pl <- pl + geom_line(lty='dotted',color='red',size=3)

print(pl)


###the graph shows that the optimum k value will be between 2.5 to 6 since data is small



