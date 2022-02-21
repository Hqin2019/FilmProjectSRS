library(MASS)
library(neuralnet)
library(caret)
library(plyr)
library(e1071)

#Importing data frame from excel into R 
Data<- read_excel("Movies_gross_rating.xlsx", col_names = TRUE)
#510 obs. of 10 variables
Film_Data<- data.frame(Data[, c(4, 5, 8, 9, 10)], Genre=as.factor(Data$Genre))
#510 obs. of 6 variables
#Response: Rating
#Predictors: Budget, Gross, Runtime, Rating.Count, Genre
#Genre: 16 levels

#Remove NA's
Films_omit<- na.omit(Film_Data)
sum(is.na(Films_omit))
#[1] 0


#Normalize the data
normalize <- function(x) {
  return ((x - min(x, na.rm=T)) / (max(x, na.rm = T) - min(x, na.rm=T)))
}
film_norm<- data.frame(apply(Films_omit[, -6], 2, normalize), Genre=Films_omit[, 6])
#508 obs. of 6 variables

#Perform One-Hot Encoding: Genre
dummy<- dummyVars("~.", data=film_norm)
film_norm<- data.frame(predict(dummy, newdata=film_norm))
#508 obs. of 21 variables(5 num + 16 Genre).

#Divide the data into training data and test data with 20-80 principle.
set.seed(123)
index<- sample(nrow(film_norm), floor(nrow(film_norm)*0.8))
#406 obs for test data and 102 obs for training data
train<- film_norm[index, ]
row.names(train)<- NULL
test<- film_norm[-index, ]
row.names(test)<-NULL


#Fit the neural network
#neuralnet() does not accept y~.
#hidden= c(5, 3) means 5 and 3 neurons for layer 1 and layer 2, respectively.
#linear.output =TRUE do regression; linear.output= FALSE do classification
n<- names(train)
f<- as.formula(paste("Rating ~", paste(n[!n %in% "Rating"], collapse = "+")))
nn<- neuralnet(f, data=train, hidden=c(5, 3), linear.output=TRUE)
#Blue lines show the bias term added in each step.
plot(nn)

#Predicting medv using the neural network
#Scale the prediction output back in order to make a meaningful comparison.
pr.nn<- compute(nn, test[, -4])
pr.nn_ <- pr.nn$net.result*(max(Data$Rating, na.rm=TRUE)- min(Data$Rating, na.rm=TRUE))+min(Data$Rating, na.rm=TRUE)
test.r <- (test$Rating)*(max(Data$Rating, na.rm=TRUE)- min(Data$Rating, na.rm=TRUE))+min(Data$Rating, na.rm=TRUE)
# Model performance
# (a) Prediction error, RMSE
RMSE(pr.nn_, test.r)
#[1] 0.6044809

#Visual plot
par(mfrow=c(1, 2))
plot(test.r, pr.nn_, col = 'red', main= 'Real vs predicted NN', pch=18, cex= 0.7)
abline(0, 1, lwd=2)
legend('bottomright', legend='NN', pch=18, col ='red', bty='n')
plot(test.results, pr.results, col = 'blue', main='Real vs predicted lm', pch=18, cex=0.7)
abline(0, 1, lwd=2)
legend("bottomright", legend= 'LM', pch=18, col = 'blue', bty='n', cex=.95)
dev.off()
#A (fast) cross validation
#For the NN, we split the data as 90% train and 10% test in a random way for 10 times.
set.seed(450)
cv.error<- NULL
k<- 10
#initializing a progress bar using the "plyr" package
pbar<- create_progress_bar('text')
pbar$init(k)
for (i in 1:k){
  index<- sample(1:nrow(film_norm), round(0.9*nrow(film_norm)))
  train.cv<- film_norm[index, ]
  test.cv<- film_norm[-index, ]
  nn<- neuralnet(f, data=train.cv, hidden=c(5, 3), linear.output=TRUE)
  pr.nn<- compute(nn, test.cv[, -4])
  pr.nn_<- pr.nn$net.result*(max(Data$Rating, na.rm=TRUE)- min(Data$Rating, na.rm=TRUE))+min(Data$Rating, na.rm=TRUE)
  test.cv.r<- (test.cv$Rating)*(max(Data$Rating, na.rm=TRUE)- min(Data$Rating, na.rm=TRUE))+min(Data$Rating, na.rm=TRUE)
  cv.error[i]<- RMSE(pr.nn_, test.cv.r)
  pbar$ste()
}

#Calculate the average RMSE and plot the results as a boxplot
mean(cv.error)
#[1] 0.5344207
cv.error
boxplot(cv.error, xlab='RMSE CV', col='cyan', border = 'blue', names='CV error (MSE)', main = 'CV error (MSE) for NN', horizontal = TRUE)

#Fit the Support Vector Regression
set.seed(1)
tune.out=tune(svm,f,data=train,kernel="radial", ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100) ))
summary(tune.out)
bestmod<- tune.out$best.model
summary(bestmod)

#Prediction
#Use the best model to predict the class label of the test data
ypred=predict(bestmod ,test) 
pr.SVR <- ypred*(max(Data$Rating, na.rm=TRUE)- min(Data$Rating, na.rm=TRUE))+min(Data$Rating, na.rm=TRUE)
# Model performance
# (a) Prediction error, RMSE
RMSE(pr.SVR, test.r)
#[1] 0.5157974