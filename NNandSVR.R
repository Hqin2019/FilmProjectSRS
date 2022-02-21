library(MASS)
library(neuralnet)
library(caret)

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
