library(MASS)

#Importing data frame from excel into R 
Data<- read_excel("Movies_gross_rating.xlsx", col_names = TRUE)
#510 obs. of 10 variables
Film_Data<- data.frame(Data[, c(4, 5, 7, 8, 9, 10)])
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
film_norm<- data.frame(apply(Films_omit[, -3], 2, normalize), Genre=factor(Films_omit[, 3]))

#Divide the data into training data and test data with 20-80 principle.
set.seed(123)
index<- sample(nrow(film_norm), floor(nrow(film_norm)*0.8))
#406 obs for test data and 102 obs for training data
train<- film_norm[index, ]
row.names(train)<- NULL
test<- film_norm[-index, ]
row.names(test)<-NULL
