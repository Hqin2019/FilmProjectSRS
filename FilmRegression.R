install.packages("readxl")
install.packages("gvlma")
install.packages("lmtest")

library("readxl")
library("gvlma")
library("lmtest")
library(car)

#Importing data frame from excel into R 
Data<- read_excel("Movies_gross_rating.xlsx", col_names = TRUE)
#510 obs. of 10 variables
#Only include the numerical columns
Film_Data<- data.frame(Data[, c(4, 5, 8, 9, 10)])
#510 obs. of 5 variables
#Response: Budget
#Predictors: Gross, Runtime, Rating, Rating.Count

