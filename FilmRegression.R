#install.packages("readxl")
#install.packages("gvlma")
#install.packages("lmtest")
#install.packages("car")
#install.packages("tidyverse")
#install.packages("caret")
#install.packages("jtools")
#install.packages("interactions")


library("readxl")
library("gvlma")
library("lmtest")
library(car)
library(tidyverse)
library(caret)
library(jtools)
library(interactions)

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

#First (full) model
fit.model<- lm(Rating ~., data=train)
summary(fit.model)
#significant at 0.05 level: Budget, Gross, Runtime, Rating.Count, GenreAnimation,
#GenreFamily, GenreRomance
#Adjusted R-squared: 0.5692
#plot the diagnostic plots
par(mar=c(1,1,1,1))
#the above line can solves the issue of "figure margins too large"
par(mfrow=c(2,2))
plot(fit.model)
dev.off()
extractAIC(fit.model)
#[1]    20.000 -1711.969

#check constant variance
bptest(fit.model) #p-value = 0.106, pass.
#check normality
shapiro.test(resid(fit.model))  #fails, too small p-value.
#Check all assumption at once.
gvlma(fit.model)#pass "Heteroscedasticity".

#Detect high influence points by Cook's distance.
train[cooks.distance(fit.model)> (4 / length(cooks.distance(fit.model))), ]
#new data
cleantrain<- train[-c(27, 32, 97, 133, 147, 155, 164, 187, 222, 223, 226, 243, 248, 258, 265, 271, 273, 377, 390, 395),]
#380 obs of 6 variables.

CleanData.norm[cooks.distance(rating.model)> (4 / length(cooks.distance(rating.model))), ]
cleandata2<- CleanData.norm[-c(22, 46, 55, 97, 154, 170, 197, 258, 175, 176, 284, 313, 331, 339, 352, 358),]
CleanData.norm2<- data.frame(cleandata2)
#364 obs. of 6 variables.
row.names(CleanData.norm2)<- NULL
rating.model2<- lm(Rating ~., data=CleanData.norm2)
summary(rating.model2)
par(mar=c(1,1,1,1))
par(mfrow=c(2,2))
plot(rating.model2)
dev.off()
#check assumptions
bptest(rating.model2) #p-value 0.2939, pass the test.
shapiro.test(resid(rating.model2)) #too small p-value, fails.
gvlma(rating.model)# pass "Kurtosis" and "Heteroscedasticity"


CleanData.norm<- data.frame(cleantrain)
row.names(CleanData.norm)<- NULL
rating.model<- lm(Rating ~., data=CleanData.norm)
summary(rating.model)
#Significant: Budget, Gross, Runtime, Rating.Count, GenreAnimation, GenreDrama
#Adjusted R-squared: 0.6033
extractAIC(rating.model)
#[1]    20.00 -1731.261
par(mar=c(1,1,1,1))
par(mfrow=c(2,2))
plot(rating.model)
dev.off()
#check assumptions
bptest(rating.model) #p-value 0.1759, pass the test.
shapiro.test(resid(rating.model)) #too small p-value, fails.
gvlma(rating.model)# pass "Kurtosis" and "Heteroscedasticity"

#check multicollinearity (Variation Inflation Factors)
all_vifs<- vif(rating.model)
print(all_vifs) #Genre has VIF 3.177127

#check interactions
add1.test<- add1(rating.model, scope = .~. + .^2, test = "F")
add1.test[order(add1.test$`Pr(>F)`),]   #runtime/rating count and gross/runtime interact w/ each other
#add Runtime*Rating.Count
newmodel1<- lm(Rating ~ Budget + Gross + Runtime + Rating.Count + Runtime*Rating.Count + Genre, data = CleanData.norm)
summary(newmodel1)
#Significant: Budget, Gross, Rating.Count, GenreAnimation, GenreDrama, Runtime:Rating.Count
#Adjsuted R-squared: 0.6274, better
extractAIC(newmodel1)
#[1]    21.00 -1754.598

par(mar=c(1,1,1,1))
par(mfrow=c(2,2))
plot(newmodel1)
dev.off()
#check assumptions
bptest(newmodel1) #p-value 0.3124, pass the test.
shapiro.test(resid(newmodel1)) #too small p-value, fails.
gvlma(newmodel1)# pass "Kurtosis" and "Heteroscedasticity"

#Making interaction plot.
interact_plot(newmodel1, pred = Rating.Count, modx = Runtime)


#Prediction comparisons
predict(newmodel1, newdata=test)
predictions <- newmodel1 %>% predict(test)
# Model performance
# (a) Prediction error, RMSE
RMSE(predictions, test$Budget)
#[1] 0.4265564


