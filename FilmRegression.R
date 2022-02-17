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
cleantrain<- train[-c(27, 32, 55, 97, 121, 131, 133, 147, 155, 164, 178, 187, 222, 223, 226, 243, 248, 258, 265, 271, 273, 311, 313, 377, 390, 395),]
#380 obs of 6 variables.
CleanData.norm<- data.frame(cleantrain)
row.names(CleanData.norm)<- NULL
rating.model<- lm(Rating ~., data=CleanData.norm)
summary(rating.model)
#Significant: Budget, Gross, Runtime, Rating.Count, GenreAnimation, GenreDrama
#Adjusted R-squared: 0.5965
extractAIC(rating.model)
#[1]    18.00 -1711.49
par(mar=c(1,1,1,1))
par(mfrow=c(2,2))
plot(rating.model)
dev.off()
#check assumptions
bptest(rating.model) #p-value 0.2939, pass the test.
shapiro.test(resid(rating.model)) #too small p-value, fails.
gvlma(rating.model)# pass "Kurtosis" and "Heteroscedasticity"

#check multicollinearity (Variation Inflation Factors)
all_vifs<- vif(rating.model)
print(all_vifs) #Genre has VIF 3.046650

#check interactions
add1.test<- add1(rating.model, scope = .~. + .^2, test = "F")
add1.test[order(add1.test$`Pr(>F)`),]   #runtime/rating count and gross/runtime interact w/ each other
#add Runtime*Rating.Count
newmodel1<- lm(Rating ~ Budget + Gross + Runtime + Rating.Count + Runtime*Rating.Count + Genre, data = CleanData.norm)
summary(newmodel1)
#Significant: Budget, Gross, Rating.Count, GenreAnimation, GenreDrama, Runtime:Rating.Count
#Adjsuted R-squared: 0.619, better
extractAIC(newmodel1)
#[1]    19.00 -1732.38

