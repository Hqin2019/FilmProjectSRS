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
#Response: Rating
#Predictors: Budget, Gross, Runtime, Rating.Count

#Remove NA's
Films_omit<- na.omit(Film_Data)
sum(is.na(Films_omit))
#[1] 0

#Normalize the data
normalize <- function(x) {
  return ((x - min(x, na.rm=T)) / (max(x, na.rm = T) - min(x, na.rm=T)))
}
film_norm<- apply(Films_omit, 2, normalize)

#Divide the data into training data and test data with 20-80 principle.
set.seed(123)
index<- sample(nrow(Films_omit), floor(nrow(Films_omit)*0.8))
#406 obs for test data and 102 obs for training data
train<- TopFilms_Norm[index, ]
row.names(train)<- NULL
test<- TopFilms_Norm[-index, ]
row.names(test)<-NULL

#First (full) model
fit.model<- lm(Rating ~., data=train)
summary(fit.model)
#significant: Budget, Gross, Runtime, Rating.Count
#Adjusted R-squared: 0.4815
#plot the diagnostic plots
par(mar=c(1,1,1,1))
#the above line can solves the issue of "figure margins too large"
par(mfrow=c(2,2))
plot(fit.model)
dev.off()
extractAIC(fit.model)
#[1]     5.000 -1651.305

#check constant variance
bptest(fit.model) #p-value = 0.02217, less than 0.05 but bigger than 0.01
#check normality
shapiro.test(resid(fit.model))  #fails, too small p-value.
#Check all assumption at once.
gvlma(fit.model)#pass one.

#Detect high influence points by Cook's distance.
train[cooks.distance(fit.model)> (4 / length(cooks.distance(fit.model))), ]
#new data
cleantrain<- train[-c(32, 47, 55, 101, 133, 147, 149, 164, 167, 192, 222, 248, 258, 271, 296, 297, 336, 377, 382, 390, 395),]
#385 obs of 5 variables.
CleanData.norm<- data.frame(cleantrain)
row.names(CleanData.norm)<- NULL
rating.model<- lm(Rating ~., data=CleanData.norm)
summary(rating.model)
#Significant: Budget, Gross, Runtime, Rating.Count
#Adjusted R-squared: 0.5058
extractAIC(rating.model)
#[1]     5.000 -1671.377, better
par(mar=c(1,1,1,1))
par(mfrow=c(2,2))
plot(rating.model)
dev.off()
#check assumptions
bptest(rating.model) #p-value 0.005668 becomes smaller, fail the test.
shapiro.test(resid(rating.model)) #normality- show plots for improvement, improved p-value: 0.001524, still small though
gvlma(rating.model)# pass two

#check multicollinearity (Variation Inflation Factors)
all_vifs<- vif(rating.model)
print(all_vifs) #maximum VIF is Gross 2.251895, nothing to worry about.

#check interactions
add1.test<- add1(rating.model, scope = .~. + .^2, test = "F")
add1.test[order(add1.test$`Pr(>F)`),]   #runtime/rating count and gross/runtime interact w/ each other
#add Runtime*Rating.Count
newmodel1<- lm(Rating ~ Budget + Gross + Runtime + Rating.Count + Runtime*Rating.Count, data = CleanData.norm)
summary(newmodel1)
#Significant: Budget, Gross, Rating.Count, Runtime:Rating.Count
#Adjsuted R-squared: 0.5362, better
extractAIC(newmodel1)
#[1]     6.000 -1694.784, better

