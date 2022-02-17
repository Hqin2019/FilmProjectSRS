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
