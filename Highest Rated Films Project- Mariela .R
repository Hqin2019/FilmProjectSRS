install.packages("readxl")
install.packages("gvlma")
install.packages("lmtest")

library("readxl")
library("gvlma")
library("lmtest")

#Importing data frame from excel into R 
#using file path from my computer
Data<- read_excel("Movies_gross_rating.xlsx", col_names = TRUE)
Data
str(Data)
#510 obs. of 10 variables
#Obtain tables for chr variables (except "Title")
table(Data$`MPAA Rating`)
# G    PG PG-13     R 
#26   127   228   129 
table(Data$Genre)
#         Action       Adventure       Animation 
#             77              26              63 
#         Comedy           Crime           Drama 
#             94              16              57 
#         Family         Fantasy         History 
#             28              22               4 
#         Horror         Mystery         Romance 
#              9              10              26 
#Science Fiction         Thriller            War 
#             31              34               8 
#        Western 
#              5

HighestRatedFilms<- data.frame(Data[, -1])
HighestRatedFilms

#Determine if there are NA's and remove NA's.
sum(is.na(HighestRatedFilms))
#[1] 4
Films_omit<- na.omit(HighestRatedFilms)
sum(is.na(Films_omit))
#[1] 0

#Keep the numerical variables only and Normalize Data so that data points range from [0,1]
normalize <- function(x) {
  return ((x - min(x, na.rm=T)) / (max(x, na.rm = T) - min(x, na.rm=T)))
}
film_norm<- apply(Films_omit[ ,c(3, 4, 7, 8, 9)], 2, normalize)
head(film_norm)
#Data frame with normalized columns only
TopFilms_Norm<-data.frame(film_norm)
names(TopFilms_Norm)
#[1] "Budget"       "Gross"        "Runtime"     
#[4] "Rating"       "Rating.Count"
sum(is.na(TopFilms_Norm[ ,4]))
sum(is.na(TopFilms_Norm[ ,5]))

#Original Data Frame with Normalized Columns
#TopFilms<- data.frame(Films_omit[, -c(3, 4, 7, 8, 9)], TopFilms_Norm)
#head(TopFilms)

summary(TopFilms_Norm)
#Divide the data into training data and test data with 20-80 principle.
set.seed(123)
index<- sample(nrow(TopFilms_Norm), floor(nrow(TopFilms_Norm)*0.8))
#406 obs for test data and 102 obs for training data
train<- TopFilms_Norm[index, ]
row.names(train)<- NULL
test<- TopFilms_Norm[-index, ]
row.names(test)<-NULL

#Multiple Linear Regression 
#Do the other factors (Budget, Gross, Runtime, Rating Count) have a significant effect on the rating?
#Independent variables: Budget, gross, runtime, rating count
#Dependent variable: Rating

#Practice
fit.model<- lm(Rating ~., data=train)
fit.model
summary(fit.model)
#plot the diagnostic plots
par(mar=c(1,1,1,1))
#the above line can solves the issue of "figure margins too large"
par(mfrow=c(2,2))
plot(fit.model)
dev.off()
extractAIC(fit.model)
#[1]     5.000 -1651.305

#Assumptions
#1. Independence of observations
cor(TopFilms_Norm$Budget, TopFilms_Norm$Gross) #0.65 correlation
cor(TopFilms_Norm$Budget, TopFilms_Norm$Runtime)  #small correlation (0.26)
cor(TopFilms_Norm$Budget, TopFilms_Norm$Rating.Count) #small (0.298)
cor(TopFilms_Norm$Gross, TopFilms$Runtime)   #small correlation (0.3)
cor(TopFilms_Norm$Gross, TopFilms_Norm$Rating.Count) #moderate (0.5)
cor(TopFilms_Norm$Runtime, TopFilms_Norm$Rating.Count) #moderate (0.4)

#2. Normality
hist(TopFilms_Norm$Rating) #rough bell shape

#3. Linearity
plot(Rating~Budget, data = TopFilms_Norm) # ask
plot(Rating~Gross, data = TopFilms_Norm) # ask
plot(Rating~Runtime, data=TopFilms_Norm) #linear
plot(Rating~Rating.Count, data=TopFilms_Norm) #roughly linear

#constant variance and normality are most important assumptions (bptest, plots for normality)
#remove outliers to improve model, regression is sensitive 

#CHECK ASSUMPTIONS
#check constant variance
bptest(fit.model) #ok p-value, we want bigger than .05, but this one is ok
#check normality
shapiro.test(resid(fit.model))   #normality fails. too small p-value.
gvlma(fit.model) 

#Detect high influence points by Cook's distance.
train[cooks.distance(fit.model)> (4 / length(cooks.distance(fit.model))), ]
#new data
cleantrain<- train[-c(32, 47, 55, 101, 133, 147, 149, 164, 167, 192, 222, 248, 258, 271, 296, 297, 336, 377, 382, 390, 395),]
#385 obs of 5 variables.
CleanData.norm<- data.frame(cleantrain)
row.names(CleanData.norm)<- NULL
rating.model<- lm(Rating ~., data=CleanData.norm)
summary(rating.model)
extractAIC(rating.model)
#[1]     5.000 -1671.377
#a better AIC
par(mfrow=c(2,2))
plot(rating.model)
dev.off()

CleanData.norm[cooks.distance(rating.model)> (4 / length(cooks.distance(rating.model))), ]
cleandata2<- CleanData.norm[-c(31, 98, 109, 125, 175, 209, 212, 239, 260, 281, 329, 371, 395, 398, 399, 409, 439, 440, 466),]
CleanData.norm2<- data.frame(cleandata2)
#464 obs. of 5 variables.
row.names(CleanData.norm2)<- NULL
rating.model2<- lm(Rating ~., data=CleanData.norm2)
summary(rating.model2)
extractAIC(rating.model2)
#[1]     5.000 -2108.358
#a better AIC
par(mar=c(1,1,1,1))
par(mfrow=c(2,2))
plot(rating.model2)
dev.off()

bptest(rating.model) #p-value 0.006052 becomes smaller, fail the test.
shapiro.test(resid(rating.model)) #normality- show plots for improvement, improved p-value: 0.002749, still small though
gvlma(rating.model)


bptest(rating.model2) #p-value 0.007419 becomes smaller, fail the test.
shapiro.test(resid(rating.model2)) #normality- show plots for improvement, improved p-value: 0.02195, still small though
gvlma(rating.model2) #no improvement.


#check multicollinearity (Variation Inflation Factors)
library("car")
all_vifs<- vif(rating.model)
print(all_vifs) #maximum VIF is Gross 2.152045

#firsttvifs<- vif(fit.model)
#print(firsttvifs)

#check interactions
add1.test<- add1(rating.model, scope = .~. + .^2, test = "F")
add1.test[order(add1.test$`Pr(>F)`),]   #runtime/rating count and gross/runtime interact w/ each other
#add Runtime*Rating.Count
newmodel1<- lm(Rating ~ Budget + Gross + Runtime + Rating.Count + Runtime*Rating.Count, data = CleanData.norm)
summary(newmodel1)
extractAIC(newmodel1)
#[1]     6.000 -1694.784, a little better
bptest(newmodel1) #p-value 0.004565, fail the test.
shapiro.test(resid(newmodel1)) #p-value 0.007025, fail the test.
gvlma(newmodel1)# no improvement.
#add Gross*Runtime
newmodel2<- lm(Rating ~ Budget + Gross + Runtime + Rating.Count + Gross*Runtime, data = CleanData.norm)
summary(newmodel2)
extractAIC(newmodel2)
#[1]     6.000 -2122.099
bptest(newmodel2) #p-value 0.002982, fail the test.
shapiro.test(resid(newmodel2)) #p-value 0.005629, fail the test.
gvlma(newmodel2)# no improvement.
#add the three-way interaction term.
newmodel3<- lm(Rating ~ Budget + Gross + Runtime + Rating.Count + Runtime*Rating.Count*Gross, data = CleanData.norm)
summary(newmodel3)
extractAIC(newmodel3)
#[1]     9.000 -2136.71, smallest so far
bptest(newmodel3) #p-value 0.003908, fail the test.
shapiro.test(resid(newmodel3)) #p-value 0.01633, fail the test.
gvlma(newmodel3)# no improvement.
#try to explain how to interpret interactions, Runtime*RatingCount interact with rating

#Transformation
fit<- lm(Rating^2 ~ Budget + Gross + Runtime + Rating.Count + Runtime*Rating.Count, data = CleanData.norm)
summary(fit)
bptest(fit) #p-value 0.2913, pass the test.
shapiro.test(resid(fit)) #p-value 0.08414, pass the test.
gvlma(fit) #all pass
extractAIC(fit)
#[1]     6.000 -1658.558

#Remove Outliers
Q1rating<- quantile(TopFilms_Norm$Rating, .25)
Q3rating<- quantile(TopFilms_Norm$Rating, .75)
IQRrating<- IQR(TopFilms_Norm$Rating)
rating.new<-subset(TopFilms_Norm, TopFilms_Norm$Rating> (Q1rating - 1.5*IQRrating) & TopFilms_Norm$Rating< (Q3rating + 1.5*IQRrating))
dim(rating.new)
df1<- data.frame(rating.new)


Q1budget<- quantile(df1$Budget, .25)
Q3budget<- quantile(df1$Budget, .75)
IQRbudget<- IQR(df1$Budget)
Budget.new<- subset(df1, df1$Budget> (Q1budget - 1.5*IQRbudget) & df1$Budget< (Q3budget + 1.5*IQRbudget))
dim(Budget.new)
df2<- data.frame(Budget.new)

Q1gross<- quantile(df2$Gross, .25)
Q3gross<- quantile(df2$Gross, .75)
IQRgross<- IQR(df2$Gross)
Gross.new<- subset(df2, df2$Gross> (Q1gross - 1.5*IQRgross) & df2$Gross< (Q3gross + 1.5*IQRgross))
df3<- data.frame(Gross.new)
dim(df3)

Q1runtime<- quantile(df3$Runtime, .25)
Q3runtime<- quantile(df3$Runtime, .75)
IQRruntime<- IQR(df3$Runtime)
Runtime.new<- subset(df3, df3$Runtime> (Q1runtime - 1.5*IQRruntime) & df3$Runtime< (Q3runtime + 1.5*IQRruntime))
df4<- data.frame(Runtime.new)
dim(df4)

Q1ratingcount<- quantile(df4$Rating.Count, .25)
Q3ratingcount<- quantile(df4$Rating.Count, .75)
IQRratingcount<- IQR(df4$Rating.Count)
Ratingcount.new<- subset(df4, df4$Rating.Count> (Q1ratingcount - 1.5*IQRratingcount) & df4$Rating.Count< (Q3ratingcount + 1.5*IQRratingcount))
dim(Ratingcount.new)
df5clean<- data.frame(Ratingcount.new)
dim(df5clean)

#multiple linear regression #3
mlr<-lm(Rating ~., data=df5clean)
summary(mlr)
gvlma(mlr)


#Pie charts
#Top MPA Ratings
table(Films_omit$MPAA.Rating)
MPArating<- c(26, 126, 227, 129)
labels<- c("G", "PG", "PG-13", "R")
MPApercent<- round(MPArating/sum(MPArating)*100, 1)
lbls<- paste(MPApercent, "%", sep = "")
pie(MPArating, labels = lbls, main = "MPA Ratings of Top Films", cex.main = 1.4, cex = 1.3, col = rainbow(length(MPArating)))
legend("topright", c("G", "PG", "PG-13", "R"), cex = 1.1, fill = rainbow(length(MPArating)))

#3D Pie Chart of MPA Ratings
library("plotrix")
MPApie<- pie3D(MPArating, labels = labels, labelcex = 1.4, col = rainbow(length(MPArating)), explode = 0.07, main = "MPA Rating Top Films", cex.main = 1.4)

#Top Genres 
table(Films_omit$Genre)
genre<- c(76, 26, 63, 94, 16, 56, 28, 22, 4, 9, 10, 26, 31, 34, 8, 5)
glabels<- c("Action", "Adventure", "Animation", "Comedy", "Crime", "Drama", "Family", "Fantasy", "History", "Horror", "Mystery", "Romance", "Science Fiction", "Thriller", "War", "Western")
gpercent<- round(genre/sum(genre)*100, 1)
glbls<- paste(gpercent, "%", sep = "")
pie(genre, labels = glbls, main = "Top Film Genres", cex=0.59, cex.main = 1.7, col = c("pink", "yellow", "brown", "white", "black", "grey", "turquoise", "red", "grey90", "purple", "blue", "orange", "green", "navy blue", "grey40", "gold"))
legend("bottomleft", c("Action", "Adventure", "Animation", "Comedy", "Crime", "Drama", "Family", "Fantasy", "History", "Horror", "Mystery", "Romance", "Science Fiction", "Thriller", "War", "Western"), cex = 0.7, fill =c("pink", "yellow", "brown", "white", "black", "grey", "turquoise", "red", "grey90", "purple", "blue", "orange", "green", "navy blue", "grey40", "gold"))

#3D Pie Chart of Top Genres
install.packages("plotrix")
library(plotrix)
gslices<- c(76, 26, 63, 94, 16, 56, 28, 22, 4, 9, 10, 26, 31, 34, 8, 5)
glabel<- c("Action", "Adventure", "Animation", "Comedy", "Crime", "Drama", "Family", "Fantasy", "History", "Horror", "Mystery", "Romance", "Science Fiction", "Thriller", "War", "Western")
genrepie<-pie3D(gslices, labels = glabel, labelcex = 1, col = c("pink", "yellow", "brown", "white", "black", "grey", "turquoise", "red", "grey90", "purple", "blue", "orange", "green", "navy blue", "grey40", "gold"), explode = 0.1,main = "Top Film Genres", cex.main=1.7)

#Prediction comparisons
predict(fit, newdata=test)



