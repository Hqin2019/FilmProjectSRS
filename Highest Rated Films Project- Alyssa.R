install.packages("readxl")
library("readxl")
Data<- read_excel("/Users/marielaponce/Downloads/Movies_gross_rating.xlsx")
Data
MovieID<- Data[,1]
Title<- Data[,2]
MPARating<- Data[,3]
Budget<- Data[,4]
Gross<- Data[,5]
ReleaseDate<- Data[,6]
Genre<- Data[,7]
Runtime<- Data[,8]
Rating<- Data[,9]
RatingCount<- Data[,10]
HighestRatedFilms<- data.frame(Title, MPARating, Budget, Gross, ReleaseDate, Genre, Runtime, Rating, RatingCount)
HighestRatedFilms

#normalize 0 to 1
normalize<- function(x) {
  return((x-min(x, na.rm=T))/(max(x, na.rm = T)- min(x, na.rm=T)))
}
film_norm<- apply(NHighestRatedFilms[ ,c(3,4, 7, 8, 9)], 2, normalize)
film_norm

# to omit NA from whole data frame
NHighestRatedFilms<- na.omit(HighestRatedFilms)
NHighestRatedFilms

#to omit from columns
newdatarating<- na.omit(Rating)
newdatarating
newdatacount<- na.omit(RatingCount)
newdatacount
mean(newdatarating)
mean(newdatacount)

# Calculate mean of each numerical category
mrun<- mean(Data$Runtime)
mean(film_norm$Budget)
mrun
mbudget<- mean(Data$Budget)
mbudget
mgross<- mean(Data$Gross)
mgross
mrating<- mean(newdatarating)
mrating
mcount<- mean(newdatacount)
mcount
means<- c(mrun, mbudget, mgross, mrating, mcount)
means
barplot(means, xlab= "Mean", ylab="Numerical Value", log='y',col="light blue", border="black",main="Means of Highest Rated Films", names.arg =c("Runtime", "Budget", "Gross", "Rating", "Rating Count"), )

#Get min and max of our Data 
max(Data$Budget)
min(Data$Budget)
max(Data$Gross)
min(Data$Gross)
max(Data$Runtime)
min(Data$Runtime)
max(newdatarating)
min(newdatarating)
max(newdatacount)
min(newdatacount)
minsandmax<- c(max(Data$Budget), min(Data$Budget), max(Data$Gross), min(Data$Gross), max(Data$Runtime), min(Data$Runtime), max(newdatarating), min(newdatarating), max(newdatacount), min(newdatacount))
barplot(minsandmax)


#correlation graphs
plot(Data[, -(1:3)])
plot(Data$Runtime, Data$Rating, xlab= "Runtime (minutes)", ylab="Rating (on a scale of 1-10)", main="Runtime vs Rating", col="blue")
lmrr<- lm(Data$Rating~Data$Runtime, data=Data)
lmrr
abline(lmrr, col="red")
cor(NHighestRatedFilms$Runtime, NHighestRatedFilms$Rating)
#positive but not very strong relationship

plot(Data$`Rating Count`,Data$Rating, xlab= "Rating Count", ylab="Rating (on a scale of 1-10)", main="Rating Count vs Rating", col="blue")
lmrcr<- lm(Data$Rating~Data$`Rating Count`, data=Data)
lmrcr
abline(lmrcr, col="red")
cor(NHighestRatedFilms$RatingCount, NHighestRatedFilms$Rating)
#positive and moderate relationship

plot(Data$Budget, Data$Rating, xlab= "Budget",  ylab =  "Rating", main="Budget vs Rating", col="blue")
lmbr<- lm(Data$Rating~Data$Budget, data=Data)
lmbr
abline(lmbr, col="red")
cor(NHighestRatedFilms$Budget, NHighestRatedFilms$Rating)
#positive and weak relationship

plot( Data$Gross, Data$Rating, ylab= "Rating", xlab= "Gross Income",  main="Gross Income vs Rating", col="blue")
lmbg<- lm(Data$Rating~Data$Gross, data=Data)
lmbg
abline(lmbg, col="red")
cor(NHighestRatedFilms$Gross, NHighestRatedFilms$Rating)
#positive and weak relationship

#histograms to better understand data
hist(Data$Runtime,xlab="Runtime (minutes)", xlim=c(50,250), ylab="# of movies", ylim= c(0,100), main="Movie Runtimes", col="pink")
hist(Data$Budget, xlab="Budget($)", ylim=c(0,200), main="Histogram of Budget")
hist(Data$Gross, xlab="Gross ($)", xlim= c(0,3000000000), ylim=c(0,250), main="Histogram of Gross Income")
hist(Data$`Rating Count`, xlab="Rating Count", xlim=c(0,2500000), ylim=c(0,250), main="Histogram of Rating Count")
hist(Data$Rating, xlab="Rating", ylim=c(0,150), main="Histogram of Rating")

table(Data$`MPAA Rating`)
table(Data$Genre)

plot(Data[, -(1:3)])

# CATEGORICAL DATA
mparate<- table(Data$`MPAA Rating`)
mparate
genretbl<- table(Data$Genre)
genretbl
barplot(genretbl, horiz=TRUE, xlim= c(0, 100), ylab= "Genre", main= "Movie Genres", names.arg=c("Action","Adv.","Animation","Comedy","Crime","Drama","Family","Fantasy","History","Horror","Mystery","Romance","Science Fiction","Thriller","War","Western"))
pie(genretbl, main="Genre")
pie(mparate, main="MPAA Rating")


#boxplots and add summaries
summary(Data$Budget)
b1<- boxplot(Data$Budget, horizontal=TRUE, main="Budget", border="blue", xlab="Amount ($) ")
summary(Gross)
b2<- boxplot(Data$Gross, horizontal=TRUE, main=" Gross Income", border="blue", xlab="Gross amount ($) ")
summary(Runtime)
b3<- boxplot(Data$Runtime, horizontal=TRUE, main="Runtime", border="blue", xlab= "Runtime (minutes) ")
summary(Rating)
b4<- boxplot(Data$Rating, horizontal= TRUE, main= "Rating", border="blue", xlab= " Rating (scale 1-10) ")
summary(Data$`Rating Count`)
b5<- boxplot(Data$`Rating Count`, horizontal=TRUE, main="Rating Count", border="blue", xlab= "Rating Count") 
