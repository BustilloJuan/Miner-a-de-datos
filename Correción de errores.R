install.packages("deducorrect")
library(deducorrect)

file.choose()
id<-read.csv("C:\\Users\\34634\\Desktop\\Algoritmos de Machine Learning\\marx.csv",
             sep=" ", stringsAsFactors = F
             );id
R<-correctionRules("C:\\Users\\34634\\Desktop\\Algoritmos de Machine Learning\\conversions.txt")
R

corregidos<-correctWithRules(R,id);corregidos$corrected

#########Correción deductiva
e <- editmatrix("x + y == z")
d <- data.frame(x = 100, y = -100, z = 200)
cor <- correctSigns(e, d)
cor$corrected




e <- editmatrix("x + y == z")
d <- data.frame(x = 123, y = 132, z = 246)
cor <- correctTypos(e, d)
cor$corrected


####Imputación determinista
E <- editmatrix(expression(
  staff + cleaning + housing == total,
  staff >= 0,
  housing >= 0,
  cleaning >= 0
))
dat <- data.frame(
  staff = c(100,100,100),
  housing = c(NA,50,NA),
  cleaning = c(NA,NA,NA),
  total = c(100,180,NA)
)
dat


correction<-deduImpute(E,dat)
correction$corrected


E <- editarray(expression(
  age %in% c("adult","under-aged"),
  driverslicense %in% c(TRUE, FALSE),
  if ( age == "under-aged" ) !driverslicense
))
E


dat <- data.frame( age = NA,driverslicense = TRUE)
dat

cor <- deduImpute(E,dat)
cor$corrected