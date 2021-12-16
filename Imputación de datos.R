#####Imputación de datos
dat <- data.frame(
  staff = c(100,100,100),
  housing = c(NA,50,NA),
  cleaning = c(NA,NA,NA),
  total = c(100,180,NA)
)
dat


dat$housing[is.na(dat$housing)]<-mean(dat$housing,na.rm=T)
 #Mi algoritmo
for(i in 2:4){
  dat[which(is.na(dat[,i])),i]<-mean(dat[,i],na.rm=T)
}


library(Hmisc)
dat$housing<-impute(dat$housing,fun=mean)
dat$housing<-impute(dat$housing,fun=median)
which(is.na(dat))


##Imputación de la razón
x=c(1:10)
y=x*c(0.2:1)
x[3]=NA
x[7]=NA



I<-is.na(x)
R<-sum(x[!I])/sum(y[!I])
x[I]<-R*y[I]



#Regresión lineal
iris$Sepal.Length[1:10] <- NA #Se cambia a NA las longitudes de los sépalos de los 10 primeros lirios 

model <- lm(Sepal.Length ~ Sepal.Width + Petal.Width, data = iris)
I <- is.na(iris$Sepal.Length)
iris$Sepal.Length[I] <- predict(model, newdata = iris[I, ])
iris[1:25,]



####Imputación Hot Deck

#Aleatoria
height <- women$height
height[c(6, 9)] <- NA
height

height<-impute(height,"random")


#Secuencial
seqImpute <- function(x,last){
  n <- length(x)
  x <- c(x,last)
  i <- is.na(x)
  print(i)
  while(any(i)){  
    x[i] <- x[which(i) + 1] 
    i <- is.na(x) 
  }
  x[1:n] 
}

x<-c(1:10)
x[3]<-NA
x[4]<-NA
x[10]<-NA
print(x)

x<-seqImpute(x,20)




##Imputación Knn
library(Rcpp)
library(VIM)
library(reactable)


n <- nrow(iris)
for (i in 1:ncol(iris)) {
  iris[sample(1:n, 10, replace = FALSE), i] <- NA
}

which(is.na(iris))


a <- aggr(iris, plot = FALSE)
plot(a, numbers = TRUE, prop = FALSE)

iris2<-kNN(iris)

tableMiss(iris2[-10])



##Mediante el valor minimal:
library(rspa)
library(editrules)

E <- editmatrix(expression(x + y == z, x >= 0, y >= 0))
d <- data.frame(x = 10, y = 10, z = 21)
d

d1 <- adjustRecords(E, d)


violatedEdits(E, d1$adjusted, tol = 0.01)

A <- array(c(x = FALSE, y = FALSE, z = TRUE), dim = c(1, 3))
A

d2 <- adjustRecords(E, d, adjust = A)
d2$adjusted