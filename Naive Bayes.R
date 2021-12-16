###Clasificador Naive Bayes
#Para este primer método usaremos la base de datos iris
str(iris) #Vemos su estructura, 3 tipos de especies


modelo<-e1071::naiveBayes(Species ~ ., data = iris) #Creamos el modelo
print(modelo) #Probabilidades de cada clase, condicionada a los valores de los atributos


p1<-predict(modelo,iris,type = "raw") #Probabilidades
p2<-predict(modelo,iris) #Asignando a clases

validacion<-table(p2, iris[,5]);validacion #Comparamos los casos predichos por nuestro modelo,
#con los recogidos en la base de datos

Nejemplo<-predict(modelo,data.frame(Sepal.Length=5.0, Sepal.Width=4.1,
                                    Petal.Length=6.2, Petal.Width=1.3))

print(Nejemplo)
