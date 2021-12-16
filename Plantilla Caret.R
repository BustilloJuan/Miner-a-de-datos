datos<-Sonar
levels(datos$Class)
library(caret)

subconjunto<-createDataPartition(y=datos$Class, #La clase del cancer
                                 p=.75, #Porcentaje para entrenamiento
                                 list=F) #Formato salida

entrena<-datos[subconjunto,]
test<-datos[-subconjunto,]
nrow(entrena)+nrow(test) #Para confirmar que no me dejo ninguna observación

valores<-expand.grid(C=c(0.4,0.3),M=c(3,5,6))

ctrl<-trainControl(method = "repeatedcv",repeats = 3,
                   summaryFunction = twoClassSummary,
                   classProbs = T)

c4.5 <- train(
  Class ~ .,
  data = entrena,
  method = "J48",
  tuneGrid=valores,
  trControl=ctrl,
  metric="ROC"
)
c4.5