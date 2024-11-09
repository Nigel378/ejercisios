install.packages("dplyr")
install.packages("rpart")
install.packages("randomForest")
install.packages("caret")
install.packages("ROSE") 
library(dplyr)
library(rpart)
library(randomForest)
library(caret)
library(ROSE)
transacciones <- read.csv("transacciones.csv")
head(transacciones)
transacciones$flag_fraude <- as.factor(transacciones$flag_fraude)
transacciones$ubicacion_comercio <- as.factor(transacciones$ubicacion_comercio)
transacciones$hora_dia <- as.factor(transacciones$hora_dia)
transacciones$tipo_dispositivo <- as.factor(transacciones$tipo_dispositivo)
table(transacciones$flag_fraude)
transacciones_balanceadas <- ROSE(flag_fraude ~ monto + ubicacion_comercio + hora_dia + tipo_dispositivo, 
                                  data = transacciones, 
                                  seed = 1)$data
set.seed(1)
train_index <- createDataPartition(transacciones_balanceadas$flag_fraude, p = 0.7, list = FALSE)
train_data <- transacciones_balanceadas[train_index, ]
test_data <- transacciones_balanceadas[-train_index, ]
modelo_arbol <- rpart(flag_fraude ~ monto + ubicacion_comercio + hora_dia + tipo_dispositivo, 
                      data = train_data, method = "class", 
                      control = rpart.control(minsplit = 10, cp = 0.01))
plot(modelo_arbol)
text(modelo_arbol, pretty = 1)

predicciones_arbol <- predict(modelo_arbol, test_data, type = "class")
confusionMatrix(predicciones_arbol, test_data$flag_fraude)

set.seed(1)
modelo_rf <- randomForest(flag_fraude ~ monto + ubicacion_comercio + hora_dia + tipo_dispositivo, 
                          data = train_data, ntree = 100, mtry = 2) 

importance(modelo_rf)
varImpPlot(modelo_rf)

predicciones_rf <- predict(modelo_rf, test_data)
confusionMatrix(predicciones_rf, test_data$flag_fraude)
