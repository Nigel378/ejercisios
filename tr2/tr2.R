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

bienes_raices <- read.csv("bienes_raices.csv")

head(bienes_raices)

bienes_raices$ubicacion <- as.factor(bienes_raices$ubicacion)
bienes_raices$tipo_propiedad <- as.factor(bienes_raices$tipo_propiedad)
bienes_raices$estado <- as.factor(bienes_raices$estado)

table(bienes_raices$precio)

bienes_raices_balanceadas <- ROSE(estado ~ precio + ubicacion + tipo_propiedad + area + num_habitaciones, 
                                  data = bienes_raices, 
                                  seed = 1)$data

set.seed(1)
train_index <- createDataPartition(bienes_raices_balanceadas$estado, p = 0.7, list = FALSE)
train_data <- bienes_raices_balanceadas[train_index, ]
test_data <- bienes_raices_balanceadas[-train_index, ]

modelo_arbol <- rpart(estado ~ precio + ubicacion + tipo_propiedad + area + num_habitaciones, 
                      data = train_data, method = "class", 
                      control = rpart.control(minsplit = 10, cp = 0.01))

plot(modelo_arbol)
text(modelo_arbol, pretty = 1)

predicciones_arbol <- predict(modelo_arbol, test_data, type = "class")

confusionMatrix(predicciones_arbol, test_data$estado)

set.seed(1)
modelo_rf <- randomForest(estado ~ precio + ubicacion + tipo_propiedad + area + num_habitaciones, 
                          data = train_data, ntree = 100, mtry = 2)

importance(modelo_rf)
varImpPlot(modelo_rf)

predicciones_rf <- predict(modelo_rf, test_data)
confusionMatrix(predicciones_rf, test_data$estado)
