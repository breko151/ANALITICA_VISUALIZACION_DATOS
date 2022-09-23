# Suárez Pérez Juan Pablo, 5AM1
main <- function() {
  print("Suárez Pérez Juan Pablo")
  # Importamos los datos
  iris_data <- data.frame(iris)
  #View(datos)
  # Respuesta a la pregunta 1
  # Respuesta a la pregunta 2
  resumen_datos()
  # Respuesta a la pregunta 3
  correlacion_petalo(iris_data)
  # Respuesta a la pregunta 4
  correlacion(iris_data[,-5])
  # Respuesta a la pregunta 5
  regresion(iris_data)
  # Respuesta a la pregunta 6
  clasificacion()
  # Respuesta a la pregunta 7
  subespecies() 
}

resumen_datos <- function() {
  library(datasets)
  flower_type = levels(factor(iris$Species))
  flower_type
  length(flower_type)
  
  setosa <- data.frame(iris[iris$Species==flower_type[1], c(- ncol(iris))])
  setosa_razones <- data.frame(sepalo = setosa$Sepal.Length / setosa$Sepal.Width, petalo = setosa$Petal.Length / setosa$Petal.Width)
  
  versicolor <- data.frame(iris[iris$Species==flower_type[2], c(- ncol(iris))])
  versicolor_razones <- data.frame(sepalo = versicolor$Sepal.Length / versicolor$Sepal.Width, petalo = versicolor$Petal.Length / versicolor$Petal.Width)
  
  virginica <- data.frame(iris[iris$Species==flower_type[3], c(- ncol(iris))])
  virginica_razones <- data.frame(sepalo = virginica$Sepal.Length / virginica$Sepal.Width, petalo = virginica$Petal.Length / virginica$Petal.Width)
  
  #Especies
  cat("\nSummary por especies\n")
  cat("\nSetosa\n")
  print(summary(setosa))
  cat("\nVersicolor\n")
  print(summary(versicolor))
  cat("\nVirginica\n")
  print(summary(virginica))
  
  #Razones
  cat("\nRazones longitud/anchura\n")
  cat("\nSetosa\n")
  print(summary(setosa_razones))
  cat("\nVersicolor\n")
  print(summary(versicolor_razones))
  cat("\nVirginica\n")
  print(summary(virginica_razones))
  cat("\nPregunta 1...¿Cuál de los datos podría contener errores o
asignaciones de clases falsas?\n")
  cat("\tLos registros pueden llegar a ser catalogados como especies erróneas por sus medidas
      de tendencia central y las de posición, ya que algunos datos podrían llegar a ser catalogados de manera erronea,
      ya que se llegan a traslapar medidas.\n")
  cat("\nPregunta 2...¿Cuál es el error causado por redondear los
datos a un decimal?\n")
  cat("\tSi se usa algún método de regresión, es posible que los datos se encuentren mal categorizados, porque existen medias que rozan el rango de valores de otras categorias.\n\n")
  
}

correlacion_petalo <- function(dataset) {
  cat("Pregunta 3...¿Cuál es la correlación entre la longitud y el
ancho de los pétalos?\n")
  cor_petalo <- cor(dataset$Petal.Length, dataset$Petal.Width)
  cat("\tLa correlación existente entre la longitud del Pétalo y 
       el ancho del Pétalo es: ", cor_petalo, ".")
  cat("\n\n")
}

correlacion <- function(dataset) {
  cat("Pregunta 4...¿Qué par de dimensiones están más
correlacionadas?\n")
  matriz_correlacion <- cor(dataset)
  print(matriz_correlacion)
  cat("\n")
  cat("\tComo se puede observar en la matriz de correlaciones las dimensiones de
       ancho de Pétalo y la longitud de Pétalo están más correlacionadas.\n\n")
}

regresion <- function(dataset) {
  cat("Pregunta 5...Ninguna de las flores en el dataset tiene un
ancho de sépalo de 1.8 cm. ¿Qué longitud de
sépalo esperaríamos para una flor que
tuviera 1.8 cm como ancho de sépalo?\n")
  regresor <- lm(Sepal.Length ~ Sepal.Width, dataset)
  #print(summary(regresor))
  plot(dataset$Sepal.Width, dataset$Sepal.Length, 
       xlab = 'Ancho Sepalo', ylab = 'Largo')
  abline(regresor)
  cat("\tPara un ancho de Sépalo de 1.8 cm, tenemos un largo de Sépalo: ")
  cat(predict(regresor, data.frame(Sepal.Width = (1.8))))
  cat(".\n\n")
}

clasificacion <- function() {
  cat("Pregunta 6...¿A qué especie pertenecería un Iris con un
ancho de sépalo de 1.8 cm?\n")
  library(caTools)
  library(class)
  
  library(datasets)
  irisdf <- data.frame(sapply(iris,c))
  
  #Escalado de datos
  irisdf.numeric <- irisdf[,1:2]
  specie <- irisdf[,5]
  irisdf.scaled <- data.frame(scale(irisdf.numeric))
  sapply(irisdf.scaled, var)
  sapply(irisdf.scaled, mean)
  
  #Train y test dataset
  irisdf.final <-  cbind(irisdf.scaled, specie)
  sample <- sample.split(irisdf.final$specie, SplitRatio = .70)
  train.data <- subset(irisdf.final, sample==TRUE) 
  test.data <- subset(irisdf.final, sample==FALSE)
  
  #Predicciones
  predicted.species <- knn(train.data[1:2], test.data[1:2], train.data[,3], k=1)
  head(predicted.species)
  error <- mean(test.data$specie!=predicted.species)
  error
  cat("El error del modelo es del ",error*100, "%\n")
  
  mytest<- test.data[1,-3]# creating format for my data
  mytest[1,]<-c(5.32, 1.8) # passing what you said
  predicted_1.8 <- knn(train.data[1:2], mytest, train.data[,3], k=1)
  cat("\tLa flor con sépalo de 1.8 cm se clasifica como flor de tipo: ",
      predicted_1.8, " que es Virginica\n")
}

subespecies <- function() {
  cat("\nPregunta 7... ¿Las 3 especies contienen subespecies que
pueden identificarse a partir de los datos?")
  plot(x = iris$Petal.Length, y = iris$Petal.Width, col = iris$Species, 
       main = "Distribución Pétalo Especie", xlab = "Petal Length", ylab = "Petal Width") 
  legend(x = "topleft", legend = c("Setosa", "Versicolor", "Virginica"), 
         fill = c("black", "red", "green"), title = "Especie")
  
  plot(x = iris$Sepal.Length, y = iris$Sepal.Width, col = iris$Species, 
       main = "Distribución Sétalo Especie", xlab = "Sepal Length", ylab = "Sepal Width")
  legend(x = "topleft", legend = c("Setosa", "Versicolor", "Virginica"), 
         fill = c("black", "red", "green"), title = "Especie")
  cat("\tComo se observa en la gráfica es dificil clasificar los datos en subespecies, ya que inclusive los datos de algunas especies son parecidos entre sí.")
}

main()
