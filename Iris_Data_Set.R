main <- function() {
  # Importamos los datos
  iris_data <- data.frame(iris)
  #View(datos)
  # Respuesta a la pregunta 1
  # Respuesta a la pregunta 2
  # Respuesta a la pregunta 3
  correlacion_petalo(iris_data)
  # Respuesta a la pregunta 4
  correlacion(iris_data[,-5])
  # Respuesta a la pregunta 5
  regresion(iris_data)
  # Respuesta a la pregunta 6
  # Respuesta a la pregunta 7
}

correlacion_petalo <- function(dataset) {
  cat("Pregunta 3...\n")
  cor_petalo <- cor(dataset$Petal.Length, dataset$Petal.Width)
  cat("\tLa correlación existente entre la longitud del Pétalo y 
       el ancho del Pétalo es: ", cor_petalo, ".")
  cat("\n\n")
}

correlacion <- function(dataset) {
  cat("Pregunta 4...\n")
  matriz_correlacion <- cor(dataset)
  print(matriz_correlacion)
  cat("\n")
  cat("\tComo se puede observar en la matriz de correlaciones las dimensiones de
       ancho de Pétalo y la longitud de Pétalo están más correlacionadas.\n\n")
}

regresion <- function(dataset) {
  cat("Pregunta 5...\n")
  regresor <- lm(Sepal.Length ~ Sepal.Width, dataset)
  #print(summary(regresor))
  plot(dataset$Sepal.Width, dataset$Sepal.Length, 
       xlab = 'Ancho Sepalo', ylab = 'Largo')
  abline(regresor)
  cat("\tPara un ancho de Sépalo de 1.8 cm, tenemos un largo de Sépalo: ")
  cat(predict(regresor, data.frame(Sepal.Width = (1.8))))
  cat(".\n\n")
}

main()
