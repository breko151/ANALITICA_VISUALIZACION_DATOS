main <- function() {
  datos <- data.frame(iris)
  # Obtención Áreas
  datos <- obt_area(datos)
  View(datos)
  # Obtención de Medidas de Tendencia Central
  obt_med_ten(datos)
}

media_aritmetica <- function(values) {
  sumatoria = 0
  for(i in 1:length(values)) {
    sumatoria = sumatoria + values[i]
  }
  media_aritmetica = sumatoria / length(values)
  return(media_aritmetica)
}

mediana <- function(values) {
  values <- sort(values)
  longitud = length(values)
  mitad = ceiling(longitud / 2)
  if(longitud %% 2 == 0) {
    valor_1 = values[mitad]
    valor_2 = values[mitad + 1]
    valores <- c(valor_1, valor_2)
    mediana <- media_aritmetica(valores)
  } else {
    mitad = ceiling(longitud / 2)
    mediana = values[mitad]
  }
  return(mediana)
}

media_generalizada <- function(values, alpha) {
  sumatoria = 0
  for(i in 1:length(values)) {
    sumatoria = sumatoria + (values[i] ** alpha)
  }
  media_generalizada = (sumatoria / length(values)) ** (1 / alpha)
  return(media_generalizada)
}

moda <- function(values) {
  
  longitud <- length(values)
  rep <- 0
  modas <- integer()
  reps <- integer()
  
  # Obtenemos factor
  num_unicos <- factor(values)
  niveles <- levels(num_unicos)
  for(num in niveles) {
    for(i in 1:longitud) {
      if(num == as.character(values[i])) {
        rep <- rep + 1
      }
    }
    reps <- c(reps, rep)
    rep <- 0
  }
  
  pos_moda <- which.max(reps)
  rep_max <- reps[pos_moda]
  for(j in 1:length(reps)) {
    if(reps[j] == rep_max) {
      modas <- c(modas, niveles[j])
    }
  } 
  
  modas <- paste(modas, collapse = ", ")
  return(modas)
}

varianza <- function(values) {
  media <- media_aritmetica(values)
  sumatoria <- 0
  for(i in 1:length(values)) {
    sumatoria = sumatoria + ((values[i] - media) ** 2)
  }
  if(length(values) > 30) {
    varianza <- sumatoria / length(values)
  } else {
    varianza <- sumatoria / (length(values) - 1)
  }
  return(varianza)
}

des_std <- function(values) {
  des_std <- varianza(values) ** 0.5
}

min_c <- function(values) {
  values <- sort(values)
  min <- values[1]
  return(min)
}

max_c <- function(values) {
  values <- sort(values)
  posicion <- length(values)
  max <- values[posicion]
  return(max)
}

area <- function(altura, base) {
  area <- base * altura
  return(area)
}

obt_area <- function(values) {
  longitud_sepalo <- values$Sepal.Length
  ancho_sepalo <- values$Sepal.Width
  longitud_petalo <- values$Petal.Length
  ancho_petalo <- values$Petal.Width
  areas_sepalo <- c()
  for(i in 1:length(longitud_sepalo)) {
    areas_sepalo <- c(areas_sepalo, area(longitud_sepalo[i], ancho_sepalo[i]))
  }
  areas_petalo <- c()
  for(i in 1:length(longitud_sepalo)) {
    areas_petalo <- c(areas_petalo, area(longitud_petalo[i], ancho_petalo[i]))
  }
  values <- cbind(values, "Sepal.Area" = areas_sepalo)
  values <- cbind(values, "Petal.Area" = areas_petalo)
  return(values)
}

obt_med_ten <- function(values) {
  flower_type = levels(factor(values$Species))
  flower_type
  length(flower_type)
  setosa <- data.frame(values[values$Species==flower_type[1], ])
  versicolor <- data.frame(values[values$Species==flower_type[2], ])
  virginica <- data.frame(values[values$Species==flower_type[3], ])
  med_ten(setosa, 'setosa')
  med_ten(versicolor, 'versicolor')
  med_ten(virginica, 'virginica')
}

med_ten <- function(values, name) {
  media_aritmetica_area_sepalo <- media_aritmetica(values$Sepal.Area)
  mediana_area_sepalo <- mediana(values$Sepal.Area)
  media_minima_area_sepalo <- media_generalizada(values$Sepal.Area, -Inf)
  media_armonica_area_sepalo <- media_generalizada(values$Sepal.Area, -1)
  media_geometrica_area_sepalo <- media_generalizada(values$Sepal.Area, 0)
  media_cuadratica_area_sepalo <- media_generalizada(values$Sepal.Area, 2)
  media_maxima_area_sepalo <- media_generalizada(values$Sepal.Area, Inf)
  moda_area_sepalo <- moda(values$Sepal.Area)
  varianza_area_sepalo <- varianza(values$Sepal.Area)
  des_std_area_sepalo <- des_std(values$Sepal.Area)
  min_area_sepalo <- min_c(values$Sepal.Area)
  max_area_sepalo <- max_c(values$Sepal.Area)
  
  media_aritmetica_area_petalo <- media_aritmetica(values$Petal.Area)
  mediana_area_petalo <- mediana(values$Petal.Area)
  media_minima_area_petalo <- media_generalizada(values$Petal.Area, -Inf)
  media_armonica_area_petalo <- media_generalizada(values$Petal.Area, -1)
  media_geometrica_area_petalo <- media_generalizada(values$Petal.Area, 0)
  media_cuadratica_area_petalo <- media_generalizada(values$Petal.Area, 2)
  media_maxima_area_petalo <- media_generalizada(values$Petal.Area, Inf)
  moda_area_petalo <- moda(values$Petal.Area)
  varianza_area_petalo <- varianza(values$Petal.Area)
  des_std_area_petalo <- des_std(values$Petal.Area)
  min_area_petalo <- min_c(values$Petal.Area)
  max_area_petalo <- max_c(values$Petal.Area)
  print('Moda Sepalo')
  print(moda_area_sepalo)
  print('Moda Petalo')
  print(moda_area_petalo)
  nombres <- c(paste('Area Sepalo', name), paste('Area Petalo', name))
  medias_aritemeticas <- c(media_maxima_area_sepalo, media_aritmetica_area_petalo)
  medianas <- c(mediana_area_sepalo, mediana_area_petalo)
  medias_minimas <- c(media_minima_area_sepalo, media_minima_area_petalo)
  medias_armonicas <- c(media_armonica_area_sepalo, media_armonica_area_petalo)
  medias_geometicas <- c(media_geometrica_area_sepalo, media_geometrica_area_petalo)
  medias_cuadraticas <- c(media_cuadratica_area_sepalo, media_cuadratica_area_petalo)
  medias_maximas <- c(media_maxima_area_sepalo, media_maxima_area_petalo)
  modas <- c(moda_area_sepalo, moda_area_petalo)
  varianzas <- c(varianza_area_sepalo, varianza_area_petalo)
  desviaciones_estandar <- c(des_std_area_sepalo, des_std_area_petalo)
  minimos <- c(min_area_sepalo, min_area_petalo)
  maximos <- c(max_area_sepalo, max_area_petalo)
  medidas_tendencia <- cbind(nombres, medias_aritemeticas,
                             medias_minimas, medias_armonicas, medias_geometicas,
                             medias_cuadraticas, medias_maximas, medianas, modas,
                             varianzas, desviaciones_estandar, minimos, maximos)
}

main()