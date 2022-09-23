# Proyecto 2
# 5AM1 | Analítica y Visualización de Datos
# Suárez Pérez Juan Pablo
# Ojeda Contreras Braulio Melquisedec

main <- function() {
  data <- data.frame(iris)
  data <- get_areas(data)
  View(data)
  # Get Different Flower Type
  flower_type = levels(factor(data$Species))
  setosa <- data.frame(data[data$Species == flower_type[1], c(1, 2, 3, 4, 6, 7, 5)])
  versicolor <- data.frame(data[data$Species == flower_type[2], c(1, 2, 3, 4, 6, 7, 5)])
  virginica <- data.frame(data[data$Species == flower_type[3], c(1, 2, 3, 4, 6, 7, 5)])
  # Get Individual Euclidian Norms
  print('Get individual euclidian norms')
  e_norm_setosa <- e_norm(data.frame(setosa[, c(1,2,3,4)]))
  e_norm_versicolor <- e_norm(data.frame(versicolor[, c(1,2,3,4)]))
  e_norm_virginica <- e_norm(data.frame(virginica[, c(1,2,3,4)]))
  cat('Euclidian Norm from setosa values:', e_norm_setosa, '\n')
  cat('Euclidian Norm from versicolor values:', e_norm_versicolor, '\n')
  cat('Euclidian Norm from virginica values:', e_norm_virginica, '\n\n')
  # Get Individual Area Threshold
  print('Get Area Threshold')
  e_norm_setosa <- e_norm(data.frame(setosa[, c(5,6)]))
  e_norm_versicolor <- e_norm(data.frame(versicolor[, c(5,6)]))
  e_norm_virginica <- e_norm(data.frame(virginica[, c(5,6)]))
  cat('Euclidian Norm from setosa areas:', e_norm_setosa, '\n')
  cat('Euclidian Norm from versicolor areas:', e_norm_versicolor, '\n')
  cat('Euclidian Norm from virginica areas:', e_norm_virginica, '\n\n')
  # Get Distance by elements
  distance_s_ve <- distance(data.frame(setosa[, c(1,2,3,4)]), 
                            data.frame(versicolor[, c(1,2,3,4)]))
  distance_s_vi <- distance(data.frame(setosa[, c(1,2,3,4)]), 
                            data.frame(virginica[, c(1,2,3,4)]))
  distance_ve_s <- distance(data.frame(versicolor[, c(1,2,3,4)]), 
                            data.frame(setosa[, c(1,2,3,4)]))
  distance_ve_vi <- distance(data.frame(versicolor[, c(1,2,3,4)]), 
                             data.frame(virginica[, c(1,2,3,4)]))
  distance_vi_ve <- distance(data.frame(virginica[, c(1,2,3,4)]), 
                             data.frame(versicolor[, c(1,2,3,4)]))
  distance_vi_s <- distance(data.frame(virginica[, c(1,2,3,4)]), 
                            data.frame(setosa[, c(1,2,3,4)]))
  print('Distance by elements')
  cat('Distance Setosa-Versicolor:', distance_s_ve, '\n')
  cat('Distance Setosa-Virginica:', distance_s_vi, '\n')
  cat('Distance Versicolor-Setosa:', distance_ve_s, '\n')
  cat('Distance Versicolor-Virginica:', distance_ve_vi, '\n')
  cat('Distance Virginica-Versicolor:', distance_vi_ve, '\n')
  cat('Distance Virginica-Setosa:', distance_vi_s, '\n\n')
  # Get Distance by areas
  distance_s_ve_a <- distance(data.frame(setosa[, c(5,6)]), 
                            data.frame(versicolor[, c(5,6)]))
  distance_s_vi_a <- distance(data.frame(setosa[, c(5,6)]), 
                            data.frame(virginica[, c(5,6)]))
  distance_ve_s_a <- distance(data.frame(versicolor[, c(5,6)]), 
                            data.frame(setosa[, c(5,6)]))
  distance_ve_vi_a <- distance(data.frame(versicolor[, c(5,6)]), 
                             data.frame(virginica[, c(5,6)]))
  distance_vi_ve_a <- distance(data.frame(virginica[, c(5,6)]), 
                             data.frame(versicolor[, c(5,6)]))
  distance_vi_s_a <- distance(data.frame(virginica[, c(5,6)]), 
                            data.frame(setosa[, c(5,6)]))
  print('Distance by areas')
  cat('Distance Setosa-Versicolor:', distance_s_ve_a, '\n')
  cat('Distance Setosa-Virginica:', distance_s_vi_a, '\n')
  cat('Distance Versicolor-Setosa:', distance_ve_s_a, '\n')
  cat('Distance Versicolor-Virginica:', distance_ve_vi_a, '\n')
  cat('Distance Virginica-Versicolor:', distance_vi_ve_a, '\n')
  cat('Distance Virginica-Setosa:', distance_vi_s_a, '\n\n')
  # Get Internal Product by elements
  ip_s_ve <- internal_product(data.frame(setosa[, c(1,2,3,4)]), 
                              data.frame(versicolor[, c(1,2,3,4)]))
  ip_s_vi <- internal_product(data.frame(setosa[, c(1,2,3,4)]), 
                              data.frame(virginica[, c(1,2,3,4)]))
  ip_ve_s <- internal_product(data.frame(versicolor[, c(1,2,3,4)]), 
                              data.frame(setosa[, c(1,2,3,4)]))
  ip_ve_vi <- internal_product(data.frame(versicolor[, c(1,2,3,4)]), 
                               data.frame(virginica[, c(1,2,3,4)]))
  ip_vi_ve <- internal_product(data.frame(virginica[, c(1,2,3,4)]), 
                               data.frame(versicolor[, c(1,2,3,4)]))
  ip_vi_s <- internal_product(data.frame(virginica[, c(1,2,3,4)]), 
                              data.frame(setosa[, c(1,2,3,4)]))
  print('Internal Product by elements')
  cat('Internal Product Setosa-Versicolor:', ip_s_ve, '\n')
  cat('Internal Product Setosa-Virginica:', ip_s_vi, '\n')
  cat('Internal Product Versicolor-Setosa:', ip_ve_s, '\n')
  cat('Internal Product Versicolor-Virginica:', ip_ve_vi, '\n')
  cat('Internal Product Virginica-Versicolor:', ip_vi_ve, '\n')
  cat('Internal Product Virginica-Setosa:', ip_vi_s, '\n\n')
  # Get Internal Product by areas
  ip_s_ve_a <- internal_product(data.frame(setosa[, c(5,6)]), 
                              data.frame(versicolor[, c(5,6)]))
  ip_s_vi_a <- internal_product(data.frame(setosa[, c(5,6)]), 
                              data.frame(virginica[, c(5,6)]))
  ip_ve_s_a <- internal_product(data.frame(versicolor[, c(5,6)]), 
                              data.frame(setosa[, c(5,6)]))
  ip_ve_vi_a <- internal_product(data.frame(versicolor[, c(5,6)]), 
                               data.frame(virginica[, c(5,6)]))
  ip_vi_ve_a <- internal_product(data.frame(virginica[, c(5,6)]), 
                               data.frame(versicolor[, c(5,6)]))
  ip_vi_s_a <- internal_product(data.frame(virginica[, c(5,6)]), 
                              data.frame(setosa[, c(5,6)]))
  print('Internal Product by area')
  cat('Internal Product Setosa-Versicolor:', ip_s_ve_a, '\n')
  cat('Internal Product Setosa-Virginica:', ip_s_vi_a, '\n')
  cat('Internal Product Versicolor-Setosa:', ip_ve_s_a, '\n')
  cat('Internal Product Versicolor-Virginica:', ip_ve_vi_a, '\n')
  cat('Internal Product Virginica-Versicolor:', ip_vi_ve_a, '\n')
  cat('Internal Product Virginica-Setosa:', ip_vi_s_a, '\n\n')
}

area <- function(width, height) {
  area <- width * height
  return(area)
}

get_areas <- function(data) {
  length_sepal <- data$Sepal.Length
  width_sepal <- data$Sepal.Width
  length_petal <- data$Petal.Length
  width_petal <- data$Petal.Width
  area_sepal <- c()
  for(i in 1:length(length_sepal)) {
    area_sepal <- c(area_sepal, area(length_sepal[i], width_sepal[i]))
  }
  area_petal <- c()
  for(i in 1:length(length_petal)) {
    area_petal <- c(area_petal, area(length_petal[i], width_petal[i]))
  }
  data <- cbind(data, "Sepal.Area" = area_sepal)
  data <- cbind(data, "Petal.Area" = area_petal)
  return(data)
}

e_norm <- function(data) {
  shape <- dim(data)
  n_row <- shape[1]
  n_col <- shape[2]
  sumatoria <- 0
  for(i in 1:n_row) {
    for(j in 1:n_col) {
      sumatoria <- sumatoria + (data[i, j] ** 2)
    }
  }
  norma_value <- sumatoria ** (0.5)
  return(norma_value)
}

substraction <- function(data_A, data_B) {
  shape_A <- dim(data_A)
  shape_B <- dim(data_B)
  if(shape_A[1] == shape_B[1] && shape_A[2] == shape_B[2]) {
    new_data <- data.frame()
    for(i in 1:shape_A[1]) {
      new_row <- c()
      for(j in 1:shape_A[2]) {
        new_row <- c(new_row, data_A[i,j] - data_B[i,j])
      }
      new_data <- rbind(new_data, new_row)
    }
    return(new_data)
  }
}

distance <- function(data_A, data_B) {
  new_data <- substraction(data_A, data_B)
  distance_value <- e_norm(new_data)
  return(distance_value)
}

internal_product <- function(data_A, data_B) {
  shape_A <- dim(data_A)
  shape_B <- dim(data_B)
  if(shape_A[1] == shape_B[1] && shape_A[2] == shape_B[2]) {
    internal_product_v <- 0
    for(i in 1:shape_A[1]) {
      for(j in 1:shape_A[2]) {
        internal_product_v <- internal_product_v + (data_A[i,j] * data_B[i,j])
      }
    }
    return(internal_product_v)
  }
}

main()