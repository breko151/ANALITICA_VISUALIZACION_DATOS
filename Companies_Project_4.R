# Proyecto 4
# 5AM1 | Analítica y Visualización de Datos
# Suárez Pérez Juan Pablo
# Ojeda Contreras Braulio Melquisedec

# Selecciona la ubicación del Archivo CSV
path <- file.choose()
main <- function(path) {
  print(path)
  data <- read.csv(path, 
                   header = T,
                   sep = ",")
  row.names(data) <- data[,1]
  data <- data[,2:dim(data)[2]]
  View(data)
  means_data <- means(data)
  print("Means")
  print(means_data)
  covars_data <- covars(data) 
  print("Cov")
  print(covars_data)
  print("Distancia de Mahalanobis entre Ford y Exxon")
  FE_D <- mahalanobis_d(data["Ford",], data["Exxon",], covars_data)
  print(FE_D)
  print("Distancia de Mahalanobis entre General Motors e IBM")
  GMIBM_D <- mahalanobis_d(data["General Motors",], data["IBM",], covars_data)
  print(GMIBM_D)
  print("Distancia de Mahalanobis entre Philip Morris y Texaco")
  MT_D <- mahalanobis_d(data["Philip Morris",], data["Texaco",], covars_data)
  print(MT_D)
}

means <- function(data) {
  col_no <- dim(data)[2]
  row_no <- dim(data)[1]
  means_list <- list()
  for(i in 1:col_no) {
    suma <- 0
    for(j in 1:row_no) {
      suma <- suma + data[j,i]
    }
    means_list <- c(means_list, suma / row_no)
  }
  return(means_list)
}

covars <- function(data) {

  return(cov(data))
}

mahalanobis_d <- function(vector1, vector2, covars_data) {
  dis_vect <- vector1 - vector2
  covars_data_inv <- solve(covars_data)
  dis_vect_covars_inv <- dis_vect * covars_data_inv
  t_dis_vect <- t(dis_vect)
  mahalanobis_dis <- dis_vect_covars_inv * t_dis_vect
  mahalanobis_dis <- sum(mahalanobis_dis)
  return(mahalanobis_dis)
}

main(path)

