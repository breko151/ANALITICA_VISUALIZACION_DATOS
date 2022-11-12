# Proyecto 3
# 5AM1 | Analítica y Visualización de Datos
# Suárez Pérez Juan Pablo
# Ojeda Contreras Braulio Melquisedec

main <- function() {
  data <- data.frame(iris)
  # View(iris_data)
  # Get Different Flower Type
  flower_type = levels(factor(data$Species))
  setosa <- data.frame(data[data$Species == flower_type[1], c(1, 2, 3, 4, 5)])
  versicolor <- data.frame(data[data$Species == flower_type[2], c(1, 2, 3, 4, 5)])
  virginica <- data.frame(data[data$Species == flower_type[3], c(1, 2, 3, 4, 5)])
  #View(setosa)
  #View(versicolor)
  #View(virginica)
  setosa_mahalanobis <- mahalanobis_dist(setosa[,1:4])
  versicolor_mahalanobis <- mahalanobis_dist(versicolor[,1:4])
  virginica_mahalanobis <- mahalanobis_dist(virginica[,1:4])
  print("setosa_mahalanobis")
  print(setosa_mahalanobis)
  graficate(setosa_mahalanobis, "setosa_mahalanobis")
  print("versicolor_mahalanobis")
  print(versicolor_mahalanobis)
  graficate(versicolor_mahalanobis, "versicolor_mahalanobis")
  print("virginica_mahalanobis")
  print(virginica_mahalanobis)
  graficate(virginica_mahalanobis, "virginica_mahalanobis")
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

mult_mat <- function(mat1, mat2) {
  row_no1 <- dim(mat1)[1]
  row_no2 <- dim(mat2)[1]
  col_no1 <- dim(mat1)[2]
  col_no2 <- dim(mat2)[2]
  if(col_no1 == row_no2) {
    new_mat <- data.frame()
    for(a in 1:col_no2) {
      rown <- c()
      for(i in 1:row_no1) {
        suma <- 0
        for(j in 1:col_no1) {
          suma <- suma + (mat1[i,j] * mat2[j,a])
        }
        rown <- c(rown, suma)
      }
      if(dim(new_mat)[1] == 0) {
        new_mat <- cbind(rown)
      } else {
        new_mat <- cbind(new_mat, rown)
      }
    }
    return(new_mat)
  }
}

mahalanobis_dist <- function(data) {
  cov_matrix <- cov(data)
  means_data <- means(data)
  row_no <- dim(data)[1]
  col_no <- dim(data)[2]
  identity_matrix <- diag(row_no)
  ones_matrix <- matrix(1, row_no, row_no)
  identity_one <- identity_matrix - ((1 / row_no) * ones_matrix)
  t_data <- t(data)
  cov_matrix_inv <- solve(cov_matrix)
  first_step <- mult_mat(identity_one, data)
  second_step <- mult_mat(first_step, cov_matrix_inv)
  third_step <- mult_mat(second_step, t_data)
  four_step <- mult_mat(third_step, identity_one)
  mahalanobis_dists <- diag(four_step)
  return(mahalanobis_dists ** 0.5)
}

graficate <- function(data, name) {
  plot(density(data, bw = 0.5),
       main="Squared Mahalanobis distances") ; rug(data)
  qqplot(qchisq(ppoints(length(data)), df = 3), data,
         main = expression("Q-Q plot of Mahalanobis quantiles of" * ~ chi[3]^2))
  abline(0, 1, col = 'gray')
}

main()