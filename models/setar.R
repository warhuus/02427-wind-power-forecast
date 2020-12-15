
create_setar_thresholds <- function(data, thresh1_name,
                                    thresh2_name, thresh_values,
                                    reg_dim)
{
  # Extend threshold values
  t1 <- c(floor(min(data[[thresh1_name]])),
          thresh_values[[1]],
          ceiling(max(data[[thresh1_name]])))
  t2 <- c(floor(min(data[[thresh2_name]])),
          thresh_values[[2]],
          ceiling(max(data[[thresh2_name]])))
  return(list(t1, t2))
}

setarRSS <- function(params)
{
  # browser()
  stopifnot(length(params) == 18)
  means <- matrix(params[1:9], nrow=3)
  coefs <- matrix(params[10:18], nrow=3)
  data <- training_data
  thresh_names <- list("Ws1", "Wd1")
  thresh_values <- list(c(7, 15), c(180, 300))
  
  reg_dim <- c(length(thresh_values[[1]]) + 1, length(thresh_values[[2]]) + 1)
  
  # Check that only 2 variables for the thresholds are included
  stopifnot(reg_dim == dim(means))
  stopifnot(reg_dim == dim(coefs))
  stopifnot(length(thresh_names) == 2)
  
  # Get threshold names
  thresh1_name = thresh_names[[1]]
  thresh2_name = thresh_names[[2]]
  
  # Create thresholds
  ts <- create_setar_thresholds(data,
                                thresh1_name,
                                thresh2_name,
                                thresh_values)
  t1 <- ts[[1]]
  t2 <- ts[[2]]
  
  y <- data$p[-1]
  y1 <- data$p[-1000]
  
  output = rep(NA, dim(data)[1])
  
  for (i in 1:reg_dim[1])
  {
    for (j in 1:reg_dim[2])
    {
      # browser
      idx <- which(  data[[thresh1_name]] >= t1[i]
                   & data[[thresh1_name]] <  t1[(i+1)]
                   & data[[thresh2_name]] >= t2[j]
                   & data[[thresh2_name]] <  t2[(j+1)]
                   )
      output[idx] <- means[i, j] + coefs[i, j] * y1[idx]
    }
  }

  # print(output)
  
  return(sum((data[!is.na(output), "p"] - output[!is.na(output)])^2))
}

setar <- function(params)
{
  # browser()
  stopifnot(length(params) == 18)
  means <- matrix(params[1:9], nrow=3)
  coefs <- matrix(params[10:18], nrow=3)
  thresh_names <- list("Ws1", "Wd1")
  thresh_values <- list(c(7, 15), c(180, 300))
  
  reg_dim <- c(length(thresh_values[[1]]) + 1, length(thresh_values[[2]]) + 1)
  
  # Check that only 2 variables for the thresholds are included
  stopifnot(reg_dim == dim(means))
  stopifnot(reg_dim == dim(coefs))
  stopifnot(length(thresh_names) == 2)
  
  # Get threshold names
  thresh1_name = thresh_names[[1]]
  thresh2_name = thresh_names[[2]]
  
  # Create thresholds
  ts <- create_setar_thresholds(data,
                                thresh1_name,
                                thresh2_name,
                                thresh_values)
  t1 <- ts[[1]]
  t2 <- ts[[2]]
  
  y <- data$p[-1]
  y1 <- data$p[-1000]
  
  output = rep(NA, dim(data)[1])
  
  for (i in 1:reg_dim[1])
  {
    for (j in 1:reg_dim[2])
    {
      # browser
      idx <- which(  data[[thresh1_name]] >= t1[i]
                     & data[[thresh1_name]] <  t1[(i+1)]
                     & data[[thresh2_name]] >= t2[j]
                     & data[[thresh2_name]] <  t2[(j+1)]
      )
      output[idx] <- means[i, j] + coefs[i, j] * y1[idx]
    }
  }
  
  # print(output)
  
  return(output)
}