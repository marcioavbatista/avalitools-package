qme <- function(modelo) {
  
  residuals <- modelo$residuals
  num_dados <- length(residuals)
  p <- length(modelo$coefficients)
  
  sqe <- sum(residuals^2)
  qme <- sqrt(sqe/(num_dados - p))
  
}