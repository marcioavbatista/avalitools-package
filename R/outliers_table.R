outliers_table <- function(modelo) {
  
  # modelo is an lm object
  
  # takes the residuals values of the given lm model
  residuos_modelo <- modelo$residuals
  
  # calculates the estimative of the std
  qme_ <- qme(modelo)
  
  
  # calculates the standardized residual of the model
  std_residuals <- residuos_modelo/qme_
  
  # find the number of outliers 
  outliers_residuos <- sum(std_residuals <= -2 | std_residuals >= 2)
  
  # find the proportion of outliers in the model
  prop_out <- outliers_residuos / length(std_residuals)
  
  
  df_outliers <- data.frame(c("Quantidade de outliers:","% de outliers:"),
                            c(scales::number(outliers_residuos),
                              scales::percent(prop_out, accuracy = 0.01, decimal.mark = ",")))
  
  return(df_outliers)
  
  
}