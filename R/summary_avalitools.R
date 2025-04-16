summary_avalitools <- function(modelo) {
  
  # the parameter modelo is a linear regression object
  
  
  # creates a standard summary for the model
  sumario <- summary(modelo)
  
  #catches the informations that will be used in the avalitools summary
  coef_det <- sumario[[8]]
  coef_det_ajust <- sumario[[9]]
  teste_f <- sumario[[10]]
  p_obs <- pf(q = teste_f[1],df1 = teste_f[2],df2 = teste_f[3], lower.tail = F)
  
  
  
  
  
  labesl_estat_modelo <- c("Coeficiente de correlação:",
                           "Coeficiente de determinação:",
                           "Fisher - Snedecor:",
                           "Significancia do modelo(%):")
  
  
  # creates a vector with all measures in the brazilian format 
  values_estat_modelo <- c(avalitools::br7_format( sqrt(coef_det) ),
                           avalitools::br7_format( coef_det ),
                           avalitools::br2_format( teste_f[1] ),
                           scales::percent( p_obs, accuracy = 10^-4, decimal.mark = "," )
  )
  
  # creates df with the measures and their titles
  df_sumario <- data.frame(labesl_estat_modelo,values_estat_modelo)
  
  colnames(df_sumario) <- c("Estatísticas do modelo",
                            "Estatística")
  
  return(df_sumario)
  
}