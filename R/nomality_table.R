normality_table <- function(modelo) {
  
  # modelo is an lm object
  
  # takes the residuals of the given lm model
  lm_residuals <- modelo$residuals
  
  # calculates the estimative of the std for the residuals
  qme_ <- qme(modelo)
  
  # calculates the standardized residuals
  std_residuals <- lm_residuals/qme_
  
  
  prop_entre_1 <- sum(std_residuals >= -1 & std_residuals <= 1) / length(std_residuals)
  prop_entre_164 <- sum(std_residuals >= -1.64 & std_residuals <= 1.64) / length(std_residuals)
  prop_entre_196 <- sum(std_residuals >= -1.96 & std_residuals <= 1.96) / length(std_residuals)
  
  
  
  
  labels_norm_resid <- c("Resíduos situados entre -1(DP) e + 1(DP)",
                         "Resíduos situados entre -1,64(DP) e + 1,64(DP)",
                         "Resíduos situados entre -1,96(DP) e + 1,96(DP)")
  
  prop_curva_normal <- scales::percent(c(0.68,0.90,0.95))
  
  prop_residuos <- scales::percent(c(prop_entre_1,
                                     prop_entre_164,
                                     prop_entre_196),
                                   accuracy = 1)
  
  df_normalidade <- data.frame(labels_norm_resid,
                               prop_curva_normal,
                               prop_residuos)
  
  return(df_normalidade)
  
  
}