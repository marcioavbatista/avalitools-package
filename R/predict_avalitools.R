predict_avalitools <- function(df_novo = df_novo, modelo = modelo, trans = trans) {
  
  # df_novo is a data.frames tha will be used to predict a new value of y
  # modelo is an lm object 
  # trans is an string vector that contains the transformation used in the model
  
  
  # calculates the variance estimative
  qme_ <- qme(modelo)^2
  
  # creates a list of functions for every dependent variable
  fun_transformacoes_inv <- as_functions(trans[-length(trans)])
  
  
  # apply that given transformation in the new data
  novos_trans <- avalitools::apply_transf(df = df_novo, trans = fun_transformacoes_inv, is.string = F)
  
  # predicts the E(y*) value given that new data
  estimativa <- predict(modelo, newdata = novos_trans, interval = "confidence", level = 0.8)
  
  
  # transform the estimatives to the initial y values
  estimativa_trans <- sapply(estimativa,inverse_function(trans[ length(trans) ]) )
  
  
  # calculates the proportion of the distance between the fit ant lwr and upr
  prop <- c(  (estimativa_trans[2]-estimativa_trans[1])/estimativa_trans[1],
              0,
              (estimativa_trans[3]-estimativa_trans[1])/estimativa_trans[1] )
  
  # sum that distances 
  ampli <- abs(prop[1]) + abs(prop[3])
  
  
  # declaretes the prediction degree
  if (ampli <= 0.3) {
    grau <- "III"
  }else if (ampli <= 0.5){
    grau <- "II"
  }else {
    grau <- "I"
  }
  
  
  
  
  # if the transrformation of the independent variable is ln,
  # there will be 3 diferent estimatives for the central statistic 
  
  if(trans[length(trans)] == "lny") {
    
    
    estimativa_mode <- exp(estimativa - qme_)
    estimativa_median <- exp(estimativa)
    estimativa_mean <- exp(estimativa + (qme_/2))
    
    df_estimativa <- data.frame(c("Valor Mínimo","Valor Médio","Valor Máximo"),
                                avalitools::br2_format(estimativa_mode[c(2,1,3)]),
                                avalitools::br2_format(estimativa_median[c(2,1,3)]),
                                avalitools::br2_format(estimativa_mean[c(2,1,3)]),
                                scales::percent(prop, accuracy = 0.01),
                                c("-", grau, "-")
    )
    
    colnames(df_estimativa) <- c("Estimativa",
                                 "Moda",
                                 "Mediana",
                                 "Média",
                                 "Amplitude",
                                 "Grau de Precisão")
    
    
  } else {
    
    df_estimativa <- data.frame(c("Valor Mínimo","Valor Médio","Valor Máximo"),
                                avalitools::br2_format(estimativa_trans[c(2,1,3)]),
                                scales::percent(prop, accuracy = 0.01),
                                c("-", grau, "-")
    )
    
    colnames(df_estimativa) <- c("Estimativa",
                                 "Média",
                                 "Amplitude",
                                 "Grau de Precisão")
    
    
    
  }
  
  
  
  return(df_estimativa)  
  
}