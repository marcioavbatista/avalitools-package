residuals_table <- function(modelo = modelo, df = df, indices = indices) {
  
  # the parameter modelo is a lm model object
  
  # the parameter df is the data.frame containing the variables
  # used to calculate the lm
  
  # catches the sd of the residuals
  qme_ <- qme(modelo)
  
  # creates a table with the y observed and estimates
  # and creates other columns relative to the inicial
  tabela_residuos <- data.frame(v1 = df[[ncol(df)]],
                                v2 =modelo$fitted.values)%>%
    mutate(res = v1 - v2,
           res_p = scales::percent(res/v1),
           res_dp = res/qme_,
           d_cook = avalitools::br2_format( cooks.distance(modelo) )
    )
  
  # bind the samplu number with the table
  tabela_residuos <- cbind(indices, tabela_residuos)
  
  
  colnames(tabela_residuos) <- c("Dado",
                                 "Observado",
                                 "Estimado",
                                 "Resíduo",
                                 "Resíduo (%)",
                                 "Resíduo / DP",
                                 "DCook")
  
  
  
  return(tabela_residuos)
  
}

