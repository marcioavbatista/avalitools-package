cor_avalitools <- function (df, vari_uti, inf = F) {
  
  # df is the data.frame with the transformed variables used in the linear model
  # vari_uti is an vector with the names of the used variables 
  # inf is a bool variable that informs which type of correlations will be calculated
  

  
  # calculates the correlations
  if (!inf) {
    matriz_correlacao <- cor(df) %>% 
                           apply(FUN = br2_format, MARGIN = c(1,2))

  }else {

    matriz_correlacao <- ppcor::pcor(df)$estimate %>% 
                         apply(FUN = br2_format, MARGIN = c(1,2))
    
  }
  
  #  create a vector with the simplifyed names of the variables
  alias <- colnames(df)
  
  # bind the ull names with 
  matriz_correlacao <- cbind(vari_uti,alias, matriz_correlacao)
  
  colnames(matriz_correlacao) <-  c("Variáveis","Alias" , alias)
  
  return(matriz_correlacao)
  
  
}