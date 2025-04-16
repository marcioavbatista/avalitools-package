ttests_table <- function (modelo, vari_uti) {
  # modelo is an lm object
  # vari_uti is an string vector with the used variables
  
  # creates a summary for the lm model
  sumario <- summary(modelo)
  
  # take the significance tests
  testes_sig <- sumario[[4]]
  
  # crates an int vector for the new indices
  indices_sig <- c(2:nrow(testes_sig), 1)
  
  # reorganize the matrix to the indices_sig
  testes_sig <-  testes_sig[indices_sig,]
  
  
  
  
  # creates a data frame using column 3 and four 4 that are the coluns related
  # to the t obs and sginficance respectively
  df_teste_sign <- data.frame(vari_uti,
                              br2_format(testes_sig[,3]),
                              scales::percent(testes_sig[,4],accuracy = 0.01,decimal.mark = ","))
  
  colnames(df_teste_sign) <- c("Variáveis",
                               "t Obs.",
                               "Sig.(%)")
  
  return(df_teste_sign)
  
  
}