complementary_info <- function(num_v, num_vu, num_d, num_du) {
  
  # sets the labels for each statistic
  labels_tabela_info_complementares <- c('Total de variáveis:',
                                         'Variáveis utilizadas no modelo:',
                                         'Total de dados:',
                                         'Dados utilizados no modelo:')
  
  # create a data.frame with the given data
  df_info_complementares <- data.frame(vari= labels_tabela_info_complementares,
                                       quant = c(num_v, 
                                                 num_vu,
                                                 num_d,
                                                 num_du
                                       )
  )
  
  return(df_info_complementares)
  
}