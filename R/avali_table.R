avali_table <- function(df_imo, df_inicial) {
  
  # Função interna teste que faz a comparação
  teste <- function(cont, df) {
    
    # Inicializando o vetor retorno
    retorno <- vector("character", length = length(cont))
    
    # Verificar se o comprimento de 'cont' e o número de colunas de 'df' são compatíveis
    if(length(cont) != ncol(df)) {
      stop("O número de elementos em 'Conteudo' não corresponde ao número de colunas de 'df_inicial'.")
    }
    
    for (i in 1:length(cont)) {
      it_atual <- cont[i]
      vetor <- df[[i]]
      
      # Verificar se o vetor é numérico
      if (!is.numeric(vetor)) {
        retorno[i] <- "-"  # Se 'cont' não for numérico, retorna "-"
      } else {
        # Verificar se 'it_atual' é um número válido (não NA)
        if (!is.na(it_atual)) {
          # Verifica se o valor de 'cont' está fora dos limites do 'vetor'
          min_val <- min(vetor, na.rm = TRUE)
          max_val <- max(vetor, na.rm = TRUE)
          
          if (it_atual > max_val | it_atual < min_val) {
            retorno[i] <- "Sim"
          } else {
            retorno[i] <- "Não"
          }
        } else {
          retorno[i] <- "-"  # Se 'it_atual' for NA, retorna "-"
        }
      }
    }
    
    return(retorno)
  }
  
  # Adicionando uma coluna 'id' para indexar as linhas de df_imo
  df_imo <- df_imo %>% mutate(id = row_number())
  
  # Aplicando a função teste para cada linha de df_imo, comparando 'Conteudo' com valores de df_inicial
  df_imo$extr <- teste(df_imo$Conteudo, df_inicial)
  
  # Removendo a coluna 'id'
  return(select(df_imo, -id))
  
}