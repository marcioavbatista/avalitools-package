descr_avalitools <- function(df) {
  
  # is the df of the inicial data (before transform the variables)
  
  
  # creates a vector with the statistics (mean,max,min and ampli) of each column of the data.frame
  v_mean <- apply(df,mean,MARGIN = 2)
  v_max <- apply(df,max,MARGIN = 2)
  v_min <- apply(df,min,MARGIN = 2)
  v_ampli <- v_max - v_min
  
  
  # creates a data.frame with all the statistics
  df_descr <- data.frame(
    v_min,
    v_max,
    v_mean,
    v_ampli
  )

  colnames(df_descr) = c("Mínimo", "Máximo", "Média", "Amplitude")
  
  return(df_descr)
  
}