apply_transf <- function(df = df, trans = trans, is.string = T) {
  # trans is a list of transformations or a vector string that contaings the
  # transformations tha will be aplied in each variable in order.
  
  # df is a data.frame tha will be transformed
  
  # is.string indicates if the parameter trans is a vector string or a list of functions
  
  # if trans is a vector string transform it into a list of functions
  if(is.string) {
    trans <- avalitools::as_functions(trans)
  }
  
  # copy df into df_dados_trans for secure the data 
  df_dados_trans <- df
  
  
  #iterate between the columns of df and apply the given transformations
  for(i in 1:ncol(df) ) {
    df_dados_trans[[i]] <- sapply(df_dados_trans[[i]], trans[[i]])
  }
  
  # returns the trasformed data.frame
  return(df_dados_trans)
}
