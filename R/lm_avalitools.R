lm_avalitools <- function(df = df) {
  
  # the parameter df is the data.frame that will be used in the calculus
  # obs: the response variable needs to be the last one
  
  
  # vari is a string vector that contaings the names of the variables
  vari <- colnames(df)

  tam_vari <- length(vari)
  
  
  # creates the formula name that need to be inputed -
  # in the tradional lm function
  
  # ---
  
  vari_indep <- vari[-tam_vari]
  vari_dep <- vari[tam_vari]
  
  formula_vari_indep <- paste(vari_indep, collapse = ' + ')
  
  formula_primeiro_modelo <- paste(vari_dep, " ~ ", formula_vari_indep)
  formula_primeiro_modelo <- as.formula(formula_primeiro_modelo)
  
  # ---
  
  
  # uses the standard function to calculate the lm
  modelo <- lm(formula_primeiro_modelo,data = df)
  
  #uses the standard funcion to create the anova table for the lm
  anova <- anova(modelo)
  
  #creates and return a list of the lm and anova-table
  return_list <- list(modelo = modelo, anova =  anova)
  return(return_list)
}