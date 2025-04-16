regression_equation <- function (modelo,vari, trans) {
  
  
  #Adjust name of independent variables for transformations
  vari_uti_trans <- avalitools::transf_vari_names(vari, trans = trans)
  
  
  
  #Collect all independent variable names
  nome_vari_indep <- vari_uti_trans[-length(vari_uti_trans)]
  
  #Collect the intercept coefficient
  coeficiente_intercept_formula <- scales::number(modelo$coefficients[1],
                                                  big.mark = "",
                                                  decimal.mark = ",",
                                                  accuracy = 10^-4,
                                                  suffix = " "
  )
  
  
  #Collect the intercept coefficient for the independent variables
  coeficiente_vari_indep <- scales::number(modelo$coefficients[-1],
                                           style_positive = "plus",
                                           big.mark = "",
                                           decimal.mark = ",",
                                           accuracy = 10^-4)
  
  
  
  
  
  
  #Create the independent variables part of the formula
  formula_vari_indep <- paste(coeficiente_vari_indep,
                              nome_vari_indep,
                              collapse = ' ')
  
  #Create formula and change class to formula class
  formula_modelo <- paste(vari_uti_trans[length(vari_uti_trans)],
                          " = ",
                          coeficiente_intercept_formula,
                          formula_vari_indep)
  
  
  return(formula_modelo)
  
  
}