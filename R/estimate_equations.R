estimate_equations <- function (modelo, vari, transf) {
  # modelo is an lm object
  # vari is an string vector with the names of the used variables
  # tranf is an string vector with the transforations used at the model 
  
  #variance estimative 
  qme_ <- qme(modelo)^2
  
  # transormed variables names for te equation
  vari_trans <- avalitools::transf_vari_names(vari, trans = transf)
  # select independent and dependent variables
  vari_indep <- vari_trans[-length(vari_trans)]
  vari_dep <- vari[length(vari)]
  
  # coefficent for the independent variable
  coef_vari_indep <- scales::number(modelo$coefficients[-1],
                                           style_positive = "plus",
                                           big.mark = "",
                                           decimal.mark = ",",
                                           accuracy = 10^-4)

  
  # sets the independent part of the formula
  formula_vari_indep <- paste(coef_vari_indep,
                              vari_indep,
                              sep = ' ')
  
  
  
  
  
  if (transf[length(transf)] == "lny") {
    
    # when the transformation for the dependent variable is ln
    # we will have trhee estimate equations
    
    formula_vari_indep <- paste(rep("e^(", (length(vari) - 1) ),
                                formula_vari_indep,
                                rep(")", (length(vari) - 1) ),
                                sep = " ")
    
    formula_direito <- paste(formula_vari_indep,
                             collapse = " * ")
    
    
    coef_intercept_median <- scales::number( exp(modelo$coefficients[1]),
                                              big.mark = "",
                                              decimal.mark = ",",
                                              accuracy = 10^-4,
                                              suffix = " ")
    
    
    formula_modelo_median <- paste(vari_dep,
                                    " = ",
                                    coef_intercept_median,
                                    " * ",
                                    formula_direito)
    
    
    coef_intercept_mode <- scales::number(exp(modelo$coefficients[1] - (qme_) ),
                                                    big.mark = "",
                                                    decimal.mark = ",",
                                                    accuracy = 10^-4,
                                                    suffix = " ")
    
    formula_modelo_mode <- paste(vari_dep,
                            " = ",
                            coef_intercept_mode,
                            " * ",
                            formula_direito)
    
    coef_intercept_mean <- scales::number(exp(modelo$coefficients[1] + (qme_/2) ),
                                                    big.mark = "",
                                                    decimal.mark = ",",
                                                    accuracy = 10^-4,
                                                    suffix = " ")
    
    formula_modelo_mean <- paste(vari_dep,
                             " = ",
                             coef_intercept_mean,
                             " * ",
                             formula_direito)
    
    
    
    vetor_retorno <- c(formula_modelo_mode,
                       formula_modelo_median,
                       formula_modelo_mean)
    
    return(vetor_retorno)
    

    
    
  }else if (transf[length(transf)] == "1/y") {
    
    
    coef_intercept <- scales::number((modelo$coefficients[1]),
                                     big.mark = "",
                                     decimal.mark = ",",
                                     accuracy = 10^-4,
                                     suffix = " ")
    
    
    
    formula_vari_indep <- paste(coef_vari_indep,
                                vari_indep,
                                collapse = ' ')
    
    
    formula_modelo <- paste(vari[length(vari)],
                            " = ",
                            "1/ (",
                            coef_intercept,
                            formula_vari_indep,
                            " )")
  
    
    return(formula_modelo)
    
    
    
    
    
    
    
    
  } else if (transf[length(transf)] == "y2") {
    
    coef_intercept <- scales::number((modelo$coefficients[1]),
                                     big.mark = "",
                                     decimal.mark = ",",
                                     accuracy = 10^-4,
                                     suffix = " ")
    
  
    formula_vari_indep <- paste(coef_vari_indep,
                                vari_indep,
                                collapse = ' ')
    
    
    formula_modelo <- paste(vari[length(vari)],
                            " = ",
                            "(",
                            coef_intercept,
                            formula_vari_indep,
                            ")^(1/2)")
    
    
    return(formula_modelo)
    
    
    
  
  } else if (transf[length(transf)] == "y3") {
    
    
    coef_intercept <- scales::number((modelo$coefficients[1]),
                                     big.mark = "",
                                     decimal.mark = ",",
                                     accuracy = 10^-4,
                                     suffix = " ")
    
    
    formula_vari_indep <- paste(coef_vari_indep,
                                vari_indep,
                                collapse = ' ')
    
    
    
    
    
    formula_modelo <- paste(vari[length(vari)],
                            " = ",
                            "(",
                            coef_intercept,
                            formula_vari_indep,
                            ")^(1/3)")
    
    
    return(formula_modelo)
    
    
    
    
  } else if (transf[length(transf)] == "y") {
    
    coef_intercept <- scales::number((modelo$coefficients[1]),
                                     big.mark = "",
                                     decimal.mark = ",",
                                     accuracy = 10^-4,
                                     suffix = " ")
    
    
    formula_vari_indep <- paste(coef_vari_indep,
                                vari_indep,
                                collapse = ' ')
    
    
    
    
    
    formula_modelo <- paste(vari[length(vari)],
                            " = ",
                            coef_intercept,
                            formula_vari_indep)
    
    
    return(formula_modelo)
    
    
  }
 
  
  
  
  
  
  
}