inverse_function <- function(fun = fun) {
  
  # the parameter fun is a string
  
  # defines the funtions that will be used 
  inversa <- function(x){
    return(1/x)
  }
  
  constante <- function(x){
    return(x)
  }
  
  segunda_potencia <- function(x){
    return(x^(1/2))
  }
  terceira_potencia <- function(x){
    return(x^(1/3))
  }
  
  
  
  # logical conditions that determinate wich function is correspondent to the
  # inverse function of the parameter fun.
  if(fun == "x"| fun == "y"){
    inv_fun <- constante
  }
  
  if(fun == "1/x"| fun == "1/y"){
    inv_fun <- inversa
  }
  if(fun == "lnx" | fun == "lny"){
    inv_fun <- exp
  }
  if(fun == "x2"| fun == "y2"){
    inv_fun <- segunda_potencia
  }
  if(fun == "x3"| fun == "y3"){
    inv_fun <- terceira_potencia
  }
  
  #in_fun is a function 
  
  return(inv_fun)
  
}