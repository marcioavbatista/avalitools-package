as_functions <- function(trans = trans) {
  
  # the trans parameter is a string vector containing -
  # the transformations of the model 
  
  
  # define the transformating functions used
  inversa <- function(x){
    return(1/x)
  }
  
  constante <- function(x){
    return(x)
  }
  
  segunda_potencia <- function(x){
    return(x^2)
  }
  
  terceira_potencia <- function(x){
    return(x^3)
  }
  
  
  # obs: we will use log() for the natural logharithm -
  # and sqrt() for the squared root
  
  
  # f-trans is the list of the functions in the same order -
  # as the string vector
  f_trans <- list()  
  
  #creates a loop that iterates every position of the vector trans-
  #and determinates the correponding function
  for(i in 1:length(trans)) {
    it_atual <- trans[i]
    
    if(it_atual == "x"| it_atual == "y"){
      f_trans[[i]] <- constante
    }
    
    if(it_atual == "1/x"| it_atual == "1/y"){
      f_trans[[i]] <- inversa
    }
    if(it_atual == "lnx" | it_atual == "lny"){
      f_trans[[i]] <- log
    }
    if(it_atual == "x2"| it_atual == "y2"){
      f_trans[[i]] <- segunda_potencia
    }
    if(it_atual == "x3"| it_atual == "y3"){
      f_trans[[i]] <- terceira_potencia
    }
    
  }
  
  
  return(f_trans)
  
}