transf_vari_names <- function(name = name,trans = trans) {
  
  # the parameter name is the string vector of the names 
  # the parameter trans is the string vector of the transformations 
  
  # starts a empty string vector 
  nomes_vari_trans <- c("")
  
  # iterates every name and trans and alocate the respective transformed name
  # in the nomes_vari_trans variable
  for(i in 1:length(trans)) {
    it_atual <- trans[i]
    
    if(it_atual == "x"| it_atual == "y"){
      nomes_vari_trans[i] <- paste0(name[i])
    }
    
    if(it_atual == "1/x"){
      nomes_vari_trans[i] <- paste0(" / ", name[i])
    }
    if(it_atual == "lnx"| it_atual == "lny"){
      nomes_vari_trans[i] <- paste0("ln(",name[i],")")
    }
    if(it_atual == "x2"| it_atual == "y2"){
      nomes_vari_trans[i] <- paste0(name[i], "²")
    }
    if(it_atual == "x3"| it_atual == "y3"){
      nomes_vari_trans[i] <- paste0(name[i], "³")
    }
    if(it_atual == "1/y"){
      nomes_vari_trans[i] <- paste0("1 / ",name[i])
    }
    
  }
  
  return(nomes_vari_trans)
}