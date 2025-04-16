as_avaliation_anova <- function(anova) {
  
  # the parameter anova is an anova object 
  
  
  gl_original <- anova[[1]]
  sq_original <- anova[[2]]
  qm_original <- anova[[3]]
  
  tam_anova <- nrow(anova)
  
  # adapt the coefficients of the table into explained and not explained
  
  gl_adaptado <- c(sum(gl_original[1:tam_anova-1]),
                   gl_original[tam_anova],
                   sum(gl_original))
  
  
  sq_adaptado <- c(sum(sq_original[1:tam_anova-1]),
                   sq_original[tam_anova],
                   sum(sq_original))
  
  
  qm_adaptado <- c(sum(qm_original[1:tam_anova-1])/(tam_anova-1),
                   qm_original[tam_anova])
  
  valor_f <- qm_adaptado[1]/qm_adaptado[2]
  
  
  
  # creates the adapted anova table
  tabela_anova <- data.frame(c("Explicada", "Não Explicada", "Total"),
                             gl_adaptado,
                             avalitools::br3_format(sq_adaptado),
                             c(avalitools::br3_format(qm_adaptado),'-'),
                             c(avalitools::br3_format(valor_f ), rep('-',2)))
  
  # defines columns names for the tables
  colnames(tabela_anova) <- c("Fonte de Variação",
                              "Graus de Liberdade",
                              "Soma dos Quadrados",
                              "Quadrado Médio",
                              "F")
  
  
  return(tabela_anova)
  
  
} 