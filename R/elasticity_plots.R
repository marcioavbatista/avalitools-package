elasticity_plots <- function(df = df, modelo = modelo, trans = trans, vari = vari) {
  
  # df is the inicial data
  # modelo is an lm object
  # trans is a sting vector with the used transformation in the model
  # vari is a sting vector with the names of the used variables
  
  
  # iniciates a list object that later will receive the plots generated
  lista <- list()
  
  # sets the estimated values
  predicts <- modelo$fitted.values
  # transform the sring vector to a list of functions
  trans_f <- avalitools::as_functions(trans)
  # sets a string variable with the name of the dependent variable
  vari_dep <- vari[length(vari)]
  
  
  df_transf <- avalitools::apply_transf(df,trans_f, is.string = F)
  df_transf <- df_transf[,-ncol(df)]
  
  df_resumo <- df_transf %>% summarise_all(mean)
  df_resumo <- df_resumo[rep(1,200),]
  
  
  # iterates betwen each independent variable and genearate the elasticity plots
  for(i in 1:(ncol(df) - 1)) {
    
    plot_title <- paste0("Estimativa p/", vari[i])
    
    table <- data.frame(v1 = seq( min(df[[i]]), max(df[[i]]), length.out = 200 ) )
    
    
    table <- table %>%
             mutate(v2 = sapply(v1, trans_f[[i]]) )
      
    
    df_resumo_it <- df_resumo
    
    df_resumo_it[[i]] <- table$v2
    
    
    preditos <- predict(modelo, newdata = df_resumo_it, interval = "confidence", level = 0.8)
    
    preditos <- preditos[,1]
    
    table$y <- preditos
    
    table <- table %>% 
             mutate(y = sapply(y ,inverse_function(trans[ncol(df)]) ) )
    
    
    
    plot <- ggplot(data = table, aes(x = v1, y = y ))+
      geom_line(col="green4")+
      scale_y_continuous(labels = scales::number_format(big.mark = "."), n.breaks = 12 )+
      scale_x_continuous(labels = scales::number_format(big.mark = "."), n.breaks = 3 )+
      labs( x = vari[i], y = vari_dep, title = plot_title)+
      theme_bw()
    
    
    
    
    
    lista[[i]] <- plot
    
    
    
  }
  
  return(lista)
  
} 