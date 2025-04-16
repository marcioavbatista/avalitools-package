read_avaliation <- function(path = path) {
  
  # read the avaliation data
  df_dados <- read_excel(path=path,sheet = 1)
  df_info <- read_excel(path=path,sheet = 2)
  df_vari <- read_excel(path= path,sheet = 3)
  df_imo <- read_excel(path= path,sheet = 4)
  
  # add an column related to the number of samples
  
  df_dados <- df_dados %>% 
              dplyr::mutate( i = 1:n() )
  
  # Saves initial data values
  
  df_dados_inicial <- df_dados
  
  #Treats this data
  
  # 1) Removes deactivated samples from the dataframe of the variables to be used
  df_dados_uti <- df_dados %>% 
                  dplyr::filter(ativo==1) %>% 
                  dplyr::select(-ativo)
  
  # Adds to a vector which samples are being used
  
  indices <- df_dados_uti %>% 
             dplyr::select(i)
  
  #1.1) Get the names of numerical variables
  nome_vari_num <- df_vari %>%
                   dplyr::filter(Tipo == 'Numérica') %>%
                   dplyr::select(Nome)
  
  nome_vari_num<- nome_vari_num$Nome
  
  
  #Get the name of the numerical variables used
  nome_vari_num_uti <- df_vari %>%
                       dplyr::filter( Habilitada == 'Sim', Tipo == 'Numérica') %>%
                       dplyr::select(Nome)
                       nome_vari_num_uti <- nome_vari_num_uti$Nome
  
  #1.2) Selects only the variables used in the calculation
  
  df_dados_num <- df_dados_uti %>% 
                  dplyr::select(all_of(nome_vari_num_uti)) 
  
  # Simplifies variable names
  
  range_vari_uti <- 1:(ncol(df_dados_num) - 1)
  
  nomes_vari_indep <- paste0("x",range_vari_uti)
  
  nomes_vari_num_uti_simpli <- c(nomes_vari_indep, "y")
  
  colnames(df_dados_num) <- nomes_vari_num_uti_simpli
  
  
  # takes the transformations used in the model
  transf <- df_vari %>% 
            dplyr::filter(Tipo == "Numérica", Habilitada == "Sim") %>% 
            dplyr::select(Transformação)
  
  transf <- transf$Transformação
  
  
  
  
  # returns everything 
  return_list <- list(dados = df_dados_num,
                      vari = df_vari,
                      info = df_info,
                      inicial = df_dados_inicial,
                      imo = df_imo,
                      transf = transf,
                      indices = indices
  ) 
  
  return(return_list)
  
  
  
}