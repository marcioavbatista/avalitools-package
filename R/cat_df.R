cat_df <- function(df, colnames = NULL, rownames = NULL) {
  
  colnames(df) <- colnames
  rownames(df) <- rownames
  
  knitr::kable(df,format="pipe") 
  
}