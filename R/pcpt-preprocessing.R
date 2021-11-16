preprocess_experiment <- function(df_pcpt){
  df_pcpt$corr <- df_pcpt$corr == 1
  df_pcpt$responded <- df_pcpt$responded == 1
  df_pcpt$rt[df_pcpt$rt < 0] <- NA_integer_
  # add block trials as well

  return(df_pcpt)
}
