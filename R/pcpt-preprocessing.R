#' Title
#'
#' @param df_pcpt
#'
#' @return
#' @export
#'
#' @examples
preprocess_experiment <- function(df_pcpt){
  df_pcpt$corr <- df_pcpt$corr == 1
  df_pcpt$responded <- df_pcpt$responded == 1
  df_pcpt$rt[df_pcpt$rt < 0] <- NA_integer_
  # add block trials as well
  df_pcpt$block_trial <- as.vector(sapply(1:length(rle_res$lengths),
                                          function(x){seq_len(rle_res$lengths[x])}))

  return(df_pcpt)
}
