#' Preprocesses some of the columns int he loaded dataframe
#'
#' @description Converts column types, adds new columns
#'
#' @param df_pcpt data.frame loaded with `\code{\link{load_experiment}}`
#'
#' @return data.frame
#' @export
#'
#' @examples
preprocess_experiment <- function(df_pcpt){
  df_pcpt$corr <- df_pcpt$corr == 1
  df_pcpt$responded <- df_pcpt$responded == 1
  df_pcpt$rt[df_pcpt$rt < 0] <- NA_integer_
  # add block trials as well
  rle_res <- rle(df_pcpt$block)
  df_pcpt$block_trial <- as.vector(sapply(1:length(rle_res$lengths),
                                          function(x){seq_len(rle_res$lengths[x])}))
  df_pcpt$should_respond <- (df_pcpt$responded & df_pcpt$corr) |
    (!df_pcpt$responded & !df_pcpt$corr)
  return(df_pcpt)
}

