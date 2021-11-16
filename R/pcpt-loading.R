#' Loads experimental data from a folder. Expects two files _matr.csv
#' and report _matr.txt.0
#' @param folder Folder in which to search for the foiles.
#'
#' @return
#'
#'
#' @examples
load_experiment <- function(folder, preprocess = TRUE){
  ptr <- ".*\\.csv"
  files <- list.files(TEST_PTH, ptr, full.names = TRUE)
  participant <- gsub("pcpt-(.*)\\.csv", "\\1", basename(files[1]))
  df_pcpt <- read.table(files[1], sep=",", header = TRUE)
  if(preprocess){
    df_pcpt <- preprocess_experiment(df_pcpt)
  }
  return(df_pcpt)
}

