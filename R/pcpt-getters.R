#' Returns table for only specific trial dirations
#'
#' @param trial_duration
#'
#' @return
#' @export
#'
#' @examples
get_condition <- function(df_pcpt, trial_duration){
  if(!is.integer(trial_duration)){
    warning("Can only select based on integer values of trial durations")
    return(NULL)
  }
  return(df_pcpt[df_pcpt$cond == trial_duration, ])
}
