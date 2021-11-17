#' Title
#'
#' @param df_pcpt preprocessed data frame with pebl data
#'
#' @return data.frame
#' @export
#'
#' @examples
analyze_performance <- function(df_pcpt){
  res <- list()

  res$n_trials <- nrow(df_pcpt)
  res$n_targets <- sum(df_pcpt$should_respond)
  res$n_foils <- sum(!df_pcpt$should_respond)
  res$trial_accuracy <- sum(df_pcpt$corr)/res$n_trials
  res$target_accuracy <- sum(df_pcpt$corr[df_pcpt$should_respond])/res$n_targets
  res$foil_accuracy <- sum(df_pcpt$corr[!df_pcpt$should_respond])/res$n_foils

  res$error_comission <- sum(!df_pcpt$corr[!df_pcpt$should_respond])
  res$error_omission <-  sum(!df_pcpt$corr[df_pcpt$should_respond])

  ## Reaction time
  res$rt_mean <- mean(df_pcpt$rt, na.rm = TRUE)
  res$rt_sd <- sd(df_pcpt$rt, na.rm = TRUE)
  res$rt_correct_mean <- mean(df_pcpt$rt[df_pcpt$corr], na.rm=TRUE)
  res$rt_correct_sd <- sd(df_pcpt$rt[df_pcpt$corr], na.rm=TRUE)
  res$rt_error_mean <- mean(df_pcpt$rt[!df_pcpt$corr], na.rm=TRUE)
  res$rt_error_sd <- sd(df_pcpt$rt[!df_pcpt$corr], na.rm=TRUE)

  res$sensitivity_c <- sensitivity(res$target_accuracy, 1-res$foil_accuracy)
  #res$beta_prime_d <- bias_beta_prime_d(res$target_accuracy, 1-res$foil_accuracy)
  res$response_bias <- response_bias(res$target_accuracy, 1-res$foil_accuracy)
  res <- as.data.frame(res)

  return(res)
}

sensitivity <- function(hit_rate, false_alarm){
  sens <- qnorm(hit_rate)-qnorm(false_alarm)
  return(sens)
}

response_bias <- function(hit_rate, false_alarm){
  #bias <- -0.5*(qnorm(hit_rate)^2-qnorm(false_alarm)^2)
  # taken from https://github.com/neuropsychology/psycho.R/blob/master/R/dprime.R
  bias <- exp((qnorm(false_alarm)^2/2)-(qnorm(hit_rate)^2/2))
  return(bias)
}

bias_beta_prime_d <- function(hit_rate, false_alarm){
  beta <- ((hit_rate * (1-hit_rate)) - (false_alarm* (1-false_alarm)))/
    ((hit_rate * (1-hit_rate)) + (false_alarm* (1-false_alarm)))
  return(beta)
}

