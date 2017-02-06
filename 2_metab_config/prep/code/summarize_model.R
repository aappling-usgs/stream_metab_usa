#' For prep MLE run, extract daily K and Q
summarize_model(model_out) {
  metpreds <- predict_metab(model_out)
  K <- metpreds$K600.daily
  Q <- 10
}