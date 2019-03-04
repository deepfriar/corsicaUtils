#' Logarithmic Loss
#'
#' \code{log_loss()} returns the logarithmic loss obtained from a given prediction and known result
#'
#' The allow_inf parameter controls whether infinite loss is allowed (default is FALSE)
#' Setting allow_inf to FALSE will cause large but finite penalties at the extremes
#'
#' @param act          actual results
#' @param pred      predicted probabilties
#' @param allow_inf pass through infinite loss? \code{FALSE} replaces Inf with large but finite penalties at the extremes. Default \code{FALSE}.
#' @return numeric.
#' @export
log_loss <- function(act, pred, allow_inf = FALSE) {
  eps <- as.numeric(!allow_inf) * 1e-15

  pred <- matrix(sapply(pred, function(x) max(eps, x)),     nrow = nrow(pred))
  pred <- matrix(sapply(pred, function(x) min(1 - eps, x)), nrow = nrow(pred))

  ll <- sum(act*log(pred) + (1 - act)*log(1 - pred))
  ll <- -ll/(nrow(act))

  ll

}

#' Brier Score
#' \code{brier()} returns the Brier score obtained from a given prediction and known result
#' @inheritParams log_loss
#' @return numeric.
#' @export
brier <- function(act, pred) {sum((act - pred)^2)/length(act)}
