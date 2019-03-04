#' Do Call Apply
#'
#' \code{dcapply()} uses \code{do.call()} to merge the products of an applied function according to specifications
#'
#' This function is no longer used for anything in \code{dryscrape}.
#'
#' The function will be applied in parallel if \code{cores >= 1}.
#' @param x           list. the input list
#' @param fun     function. the function to map over \code{x}
#' @param combine function. the function to call on the mapped results
#' @param cores    numeric. Number of cores to use in parallel. Default \code{1}.
#' @param ... arguments passed to \code{fun}.
#' @return a single entity.
#' @export
dcapply <- function(x, fun, combine, cores = 1, ...) {
  y <- if(cores > 1) {
    doMC::registerDoMC(cores)

    chunks <- split(x, cut(1:length(x), cores))

    foreach::foreach(i = 1:cores, .combine = c) %dopar% {chunks[[i]] %>% lapply(fun, ...)}
  } else {y <- lapply(x, fun, ...)}

  do.call(combine, y)
}
