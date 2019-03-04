#' F Table to Data Frame
#' \code{ftable2df()} returns a \code{data.frame} from an \code{ftable} object
#' @param mydata an \code{ftable}
#' @return a \code{data.frame}
#' @export
ftable2df <- function(mydata) {
  mydata <- if(class(mydata) == "ftable") {mydata} else {stats::ftable(mydata)}

  dfrows <- rev(expand.grid(rev(attr(mydata, "row.vars"))))

  dfcols <- as.data.frame.matrix(mydata)

  names(dfcols) <- do.call(paste, c(rev(expand.grid(rev(attr(mydata, "col.vars")))), sep = "_"))

  cbind(dfrows, dfcols)

}
