### USER FUNCTIONS ###
#     Last edit: friar (2019-03-03)
# Previous edit: Manny (2017-05-06)

## Description
# User Functions are meta functions and methods for more efficient code writing

## General Functions

#' Numeric Absolute
#'
#' \code{nabs()} returns x after first converting it to class numeric via character
#'
#' Its primary use is converting objects of class factor to numeric
#' @param x vector.
#' @return numeric vector.
#' @export
nabs <- function(x) {return(as.numeric(as.character(x)))}

#' Moving
#' \code{moving()} returns a vector of averages obtained from the n elements of x preceding and including the element at each respective index
#' @param x the input vector.
#' @param n the window size. Default \code{5}.
#' @return a vector of moving averages.
#' @export
moving <- function(x, n = 5) {if(length(x) < n) {NA} else {stats::filter(x, rep(1/n, n), sides = 1)}}

#' NA if NULL
#' \code{na_if_null()} returns an object's value if it is not \code{NULL} and \code{NA} otherwise
#' @param x input vector.
#' @return \code{x} unless \code{x} is \code{NULL}, in which case \code{NA}
#' @export
na_if_null <- function(x) {ifelse(is.null(x) == TRUE, NA, x)}

#' NA as String
#' \code{na_as_string()} returns a character vector with \code{NA} values aliased to \code{"NA"}
#' @inheritParams na_if_null
#' @return character vector.
#' @export
na_as_string <- function(x) {ifelse(is.na(as.character(x)), "NA", as.character(x))}

#' NA as zero
#' \code{na_as_zero()} returns a numeric vector with \code{NA} values aliased to \code{0}
#' @inheritParams na_if_null
#' @return numeric vector.
#' @export
na_as_zero <- function(x) {ifelse(is.na(nabs(x)), 0, nabs(x))}

