modules <- new.env(parent=emptyenv())

int32 <- function(x) {
  modules$np$int32(x)
}

#' Should examples be run on this system
#'
#' This function decides whether examples should be
#' run or not. Answers \code{TRUE} if and only if the
#' package is able to find an installation of keras.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @export
run_examples <- function() {
  keras_available()
}
