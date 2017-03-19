modules <- new.env(parent=emptyenv())

int32 <- function(x) {
  modules$np$int32(x)
}

#' Should examples be run on this system
#'
#' This function decided whether examples should be
#' run or not. Currently always answers FALSE to avoid
#' problems with checking the package on systems that
#' do not have keras installed.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @export
run_examples <- function() {
  FALSE
}
