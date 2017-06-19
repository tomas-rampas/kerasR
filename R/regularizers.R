#' Apply penalties on layer parameters
#'
#' Regularizers allow to apply penalties on layer parameters or
#' layer activity during optimization. These penalties are
#' incorporated in the loss function that the network optimizes.
#'
#' The penalties are applied on a per-layer basis. The exact API
#' will depend on the layer, but the layers Dense,  Conv1D, Conv2D
#' and Conv3D have a unified API.
#'
#' @param l   multiplicitive factor to apply to the the penalty term
#' @param l1  multiplicitive factor to apply to the the l1 penalty term
#' @param l2  multiplicitive factor to apply to the the l2 penalty term
#'
#' @example inst/examples/regularizers.R
#' @template boilerplate
#' @name Regularizers
NULL

#' @rdname Regularizers
#' @export
l1 <- function(l = 0.01) {
  keras_check()

  modules$keras.regularizers$l1(l = l)
}

#' @rdname Regularizers
#' @export
l2 <- function(l = 0.01) {
  keras_check()

  modules$keras.regularizers$l2(l = l)
}

#' @rdname Regularizers
#' @export
l1_l2 <- function(l1 = 0.01, l2 = 0.01) {
  keras_check()

  modules$keras.regularizers$l1_l2(l1 = l1, l2 = l2)
}
