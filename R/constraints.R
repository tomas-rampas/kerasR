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
#' @param max_value   maximum value to allow for the value (max_norm only)
#' @param axis        axis over which to apply constraint (max_norm only)
#'
#' @example inst/examples/constraints.R
#' @template boilerplate
#' @name Constraints
NULL

#' @rdname Constraints
#' @export
max_norm <- function(max_value = 2.0, axis=0) {
  modules$keras.constraints$max_norm(max_value, axis = int32(axis))
}

#' @rdname Constraints
#' @export
non_neg <- function() {
  modules$keras.constraints$non_neg()
}

#' @rdname Constraints
#' @export
unit_norm <- function() {
  modules$keras.constraints$unit_norm()
}
