#' Layer wrappers
#'
#' Apply a layer to every temporal slice of an input or to bi-directional
#' RNN.
#'
#' @param layer      a layer instance (must be a recurrent layer for the
#'                   bi-directional case)
#' @param merge_mode Mode by which outputs of the forward and backward RNNs
#'                   will be combined. One of {'sum', 'mul', 'concat', 'ave',
#'                   None}. If None, the outputs will not be combined,
#'                   they will be returned as a list.

#' @example inst/examples/layer_wrappers.R
#' @template boilerplate
#' @name LayerWrapper
NULL

#' @rdname LayerWrapper
#' @export
#' @family layers
TimeDistributed <- function(layer) {
  keras_check()

  modules$keras.layers.wrappers$TimeDistributed(layer)
}

#' @rdname LayerWrapper
#' @export
Bidirectional <- function(layer, merge_mode = 'concat') {
  keras_check()

  modules$keras.layers.wrappers$Bidirectional(layer, merge_mode = merge_mode)
}
