#' Advanced activation layers
#'
#' @param alpha                   float >= 0. Negative slope coefficient in
#'                                [LeakyReLU] and scale for the negative
#'                                factor in [ELU].
#' @param theta                   float >= 0. Threshold location of
#'                                activation in [ThresholdedReLU].
#' @param input_shape             only need when first layer of a model;
#'                                sets the input shape of the data
#' @example inst/examples/layers_advanced_activations.R
#' @template boilerplate
#' @name AdvancedActivation
NULL

#' @rdname AdvancedActivation
#' @export
#' @family layers
LeakyReLU <- function(alpha = 0.3, input_shape = NULL) {

  keras_check()

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.advanced_activations$LeakyReLU(alpha = alpha)
  } else {

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.advanced_activations$LeakyReLU(alpha = alpha,
        input_shape = input_shape)

  }

  return(res)
}

#' @rdname AdvancedActivation
#' @export
PReLU <- function(input_shape = NULL) {

  keras_check()

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.advanced_activations$PReLU()
  } else {

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.advanced_activations$PReLU(
              input_shape = input_shape)

  }

  return(res)
}

#' @rdname AdvancedActivation
#' @export
ELU <- function(alpha = 1.0, input_shape = NULL) {

  keras_check()

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.advanced_activations$ELU(alpha = alpha)
  } else {

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.advanced_activations$ELU(alpha = alpha,
                  input_shape = input_shape)

  }

  return(res)
}

#' @rdname AdvancedActivation
#' @export
ThresholdedReLU <- function(theta = 1.0, input_shape = NULL) {

  keras_check()

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.advanced_activations$ThresholdedReLU(
          theta = theta)
  } else {

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.advanced_activations$ThresholdedReLU(
          theta = theta, input_shape = input_shape)

  }

  return(res)
}


