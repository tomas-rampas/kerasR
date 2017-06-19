#' Apply Gaussian noise layer
#'
#' The function [GaussianNoise] applies additive noise,
#' centered around 0 and [GaussianDropout] applied multiplicative
#' noise centered around 1.
#'
#' @param stddev         standard deviation of the random Gaussian
#' @param rate           float, drop probability
#' @param input_shape    only need when first layer of a model;
#'                       sets the input shape of the data
#'
#' @example inst/examples/noise.R
#' @template boilerplate
#' @name GaussianNoise
NULL

#' @rdname GaussianNoise
#' @export
#' @family layers
GaussianNoise <- function(stddev = 1, input_shape = NULL) {
  keras_check()

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.noise$GaussianNoise(stddev = stddev)
  } else {

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.noise$GaussianNoise(stddev = stddev,
                              input_shape = input_shape)

  }

  return(res)
}


#' @rdname GaussianNoise
#' @export
GaussianDropout <- function(rate = 0.5, input_shape = NULL) {
  keras_check()

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.noise$GaussianDropout(rate = rate)
  } else {

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.noise$GaussianDropout(rate = rate,
                  input_shape = input_shape)

  }

  return(res)
}
