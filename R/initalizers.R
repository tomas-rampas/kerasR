#' Define the way to set the initial random weights of Keras layers.
#'
#' These functions are used to set the initial weights and biases in
#' a keras model.
#'
#' @param value          constant value to start all weights at
#' @param mean           average of the Normal distribution to sample from
#' @param stddev         standard deviation of the Normal distribution to
#'                       sample from
#' @param seed           Integer. Used to seed the random generator.
#' @param minval         Lower bound of the range of random values to
#'                       generate.
#' @param maxval         Upper bound of the range of random values to
#'                       generate.
#' @param scale          Scaling factor (positive float).
#' @param mode           One of "fan_in", "fan_out", "fan_avg".
#' @param distribution   distribution to use. One of 'normal' or 'uniform'
#' @param gain           Multiplicative factor to apply to the orthogonal
#'                       matrix
#'
#' @example inst/examples/initializers.R
#' @template boilerplate
#' @name Initalizers
NULL

#' @rdname Initalizers
#' @export
Zeros <- function() {
  modules$keras.initializers$Zeros()
}

#' @rdname Initalizers
#' @export
Ones <- function() {
  modules$keras.initializers$Ones()
}

#' @rdname Initalizers
#' @export
Constant <- function(value = 0.0) {
  modules$keras.initializers$Constant(value = value)
}

#' @rdname Initalizers
#' @export
RandomNormal <- function(mean = 0.0, stddev = 0.05, seed = NULL) {
  if (!is.null(seed))
    seed <- int32(seed)

  modules$keras.initializers$RandomNormal(mean = mean, stddev = stddev,
                                          seed = seed)
}

#' @rdname Initalizers
#' @export
RandomUniform <- function(minval = -0.05, maxval = 0.05, seed = NULL) {
  if (!is.null(seed))
    seed <- int32(seed)

  modules$keras.initializers$RandomUniform(minval = minval, maxval = maxval,
                                           seed = seed)
}

#' @rdname Initalizers
#' @export
TruncatedNormal <- function(mean = 0.0, stddev = 0.05, seed = NULL) {
  if (!is.null(seed))
    seed <- int32(seed)

  modules$keras.initializers$TruncatedNormal(mean = mean, stddev = stddev,
                                             seed = seed)
}

#' @rdname Initalizers
#' @export
VarianceScaling <- function(scale = 1.0, mode = 'fan_in',
                            distribution = 'normal', seed = NULL) {
  if (!is.null(seed))
    seed <- int32(seed)

  modules$keras.initializers$VarianceScaling(scale = scale, mode = mode,
                                        distribution = distribution,
                                        seed = seed)
}

#' @rdname Initalizers
#' @export
Orthogonal <- function(gain = 1.0, seed = NULL) {
  if (!is.null(seed))
    seed <- int32(seed)

  modules$keras.initializers$Orthogonal(gain = gain, seed = seed)
}

#' @rdname Initalizers
#' @export
Identity <- function(gain = 1.0) {
  modules$keras.initializers$Identity(gain = gain)
}

#' @rdname Initalizers
#' @export
lecun_uniform <- function(seed = NULL) {
  if (!is.null(seed))
    seed <- int32(seed)

  modules$keras.initializers$lecun_uniform(seed = seed)
}

#' @rdname Initalizers
#' @export
glorot_normal <- function(seed = NULL) {
  if (!is.null(seed))
    seed <- int32(seed)

  modules$keras.initializers$glorot_normal(seed = seed)
}

#' @rdname Initalizers
#' @export
glorot_uniform <- function(seed = NULL) {
  if (!is.null(seed))
    seed <- int32(seed)

  modules$keras.initializers$glorot_uniform(seed = seed)
}

#' @rdname Initalizers
#' @export
he_normal <- function(seed = NULL) {
  if (!is.null(seed))
    seed <- int32(seed)

  modules$keras.initializers$he_normal(seed = seed)
}

#' @rdname Initalizers
#' @export
he_uniform <- function(seed = NULL) {
  if (!is.null(seed))
    seed <- int32(seed)

  modules$keras.initializers$he_uniform(seed = seed)
}

