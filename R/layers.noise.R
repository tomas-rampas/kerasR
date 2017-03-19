#' Apply Gaussian noise layer
#'
#' The function \code{GaussianNoise} applies additive noise,
#' centered around 0 and \code{GaussianDropout} applied multiplicative
#' noise centered around 1.
#'
#' @param stddev         standard deviation of the random Gaussian
#' @param rate           float, drop probability
#' @param input_shape    only need when first layer of a model; sets the input shape
#'                         of the data
#'
#' @examples
#' if (run_examples()) {
#' X_train <- matrix(rnorm(100 * 10), nrow = 100)
#' Y_train <- to_categorical(matrix(sample(0:2, 100, TRUE), ncol = 1), 3)
#'
#' mod <- Sequential()
#' mod$add(Dense(units = 50, input_shape = dim(X_train)[2]))
#' mod$add(Dropout(rate = 0.5))
#' mod$add(Activation("relu"))
#' mod$add(GaussianNoise())
#' mod$add(GaussianDropout())
#' mod$add(Dense(units = 3))
#' mod$add(ActivityRegularization(l1 = 1))
#' mod$add(Activation("softmax"))
#' keras_compile(mod,  loss = 'categorical_crossentropy', optimizer = RMSprop())
#'
#' keras_fit(mod, X_train, Y_train, batch_size = 32, epochs = 5,
#'           verbose = 0, validation_split = 0.2)
#' }
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @references
#'
#'   Chollet, Francois. 2015. \href{https://keras.io/}{Keras: Deep Learning library for Theano and TensorFlow}.
#'
#' @name GaussianNoise
NULL

#' @rdname GaussianNoise
#' @export
GaussianNoise <- function(stddev = 1, input_shape = NULL) {

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.noise$GaussianNoise(stddev = stddev)
  } else {

    input_shape <- sapply(input_shape, list)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.noise$GaussianNoise(stddev = stddev, input_shape = input_shape)

  }

  return(res)
}


#' @rdname GaussianNoise
#' @export
GaussianDropout <- function(rate = 0.5, input_shape = NULL) {

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.noise$GaussianDropout(rate = rate)
  } else {

    input_shape <- sapply(input_shape, list)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.noise$GaussianDropout(rate = rate, input_shape = input_shape)

  }

  return(res)
}
