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
#' @examples
#' if (run_examples()) {
#' X_train <- matrix(rnorm(100 * 10), nrow = 100)
#' Y_train <- to_categorical(matrix(sample(0:2, 100, TRUE), ncol = 1), 3)
#'
#' mod <- Sequential()
#' mod$add(Dense(units = 50, input_shape = dim(X_train)[2]))
#' mod$add(Activation("relu"))
#' mod$add(Dense(units = 3, kernel_regularizer = l1(l = 0.05),
#'                          bias_regularizer = l2(l = 0.05)))
#' mod$add(Dense(units = 3, kernel_regularizer = l1_l2(l1 = 0.05, l2 = 0.1)))
#' mod$add(Activation("softmax"))
#' keras_compile(mod,  loss = 'categorical_crossentropy', optimizer = RMSprop())
#'
#' keras_fit(mod, X_train, Y_train, batch_size = 32, epochs = 5, verbose = 0)
#' }
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @references
#'
#'   Chollet, Francois. 2015. \href{https://keras.io/}{Keras: Deep Learning library for Theano and TensorFlow}.
#'
#' @name Regularizers
NULL

#' @rdname Regularizers
#' @export
l1 <- function(l = 0.01) {
  modules$keras.regularizers$l1(l = l)
}

#' @rdname Regularizers
#' @export
l2 <- function(l = 0.01) {
  modules$keras.regularizers$l2(l = l)
}

#' @rdname Regularizers
#' @export
l1_l2 <- function(l1 = 0.01, l2 = 0.01) {
  modules$keras.regularizers$l1_l2(l1 = l1, l2 = l2)
}