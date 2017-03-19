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
#' @examples
#' if (run_examples()) {
#' X_train <- matrix(rnorm(100 * 10), nrow = 100)
#' Y_train <- to_categorical(matrix(sample(0:2, 100, TRUE), ncol = 1), 3)
#'
#' mod <- Sequential()
#' mod$add(Dense(units = 50, input_shape = dim(X_train)[2]))
#' mod$add(Activation("relu"))
#' mod$add(Dense(units = 3, kernel_constraint = max_norm(),
#'                          bias_constraint = non_neg()))
#' mod$add(Dense(units = 3, kernel_constraint = unit_norm()))
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