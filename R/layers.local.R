#' Locally-connected layer
#'
#' The LocallyConnected layers works similarly to the Conv layers,
#' except that weights are unshared, that is, a different set of
#' filters is applied at each different patch of the input.
#'
#' @param filters            Integer, the dimensionality of the output space
#'                           (i.e. the number output of filters in the convolution).
#' @param kernel_size        A pair of integers specifying the dimensions of the 2D
#'                           convolution window.
#' @param strides            A pair of integers specifying the stride length of the
#'                           convolution.
#' @param padding            One of "valid", "causal" or "same" (case-insensitive).
#' @param data_format        A string, one of channels_last (default) or channels_first.
#'                           The ordering of the dimensions in the inputs.
#' @param activation         Activation function to use
#' @param use_bias           Boolean, whether the layer uses a bias vector.
#' @param kernel_initializer Initializer for the kernel weights matrix
#' @param bias_initializer   Initializer for the bias vector
#' @param kernel_regularizer Regularizer function applied to the kernel weights matrix
#' @param bias_regularizer   Regularizer function applied to the bias vector
#' @param activity_regularizer  Regularizer function applied to the output of the layer (its "activation").
#' @param kernel_constraint  Constraint function applied to the kernel matrix
#' @param bias_constraint    Constraint function applied to the bias vector
#' @param input_shape            only need when first layer of a model; sets the input shape
#'                               of the data
#'
#' @examples
#' if (run_examples()) {
#' X_train <- array(rnorm(100 * 28 * 28), dim = c(100, 28, 28, 1))
#' Y_train <- to_categorical(matrix(sample(0:2, 100, TRUE), ncol = 1), 3)
#'
#' mod <- Sequential()
#' mod$add(Conv2D(filters = 2, kernel_size = c(2, 2),
#'                input_shape = c(28, 28, 1)))
#' mod$add(Activation("relu"))
#' mod$add(MaxPooling2D(pool_size=c(2, 2)))
#' mod$add(LocallyConnected2D(filters = 2, kernel_size = c(2, 2)))
#' mod$add(Activation("relu"))
#' mod$add(MaxPooling2D(pool_size=c(2, 2)))
#' mod$add(Dropout(0.25))
#'
#' mod$add(Flatten())
#' mod$add(Dropout(0.5))
#' mod$add(Dense(3, activation='softmax'))
#'
#' keras_compile(mod, loss='categorical_crossentropy', optimizer=RMSprop())
#' keras_fit(mod, X_train, Y_train, verbose = 0)
#' }
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @references
#'
#'   Chollet, Francois. 2015. \href{https://keras.io/}{Keras: Deep Learning library for Theano and TensorFlow}.
#'
#' @name LocallyConnected
NULL

#' @rdname LocallyConnected
#' @export
LocallyConnected1D <- function(filters,
                   kernel_size,
                   strides = 1,
                   padding = 'valid',
                   activation = NULL,
                   use_bias = TRUE,
                   kernel_initializer = 'glorot_uniform',
                   bias_initializer = 'zeros',
                   kernel_regularizer = NULL,
                   bias_regularizer = NULL,
                   activity_regularizer = NULL,
                   kernel_constraint = NULL,
                   bias_constraint = NULL,
                   input_shape = NULL) {

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.local$LocallyConnected1D(filters = int32(filters),
                   kernel_size = int32(kernel_size),
                   strides = int32(strides),
                   padding = padding,
                   activation = activation,
                   use_bias = use_bias,
                   kernel_initializer = kernel_initializer,
                   bias_initializer = bias_initializer,
                   kernel_regularizer = kernel_regularizer,
                   bias_regularizer = bias_regularizer,
                   activity_regularizer = activity_regularizer,
                   kernel_constraint = kernel_constraint,
                   bias_constraint = bias_constraint)
  } else {

    input_shape <- sapply(input_shape, list)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.local$LocallyConnected1D(filters = int32(filters),
                   kernel_size = int32(kernel_size),
                   strides = int32(strides),
                   padding = padding,
                   activation = activation,
                   use_bias = use_bias,
                   kernel_initializer = kernel_initializer,
                   bias_initializer = bias_initializer,
                   kernel_regularizer = kernel_regularizer,
                   bias_regularizer = bias_regularizer,
                   activity_regularizer = activity_regularizer,
                   kernel_constraint = kernel_constraint,
                   bias_constraint = bias_constraint,
                   input_shape = input_shape)

  }

  return(res)
}

#' @rdname LocallyConnected
#' @export
LocallyConnected2D <- function(filters,
                   kernel_size,
                   strides = c(1, 1),
                   padding = 'valid',
                   data_format = NULL,
                   activation = NULL,
                   use_bias = TRUE,
                   kernel_initializer = 'glorot_uniform',
                   bias_initializer = 'zeros',
                   kernel_regularizer = NULL,
                   bias_regularizer = NULL,
                   activity_regularizer = NULL,
                   kernel_constraint = NULL,
                   bias_constraint = NULL,
                   input_shape = NULL) {


  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.local$LocallyConnected2D(filters = int32(filters),
                   kernel_size = int32(kernel_size),
                   strides = int32(strides),
                   padding = padding,
                   data_format = data_format,
                   activation = activation,
                   use_bias = use_bias,
                   kernel_initializer = kernel_initializer,
                   bias_initializer = bias_initializer,
                   kernel_regularizer = kernel_regularizer,
                   bias_regularizer = bias_regularizer,
                   activity_regularizer = activity_regularizer,
                   kernel_constraint = kernel_constraint,
                   bias_constraint = bias_constraint)
  } else {

    input_shape <- sapply(input_shape, list)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.local$LocallyConnected2D(filters = int32(filters),
                   kernel_size = int32(kernel_size),
                   strides = int32(strides),
                   padding = padding,
                   data_format = data_format,
                   activation = activation,
                   use_bias = use_bias,
                   kernel_initializer = kernel_initializer,
                   bias_initializer = bias_initializer,
                   kernel_regularizer = kernel_regularizer,
                   bias_regularizer = bias_regularizer,
                   activity_regularizer = activity_regularizer,
                   kernel_constraint = kernel_constraint,
                   bias_constraint = bias_constraint,
                   input_shape = input_shape)

  }

  return(res)
}
