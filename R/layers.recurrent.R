#' Recurrent neural network layers
#'
#' @param units  Positive integer, dimensionality of the output space.
#' @param activation  Activation function to use
#' @param recurrent_activation  Activation function to use for the recurrent
#'                              step
#' @param use_bias Boolean, whether the layer uses a bias vector.
#' @param kernel_initializer  Initializer for the kernel weights matrix,
#'                          used for the linear transformation of the inputs.
#' @param recurrent_initializer Initializer for the recurrent_kernel
#'                             weights matrix, used for
#'                       the linear transformation of the recurrentstate.
#' @param bias_initializer Initializer for the bias vector
#' @param unit_forget_bias  Boolean. If True, add 1 to the bias of the
#'                        forget gate at initialization.
#' @param kernel_regularizer Regularizer function applied to the kernel
#'                           weights matrix
#' @param recurrent_regularizer Regularizer function applied to the
#'                      recurrent_kernel weights matrix
#' @param bias_regularizer Regularizer function applied to the bias vector
#' @param activity_regularizer Regularizer function applied to the output
#'                          of the layer (its "activation")
#' @param kernel_constraint Constraint function applied to the kernel
#'                          weights matrix
#' @param recurrent_constraint Constraint function applied to the
#'                  recurrent_kernel weights matrix
#' @param bias_constraint Constraint function applied to the bias vector
#' @param dropout Float between 0 and 1. Fraction of the units to drop for
#'             the linear transformation of the inputs.
#' @param recurrent_dropout Float between 0 and 1. Fraction of the units
#'         to drop for the linear transformation of the recurrent state.
#' @param return_sequences  Boolean. Whether to return the last output in
#'                    the output sequence, or the full sequence.
#' @param input_shape            only need when first layer of a model;
#'                                sets the input shape of the data
#'

#' @example inst/examples/recurrent.R
#' @template boilerplate
#' @name RNN
NULL

#' @rdname RNN
#' @export
#' @family layers
SimpleRNN <- function(units,
                 activation = 'tanh',
                 use_bias = TRUE,
                 kernel_initializer = 'glorot_uniform',
                 recurrent_initializer = 'orthogonal',
                 bias_initializer = 'zeros',
                 kernel_regularizer = NULL,
                 recurrent_regularizer = NULL,
                 bias_regularizer = NULL,
                 activity_regularizer = NULL,
                 kernel_constraint = NULL,
                 recurrent_constraint = NULL,
                 bias_constraint = NULL,
                 dropout = 0.0,
                 recurrent_dropout = 0.0,
                 input_shape = NULL) {

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.recurrent$SimpleRNN(units = int32(units),
                 activation = activation,
                 use_bias = use_bias,
                 kernel_initializer = kernel_initializer,
                 recurrent_initializer = recurrent_initializer,
                 bias_initializer = bias_initializer,
                 kernel_regularizer = kernel_regularizer,
                 recurrent_regularizer = recurrent_regularizer,
                 bias_regularizer = bias_regularizer,
                 activity_regularizer = activity_regularizer,
                 kernel_constraint = kernel_constraint,
                 recurrent_constraint = recurrent_constraint,
                 bias_constraint = bias_constraint,
                 dropout = dropout,
                 recurrent_dropout = dropout)
  } else {

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.recurrent$SimpleRNN(units = int32(units),
                 activation = activation,
                 use_bias = use_bias,
                 kernel_initializer = kernel_initializer,
                 recurrent_initializer = recurrent_initializer,
                 bias_initializer = bias_initializer,
                 kernel_regularizer = kernel_regularizer,
                 recurrent_regularizer = recurrent_regularizer,
                 bias_regularizer = bias_regularizer,
                 activity_regularizer = activity_regularizer,
                 kernel_constraint = kernel_constraint,
                 recurrent_constraint = recurrent_constraint,
                 bias_constraint = bias_constraint,
                 dropout = dropout,
                 recurrent_dropout = dropout,
                 input_shape = input_shape)

  }

  return(res)
}


#' @rdname RNN
#' @export
GRU <- function(units,
                 activation = 'tanh',
                 recurrent_activation = 'hard_sigmoid',
                 use_bias = TRUE,
                 kernel_initializer = 'glorot_uniform',
                 recurrent_initializer = 'orthogonal',
                 bias_initializer = 'zeros',
                 kernel_regularizer = NULL,
                 recurrent_regularizer = NULL,
                 bias_regularizer = NULL,
                 activity_regularizer = NULL,
                 kernel_constraint = NULL,
                 recurrent_constraint = NULL,
                 bias_constraint = NULL,
                 dropout = 0.0,
                 recurrent_dropout = 0.0,
                 input_shape = NULL) {

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.recurrent$GRU(units = int32(units),
                 activation = activation,
                 recurrent_activation = recurrent_activation,
                 use_bias = use_bias,
                 kernel_initializer = kernel_initializer,
                 recurrent_initializer = recurrent_initializer,
                 bias_initializer = bias_initializer,
                 kernel_regularizer = kernel_regularizer,
                 recurrent_regularizer = recurrent_regularizer,
                 bias_regularizer = bias_regularizer,
                 activity_regularizer = activity_regularizer,
                 kernel_constraint = kernel_constraint,
                 recurrent_constraint = recurrent_constraint,
                 bias_constraint = bias_constraint,
                 dropout = dropout,
                 recurrent_dropout = dropout)
  } else {

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.recurrent$GRU(units = int32(units),
                 activation = activation,
                 recurrent_activation = recurrent_activation,
                 use_bias = use_bias,
                 kernel_initializer = kernel_initializer,
                 recurrent_initializer = recurrent_initializer,
                 bias_initializer = bias_initializer,
                 kernel_regularizer = kernel_regularizer,
                 recurrent_regularizer = recurrent_regularizer,
                 bias_regularizer = bias_regularizer,
                 activity_regularizer = activity_regularizer,
                 kernel_constraint = kernel_constraint,
                 recurrent_constraint = recurrent_constraint,
                 bias_constraint = bias_constraint,
                 dropout = dropout,
                 recurrent_dropout = dropout,
                 input_shape = input_shape)

  }

  return(res)
}

#' @rdname RNN
#' @export
LSTM <- function(units,
                 activation = 'tanh',
                 recurrent_activation = 'hard_sigmoid',
                 use_bias = TRUE,
                 kernel_initializer = 'glorot_uniform',
                 recurrent_initializer = 'orthogonal',
                 bias_initializer = 'zeros',
                 unit_forget_bias = TRUE,
                 kernel_regularizer = NULL,
                 recurrent_regularizer = NULL,
                 bias_regularizer = NULL,
                 activity_regularizer = NULL,
                 kernel_constraint = NULL,
                 recurrent_constraint = NULL,
                 bias_constraint = NULL,
                 dropout = 0.0,
                 recurrent_dropout = 0.0,
                 return_sequences = FALSE,
                 input_shape = NULL) {

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.recurrent$LSTM(units = int32(units),
                 activation = activation,
                 recurrent_activation = recurrent_activation,
                 use_bias = use_bias,
                 kernel_initializer = kernel_initializer,
                 recurrent_initializer = recurrent_initializer,
                 bias_initializer = bias_initializer,
                 unit_forget_bias = unit_forget_bias,
                 kernel_regularizer = kernel_regularizer,
                 recurrent_regularizer = recurrent_regularizer,
                 bias_regularizer = bias_regularizer,
                 activity_regularizer = activity_regularizer,
                 kernel_constraint = kernel_constraint,
                 recurrent_constraint = recurrent_constraint,
                 bias_constraint = bias_constraint,
                 dropout = dropout,
                 recurrent_dropout = dropout,
                 return_sequences = return_sequences)
  } else {

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.recurrent$LSTM(units = int32(units),
                 activation = activation,
                 recurrent_activation = recurrent_activation,
                 use_bias = use_bias,
                 kernel_initializer = kernel_initializer,
                 recurrent_initializer = recurrent_initializer,
                 bias_initializer = bias_initializer,
                 unit_forget_bias = unit_forget_bias,
                 kernel_regularizer = kernel_regularizer,
                 recurrent_regularizer = recurrent_regularizer,
                 bias_regularizer = bias_regularizer,
                 activity_regularizer = activity_regularizer,
                 kernel_constraint = kernel_constraint,
                 recurrent_constraint = recurrent_constraint,
                 bias_constraint = bias_constraint,
                 dropout = dropout,
                 recurrent_dropout = dropout,
                 return_sequences = return_sequences,
                 input_shape = input_shape)

  }

  return(res)
}



