#' Convolution layers
#'
#' @param filters            Integer, the dimensionality of the output space
#'                           (i.e. the number output of filters in the
#'                           convolution).
#' @param kernel_size        A pair of integers specifying the dimensions of
#'                           the 2D convolution window.
#' @param strides            A pair of integers specifying the stride length
#'                           of the convolution.
#' @param padding            One of "valid", "causal" or "same"
#'                           (case-insensitive).
#' @param data_format        A string, one of channels_last (default) or
#'                           channels_first.
#'                           The ordering of the dimensions in the inputs.
#' @param depth_multiplier   The number of depthwise convolution output
#'                           channels for each input channel. The total
#'                           number of depthwise convolution output channels
#'                            will be equal to filterss_in * depth_multiplier.
#' @param dilation_rate      A pair of integers specifying the dilation rate
#'                           to use for dilated convolution
#' @param activation         Activation function to use
#' @param use_bias           Boolean, whether the layer uses a bias vector.
#' @param kernel_initializer Initializer for the kernel weights matrix
#' @param bias_initializer   Initializer for the bias vector
#' @param kernel_regularizer Regularizer function applied to the kernel
#'                           weights matrix
#' @param bias_regularizer   Regularizer function applied to the bias vector
#' @param activity_regularizer  Regularizer function applied to the output
#'                           of the layer (its "activation").
#' @param kernel_constraint  Constraint function applied to the kernel
#'                           matrix
#' @param bias_constraint    Constraint function applied to the bias vector
#' @param input_shape        only need when first layer of a model; sets the
#'                           input shape of the data
#'
#' @example inst/examples/convolutional.R
#' @template boilerplate
#' @name Conv
NULL

#' @rdname Conv
#' @export
#' @family layers
Conv1D <- function(filters,
                   kernel_size,
                   strides = 1,
                   padding = 'valid',
                   dilation_rate = 1,
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
  keras_check()

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.convolutional$Conv1D(
                   filters = int32(filters),
                   kernel_size = int32(kernel_size),
                   strides = int32(strides),
                   padding = padding,
                   dilation_rate = int32(dilation_rate),
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

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.convolutional$Conv1D(
                   filters = int32(filters),
                   kernel_size = int32(kernel_size),
                   strides = int32(strides),
                   padding = padding,
                   dilation_rate = int32(dilation_rate),
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

#' @rdname Conv
#' @export
Conv2D <- function(filters,
                   kernel_size,
                   strides = c(1, 1),
                   padding = 'valid',
                   data_format = NULL,
                   dilation_rate = c(1, 1),
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
  keras_check()

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.convolutional$Conv2D(
                   filters = int32(filters),
                   kernel_size = int32(kernel_size),
                   strides = int32(strides),
                   padding = padding,
                   data_format = data_format,
                   dilation_rate = int32(dilation_rate),
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

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.convolutional$Conv2D(
                   filters = int32(filters),
                   kernel_size = int32(kernel_size),
                   strides = int32(strides),
                   padding = padding,
                   data_format = data_format,
                   dilation_rate = int32(dilation_rate),
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

#' @rdname Conv
#' @export
SeparableConv2D <- function(filters,
                   kernel_size,
                   strides = c(1, 1),
                   padding = 'valid',
                   data_format = NULL,
                   depth_multiplier = 1,
                   dilation_rate = c(1, 1),
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

  keras_check()

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.convolutional$SeparableConv2D(
                   filters = int32(filters),
                   kernel_size = int32(kernel_size),
                   strides = int32(strides),
                   padding = padding,
                   data_format = data_format,
                   depth_multiplier = int32(depth_multiplier),
                   dilation_rate = int32(dilation_rate),
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

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.convolutional$SeparableConv2D(
                   filters = int32(filters),
                   kernel_size = int32(kernel_size),
                   strides = int32(strides),
                   padding = padding,
                   data_format = data_format,
                   depth_multiplier = int32(depth_multiplier),
                   dilation_rate = int32(dilation_rate),
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

#' @rdname Conv
#' @export
Conv2DTranspose <- function(filters,
                   kernel_size,
                   strides = c(1, 1),
                   padding = 'valid',
                   data_format = NULL,
                   dilation_rate = c(1, 1),
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

  keras_check()

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.convolutional$Conv2DTranspose(
                   filters = int32(filters),
                   kernel_size = int32(kernel_size),
                   strides = int32(strides),
                   padding = padding,
                   data_format = data_format,
                   dilation_rate = int32(dilation_rate),
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

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.convolutional$Conv2DTranspose(
                   filters = int32(filters),
                   kernel_size = int32(kernel_size),
                   strides = int32(strides),
                   padding = padding,
                   data_format = data_format,
                   dilation_rate = int32(dilation_rate),
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

#' @rdname Conv
#' @export
Conv3D <- function(filters,
                   kernel_size,
                   strides = c(1, 1, 1),
                   padding = 'valid',
                   data_format = NULL,
                   dilation_rate = c(1, 1, 1),
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

  keras_check()

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.convolutional$Conv3D(
                   filters = int32(filters),
                   kernel_size = int32(kernel_size),
                   strides = int32(strides),
                   padding = padding,
                   data_format = data_format,
                   dilation_rate = int32(dilation_rate),
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

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.convolutional$Conv3D(
                   filters = int32(filters),
                   kernel_size = int32(kernel_size),
                   strides = int32(strides),
                   padding = padding,
                   data_format = data_format,
                   dilation_rate = int32(dilation_rate),
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


#' Cropping layers for 1D input (e.g. temporal sequence).
#'
#' It crops along the time dimension (axis 1).
#'
#' @param cropping   integer or pair of integers. How many units
#'                   should be trimmed off at the beginning and end
#'                   of the cropping dimension (axis 1). If a single value
#'                   is provided, the same value will be used for both.
#' @param data_format  A string, one of channels_last (default) or
#'                     channels_first.
#' @param input_shape  only need when first layer of a model; sets the
#'                     input shape of the data
#'
#' @template boilerplate
#' @name Cropping
NULL

#' @rdname Cropping
#' @export
Cropping1D <- function(cropping = c(1,1), input_shape = NULL) {

  keras_check()

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.convolutional$Cropping1D(
                          cropping = int32(cropping))
  } else {

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.convolutional$Cropping1D(
                              cropping = int32(cropping),
                              input_shape = input_shape)

  }

  return(res)
}

#' @rdname Cropping
#' @export
Cropping2D <- function(cropping = 0, data_format = NULL,
                       input_shape = NULL) {

  keras_check()

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.convolutional$Cropping2D(
                  cropping = int32(cropping),
                  data_format = data_format)
  } else {

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.convolutional$Cropping2D(
                                                cropping = int32(cropping),
                                                data_format = data_format,
                                                input_shape = input_shape)

  }

  return(res)
}

#' @rdname Cropping
#' @export
Cropping3D <- function(cropping = 0, data_format = NULL,
                       input_shape = NULL) {

  keras_check()

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.convolutional$Cropping3D(
                                          cropping = int32(cropping),
                                          data_format = data_format)
  } else {

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.convolutional$Cropping3D(
                                            cropping = int32(cropping),
                                            data_format = data_format,
                                            input_shape = input_shape)

  }

  return(res)
}

#' UpSampling layers.
#'
#' Repeats each temporal step size a given number of times.
#'
#' @param size                   integer. Upsampling factor.
#' @param data_format            A string, one of channels_last
#'                               (default) or channels_first.
#' @param input_shape            only need when first layer of a model;
#'                               sets the input shape of the data
#'
#' @template boilerplate
#' @name UpSampling
NULL

#' @rdname UpSampling
#' @export
UpSampling1D <- function(size = 2, input_shape = NULL) {

  keras_check()

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.convolutional$UpSampling1D(size = int32(size))
  } else {

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.convolutional$UpSampling1D(size = int32(size),
                                                input_shape = input_shape)

  }

  return(res)
}

#' @rdname UpSampling
#' @export
UpSampling2D <- function(size = c(2, 2), data_format = NULL,
                         input_shape = NULL) {

  keras_check()

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.convolutional$UpSampling2D(size = int32(size))
  } else {

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.convolutional$UpSampling2D(size = int32(size),
                                                input_shape = input_shape)

  }

  return(res)
}


#' @rdname UpSampling
#' @export
UpSampling3D <- function(size = c(2, 2, 2), data_format = NULL,
                          input_shape = NULL) {

  keras_check()

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.convolutional$UpSampling3D(size = int32(size))
  } else {

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.convolutional$UpSampling3D(size = int32(size),
                                                input_shape = input_shape)

  }

  return(res)
}

#' Zero-padding layers
#'
#' @param padding     if one integer, the same symmetric padding is applied
#'                    to width and height. If two, how many to add
#'                    for height and width.
#' @param data_format   A string, one of channels_last (default) or
#'                      channels_first.
#' @param input_shape   only need when first layer of a model; sets the
#'                      input shape of the data
#'
#' @template boilerplate
#' @name ZeroPadding
NULL

#' @rdname ZeroPadding
#' @export
ZeroPadding1D <- function(padding = 1, input_shape = NULL) {

  keras_check()

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.convolutional$ZeroPadding1D(
                              padding = int32(padding))
  } else {

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.convolutional$ZeroPadding1D(
                              padding = int32(padding),
                              input_shape = input_shape)

  }

  return(res)
}

#' @rdname ZeroPadding
#' @export
ZeroPadding2D <- function(padding = 1, data_format = NULL,
                          input_shape = NULL) {

  keras_check()

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.convolutional$ZeroPadding2D(
                                padding = int32(padding),
                                data_format = data_format)
  } else {

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.convolutional$ZeroPadding2D(
                                padding = int32(padding),
                                data_format = data_format,
                                input_shape = input_shape)

  }

  return(res)
}

#' @rdname ZeroPadding
#' @export
ZeroPadding3D <- function(padding = 1, data_format = NULL,
                          input_shape = NULL) {

  keras_check()

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.convolutional$ZeroPadding3D(
                            padding = int32(padding),
                            data_format = data_format)
  } else {

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.convolutional$ZeroPadding3D(
                                                padding = int32(padding),
                                                data_format = data_format,
                                                input_shape = input_shape)

  }

  return(res)
}

