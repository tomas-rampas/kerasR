#' Max pooling operations
#'
#' @param pool_size              Integer or triplet of integers; size(s)
#'                               of the max pooling windows.
#' @param strides                Integer, triplet of integers, or None.
#'                               Factor(s) by which to downscale.
#'                               E.g. 2 will halve the input. If NULL, it
#'                               will default to pool_size.
#' @param padding                One of "valid" or "same" (case-insensitive).
#' @param data_format            A string, one of channels_last (default) or
#'                               channels_first
#' @param input_shape            only need when first layer of a model;
#'                               sets the input shape of the data
#'
#' @example inst/examples/pooling.R
#' @template boilerplate
#' @name MaxPooling
#' @family layers
NULL


#' @rdname MaxPooling
#' @export
MaxPooling1D <- function(pool_size = 2, strides = NULL, padding = 'valid',
                         input_shape = NULL) {
  keras_check()

  if (!is.null(strides))
    strides <- int32(strides)

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.pooling$MaxPooling1D(
                                             pool_size = int32(pool_size),
                                               strides = strides,
                                               padding = padding)
  } else {

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.pooling$MaxPooling1D(
                                             pool_size = int32(pool_size),
                                               strides = strides,
                                               padding = padding,
                                               input_shape = input_shape)

  }

  return(res)
}

#' @rdname MaxPooling
#' @export
MaxPooling2D <- function(pool_size = c(2, 2), strides = NULL,
                         padding = 'valid', data_format = NULL,
                         input_shape = NULL) {
  keras_check()

  if (!is.null(strides))
    strides <- int32(strides)

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.pooling$MaxPooling2D(
                                             pool_size = int32(pool_size),
                                               strides = strides,
                                               padding = padding,
                                               data_format = data_format)
  } else {

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.pooling$MaxPooling2D(
                                             pool_size = int32(pool_size),
                                               strides = strides,
                                               padding = padding,
                                               data_format = data_format,
                                               input_shape = input_shape)

  }

  return(res)
}

#' @rdname MaxPooling
#' @export
MaxPooling3D <- function(pool_size = c(2, 2, 2), strides = NULL,
                          padding = 'valid',
                          data_format = NULL, input_shape = NULL) {
  keras_check()

  if (!is.null(strides))
    strides <- int32(strides)

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.pooling$MaxPooling3D(
                                             pool_size = int32(pool_size),
                                               strides = strides,
                                               padding = padding,
                                               data_format = data_format)
  } else {

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.pooling$MaxPooling3D(
                                             pool_size = int32(pool_size),
                                               strides = strides,
                                               padding = padding,
                                               data_format = data_format,
                                               input_shape = input_shape)

  }

  return(res)
}

#' Average pooling operation
#'
#' @param pool_size              Integer or pair of integers; size(s) of
#'                               the max pooling windows.
#' @param strides                Integer, pair of integers, or None.
#'                               Factor(s) by which to downscale. E.g. 2 will
#'                               halve the input. If NULL, it will
#'                               default to pool_size.
#' @param padding                One of "valid" or "same" (case-insensitive).
#' @param data_format            A string, one of channels_last (default) or
#'                               channels_first
#' @param input_shape            nD tensor with shape: `(batch_size, ...,
#'                               input_dim)`.
#'                               The most common situation would be a 2D
#'                               input with shape `(batch_size, input_dim)`.
#' @template boilerplate
#' @name AveragePooling
NULL


#' @rdname AveragePooling
#' @export
AveragePooling1D <- function(pool_size = 2, strides = NULL,
                             padding = 'valid', input_shape = NULL) {

  if (!is.null(strides))
    strides <- int32(strides)

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.pooling$AveragePooling1D(
                                             pool_size = int32(pool_size),
                                               strides = strides,
                                               padding = padding)
  } else {

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.pooling$AveragePooling1D(
                                             pool_size = int32(pool_size),
                                               strides = strides,
                                               padding = padding,
                                               input_shape = input_shape)

  }

  return(res)
}

#' @rdname AveragePooling
#' @export
AveragePooling2D <- function(pool_size = c(2, 2), strides = NULL,
                             padding = 'valid',
                             data_format = NULL,
                             input_shape = NULL) {

  if (!is.null(strides))
    strides <- int32(strides)

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.pooling$AveragePooling2D(
                                             pool_size = int32(pool_size),
                                               strides = strides,
                                               padding = padding,
                                               data_format = data_format)
  } else {

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.pooling$AveragePooling2D(
                                             pool_size = int32(pool_size),
                                               strides = strides,
                                               padding = padding,
                                               data_format = data_format,
                                               input_shape = input_shape)

  }

  return(res)
}

#' @rdname AveragePooling
#' @export
AveragePooling3D <- function(pool_size = c(2, 2, 2), strides = NULL,
                             padding = 'valid',
                             data_format = NULL,
                             input_shape = NULL) {

  if (!is.null(strides))
    strides <- int32(strides)

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.pooling$AveragePooling3D(
                                             pool_size = int32(pool_size),
                                               strides = strides,
                                               padding = padding,
                                               data_format = data_format)
  } else {

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.pooling$AveragePooling3D(
                                             pool_size = int32(pool_size),
                                               strides = strides,
                                               padding = padding,
                                               data_format = data_format,
                                               input_shape = input_shape)

  }

  return(res)
}

#' Global pooling operations
#'
#' @param data_format            A string, one of channels_last (default)
#'                               or channels_first
#' @param input_shape            nD tensor with shape: `(batch_size, ...,
#'                               input_dim)`.
#'                               The most common situation would be a 2D
#'                               input with shape
#'                               `(batch_size, input_dim)`.
#'
#' @template boilerplate
#' @name GlobalPooling
NULL

#' @rdname GlobalPooling
#' @export
GlobalMaxPooling1D <- function(input_shape = NULL) {

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.pooling$GlobalMaxPooling1D()
  } else {

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.pooling$GlobalMaxPooling1D(
                                      input_shape = input_shape)

  }

  return(res)
}


#' @rdname GlobalPooling
#' @export
GlobalAveragePooling1D <- function(input_shape = NULL) {

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.pooling$GlobalAveragePooling1D()
  } else {

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.pooling$GlobalAveragePooling1D(
                                      input_shape = input_shape)

  }

  return(res)
}

#' @rdname GlobalPooling
#' @export
GlobalMaxPooling2D <- function(data_format = NULL, input_shape = NULL) {

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.pooling$GlobalMaxPooling2D(
                                     data_format = data_format)
  } else {

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.pooling$GlobalMaxPooling2D(
                                               data_format = data_format,
                                               input_shape = input_shape)

  }

  return(res)
}


#' @rdname GlobalPooling
#' @export
GlobalAveragePooling2D <- function(data_format = NULL, input_shape = NULL) {

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.pooling$GlobalAveragePooling2D(
                                      data_format = data_format)
  } else {

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.pooling$GlobalAveragePooling2D(
                                      data_format = data_format,
                                               input_shape = input_shape)

  }

  return(res)
}
