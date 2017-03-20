#' Load pre-trained models
#'
#' These models can be used for prediction, feature extraction, and fine-tuning.
#' Weights are downloaded automatically when instantiating a model.
#'
#' @param include_top     whether to include the fully-connected layer at the top of the
#'                          network.
#' @param weights         one of NULL (random initialization) or "imagenet"
#'                          (pre-training on ImageNet).
#' @param input_tensor    optional Keras tensor (i.e. output of layers.Input())
#'                          to use as image input for the model.
#' @param input_shape     optional shape tuple, only to be specified if include_top
#'                          is False
#' @param pooling         optional pooling mode for feature extraction when
#'                          include_top is False. None means that the output of the
#'                          model will be the 4D tensor output of the last convolutional
#'                          layer. avg means that global average pooling will be applied
#'                          to the output of the last convolutional layer, and thus the
#'                          output of the model will be a 2D tensor max means that global
#'                          max pooling will be applied.
#' @param classes         optional number of classes to classify images into, only to
#'                          be specified if include_top is True, and if no weights
#'                          argument is specified.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @references
#'
#'   Chollet, Francois. 2015. \href{https://keras.io/}{Keras: Deep Learning library for Theano and TensorFlow}.
#'
#' @name Applications
NULL

#' @rdname Applications
#' @export
Xception <- function(include_top = TRUE, weights = 'imagenet', input_tensor = NULL,
                         input_shape = NULL, pooling = NULL, classes = 1000) {

  # input_shape is a proper argument here, but we still need to
  # convert it into the correct format if not NULL
  if (!is.null(input_shape)) {
    input_shape <- sapply(input_shape, list)
    input_shape <- modules$builtin$tuple(int32(input_shape))
  }

  res <- modules$keras.applications$Xception(include_top = include_top,
                                             weights = weights,
                                             input_tensor = input_tensor,
                                             input_shape = input_shape,
                                             pooling = pooling,
                                             classes = int32(classes))

  return(res)
}

#' @rdname Applications
#' @export
VGG16 <- function(include_top = TRUE, weights = 'imagenet', input_tensor = NULL,
                         input_shape = NULL, pooling = NULL, classes = 1000) {

  # input_shape is a proper argument here, but we still need to
  # convert it into the correct format if not NULL
  if (!is.null(input_shape)) {
    input_shape <- sapply(input_shape, list)
    input_shape <- modules$builtin$tuple(int32(input_shape))
  }

  res <- modules$keras.applications$VGG16(include_top = include_top,
                                             weights = weights,
                                             input_tensor = input_tensor,
                                             input_shape = input_shape,
                                             pooling = pooling,
                                             classes = int32(classes))

  return(res)
}

#' @rdname Applications
#' @export
VGG19 <- function(include_top = TRUE, weights = 'imagenet', input_tensor = NULL,
                         input_shape = NULL, pooling = NULL, classes = 1000) {

  # input_shape is a proper argument here, but we still need to
  # convert it into the correct format if not NULL
  if (!is.null(input_shape)) {
    input_shape <- sapply(input_shape, list)
    input_shape <- modules$builtin$tuple(int32(input_shape))
  }

  res <- modules$keras.applications$VGG19(include_top = include_top,
                                             weights = weights,
                                             input_tensor = input_tensor,
                                             input_shape = input_shape,
                                             pooling = pooling,
                                             classes = int32(classes))

  return(res)
}

#' @rdname Applications
#' @export
ResNet50 <- function(include_top = TRUE, weights = 'imagenet', input_tensor = NULL,
                         input_shape = NULL, pooling = NULL, classes = 1000) {

  # input_shape is a proper argument here, but we still need to
  # convert it into the correct format if not NULL
  if (!is.null(input_shape)) {
    input_shape <- sapply(input_shape, list)
    input_shape <- modules$builtin$tuple(int32(input_shape))
  }

  res <- modules$keras.applications$ResNet50(include_top = include_top,
                                             weights = weights,
                                             input_tensor = input_tensor,
                                             input_shape = input_shape,
                                             pooling = pooling,
                                             classes = int32(classes))

  return(res)
}

#' @rdname Applications
#' @export
InceptionV3 <- function(include_top = TRUE, weights = 'imagenet', input_tensor = NULL,
                         input_shape = NULL, pooling = NULL, classes = 1000) {

  # input_shape is a proper argument here, but we still need to
  # convert it into the correct format if not NULL
  if (!is.null(input_shape)) {
    input_shape <- sapply(input_shape, list)
    input_shape <- modules$builtin$tuple(int32(input_shape))
  }

  res <- modules$keras.applications$InceptionV3(include_top = include_top,
                                                weights = weights,
                                                input_tensor = input_tensor,
                                                input_shape = input_shape,
                                                pooling = pooling,
                                                classes = int32(classes))

  return(res)
}

#' Preprocess input for pre-defined imagenet networks
#'
#' These assume you have already converted images into a three channel,
#' 224 by 224 matrix with \code{load_img} and \code{img_to_array}. The
#' processing differs based on the model so set the appropriate model
#' that you are using.
#'
#' @param img     the input image, as an array
#' @param model   the model you wish to preprocess to
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @references
#'
#'   Chollet, Francois. 2015. \href{https://keras.io/}{Keras: Deep Learning library for Theano and TensorFlow}.
#'
#' @export
preprocess_input <- function(img, model = c("Xception", "VGG16", "VGG19", "ResNet50", "InceptionV3")) {
  model <- match.arg(model)

  if (model == "Xception") {
    res <- modules$keras.applications$xception$preprocess_input(img)
  } else if (model == "VGG16") {
    res <- modules$keras.applications$vgg16$preprocess_input(img)
  } else if (model == "VGG19") {
    res <- modules$keras.applications$vgg19$preprocess_input(img)
  } else if (model == "ResNet50") {
    res <- modules$keras.applications$resnet50$preprocess_input(img)
  } else if (model == "InceptionV3") {
    res <- modules$keras.applications$inception_v3$preprocess_input(img)
  }

  return(res)
}

#' Decode predictions from pre-defined imagenet networks
#'
#' These map the class integers to the actual class names
#' in the pre-defined models.
#'
#' @param pred    the output of predictions from the specified model
#' @param model   the model you wish to preprocess to
#' @param top     integer, how many top-guesses to return.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @references
#'
#'   Chollet, Francois. 2015. \href{https://keras.io/}{Keras: Deep Learning library for Theano and TensorFlow}.
#'
#' @export
decode_predictions <- function(pred, model = c("Xception", "VGG16", "VGG19", "ResNet50", "InceptionV3"),
                                top = 5) {
  model <- match.arg(model)

  if (model == "Xception") {
    res <- modules$keras.applications$xception$decode_predictions(pred, top = int32(top))
  } else if (model == "VGG16") {
    res <- modules$keras.applications$vgg16$decode_predictions(pred, top = int32(top))
  } else if (model == "VGG19") {
    res <- modules$keras.applications$vgg19$decode_predictions(pred, top = int32(top))
  } else if (model == "ResNet50") {
    res <- modules$keras.applications$resnet50$decode_predictions(pred, top = int32(top))
  } else if (model == "InceptionV3") {
    res <- modules$keras.applications$inception_v3$decode_predictions(pred, top = int32(top))
  }

  return(res)
}
