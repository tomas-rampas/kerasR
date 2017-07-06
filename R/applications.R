#' Load pre-trained models
#'
#' These models can be used for prediction, feature extraction, and
#' fine-tuning. Weights are downloaded automatically when instantiating
#' a model.
#'
#' @param include_top     whether to include the fully-connected layer at
#'                        the top of the network.
#' @param weights         one of NULL (random initialization) or "imagenet"
#'                        (pre-training on ImageNet).
#' @param input_tensor    optional Keras tensor (i.e. output of layers.Input())
#'                        to use as image input for the model.
#' @param input_shape     optional shape tuple, only to be specified if
#'                        include_top is False
#' @param pooling         optional pooling mode for feature extraction when
#'                        include_top is False. None means that the output
#'                        of the model will be the 4D tensor output of the
#'                        last convolutional layer. avg means that global
#'                        average pooling will be applied
#'                        to the output of the last convolutional layer,
#'                        and thus the output of the model will be a 2D
#'                        tensor max means that global max pooling will be
#'                        applied.
#' @param classes         optional number of classes to classify images
#'                        into, only to be specified if include_top is True,
#'                        and if no weights argument is specified.
#'
#' @template boilerplate
#' @name Applications
NULL

#' @rdname Applications
#' @export
Xception <- function(include_top = TRUE, weights = 'imagenet',
                      input_tensor = NULL,
                      input_shape = NULL, pooling = NULL,
                      classes = 1000) {
  keras_check()

  # input_shape is a proper argument here, but we still need to
  # convert it into the correct format if not NULL
  if (!is.null(input_shape)) {
    input_shape <- as.list(input_shape)
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
VGG16 <- function(include_top = TRUE, weights = 'imagenet',
                    input_tensor = NULL,
                    input_shape = NULL, pooling = NULL, classes = 1000) {
  keras_check()

  # input_shape is a proper argument here, but we still need to
  # convert it into the correct format if not NULL
  if (!is.null(input_shape)) {
    input_shape <- as.list(input_shape)
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
VGG19 <- function(include_top = TRUE, weights = 'imagenet',
                    input_tensor = NULL,
                    input_shape = NULL, pooling = NULL, classes = 1000) {
  keras_check()

  # input_shape is a proper argument here, but we still need to
  # convert it into the correct format if not NULL
  if (!is.null(input_shape)) {
    input_shape <- as.list(input_shape)
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
ResNet50 <- function(include_top = TRUE, weights = 'imagenet',
                      input_tensor = NULL,
                      input_shape = NULL, pooling = NULL, classes = 1000) {
  keras_check()

  # input_shape is a proper argument here, but we still need to
  # convert it into the correct format if not NULL
  if (!is.null(input_shape)) {
    input_shape <- as.list(input_shape)
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
InceptionV3 <- function(include_top = TRUE, weights = 'imagenet',
                        input_tensor = NULL,
                        input_shape = NULL, pooling = NULL, classes = 1000) {
  keras_check()

  # input_shape is a proper argument here, but we still need to
  # convert it into the correct format if not NULL
  if (!is.null(input_shape)) {
    input_shape <- as.list(input_shape)
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
#' 224 by 224 matrix with [load_img] and [img_to_array]. The
#' processing differs based on the model so set the appropriate model
#' that you are using.
#'
#' @param img     the input image, as an array
#' @param model   the model you wish to preprocess to
#'
#' @template boilerplate
#' @export
preprocess_input <- function(img, model = c("Xception",
            "VGG16", "VGG19", "ResNet50", "InceptionV3")) {
  keras_check()

  model <- match.arg(model)

  k.apps <- modules$keras.applications
  res <- switch(
    model, 
    Xception    = k.apps$xception$preprocess_input(img),
    VGG16       = k.apps$vgg16$preprocess_input(img),
    VGG19       = k.apps$vgg19$preprocess_input(img),
    ResNet50    = k.apps$resnet50$preprocess_input(img),
    InceptionV3 = k.apps$inception_v3$preprocess_input(img)
  )

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
#' @template boilerplate
#' @export
decode_predictions <- function(pred, model = c("Xception", "VGG16",
                        "VGG19", "ResNet50", "InceptionV3"),
                                top = 5) {
  keras_check()

  model <- match.arg(model)

  k.apps <- modules$keras.applications
  res <- switch(
    model,
    Xception    = k.apps$xception$decode_predictions(pred, top = int32(top)),
    VGG16       = k.apps$vgg16$decode_predictions(pred, top = int32(top)),
    VGG19       = k.apps$vgg19$decode_predictions(pred, top = int32(top)),
    ResNet50    = k.apps$resnet50$decode_predictions(pred, top = int32(top)),
    InceptionV3 = k.apps$inception_v3$decode_predictions(pred, top = int32(top))
  )

  return(res)
}
