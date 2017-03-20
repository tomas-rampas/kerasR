#' Initialize sequential model
#'
#' Use this function to construct an empty model to which
#' layers will be added, or pass a list of layers directly
#' to the function. The first layer passed to a Sequential
#' model should have a defined input shape.
#'
#' @param layers   list of keras model layers
#'
#' @examples
#' require('kerasR')
#' if (run_examples()) {
#' X_train <- matrix(rnorm(100 * 10), nrow = 100)
#' Y_train <- to_categorical(matrix(sample(0:2, 100, TRUE), ncol = 1), 3)
#'
#' mod <- Sequential()
#' mod$add(Dense(units = 50, input_shape = dim(X_train)[2]))
#' mod$add(Dropout(rate = 0.5))
#' mod$add(Activation("relu"))
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
#' @export
Sequential <- function(layers = NULL) {
  modules$keras.models$Sequential(layers = layers)
}

#' Compile a keras model
#'
#' Models must be compiled before being fit or used for prediction.
#' This function changes to input model object itself, and does not produce
#' a return value.
#'
#' @param model               a keras model object created with \code{Sequential}
#' @param optimizer           name of optimizer) or optimizer object. See \link{Optimizers}.
#' @param loss                name of a loss function. See Details for possible choices.
#' @param metrics             vector of metric names to be evaluated by the model during
#'                              training and testing. See Details for possible options.
#' @param sample_weight_mode  if you need to do timestep-wise sample weighting (2D weights),
#'                            set this to "temporal". "None" defaults to sample-wise weights (1D).
#'
#' @details
#'   Possible losses are "mean_squared_error", "mean_absolute_error",
#'  "mean_absolute_percentage_error", "mean_squared_logarithmic_error",
#'  "squared_hinge", "hinge", "categorical_crossentropy",
#'  "sparse_categorical_crossentropy", "binary_crossentropy",
#'  "kullback_leibler_divergence", "poisson", "cosine_proximity".
#'
#'  Possible metrics are "binary_accuracy", "categorical_accuracy",
#'  "sparse_categorical_accuracy", and "top_k_categorical_accuracy",
#'
#' @examples
#' require('kerasR')
#' if (run_examples()) {
#' X_train <- matrix(rnorm(100 * 10), nrow = 100)
#' Y_train <- to_categorical(matrix(sample(0:2, 100, TRUE), ncol = 1), 3)
#'
#' mod <- Sequential()
#' mod$add(Dense(units = 50, input_shape = dim(X_train)[2]))
#' mod$add(Dropout(rate = 0.5))
#' mod$add(Activation("relu"))
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
#' @export
keras_compile <- function(model, optimizer, loss, metrics = NULL,
                          sample_weight_mode = NULL) {

  if (length(metrics) == 1 && !is.list(metrics))
    metrics <- list(metrics)

  model$compile(optimizer = optimizer, loss = loss, metrics = metrics,
                sample_weight_mode = sample_weight_mode)

  invisible(0)
}

#' Fit a keras model
#'
#' Learn the weight and bias values for am model given training data.
#' Model must be compiled first. The model is modified in place.
#'
#' @param model             a keras model object created with \code{Sequential}
#' @param x                 input data
#' @param y                 labels
#' @param batch_size        integer. Number of samples per gradient update.
#' @param epochs            integer, the number of epochs to train the model.
#' @param verbose           0 for no logging to stdout, 1 for progress bar logging,
#'                            2 for one log line per epoch.
#' @param callbacks         list of keras.callbacks.Callback instances. List of
#'                            callbacks to apply during training.
#' @param validation_split  float (0. < x < 1). Fraction of the data to use as
#'                            held-out validation data.
#' @param validation_data   list (x_val, y_val) or list (x_val, y_val,
#'                            val_sample_weights) to be used as held-out validation
#'                            data. Will override validation_split.
#' @param shuffle           boolean or string (for 'batch'). Whether to shuffle the
#'                            samples at each epoch. 'batch' is a special option for
#'                            dealing with the limitations of HDF5 data; it shuffles
#'                            in batch-sized chunks.
#' @param class_weight      dictionary mapping classes to a weight value, used for
#'                            scaling the loss function (during training only).
#' @param sample_weight     Numpy array of weights for the training samples
#' @param initial_epoch     epoch at which to start training
#'
#' @examples
#' require('kerasR')
#' if (run_examples()) {
#' X_train <- matrix(rnorm(100 * 10), nrow = 100)
#' Y_train <- to_categorical(matrix(sample(0:2, 100, TRUE), ncol = 1), 3)
#'
#' mod <- Sequential()
#' mod$add(Dense(units = 50, input_shape = dim(X_train)[2]))
#' mod$add(Dropout(rate = 0.5))
#' mod$add(Activation("relu"))
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
#' @export
keras_fit <- function(model, x, y, batch_size = 32, epochs = 10, verbose = 1,
                      callbacks = NULL, validation_split = 0.0, validation_data = NULL,
                      shuffle = TRUE, class_weight = NULL, sample_weight = NULL,
                      initial_epoch = 0) {

  if (length(callbacks) == 1 && !is.list(callbacks))
    callbacks <- list(callbacks)

  if (is.null(dim(y)))
    y <- matrix(y, ncol = 1)

  model$fit(x = x, y = y, batch_size = int32(batch_size),
            epochs = int32(epochs), verbose = int32(verbose),
            callbacks = callbacks, validation_split = validation_split,
            validation_data = validation_data, shuffle = shuffle,
            class_weight = class_weight, sample_weight = sample_weight,
            initial_epoch = int32(initial_epoch))

  invisible(0)
}

#' Predict values from a keras model
#'
#' Once compiled and trained, this function returns the predictions
#' from a keras model. The function \code{keras_predict} returns raw
#' predictions, \code{keras_predict_classes} gives class predictions, and
#' \code{keras_predict_proba} gives class probabilities.
#'
#' @param model             a keras model object created with \code{Sequential}
#' @param x                 input data
#' @param batch_size        integer. Number of samples per gradient update.
#' @param verbose           0 for no logging to stdout, 1 for progress bar logging,
#'                            2 for one log line per epoch.
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
#' mod$add(Dense(units = 3))
#' mod$add(ActivityRegularization(l1 = 1))
#' mod$add(Activation("softmax"))
#' keras_compile(mod,  loss = 'categorical_crossentropy', optimizer = RMSprop())
#'
#' keras_fit(mod, X_train, Y_train, batch_size = 32, epochs = 5,
#'           verbose = 0, validation_split = 0.2)
#' dim(keras_predict(mod, X_train))
#' mean(keras_predict(mod, X_train) == (apply(Y_train, 1, which.max) - 1))
#' }
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @references
#'
#'   Chollet, Francois. 2015. \href{https://keras.io/}{Keras: Deep Learning library for Theano and TensorFlow}.
#'
#' @name Predict
NULL

#' @rdname Predict
#' @export
keras_predict <- function(model, x, batch_size = 32, verbose = 1) {

  res <- model$predict(x = x, batch_size = int32(batch_size),
                       verbose = int32(verbose))

  return(res)
}

#' @rdname Predict
#' @export
keras_predict_classes <- function(model, x, batch_size = 32, verbose = 1) {

  res <- model$predict_classes(x = x, batch_size = int32(batch_size),
                       verbose = int32(verbose))

  return(res)
}

#' @rdname Predict
#' @export
keras_predict_proba <- function(model, x, batch_size = 32, verbose = 1) {

  res <- model$predict_proba(x = x, batch_size = int32(batch_size),
                       verbose = int32(verbose))

  return(res)
}

#' Load and save keras models
#'
#' These functions provide methods for loading and saving a keras
#' model. As python objects, R functions such as \code{readRDS} will
#' not work correctly. We have \code{keras_save_model} and \code{keras_load_model}
#' to save and load the entire object, \code{keras_save_weights} and \code{keras_load_weights}
#' to store only the weights, and \code{keras_model_to_json} and \code{keras_model_from_json}
#' to store only the model architecture. It is also possible to use
#' the get_weights and set_weights methods to manually extract and
#' set weights from R objects (returned weights can be saved as an R data file).
#'
#' @param model  keras model object to save; or, for \code{load_weights} the
#'                 the model in which to load the weights
#' @param path   local path to save or load the data from
#'
#' @examples
#' require('kerasR')
#' if (run_examples()) {
#' mod <- Sequential()
#' mod$add(Dense(units = 50, input_shape = 10))
#' mod$add(Dropout(rate = 0.5))
#' mod$add(Activation("relu"))
#' mod$add(Dense(units = 3))
#' mod$add(ActivityRegularization(l1 = 1))
#' mod$add(Activation("softmax"))
#' keras_compile(mod,  loss = 'categorical_crossentropy', optimizer = RMSprop())
#'
#' # save/load the entire model object
#' keras_save(mod, tf <- tempfile())
#' mod2 <- keras_load(tf)
#'
#' # save/load just the weights file
#' keras_save_weights(mod, tf <- tempfile())
#' keras_load_weights(mod, tf)
#'
#' # save/load just the architecture (as human readable json)
#' keras_model_to_json(mod, tf <- tempfile())
#' keras_model_from_json(mod, tf)
#' }
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @references
#'
#'   Chollet, Francois. 2015. \href{https://keras.io/}{Keras: Deep Learning library for Theano and TensorFlow}.
#'
#' @name LoadSave
NULL


#' @rdname LoadSave
#' @export
keras_save <- function(model, path = "model.h5") {
  model$save(path)
  invisible(0)
}

#' @rdname LoadSave
#' @export
keras_load <- function(path = "model.h5") {
  modules$keras.models$load_model(path)
}

#' @rdname LoadSave
#' @export
keras_save_weights <- function(model, path = "model.h5") {
  model$save_weights(path)
  invisible(0)
}

#' @rdname LoadSave
#' @export
keras_load_weights <- function(model, path = "model.h5") {
  model$load_weights(path)
  invisible(0)
}

#' @rdname LoadSave
#' @export
keras_model_to_json <- function(model, path = "model.json") {
  json <- model$to_json()
  writeLines(json, con = path)
  invisible(0)
}

#' @rdname LoadSave
#' @export
keras_model_from_json <- function(path = "model.json") {
  modules$keras.models$load_model(path)
}
