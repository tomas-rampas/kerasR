% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models.R
\name{LoadSave}
\alias{LoadSave}
\alias{keras_save}
\alias{keras_load}
\alias{keras_save_weights}
\alias{keras_load_weights}
\alias{keras_model_to_json}
\alias{keras_model_from_json}
\title{Load and save keras models}
\usage{
keras_save(model, path = "model.h5")

keras_load(path = "model.h5")

keras_save_weights(model, path = "model.h5")

keras_load_weights(model, path = "model.h5")

keras_model_to_json(model, path = "model.json")

keras_model_from_json(path = "model.json")
}
\arguments{
\item{model}{keras model object to save; or, for \link{keras_load_weights}
the the model in which to load the weights}

\item{path}{local path to save or load the data from}
}
\description{
These functions provide methods for loading and saving a keras
model. As python objects, R functions such as \link{readRDS} will
not work correctly. We have \link{keras_save} and \link{keras_load}
to save and load the entire object, \link{keras_save_weights} and
\link{keras_load_weights} to store only the weights, and
\link{keras_model_to_json} and \link{keras_model_from_json}
to store only the model architecture. It is also possible to use
the get_weights and set_weights methods to manually extract and
set weights from R objects (returned weights can be saved as an
R data file).
}
\examples{
if (keras_available()) {
  # X_train <- matrix(rnorm(100 * 10), nrow = 100)
  # Y_train <- to_categorical(matrix(sample(0:2, 100, TRUE), ncol = 1), 3)
  mod <- Sequential()
  mod$add(Dense(units = 50, input_shape = 10))
  mod$add(Dropout(rate = 0.5))
  mod$add(Activation("relu"))
  mod$add(Dense(units = 3))
  mod$add(ActivityRegularization(l1 = 1))
  mod$add(Activation("softmax"))
  keras_compile(mod,  loss = 'categorical_crossentropy', optimizer = RMSprop())
  # keras_fit(mod, X_train, Y_train, batch_size = 32, epochs = 5,
  #           verbose = 0, validation_split = 0.2)
  
  # save/load the entire model object
  keras_save(mod, tf <- tempfile())
  mod2 <- keras_load(tf)
  
  # save/load just the weights file
  keras_save_weights(mod, tf <- tempfile())
  keras_load_weights(mod, tf)
  
  # save/load just the architecture (as human readable json)
  tf <- tempfile(fileext = ".json")
  keras_model_to_json(mod, tf)
  cat(readLines(tf))
  mod3 <- keras_model_from_json(tf)
}
}
\references{
Chollet, Francois. 2015. \href{https://keras.io/}{Keras: Deep Learning library for Theano and TensorFlow}.
}
\seealso{
Other model functions: \code{\link{Predict}},
  \code{\link{Sequential}}, \code{\link{keras_compile}},
  \code{\link{keras_fit}}
}
\author{
Taylor B. Arnold, \email{taylor.arnold@acm.org}
}
