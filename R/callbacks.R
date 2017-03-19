#' Save the model after every epoch.
#'
#' @param filepath           string, path to save the model file.
#' @param monitor            quantity to monitor.
#' @param verbose            verbosity mode, 0 or 1.
#' @param save_best_only     if save_best_only=True, the latest
#'                             best model according to the quantity monitored will
#'                             not be overwritten.
#' @param mode               one of {auto, min, max}. If save_best_only is
#'                             True, the decision to overwrite the current
#'                             save file is made based on either the
#'                             maximization or the minimization of the monitored
#'                             quantity. For val_acc, this should be max, for
#'                             val_loss this should be min, etc. In auto mode,
#'                             the direction is automatically inferred from the
#'                             name of the monitored quantity.
#' @param save_weights_only  if True, then only the model's weights will
#'                             be saved (model.save_weights(filepath)), else
#'                             the full model is saved (model.save(filepath)).
#' @param period             Interval (number of epochs) between checkpoints.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @references
#'
#'   Chollet, Francois. 2015. \href{https://keras.io/}{Keras: Deep Learning library for Theano and TensorFlow}.
#'
#' @export
ModelCheckpoint <- function(filepath, monitor = 'val_loss', verbose = 0,
      save_best_only = FALSE, save_weights_only = FALSE, mode = 'auto',
      period = 1) {
  modules$keras.callbacks$ModelCheckpoint(filepath = filepath, monitor = monitor,
      verbose = int32(verbose), save_best_only = save_best_only,
      save_weights_only = save_weights_only, mode = mode,
      period = int32(period))
}

#' Stop training when a monitored quantity has stopped improving.
#'
#' @param monitor    quantity to be monitored.
#' @param min_delta  minimum change in the monitored quantity to qualify as an
#'                     improvement, i.e. an absolute change of less than
#'                     min_delta, will count as no improvement.
#' @param patience   number of epochs with no improvement after which
#'                     training will be stopped.
#' @param verbose    verbosity mode.
#' @param mode       one of {auto, min, max}. In min mode, training will
#'                     stop when the quantity monitored has stopped
#'                     decreasing; in max mode it will stop when the
#'                     quantity monitored has stopped increasing; in
#'                     auto mode, the direction is automatically inferred
#'                     from the name of the monitored quantity.
#'
#' @examples
#' if (run_examples()) {
#' X_train <- matrix(rnorm(100 * 10), nrow = 100)
#' Y_train <- to_categorical(matrix(sample(0:2, 100, TRUE), ncol = 1), 3)
#'
#' mod <- Sequential()
#' mod$add(Dense(units = 50, input_shape = dim(X_train)[2]))
#' mod$add(Activation("relu"))
#' mod$add(Dense(units = 3))
#' mod$add(Activation("softmax"))
#' keras_compile(mod,  loss = 'categorical_crossentropy', optimizer = RMSprop())
#'
#' callbacks <- list(CSVLogger(tempfile()),
#'                   EarlyStopping(),
#'                   ModelCheckpoint(tempfile()),
#'                   ReduceLROnPlateau(),
#'                   TensorBoard(tempfile()))
#'
#' keras_fit(mod, X_train, Y_train, batch_size = 32, epochs = 5,
#'           verbose = 0, callbacks = callbacks, validation_split = 0.2)
#' }
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @references
#'
#'   Chollet, Francois. 2015. \href{https://keras.io/}{Keras: Deep Learning library for Theano and TensorFlow}.
#'
#' @export
EarlyStopping <- function(monitor = 'val_loss', min_delta = 0, patience = 0,
                          verbose = 0, mode = 'auto') {
  modules$keras.callbacks$EarlyStopping(monitor = monitor, min_delta = min_delta,
                              patience = int32(patience), verbose = int32(verbose), mode = mode)
}

#' Tensorboard basic visualizations.
#'
#' This callback writes a log for TensorBoard, which allows you to visualize
#' dynamic graphs of your training and test metrics, as well as activation
#' histograms for the different layers in your model.
#'
#' @param log_dir         the path of the directory where to save the log files
#'                          to be parsed by Tensorboard.
#' @param histogram_freq  frequency (in epochs) at which to compute activation
#'                          histograms for the layers of the model. If set to
#'                          0, histograms won't be computed.
#' @param write_graph     whether to visualize the graph in Tensorboard. The
#'                          log file can become quite large when write_graph
#'                          is set to True.
#' @param write_images    whether to write model weights to visualize as
#'                          image in Tensorboard.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @references
#'
#'   Chollet, Francois. 2015. \href{https://keras.io/}{Keras: Deep Learning library for Theano and TensorFlow}.
#'
#' @export
TensorBoard <- function(log_dir = './logs', histogram_freq = 0, write_graph = TRUE,
                        write_images = FALSE) {
  modules$keras.callbacks$TensorBoard(log_dir = log_dir, histogram_freq = int32(histogram_freq),
                              write_graph = write_graph, write_images = write_images)
}

#' Reduce learning rate when a metric has stopped improving.
#'
#' Models often benefit from reducing the learning rate by a factor of 2-10 once
#' learning stagnates. This callback monitors a quantity and if no improvement
#' is seen for a 'patience' number of epochs, the learning rate is reduced.
#'
#' @param monitor   quantity to be monitored.
#' @param factor    factor by which the learning rate will be reduced.
#'                    new_lr = lr * factor
#' @param patience  number of epochs with no improvement after which
#'                    learning rate will be reduced.
#' @param verbose   int. 0: quiet, 1: update messages.
#' @param mode      one of {auto, min, max}. In min mode, lr will be
#'                    reduced when the quantity monitored has stopped
#'                    decreasing; in max mode it will be reduced when
#'                    the quantity monitored has stopped increasing;
#'                    in auto mode, the direction is automatically
#'                    inferred from the name of the monitored quantity.
#' @param epsilon   threshold for measuring the new optimum, to only
#'                    focus on significant changes.
#' @param cooldown  number of epochs to wait before resuming normal
#'                    operation after lr has been reduced.
#' @param min_lr    lower bound on the learning rate.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @references
#'
#'   Chollet, Francois. 2015. \href{https://keras.io/}{Keras: Deep Learning library for Theano and TensorFlow}.
#'
#' @export
ReduceLROnPlateau <- function(monitor = 'val_loss', factor = 0.1, patience = 10,
                              verbose = 0, mode = 'auto', epsilon = 0.0001,
                              cooldown = 0, min_lr = 0) {
  modules$keras.callbacks$ReduceLROnPlateau(monitor = monitor, factor = factor,
                              patience = patience, verbose = int32(verbose), mode = mode,
                              epsilon = epsilon, cooldown = int32(cooldown),
                              min_lr = min_lr)
}

#' Callback that streams epoch results to a csv file.
#'
#' Supports all values that can be represented as a string, including 1D
#' iterables such as np.ndarray.
#'
#' @param filename    filename of the csv file, e.g. 'run/log.csv'.
#' @param separator   string used to separate elements in the csv file.
#' @param append      True: append if file exists (useful for continuing training).
#'                      False: overwrite existing file,
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @references
#'
#'   Chollet, Francois. 2015. \href{https://keras.io/}{Keras: Deep Learning library for Theano and TensorFlow}.
#'
#' @export
CSVLogger <- function(filename, separator = ',', append = FALSE) {
    modules$keras.callbacks$CSVLogger(filename = filename, separator = separator,
                              append = append)
}

