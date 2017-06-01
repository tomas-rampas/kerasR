#' Plot model architecture to a file
#'
#' This function requires that you have installed graphviz and
#' pydot in Python.
#'
#' @param model             model object to plot
#' @param to_file           output location of the plot)
#' @param show_shapes       controls whether output shapes are shown in the
#'                          graph
#' @param show_layer_names  controls whether layer names are shown in the
#'                          graph
#'
#' @template boilerplate
#' @export
plot_model <- function(model, to_file = "model.png", show_shapes = FALSE,
                       show_layer_names = TRUE) {
  modules$keras.utils$plot_model(model = model, to_file = to_file,
                                 show_shapes = show_shapes,
                                 show_layer_names = show_layer_names)
}

#' Converts a class vector (integers) to binary class matrix.
#'
#' This function takes a vector or 1 column matrix of class labels
#' and converts it into a matrix with p columns, one for each category.
#' This is the format most commonly used in the fitting and predicting
#' of neural networks.
#'
#' @param y                class vector to be converted into a matrix
#'                           (integers from 0 to num_classes).
#' @param num_classes      total number of classes. Set to `NULL` to
#'                           autodetect from the input.
#'
#' @template boilerplate
#' @export
to_categorical <- function(y, num_classes = NULL) {
  if (!is.null(num_classes))
    num_classes <- int32(num_classes)
  modules$keras.utils$to_categorical(y = int32(y), num_classes = num_classes)
}

#' Normalize a Numpy array.
#'
#' It is generally very important to normalize the data matrix before
#' fitting a neural network model in keras.
#'
#' @param x     Numpy array to normalize
#' @param axis  axis along which to normalize. (starts at 0). -1
#' @param order Normalization order (e.g. 2 for L2 norm).
#'
#' @template boilerplate
#' @export
normalize <- function(x, axis = -1, order = 2) {
  modules$keras.utils$normalize(x = x, axis = int32(axis),
                                order = int32(order))
}


