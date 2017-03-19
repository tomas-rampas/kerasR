#' Embedding layer
#'
#' Turns positive integers (indexes) into dense vectors of fixed size.
#'
#' @param input_dim               int > 0. Size of the vocabulary, ie. 1 + maximum integer
#'                                index occurring in the input data.
#' @param output_dim              int >= 0. Dimension of the dense embedding.
#' @param embeddings_initializer  Initializer for the embeddings matrix
#' @param embeddings_regularizer  Regularizer function applied to the embeddings matrix
#' @param embeddings_constraint   Constraint function applied to the embeddings matrix
#' @param mask_zero               Whether or not the input value 0 is a special "padding"
#'                                value that should be masked out.
#' @param input_length            Length of input sequences, when it is constant.
#' @param input_shape             only need when first layer of a model; sets the input shape
#'                                  of the data

#' @examples
#' if (run_examples()) {
#' X_train <- matrix(sample(0:19, 100 * 100, TRUE), ncol = 100)
#' Y_train <- rnorm(100)
#'
#' mod <- Sequential()
#' mod$add(Embedding(input_dim = 20, output_dim = 10,
#'                   input_length = 100))
#' mod$add(Dropout(0.5))
#'
#' mod$add(GRU(16))
#' mod$add(Dense(1))
#' mod$add(Activation("sigmoid"))
#'
#' keras_compile(mod, loss = "mse", optimizer = RMSprop())
#' keras_fit(mod, X_train, Y_train, epochs = 3, verbose = 0)
#' }
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#' @references
#'
#'   Chollet, Francois. 2015. \href{https://keras.io/}{Keras: Deep Learning library for Theano and TensorFlow}.
#'
#' @export
Embedding <- function(input_dim,
                      output_dim,
                      embeddings_initializer = 'uniform',
                      embeddings_regularizer = NULL,
                      embeddings_constraint = NULL,
                      mask_zero = FALSE,
                      input_length = NULL,
                      input_shape = NULL) {

  if (!is.null(input_length))
    input_length <- int32(input_length)

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.embeddings$Embedding(input_dim = int32(input_dim),
                output_dim = int32(output_dim),
                embeddings_initializer = embeddings_initializer,
                embeddings_regularizer = embeddings_regularizer,
                embeddings_constraint = embeddings_constraint,
                mask_zero = mask_zero)
  } else {

    input_shape <- sapply(input_shape, list)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.embeddings$Embedding(input_dim = int32(input_dim),
                output_dim = int32(output_dim),
                embeddings_initializer = embeddings_initializer,
                embeddings_regularizer = embeddings_regularizer,
                embeddings_constraint = embeddings_constraint,
                mask_zero = mask_zero,
                input_shape = input_shape)

  }

  return(res)
}