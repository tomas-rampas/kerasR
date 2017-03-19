#' Batch normalization layer
#'
#' @param axis                    Integer, the axis that should be normalized (typically the features axis).
#' @param momentum                Momentum for the moving average.
#' @param epsilon                 Small float added to variance to avoid dividing by zero.
#' @param center                  If True, add offset of beta to normalized tensor. If False, beta is ignored.
#' @param scale                   If True, multiply by gamma. If False, gamma is not used. When the next
#'                                layer is linear (also e.g. nn.relu), this can be disabled since the
#'                                scaling will be done by the next layer.
#' @param beta_initializer        Initializer for the beta weight.
#' @param gamma_initializer       Initializer for the gamma weight.
#' @param moving_mean_initializer Initializer for the moving mean.
#' @param moving_variance_initializer Initializer for the moving variance.
#' @param beta_regularizer        Optional regularizer for the beta weight.
#' @param gamma_regularizer       Optional regularizer for the gamma weight.
#' @param beta_constraint         Optional constraint for the beta weight.
#' @param gamma_constraint        Optional constraint for the gamma weight.
#' @param input_shape             only need when first layer of a model; sets the input shape
#'                                of the data
#' @examples
#' if (run_examples()) {
#' X_train <- matrix(rnorm(100 * 10), nrow = 100)
#' Y_train <- to_categorical(matrix(sample(0:2, 100, TRUE), ncol = 1), 3)
#'
#' mod <- Sequential()
#' mod$add(Dense(units = 50, input_shape = dim(X_train)[2]))
#' mod$add(Dropout(rate = 0.5))
#' mod$add(Activation("relu"))
#' mod$add(BatchNormalization())
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
BatchNormalization <- function(axis = -1,
                               momentum = 0.99,
                               epsilon = 1e-3,
                               center = TRUE,
                               scale = TRUE,
                               beta_initializer = 'zeros',
                               gamma_initializer = 'ones',
                               moving_mean_initializer = 'zeros',
                               moving_variance_initializer = 'ones',
                               beta_regularizer = NULL,
                               gamma_regularizer = NULL,
                               beta_constraint = NULL,
                               gamma_constraint = NULL,
                               input_shape = NULL) {

  # Need special logic for input_shape because it is passed
  # via kwargs and needs to be manually adjusted
  if (is.null(input_shape)) {
    res <- modules$keras.layers.normalization$BatchNormalization(axis = int32(axis),
                               momentum = momentum,
                               epsilon = epsilon,
                               center = center,
                               scale = scale,
                               beta_initializer = beta_initializer,
                               gamma_initializer = gamma_initializer,
                               moving_mean_initializer = moving_mean_initializer,
                               moving_variance_initializer = moving_variance_initializer,
                               beta_regularizer = beta_regularizer,
                               gamma_regularizer = gamma_regularizer,
                               beta_constraint = beta_constraint,
                               gamma_constraint = gamma_constraint)
  } else {

    input_shape <- sapply(input_shape, list)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.normalization$BatchNormalization(axis = int32(axis),
                               momentum = momentum,
                               epsilon = epsilon,
                               center = center,
                               scale = scale,
                               beta_initializer = beta_initializer,
                               gamma_initializer = gamma_initializer,
                               moving_mean_initializer = moving_mean_initializer,
                               moving_variance_initializer = moving_variance_initializer,
                               beta_regularizer = beta_regularizer,
                               gamma_regularizer = gamma_regularizer,
                               beta_constraint = beta_constraint,
                               gamma_constraint = gamma_constraint,
                               input_shape = input_shape)

  }

  return(res)
}


