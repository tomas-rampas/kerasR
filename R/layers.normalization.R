#' Batch normalization layer
#'
#' @param axis                    Integer, the axis that should be normalized
#'                                (typically the features axis).
#' @param momentum                Momentum for the moving average.
#' @param epsilon                 Small float added to variance to avoid
#'                                dividing by zero.
#' @param center                  If True, add offset of beta to normalized
#'                                tensor. If False, beta is ignored.
#' @param scale                   If True, multiply by gamma. If False,
#'                                gamma is not used. When the next
#'                                layer is linear (also e.g. nn.relu),
#'                                this can be disabled since the
#'                                scaling will be done by the next layer.
#' @param beta_initializer        Initializer for the beta weight.
#' @param gamma_initializer       Initializer for the gamma weight.
#' @param moving_mean_initializer Initializer for the moving mean.
#' @param moving_variance_initializer Initializer for the moving variance.
#' @param beta_regularizer        Optional regularizer for the beta weight.
#' @param gamma_regularizer       Optional regularizer for the gamma weight.
#' @param beta_constraint         Optional constraint for the beta weight.
#' @param gamma_constraint        Optional constraint for the gamma weight.
#' @param input_shape             only need when first layer of a model; sets
#'                                the input shape of the data
#' @example inst/examples/layers_normalization.R
#' @template boilerplate
#' @export
#' @family layers
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
    res <- modules$keras.layers.normalization$BatchNormalization(
                 axis = int32(axis),
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

    input_shape <- as.list(input_shape)
    input_shape <- modules$builtin$tuple(int32(input_shape))

    res <- modules$keras.layers.normalization$BatchNormalization(
                 axis = int32(axis),
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


