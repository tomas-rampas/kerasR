#' Optimizers
#'
#' Optimization functions to use in compiling a keras model.
#'
#' @param clipnorm   float >= 0. Gradients will be clipped when their L2 norm
#'                   exceeds this value. Set to -1 to disable.
#' @param clipvalue  float >= 0. Gradients will be clipped when their
#'                   absolute value exceeds this value. Set to -1 to disable.
#' @param lr         float >= 0. Learning rate.
#' @param momentum   float >= 0. Parameter updates momentum.
#' @param decay      float >= 0. Learning rate decay over each update.
#' @param nesterov   boolean. Whether to apply Nesterov momentum.
#' @param rho        float >= 0 to be used in [RMSprop]
#' @param epsilon    float >= 0. Fuzz factor.
#' @param beta_1     float, 0 < beta < 1. Generally close to 1.
#' @param beta_2     float, 0 < beta < 1. Generally close to 1.
#' @param schedule_decay  float >= 0. Learning rate decay over each
#'                        schedule in [Nadam].
#'
#' @example inst/examples/optimizers.R
#' @template boilerplate
#' @name Optimizers
NULL

#' @rdname Optimizers
#' @export
SGD <- function(lr = 0.01, momentum = 0.0, decay = 0.0, nesterov = FALSE,
                clipnorm = -1, clipvalue = -1) {
  modules$keras.optimizers$SGD(lr = lr, momentum = momentum, decay = decay,
                               nesterov = nesterov,
                                clipnorm = clipnorm, clipvalue = clipvalue)
}

#' @rdname Optimizers
#' @export
RMSprop <- function(lr = 0.001, rho = 0.9, epsilon = 1e-08, decay = 0.0,
                    clipnorm = -1, clipvalue = -1) {
  modules$keras.optimizers$RMSprop(lr = lr, rho = rho,
                                   epsilon = epsilon,
                                   decay = decay,
                                   clipnorm = clipnorm,
                                   clipvalue = clipvalue)
}

#' @rdname Optimizers
#' @export
Adagrad <- function(lr = 0.01, epsilon = 1e-08, decay = 0.0,
                    clipnorm = -1, clipvalue = -1) {
  modules$keras.optimizers$Adagrad(lr = lr, epsilon = epsilon, decay = decay,
                                    clipnorm = clipnorm,
                                    clipvalue = clipvalue)
}

#' @rdname Optimizers
#' @export
Adadelta <- function(lr = 1.0, rho = 0.95, epsilon = 1e-08, decay = 0.0,
                     clipnorm = -1, clipvalue = -1) {
  modules$keras.optimizers$Adadelta(lr = lr, rho = rho, epsilon = epsilon,
                                    decay = decay,
                                    clipnorm = clipnorm,
                                    clipvalue = clipvalue)
}

#' @rdname Optimizers
#' @export
Adam <- function(lr = 0.001, beta_1 = 0.9, beta_2 = 0.999, epsilon = 1e-08,
                 decay = 0.0, clipnorm = -1, clipvalue = -1) {
  modules$keras.optimizers$Adam(lr = lr, beta_1 = beta_2, beta_2 = beta_2,
                                epsilon = epsilon, decay = decay,
                                clipnorm = clipnorm, clipvalue = clipvalue)
}

#' @rdname Optimizers
#' @export
Adamax <- function(lr = 0.002, beta_1 = 0.9, beta_2 = 0.999, epsilon = 1e-08,
                    decay = 0.0, clipnorm = -1, clipvalue = -1) {
  modules$keras.optimizers$Adamax(lr = lr, beta_1 = beta_1, beta_2 = beta_2,
                                  epsilon = epsilon, decay = decay,
                                  clipnorm = clipnorm, clipvalue = clipvalue)
}

#' @rdname Optimizers
#' @export
Nadam <- function(lr = 0.002, beta_1 = 0.9, beta_2 = 0.999, epsilon = 1e-08,
                  schedule_decay = 0.004, clipnorm = -1, clipvalue = -1) {
  modules$keras.optimizers$Nadam(lr = lr, beta_1 = beta_1, beta_2 = beta_2,
                                  epsilon = epsilon,
                                  schedule_decay = schedule_decay,
                                  clipnorm = clipnorm, clipvalue = clipvalue)
}
