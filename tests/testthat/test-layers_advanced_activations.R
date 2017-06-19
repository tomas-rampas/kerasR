library(kerasR)

context("Testing advanced activations")

check_keras_available <- function() {
  if (!keras_available(silent = TRUE)) {
    skip("Keras is not available on this system.")
  }
}

test_that("advanced activations", {
  skip_on_cran()
  check_keras_available()

  X_train <- matrix(rnorm(100 * 10), nrow = 100)
  Y_train <- to_categorical(matrix(sample(0:2, 100, TRUE), ncol = 1), 3)

  mod <- Sequential()
  mod$add(Dense(units = 50, input_shape = dim(X_train)[2]))
  mod$add(LeakyReLU(alpha = 0.4))
  mod$add(Dense(units = 50))
  mod$add(ELU(alpha = 0.5))
  mod$add(Dense(units = 50))
  mod$add(ThresholdedReLU(theta = 1.1))
  mod$add(Dense(units = 3))
  mod$add(Activation("softmax"))
  keras_compile(mod,  loss = 'categorical_crossentropy', optimizer = RMSprop())

  keras_fit(mod, X_train, Y_train, batch_size = 32, epochs = 5, verbose = 0)
  testthat::expect_false(mod$stateful)
})
