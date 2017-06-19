library(kerasR)

context("Testing recurrent layers")

check_keras_available <- function() {
  if (!keras_available(silent = TRUE)) {
    skip("Keras is not available on this system.")
  }
}

test_that("recurrent layers", {
  skip_on_cran()
  check_keras_available()

  X_train <- matrix(sample(0:19, 100 * 100, TRUE), ncol = 100)
  Y_train <- rnorm(100)

  mod <- Sequential()
  mod$add(Embedding(input_dim = 20, output_dim = 10,
                    input_length = 100))
  mod$add(Dropout(0.5))

  mod$add(LSTM(16))
  mod$add(Dense(1))
  mod$add(Activation("sigmoid"))

  keras_compile(mod, loss = "mse", optimizer = RMSprop())
  keras_fit(mod, X_train, Y_train, epochs = 3, verbose = 0)

  SimpleRNN(units = 4)
  SimpleRNN(units = 4, input_shape = c(3,4))
  LSTM(units = 4)
  LSTM(units = 4, input_shape = c(3,4))
  GRU(units = 4)
  GRU(units = 4, input_shape = c(3,4))

  testthat::expect_false(mod$stateful)
})
