library(kerasR)

context("Testing datasets")

check_keras_available <- function() {
  if (!keras_available(silent = TRUE)) {
    skip("Keras is not available on this system.")
  }
}

test_that("dense model", {
  skip_on_cran()
  check_keras_available()

  boston <- load_boston_housing()
  X_train <- normalize(boston$X_train, 0)
  Y_train <- boston$Y_train
  X_test <- normalize(boston$X_test, 0)
  Y_test <- boston$Y_test

  mod <- Sequential()
  mod$add(Dense(units = 200, input_shape = 13))
  mod$add(Activation("relu"))
  mod$add(Dense(units = 200))
  mod$add(Activation("relu"))
  mod$add(Dense(units = 1))
  keras_compile(mod,  loss = 'mse', optimizer = SGD())

  keras_fit(mod, scale(X_train), Y_train,
            batch_size = 32, epochs = 20,
            verbose = 1, validation_split = 0.1)

  testthat::expect_false(mod$stateful)

  # Just make sure these run without errors
  cifar10 <- load_cifar10()
  cifar100 <- load_cifar100()
  imdb <- load_imdb()
  mnist <- load_mnist()
  #reuters <- load_reuters()
})
