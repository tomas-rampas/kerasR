library(kerasR)

context("Testing pooling")

check_keras_available <- function() {
  if (!keras_available(silent = TRUE)) {
    skip("Keras is not available on this system.")
  }
}

test_that("pooling", {
  skip_on_cran()
  check_keras_available()

  X_train <- array(rnorm(100 * 28 * 28), dim = c(100, 28, 28, 1))
  Y_train <- to_categorical(matrix(sample(0:2, 100, TRUE), ncol = 1), 3)

  mod <- Sequential()
  mod$add(Conv2D(filters = 2, kernel_size = c(2, 2),
                 input_shape = c(28, 28, 1)))
  mod$add(Activation("relu"))
  mod$add(MaxPooling2D(pool_size=c(2, 2)))
  mod$add(LocallyConnected2D(filters = 2, kernel_size = c(2, 2)))
  mod$add(Activation("relu"))
  mod$add(MaxPooling2D(pool_size=c(2, 2)))
  mod$add(Dropout(0.25))

  mod$add(Flatten())
  mod$add(Dropout(0.5))
  mod$add(Dense(3, activation='softmax'))

  keras_compile(mod, loss='categorical_crossentropy', optimizer=RMSprop())
  keras_fit(mod, X_train, Y_train, verbose = 0)

  GlobalAveragePooling2D()
  GlobalAveragePooling2D(input_shape = c(1,2))
  GlobalMaxPooling2D()
  GlobalMaxPooling2D(input_shape = c(1,2))
  GlobalAveragePooling1D()
  GlobalAveragePooling1D(input_shape = c(1))
  GlobalMaxPooling1D()
  GlobalMaxPooling1D(input_shape = c(1,2))
  AveragePooling3D()
  AveragePooling3D(input_shape = c(1,2))
  AveragePooling2D()
  AveragePooling2D(input_shape = c(1,2))
  #AveragePooling1D()
  #AveragePooling1D(input_shape = c(1,2))

  MaxPooling3D(input_shape = c(1,2))
  MaxPooling2D(input_shape = c(1,2))
  #MaxPooling1D()
  MaxPooling3D(input_shape = c(1,2))
  MaxPooling2D(input_shape = c(1,2))
  #MaxPooling1D()

  testthat::expect_false(mod$stateful)

})
