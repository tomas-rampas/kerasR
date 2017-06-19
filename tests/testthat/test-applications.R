library(kerasR)

context("Testing applications")

check_keras_available <- function() {
  if (!keras_available(silent = TRUE)) {
    skip("Keras is not available on this system.")
  }
}

test_that("applications", {
  skip_on_cran()
  check_keras_available()

  mod <- Xception()
  mod <- VGG16()
  mod <- VGG19()
  mod <- ResNet50()
  mod <- InceptionV3()

  mod <- Xception(input_shape = c(71, 71, 3), include_top = FALSE)
  mod <- VGG16(input_shape = c(71, 71, 3), include_top = FALSE)
  mod <- VGG19(input_shape = c(71, 71, 3), include_top = FALSE)
  mod <- ResNet50(input_shape = c(197, 197, 3), include_top = FALSE)
  mod <- InceptionV3(input_shape = c(139, 139, 3), include_top = FALSE)

  decode_predictions(matrix(1, 1, nrow = 10, ncol = 1000),
      model = "Xception")
  decode_predictions(matrix(1, 1, nrow = 10, ncol = 1000),
      model = "VGG16")
  decode_predictions(matrix(1, 1, nrow = 10, ncol = 1000),
      model = "VGG19")
  decode_predictions(matrix(1, 1, nrow = 10, ncol = 1000),
      model = "ResNet50")
  decode_predictions(matrix(1, 1, nrow = 10, ncol = 1000),
      model = "InceptionV3")

  # img <- array(runif(300), dim = c(10, 10, 3))
  # img <- int32(img)
  # preprocess_input(img, model = "Xception")
  # preprocess_input(img, model = "VGG16")
  # preprocess_input(img, model = "VGG19")
  # preprocess_input(img, model = "ResNet50")
  # preprocess_input(img, model = "InceptionV3")

})
