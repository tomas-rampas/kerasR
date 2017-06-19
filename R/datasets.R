#' Load datasets
#'
#' These functions all return a named list with elements
#' `X_train`, `X_test`, `Y_train`, and `Y_test`. The first time
#' calling this function will download the datasets locally;
#' thereafter they will be loaded from the keras cache
#' directory.
#'
#' @param label_mode   either "fine" or "coarse"; how to construct labels
#'                     for [load_cifar100].
#' @param num_words    integer or NULL. Top most frequent words to consider.
#'                     Any less frequent word will appear as 0 in the
#'                     sequence data.
#' @param skip_top     integer. Top most frequent words to ignore (they
#'                     will appear as 0s in the sequence data).
#' @param maxlen       integer. Maximum sequence length. Any longer sequence
#'                     will be truncated.
#' @param test_split   float. Fraction of the dataset to use for testing.
#' @param seed         integer. Seed for reproducible data shuffling.
#' @param start_char   integer. The start of a sequence will be marked
#'                     with this character. Set to 1 because 0 is usually
#'                     the padding character.
#' @param oov_char     integer. words that were cut out because of the
#'                     num_words or skip_top limit will be replaced with
#'                     this character.
#' @param index_from   integer. Index actual words with this index and
#'                     higher.
#'
#' @example inst/examples/datasets.R
#' @template boilerplate
#' @name Datasets
NULL

#' @rdname Datasets
#' @export
load_cifar10 <- function() {
  keras_check()

  z <- modules$keras.datasets$cifar10$load_data()
  z <- unlist(z, recursive = FALSE)
  names(z) <- c("X_train", "Y_train", "X_test", "Y_test")
  z
}

#' @rdname Datasets
#' @export
load_cifar100 <- function(label_mode = "fine") {
  keras_check()

  z <- modules$keras.datasets$cifar100$load_data(label_mode = label_mode)
  z <- unlist(z, recursive = FALSE)
  names(z) <- c("X_train", "Y_train", "X_test", "Y_test")
  z
}

#' @rdname Datasets
#' @export
load_imdb <- function(num_words = NULL, skip_top = 0, maxlen = NULL,
                  seed = 113, start_char = 1, oov_char = 2,
                  index_from = 3) {
  keras_check()

  if (!is.null(num_words))
    num_words <- int32(num_words)

  if (!is.null(maxlen))
    maxlen <- int32(maxlen)

  z <- modules$keras.datasets$imdb$load_data(num_words = num_words,
                                             skip_top = int32(skip_top),
                                             maxlen = maxlen,
                                             seed = int32(seed),
                                             start_char = int32(start_char),
                                             oov_char = int32(oov_char),
                                             index_from = int32(index_from))
  z <- unlist(z, recursive = FALSE)
  names(z) <- c("X_train", "Y_train", "X_test", "Y_test")
  z
}

#' @rdname Datasets
#' @export
load_reuters <- function(num_words = NULL, skip_top = 0, maxlen = 1000,
                 test_split=0.2, seed = 113, start_char = 1, oov_char = 2,
                 index_from = 3) {
  keras_check()

  if (!is.null(num_words))
    num_words <- int32(num_words)

  if (!is.null(maxlen))
    maxlen <- int32(maxlen)

  z <- modules$keras.datasets$reuters$load_data(num_words = num_words,
                                             skip_top = int32(skip_top),
                                             maxlen = maxlen,
                                             test_split = test_split,
                                             seed = int32(seed),
                                             start_char = int32(start_char),
                                             oov_char = int32(oov_char),
                                             index_from = int32(index_from))
  z <- unlist(z, recursive = FALSE)
  names(z) <- c("X_train", "Y_train", "X_test", "Y_test")
  z
}

#' @rdname Datasets
#' @export
load_mnist <- function() {
  keras_check()

  z <- modules$keras.datasets$mnist$load_data()
  z <- unlist(z, recursive = FALSE)
  names(z) <- c("X_train", "Y_train", "X_test", "Y_test")
  z
}

#' @rdname Datasets
#' @export
load_boston_housing <- function() {
  keras_check()

  z <- modules$keras.datasets$boston_housing$load_data()
  z <- unlist(z, recursive = FALSE)
  names(z) <- c("X_train", "Y_train", "X_test", "Y_test")
  z
}

