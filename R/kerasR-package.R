#' Keras Models in R
#'
#' Keras is a high-level neural networks API, originally written in Python, and
#' capable of running on top of either TensorFlow or Theano. It was developed
#' with a focus on enabling fast experimentation. This package provides an
#' interface to Keras from within R. All of the returned objects from functions
#' in this package are either native R objects or raw pointers to python
#' objects, making it possible for users to access the entire keras API.
#' The main benefits
#' of the package are (1) correct, manual parsing of R inputs to python, (2)
#' R-sided documentation, and (3) examples written using the API.
#'
#' Most functions have associated examples showing a working example of how
#' a layer or object may be used. These are mostly toy examples, made with
#' small datasets with little regard to whether these are the correct models
#' for a particular task. See the package vignettes for a more thorough
#' explaination and several larger, more practical examples.
#'
#' @author
#' Taylor B. Arnold \email{taylor.arnold@@acm.org},
#'
#' Maintainer: Taylor B. Arnold \email{taylor.arnold@@acm.org}
#' @name kerasR
#' @docType package
NULL
