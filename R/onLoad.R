#' Initialise connection to the keras python libraries.
#' 
#' This function gets called automatically on package startup
#' 
#' @importFrom  reticulate import py_available py_module_available py_numpy_available
#' @export
init_keras <- function(){
  kStop <- function(msg){
    msg <- paste(msg, "Use kerasR::init_keras() to retry")
    stop(msg)
  }
  if(!reticulate::py_available(initialize = TRUE)) kStop("python not available")
  if(!reticulate::py_module_available("keras")) kStop("keras library for python not available")
  if(!reticulate::py_numpy_available()) kStop("numpy library for python not available")
  
  modules$keras.models <-               reticulate::import("keras.models")
  modules$keras.layers.core <-          reticulate::import("keras.layers.core")
  modules$keras.layers.convolutional <- reticulate::import("keras.layers.convolutional")
  modules$keras.layers.pooling <-       reticulate::import("keras.layers.pooling")
  modules$keras.layers.local <-         reticulate::import("keras.layers.local")
  modules$keras.layers.recurrent <-     reticulate::import("keras.layers.recurrent")
  modules$keras.layers.embeddings <-    reticulate::import("keras.layers.embeddings")
  modules$keras.layers.advanced_activations <- reticulate::import("keras.layers.advanced_activations")
  modules$keras.layers.normalization <- reticulate::import("keras.layers.normalization")
  modules$keras.layers.noise <-         reticulate::import("keras.layers.noise")
  modules$keras.layers.wrappers <-      reticulate::import("keras.layers.wrappers")
  modules$keras.optimizers <-           reticulate::import("keras.optimizers")
  modules$keras.callbacks <-            reticulate::import("keras.callbacks")
  modules$keras.initializers <-         reticulate::import("keras.initializers")
  modules$keras.regularizers <-         reticulate::import("keras.regularizers")
  modules$keras.constraints <-          reticulate::import("keras.constraints")
  modules$keras.utils <-                reticulate::import("keras.utils")
  modules$keras.datasets <-             reticulate::import("keras.datasets")
  modules$keras.applications <-         reticulate::import("keras.applications")
  modules$keras.preprocessing.text <-   reticulate::import("keras.preprocessing.text")
  modules$keras.preprocessing.sequence <- reticulate::import("keras.preprocessing.sequence")
  modules$keras.preprocessing.image <-  reticulate::import("keras.preprocessing.image")
  modules$builtin <-                    reticulate::import_builtins()
  modules$np <-                         reticulate::import("numpy")
  
  message("successfully loaded keras")
}


.onLoad <- function(libname, pkgname) {
  init_keras()
}