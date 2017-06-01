#' Tests if keras is available on the system.
#'
#' Returns TRUE if the python `keras` library is installed. If the function
#' returns FALSE, but you believe `keras` is installed, then see
#' [use_python] to configure the python environment, and then
#' try running [keras_init] to establish the connection to `keras`.
#'
#' @export
#' @return Logical
#' @seealso [keras_init]
keras_available <- function(){
  msg <- ""
  collapse <- function(...)paste(..., sep = "\n")
  if(!reticulate::py_available(initialize = TRUE))
    msg <- collapse(msg, "python not available")
  else if(!reticulate::py_numpy_available())
    msg <- collapse(msg, "numpy  not available")
  else if(!reticulate::py_module_available("keras"))
    msg <- collapse(msg, "keras not available")

  if(msg != ""){
    message(msg, "\n",
            "See reticulate::use_python() to set python path, ", "\n",
            "then use kerasR::keras_init() to retry")
    FALSE
  } else TRUE
}


#' Initialise connection to the keras python libraries.
#'
#' This function gets called automatically on package startup. If the
#' python `keras` libary is not installed, then the function displays
#' a message, but doesn't connect to python.
#'
#' @importFrom  reticulate import
#' @export
#' @seealso [keras_available]
keras_init <- function(){
  if(keras_available()){

    modules$builtin <-  reticulate::import_builtins()
    modules$np <-       import("numpy")

    modules$keras.models <-               import("keras.models")
    modules$keras.layers.core <-          import("keras.layers.core")
    modules$keras.layers.convolutional <- import("keras.layers.convolutional")
    modules$keras.layers.pooling <-       import("keras.layers.pooling")
    modules$keras.layers.local <-         import("keras.layers.local")
    modules$keras.layers.recurrent <-     import("keras.layers.recurrent")
    modules$keras.layers.embeddings <-    import("keras.layers.embeddings")
    modules$keras.layers.advanced_activations <-
                        import("keras.layers.advanced_activations")
    modules$keras.layers.normalization <- import("keras.layers.normalization")
    modules$keras.layers.noise <-         import("keras.layers.noise")
    modules$keras.layers.wrappers <-      import("keras.layers.wrappers")
    modules$keras.optimizers <-           import("keras.optimizers")
    modules$keras.callbacks <-            import("keras.callbacks")
    modules$keras.initializers <-         import("keras.initializers")
    modules$keras.regularizers <-         import("keras.regularizers")
    modules$keras.constraints <-          import("keras.constraints")
    modules$keras.utils <-                import("keras.utils")
    modules$keras.datasets <-             import("keras.datasets")
    modules$keras.applications <-         import("keras.applications")
    modules$keras.preprocessing.text <-   import("keras.preprocessing.text")
    modules$keras.preprocessing.sequence <-
                        import("keras.preprocessing.sequence")
    modules$keras.preprocessing.image <-  import("keras.preprocessing.image")

    message("successfully loaded keras")
  }
}
