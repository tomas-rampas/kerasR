#' Tests if keras is available on the system.
#'
#' Returns TRUE if the python `keras` library is installed. If the function
#' returns FALSE, but you believe `keras` is installed, then see
#' [use_python] to configure the python environment, and then
#' try running [keras_init] to establish the connection to `keras`.
#'
#' @param silent   logical. Should warning message be displayed
#'                 if the result is false.
#'
#' @export
#' @return Logical
#' @seealso [keras_init]
keras_available <- function(silent = FALSE){
  msg <- ""
  collapse <- function(...) paste(..., sep = "\n")
  if(!reticulate::py_available(initialize = TRUE))
    msg <- collapse(msg, "python not available")
  else if(!reticulate::py_numpy_available())
    msg <- collapse(msg, "numpy  not available")
  else if(!reticulate::py_module_available("keras"))
    msg <- collapse(msg, "keras not available")

  if(msg != ""){
    if (!silent) {
      message(msg, "\n",
            "See reticulate::use_python() to set python path, ", "\n",
            "then use kerasR::keras_init() to retry")
    }
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
    modules$np <-       import("numpy", convert = FALSE)

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

#' Called to check if keras is installed and loaded
#'
keras_check <- function() {

  error_flag <- 0L

  if(!reticulate::py_available(initialize = TRUE)) {

    error_flag <- 1L
    msg <- c("Python not available", "\n",
            "See reticulate::use_python() to set python path, ", "\n",
            "then use kerasR::keras_init() to retry")

  } else if(!reticulate::py_numpy_available()) {

    error_flag <- 2L
    py_path <- reticulate::py_config()$python
    msg <- c("The numpy module is not available from the Python\n",
            "executable located here:",
            "\n\n   ",
            py_path, "\n\n",
            "This can be installed with pip by running the following:\n",
            "\n  pip install numpy\n\n",
            "If you believe it is already installed, you may be linking \n",
            "to the wrong version of Python. See reticulate::use_python()\n",
            "to set python path, then use kerasR::keras_init() to retry.\n",
            "You may need to restart R before use_python takes effect.")


  } else if(!reticulate::py_module_available("keras")) {

    error_flag <- 3L
    py_path <- reticulate::py_config()$python
    msg <-
          c("The keras module is not available from the Python\n",
            "executable located here:",
            "\n\n   ",
            py_path, "\n\n",
            "This can be installed with pip by running the following:\n",
            "\n  pip install keras\n\n",
            "If you believe it is already installed, you may be linking \n",
            "to the wrong version of Python. See reticulate::use_python()\n",
            "to set python path, then use kerasR::keras_init() to retry.\n",
            "You may need to restart R before use_python takes effect.")

  } else if(!reticulate::py_module_available("tensorflow") &
            !reticulate::py_module_available("theano")) {

    error_flag <- 4L
    py_path <- reticulate::py_config()$python
    msg <-
          c("Neither the tensorflow nor the theano modules are\n",
            "available from the Python executable located here:",
            "\n\n   ",
            py_path, "\n\n",
            "These can be installed with pip by running either of:\n",
            "\n  pip install tensorflow",
            "\n  pip install theano\n\n",
            "If you believe it is already installed, you may be linking \n",
            "to the wrong version of Python. See reticulate::use_python()\n",
            "to set python path, then use kerasR::keras_init() to retry.\n",
            "You may need to restart R before use_python takes effect.")

  }

  if (error_flag > 0L)
    stop(msg, call. = FALSE)
  else
    invisible(error_flag)

}

