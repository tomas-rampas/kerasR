modules <- new.env(parent=emptyenv())

int32 <- function(x) {
  modules$np$int32(x)
}
