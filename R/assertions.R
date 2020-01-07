has_erlang <- function(x) {
  x != ""
}
on_failure(has_erlang) <- function(call, env) {
  paste0("Cannot finc path to erlang.")
}

is_erlang_object <- function(x){
  inherits(x, "erlang_object")
}
on_failure(has_erlang) <- function(call, env) {
  paste0(deparse(call$x), "must be of class", crayon::blue("erlang_object"))
}