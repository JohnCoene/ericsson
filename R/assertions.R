has_erlang <- function(x) {
  x != ""
}
on_failure(has_erlang) <- function(call, env) {
  paste0("Cannot finc path to erlang.")
}