#' Find Path to erlang.
#' 
#' @import assertthat
#' @export
find_erlang <- function(){
  path <- Sys.which("erl")

  assert_that(has_erlang(path))

  invisible(path)
}