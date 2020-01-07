#' Erlang Data Structure
#' 
#' Erland Data Struture class for better printing 
#' and potential future improvements.
#' 
#' @param x The data.
#' @param cl The class name
#' 
#' @keywords internal
erlang_object <- function(x, cl = NULL){
  assert_that(!missing(cl))
  cl <- paste0("erlang_", cl)
  structure(x, class = c(class(x), cl, "erlang_object"))
}

#' @export
print.erlang_object <- function(x, ...){
  cli::cli_alert_info("An erlang object.")
}

#' @export
print.erlang_map <- function(x, ...){
  cli::cli_alert_info("An erlang map.")
  
  # print map
  if(nchar(x) > 50)
    cat(substr(x, 1, 50), crayon::blue("..."), "}.\n")
  else
    cat(x)
}