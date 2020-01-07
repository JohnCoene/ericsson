#' Convert R Objects to Erlang Map.
#' 
#' Convert R objects to Erlang a key-value assocuation map.
#' 
#' @param data A data.frame or list to convert to a map.
#' @param ... Options passed to methods.
#' @param key,value Bare columns names of key-value pair columns.
#' 
#' @examples 
#' # from a data.frame
#' df <- data.frame(
#'   key = letters[1:10],
#'   value = sample(1:10, 10)
#' )
#' 
#' as_map(df, key = key, value = value)
#' 
#' # from a list
#' lst <- list(
#'   list("a", 1),
#'   list("b", 2)
#' )
#' 
#' as_map(lst)
#' 
#' @name map
#' @export 
as_map <- function(data, ...) UseMethod("as_map")

#' @rdname map
#' @export
#' @method as_map list
as_map.list <- function(data, ...){
  lapply(data, .map) %>% 
    unlist() %>% 
    paste0(collapse = ",") %>% 
    paste0("#{", ., "}.") %>% 
    erlang_object("map")
}

#' @rdname map
#' @export
#' @method as_map data.frame
as_map.data.frame <- function(data, ..., key, value){
  assert_that(!missing(key), !missing(value))

  key_enquo <- enquo(key)
  value_enquo <- enquo(value)

  data %>% 
    select(!!key_enquo, !!value_enquo) %>% 
    apply(1, .map) %>% 
    unlist() %>% 
    paste0(collapse = ",") %>% 
    paste0("#{", ., "}.") %>% 
    erlang_object("map")
}

.map <- function(pair){
  paste0(pair[1], "=>", pair[2])
}
