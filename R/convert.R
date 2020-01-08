#' Convert R Objects to Erlang Map.
#' 
#' Convert R objects to Erlang a key-value assocuation map.
#' 
#' @param data A data.frame or list to convert to a map.
#' @param ... Options passed to methods.
#' @param key,value Bare columns names of key-value pair columns.
#' 
#' @examples 
#' as_map(c("a", 1))
#' 
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
as_map.default <- function(data, ...){
  .map(data) %>% 
    unlist() %>% 
    paste0(collapse = ",") %>% 
    paste0("#{", ., "}") %>% 
    erlang_object("map")
}

#' @rdname map
#' @export
#' @method as_map list
as_map.list <- function(data, ...){
  lapply(data, .map) %>% 
    unlist() %>% 
    paste0(collapse = ",") %>% 
    paste0("#{", ., "}") %>% 
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
    paste0("#{", ., "}") %>% 
    erlang_object("map")
}


#' Convert R Objects to Tuples or Lists
#' 
#' Convert R objects to tuples or lists.
#' 
#' @param data The data to convert.
#' 
#' @examples
#' as_tuple(cars)
#' 
#' @name tuple
#' @export
as_tuple <- function(data) UseMethod("as_tuple")

#' @export
as_tuple.default <- function(data){
  .to_tuple(data) %>% 
    erlang_object("tuple")
}

#' @rdname tuple
#' @export
as_list <- function(data) UseMethod("as_list")

#' @export
as_list.default <- function(data){
  .to_tuple(data) %>% 
    gsub("^\\{", "[", .) %>% 
    gsub("\\}$", "]", .) %>% 
    erlang_object("list")
}