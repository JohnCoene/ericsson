.map <- function(pair){
  paste0(pair[1], "=>", pair[2])
}

.to_tuple <- function(data){
  data <- unname(data)
  row.names(data) <- NULL
  
  data %>% 
    jsonlite::toJSON(auto_unbox = TRUE) %>% 
    as.character() %>% 
    gsub("\\[", "{", .) %>% 
    gsub("\\]", "}", .) %>% 
    gsub('\\"', "", .)
}