#' Erlang
#' 
#' Interact with the erlang shell.
#' 
#' @importFrom dplyr enquo select
#' @importFrom magrittr `%>%`
#' @import subprocess
#' @export
Erl <- R6::R6Class(
  "Erl",
  public = list(
#' @details Initialise Class
#' 
#' @param bin Path to erlang binary. 
    initialize = function(bin = NULL){
      if(is.null(bin))
        bin <- find_erlang()

      private$bin <- bin
      private$erl <- spawn_process(bin)
      process_read(private$erl, PIPE_STDOUT, timeout = 5000)
    },
#' @details Evaluate Erlang Code
#' 
#' @param code Code to evaluate. 
#' @param wait Whether to re-attempt to evaluate if at first fails.
#' 
#' @examples
#' Erl$new()$eval("3 + 4.")
    eval = function(code, wait = TRUE){
      assert_that(!missing(code))
      process_write(private$erl, paste(code, "\n"))
      res <- process_read(private$erl, PIPE_STDOUT, timeout = 0)
      res <- res[-length(res)]
      if (wait){
        while (length(res) == 0){
          Sys.sleep(0.1)
          res <- process_read(private$erl, PIPE_STDOUT, timeout = 0)
        }
        invisible(res[-length(res)])
      }
      res <- res[-length(res)]

      # if error print return insivible
      if(grepl("^\\*", res)[1]){
        res <- gsub("\\*", "", res)
        cat(crayon::red(cli::symbol$cross), res, "\n")
        return(invisible(res))
      }
      res <- purrr::map(res, trimws) %>% 
        paste0(collapse = "")
      return(res)
    },
#' @details Create a variable
#' 
#' @param name Name of variable.
#' @param value Value of variable, an object of class \code{erlang_object}.
#' 
#' @examples
#' df <- data.frame(
#'   key = letters[1:10],
#'   value = runif(10)
#' )
#' 
#' map <- as_map(df, key = key, value = value)
#' 
#' e <- Erl$new()
#' e$assign("Vehicles", map)
#' e$eval("maps::get(a, Vehicles).")
#' e$eval("maps:update(b, 25, Vehicles).")
#' @export
    assign = function(name, value){
      assert_that(!missing(name))
      assert_that(is_erlang_object(value))

      cmd <- paste0(name, " = ", value)
      self$eval(cmd)
    },
#' @details Compile Erlang
#' 
#' @param name Name of file to compile, passed to \code{c}.
#' 
#' @examples
#' \dontrun{Erl$new()$compile("test")}
    compile = function(name) {
      name <- gsub("\\.erl$", "", name)
      cmd <- paste0("c(", name, ").")
      self$eval(cmd)
    },
#' @details Kills the session.
    finalize = function(){
      self$halt()
    },
#' @details Kills the session.
    halt = function(){
      if(process_state(private$erl) == "running"){
        process_kill(private$erl)
        cli::cli_alert_success("Erlang session halted.")
      } else {
        cli::cli_alert_danger("Erlang session already halted.\n")
      }
    },
#' @details Print the session.
    print = function(){
      cli::cli_alert_info("An erlang session.\n")
    }
  ),
  private = list(
    bin = NULL,
    erl = NULL
  )
)
