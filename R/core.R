#' Erlang
#' 
#' Interact with the erlang shell.
#' 
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
      if (wait){
        while (length(res) == 0){
          Sys.sleep(0.1)
          res <- process_read(private$erl, PIPE_STDOUT, timeout = 0)
        }
        invisible(res[-length(res)])
      }
      res[-length(res)]
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
      self$kill()
    },
#' @details Kills the session.
    kill = function(){
      process_kill(private$erl)
    },
#' @details Print the session.
    print = function(){
      cat("An erlang session.\n")
    }
  ),
  private = list(
    bin = NULL,
    erl = NULL
  )
)
