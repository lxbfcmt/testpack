#' A function for doing something
#'
#' This function takes some action. It also attempts to create a file on your
#' desktop called \code{data.txt}. If \code{data.txt} cannot be created a
#' warning is raised.
#'
#' @param force If set to \code{TRUE}, \code{data.txt} will be created on the
#' user's Desktop if their Desktop exists. If this function is used in an
#' interactive session the user will be asked whether or not \code{data.txt}
#' should be created. The default value is \code{FALSE}.
#'
#' @export
save_file <- function(file = "result.txt", savepath = "", force = FALSE){
  #
  # ... some code that does something useful ...
  #
  if(!dir.exists(savepath)){
    result2 <- select.list(c("yes", "no"),
                          title = paste("Would you like to use current path",getwd(),"as your working path?"))
    if(result2 == "yes"){
      savepath = getwd()
    }
    else{
      print("please input the directory you want to set.")
      savepath = scan()
    }
  }

  if(!force && interactive()){
    result = file.exists(file.path(normalizePath(savepath),file))
    if(result){
      ans <- select.list(c("yes","no"),title = "do you want to overwrite the exist file?")
      if(ans == "no"){
        warning("Exit without saving.")
        }
      else if(ans == "yes"){
        print(paste("result.txt will be overwrote."))
        file.create(file.path(normalizePath(savepath),file))
        }
      }
    else{
      print(paste("result.txt is saved."))
      file.create(file.path(normalizePath(savepath),file))
      }
    }
  else if(force){
    print("default path will be used compulsorily.")
    file.create(file.path(normalizePath(savepath),file))
    }
  else {
    warning("result.txt will not be created.")
    }

}
