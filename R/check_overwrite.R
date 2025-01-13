#' Check overwrite
#'
#' @param method passed from animate_paths()
#' @param return.paths passed from animate_paths()
#' @param ... passed from animate_paths(); used to check for user-specified value for img.name
#'
#' @return NULL, unless there is risk of over-writting and the user interrupts animation (\code{FALSE})
#'
check_overwrite <- function(method, return.paths, ...){
  
  #to detect if img.name argument was used
  logical_imgname <- function(...){
    args <- list(...)
    names(args)
    "img.name" %in% names(args)
  }
  imgname_T_or_F <- logical_imgname(...)
  
  #to check if img, css, js folders exist
  x <- file.exists("images")
  y <- file.exists("css") 
  z <- file.exists("js")
  folders_T_or_F <- (x == T & y == T & z == T)
  
  #run animation with no changes
  if (folders_T_or_F == F || imgname_T_or_F == T || method == "mp4" || return.paths == TRUE){
    return()
  }
  #message asking for user's permission to overwrite
  else if(folders_T_or_F == T & imgname_T_or_F == F & method == "html"){
    remove_img_resp = readline(prompt= "Would you like anipaths to write over existing files? Y/N \n")
    remove_img_resp <- gsub(" ", "", toupper(remove_img_resp))
    #user wants to overwrite existing files. Files are deleted and recreated to only store current animation
    if(remove_img_resp == 'Y' || remove_img_resp == 'YES'){
      unlink("images", recursive=TRUE)
      unlink("js", recursive=TRUE)
      unlink("css", recursive = TRUE)
      message("Overwritting existing files.")
      return()
    }
    #user does not want to overwrite existing files. animate_paths does not run. Three options presented to save past animations and not overwrite xisting files 
    else{
      return(F)
    }
  }
  
}
