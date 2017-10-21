#' login that reads from profile and only does auth if needed
safe_login <- function(){
  if (!is_logged_in()){
    tryCatch({
      login_sb()
      return()
    }, error=function(e) {
      message("couldn't log in using login_sb()")
      message(e)
    })
    tryCatch({
      profile <- load_profile()
      authenticate_sb(profile$sb_user, profile$sb_password)
      return()
    }, error=function(e) {
      message("couldn't log in using load_profile()")
      message(e)
    })
    warning("neither SB login method worked; continuing without login")
  }
}

#' This is where it starts. Use the parent id of where the data release lives
#' 
#' @param target_name the title of the parent item to be created
#' @return a sciencebase ID
create_release_parent <- function(target_name, parent.id ){
  create_release_child(parent.id, target_name)
}

#' create a child to any other item in a release
#'
#' identical to create_release_parent except for the order of the arguments
#'
#' @param parent.id sciencebase id of the parent to build off of
#' @param target_name title of the item to be created
#' @return a sciencebase ID
create_release_child <- function(parent.id, target_name){
  create_release_item(parent.id, key = target_name)
}

#' worker function for creating release items that are tagged and may have files
#'
#' if the item already exists, overwrites the entire item. creates the item if
#' it didn't exist. attaches the files given in ...
#'
#' @param parent.id a sciencebase id of the parent item
#' @param key the key value to use when tagging the item (scheme and type are
#'   fixed)
#' @param ... files to attach
#' @param \dots optional files to append to the created item
create_release_item <- function(parent.id, key, ...){
  scheme <- 'powell_center_metabolism'
  type <- 'data_release'
  safe_login()
  if (item_exists(scheme = scheme, type = type, key = key)){
    sb.id <- query_item_identifier(scheme = scheme, type = type, key = key)[[1]]$id
    if (length(c(...)) > 0){
      item_rm(sb.id, recursive = TRUE)
      Sys.sleep(1)
      sb.id <- item_create(parent_id = parent.id, title = key)$id
    }
  } else {
    sb.id <- item_create(parent_id = parent.id, title = key)$id
  }
  
  if (length(c(...)) > 0){
    append_release_files(sb.id, c(...))
  }
  item_update_identifier(sb.id, scheme = scheme, type = type, key = key)
  
  Sys.sleep(1)
  
  # check that it is indeed tagged
  if (!item_exists(scheme = scheme, type = type, key = key)){
    stop('item ', sb.id, ' failed to be properly tagged')
  }
  
  return(sb.id)
}

#' append files to a release item
#'
#' if files of the same names already exist, they are replaced
#'
#' @param sb.id a sciencebase id of the item
#' @param files a list of file paths to be uploaded
append_release_files <- function(sb.id, files){
  safe_login()
  item_replace_files(sb_id = sb.id, files = files, all = FALSE)
}
