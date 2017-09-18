#' login that reads from profile and only does auth if needed
safe_login <- function(){
  if (!is_logged_in()){
    profile <- load_profile()
    authenticate_sb(profile$sb_user, profile$sb_password)
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
#' @param parent.id sciencebase id of the parent to build off of
#' @param target_name title of the item to be created
#' @return a sciencebase ID
create_release_child <- function(parent.id, target_name){
  create_release_item(parent.id, key = target_name)
}

#' worker function for creating release items that are tagged and may have files
#' 
#' @param parent.id a sciencebase id of the parent item
#' @param key the value to use when tagging items
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
#' @param sb.id a sciencebase id of the item
#' @param files a list of file paths to be uploaded
append_release_files <- function(sb.id, files){
  safe_login()
  item_replace_files(sb_id = sb.id, files = files, all = FALSE)
}
