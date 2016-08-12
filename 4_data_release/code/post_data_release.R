safe_login <- function(){
  if (!is_logged_in()){
    profile <- load_profile()
    authenticate_sb(profile$sb_user, profile$sb_password)
  }
}

create_release_parent <- function(target_name){
  parent.id <- '57adfa86e4b0fc09faad6d87'
  create_release_child(parent.id, target_name)
}

create_release_child <- function(parent.id, target_name){
  create_release_item(parent.id, key = target_name)
}

create_release_item <- function(parent.id, key, title = key){
  if (item_exists(scheme = 'powell_center', type = 'data_release', key = key)){
    sb.id <- query_item_identifier(scheme = 'powell_center', type = 'data_release', key = key)
  } else {
    sb.id <- item_create(parent_id = parent.id, title = title)$id
    sb.id <- item_update_identifier(item.id, scheme = 'powell_center', type = 'data_release', key = key)$id
  }
  return(sb.id)
}


post_spatial <- function(files, target_name){
  spatial.id <- '57adfe26e4b0fc09faad6d97'
  profile <- load_profile()
  authenticate_sb(profile$sb_user, profile$sb_password)
  
  # item_exists
  item.id <- item_create(parent_id = spatial.id, title = target_name)$id
  item_append_files(sb_id = item.id, files = files)
  site_id <- item_update_identifier(item.id, scheme='mda_streams', type='data_release', key=target_name)
  return(site_id)
}