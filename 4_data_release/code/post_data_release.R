
post_spatial <- function(files, target_name){
  spatial.id <- '57adfe26e4b0fc09faad6d97'
  profile <- load_profile()
  authenticate_sb(profile$sb_user, profile$sb_password)
  item.id <- item_create(parent_id = spatial.id, title = target_name)
  item_append_files(sb_id = item.id$id, files = files)
}