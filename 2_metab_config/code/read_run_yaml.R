# Each of the following functions reads the entire config yaml and subsets to a 
# specific run ID as indicated by the function name. Can't use a text argument 
# to specify the run ID because remake expects all string arguments to be file 
# names.
read_run1_yaml <- function(yamlfile='../2_metab_config/in/metab_configs_config.yml') {
  yaml.load_file(yamlfile)[['run1']]
}
read_run2_yaml <- function(yamlfile='../2_metab_config/in/metab_configs_config.yml') {
  yaml.load_file(yamlfile)[['run2']]
}
read_run3_yaml <- function(yamlfile='../2_metab_config/in/metab_configs_config.yml') {
  yaml.load_file(yamlfile)[['run3']]
}
