#' create an ncml file for thredds that describes the dataset
#' 
#' @param server.files a vector of file names that are on the thredds server
#' @param ncml.out a file name for the ncml file to be created
create_ldas_ncml <- function(server.files, ncml.out, ldas.config){
  files <- server.files
  
  times <- sort(unique(unname(sapply(files,function(x) paste(strsplit(x, '[_]')[[1]][1:4], collapse='_')))))
  vars <- unique(unname(sapply(files,function(x) paste(strsplit(tail(strsplit(x, '[_]')[[1]],1),'[.]')[[1]][1], collapse='_'))))
  
  ncml <- newXMLNode('netcdf', namespace=c("http://www.unidata.ucar.edu/namespaces/netcdf/ncml-2.2", xlink="http://www.w3.org/1999/xlink"))
  if (!missing(ldas.config)){
    for (var in names(ldas.config$units)){
      var.details <- newXMLNode('variable', parent = ncml, attrs = c(name=var))
      newXMLNode('attribute', parent = var.details, attrs = c(name="units", value=ldas.config$units[[var]] ))
    }
  }
  agg <- newXMLNode('aggregation', parent = ncml, attrs = c(type="union"))
  newXMLNode('remove', parent = agg, attrs = c(type="attribute", name="Conventions"))
  for (var in vars){
    nc <- newXMLNode('netcdf', parent = agg)
    join <- newXMLNode('aggregation', parent = nc, attrs=c(type="joinExisting", dimName="time"))
    for (t in times){
      newXMLNode('netcdf', parent = join, attrs=c(location=sprintf("%s_%s.nc", t, var)))
    }
    
  }
  saveXML(ncml, file = ncml.out)
}
