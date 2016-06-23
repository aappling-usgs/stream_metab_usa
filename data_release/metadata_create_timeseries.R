library(magrittr)
library(xml2)


d <- xml_new_document() %>% 
  xml_add_child("metadata")
m <- xml_add_child(d, "idinfo")

m %>%  xml_add_child("citation") %>%
  xml_add_child("citeinfo") %>%
  xml_add_child("origin", "{{authors}}") %>%
  xml_add_sibling('pubdate', "{{pubdate}}") %>%
  xml_add_sibling('title', "{{title}}") %>%
  xml_add_sibling('geoform', "text files") %>%
  xml_add_sibling('onlink', "{{doi}}")

m %>%
  xml_add_child('descript') %>%
  xml_add_child("abstract",'{{abstract}}') %>%
  xml_add_sibling("purpose", '{{purpose}}')

m %>%
  xml_add_child('timeperd') %>%
  xml_add_child("timeinfo") %>%
  xml_add_child("rngdates") %>%
  xml_add_child('begdate','{{start-date}}') %>%
  xml_add_sibling('enddate','{{end-date}}')
m %>%
  xml_add_child('status') %>%
  xml_add_child("progress", "Complete") %>% 
  xml_add_sibling('update','{{update}}') 

m %>% 
  xml_add_child('spdom') %>% 
  xml_add_child('bounding') %>% 
  xml_add_child('westbc', "{{wbbox}}") %>% 
  xml_add_sibling('eastbc', "{{ebbox}}") %>% 
  xml_add_sibling('northbc', "{{nbbox}}") %>% 
  xml_add_sibling('southbc', "{{sbbox}}")

k <- xml_add_child(m, 'keywords') 

k %>% 
  xml_add_child('theme', "\n<themekt>none</themekt>\n{{#themekeywords}}<themekey>{{.}}</themekey>\n{{/themekeywords}}")

k %>% 
  xml_add_child('theme') %>% 
  xml_add_child('themekt','ISO 19115 Topic Category') %>% 
  xml_add_sibling('themekey','environment') %>% 
  xml_add_sibling('themekey','inlandWaters') %>% 
  xml_add_sibling('themekey','007') %>% 
  xml_add_sibling('themekey','012') 

m %>% 
  xml_add_child('place') %>% 
  xml_add_child('placekt','Department of Commerce, 1995, Countries, Dependencies, Areas of Special Sovereignty, and 
                Their Principal Administrative Divisions,  Federal Information Processing Standard (FIPS) 10-4, 
                Washington, D.C., National Institute of Standards and Technology') %>% 
  xml_add_sibling('placekey','United States') %>% 
  xml_add_sibling('placekey','US')
m %>% 
  xml_add_child('place-template')
  
  
  
write_xml(d, file = 'test.xml')

place.template = "{{#states}}<place><placekt>U.S. Department of Commerce, 1987, Codes for the identification of the States, 
                the District of Columbia and the outlying areas of the United States, and associated areas 
                (Federal Information Processing Standard 5-2): Washington, D. C., NIST</placekt>
                <placekey>{{state-name}}</placekey>\n<placekey>{{state-abbr}}</placekey>\n</place>{{/states}}"

suppressWarnings(readLines('test.xml')) %>% 
  gsub(pattern = '&gt;',replacement = '>',.) %>% 
  gsub(pattern = '&lt;',replacement = '<',.) %>% 
  gsub(pattern = '<place-template/>', replacement = place.template) %>% 
  cat(file = 'test.xml', sep = '\n')
 
states <- c('Wisconsin','New Hampshire') %>% 
  whisker::iteratelist(value='state-name')