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

k %>% 
  xml_add_child('place') %>% 
  xml_add_child('placekt','Department of Commerce, 1995, Countries, Dependencies, Areas of Special Sovereignty, and 
                Their Principal Administrative Divisions,  Federal Information Processing Standard (FIPS) 10-4, 
                Washington, D.C., National Institute of Standards and Technology') %>% 
  xml_add_sibling('placekey','United States') %>% 
  xml_add_sibling('placekey','US')
k %>% 
  xml_add_child('place-template')

k %>% 
  xml_add_child('state-template')
  
p <- xml_add_child(m, 'accconst','none')

pt <-  xml_add_sibling(p,'ptcontac') 

pt %>% 
  xml_add_child('cntinfo') %>% 
  xml_add_child('cntperp') %>% 
  xml_add_child('cntper','{{contact-person}}') %>% 
  xml_add_sibling('cntorg','U.S. Geological Survey')

p %>% 
  xml_add_sibling('useconst','{{usage-rules}}')

pt %>% xml_add_child('cntaddr') %>% 
  xml_add_child('addrtype','Mailing and Physical') %>% 
  xml_add_sibling('address', '8551 Research Way') %>% 
  xml_add_sibling('city','Middleton') %>% 
  xml_add_sibling('state','WI') %>% 
  xml_add_sibling('postal','53562') %>% 
  xml_add_sibling('country','U.S.A.')

pt %>%  xml_add_child('cntvoice','{{contact-phone}}') %>% 
  xml_add_sibling('cntemail','{{contact-email}}')

m %>% 
  xml_add_child('datacred','{{funding-credits}}') %>% 
  xml_add_sibling('native','{{build-environment}}') %>% 
  xml_add_sibling('crossref') %>% 
  xml_add_child('citeinfo') %>% 
  xml_add_child('origin','{{cite-authors}}') %>% 
  xml_add_sibling('pubdate','{{cite-date}}') %>% 
  xml_add_sibling('title','{{cite-title}}') %>% 
  xml_add_sibling('geoform','{{paper}}') %>% 
  xml_add_sibling('pubinfo') %>% 
  xml_add_child('pubplace',"{{publisher}}") %>% 
  xml_add_sibling('publish','{{journal}}')

q <- xml_add_sibling(m, 'dataqual')

q %>% xml_add_child('attracc') %>% 
  xml_add_child('attraccr','No formal attribute accuracy tests were conducted.')

q %>% xml_add_child('logic','not applicable') %>% 
  xml_add_sibling('complete','not applicable')

p <- xml_add_sibling(q, 'posacc') 
p %>% xml_add_child('horizpa') %>% 
  xml_add_child('horizpar','A formal accuracy assessment of the horizontal positional information in the data set has not been conducted.')

p %>% 
  xml_add_child('vertacc') %>% 
  xml_add_child('vertaccr','A formal accuracy assessment of the vertical positional information in the data set has either not been conducted, or is not applicable.')

xml_add_sibling(p, 'spdoinfo') %>% 
  xml_add_child('indspref','{{indirect-spatial}}') %>% 
  xml_add_sibling('direct','Point') %>% 
  xml_add_sibling('ptvctinf') %>% 
  xml_add_child('sdtsterm') %>% 
  xml_add_child('sdtstype','Point') %>% 
  xml_add_sibling('ptvctcnt','{{point-count}}')
  
p %>% xml_add_sibling('lineage') %>% 
  xml_add_child('procstep') %>% 
  xml_add_child('procdesc','{{process-description}}') %>% 
  xml_add_sibling('procdate','{{process-date}}')

write_xml(d, file = 'test.xml')

place.template = "{{#states}}<place><placekt>U.S. Department of Commerce, 1987, Codes for the identification of the States, 
                the District of Columbia and the outlying areas of the United States, and associated areas 
                (Federal Information Processing Standard 5-2): Washington, D. C., NIST</placekt>
                <placekey>{{state-name}}</placekey>\n<placekey>{{state-abbr}}</placekey>\n</place>{{/states}}"

state.template = "{{#states}}<place><placekt>none</placekt>
                <placekey>{{state-name}}</placekey>\n</place>{{/states}}"

suppressWarnings(readLines('test.xml')) %>% 
  gsub(pattern = '&gt;',replacement = '>',.) %>% 
  gsub(pattern = '&lt;',replacement = '<',.) %>% 
  gsub(pattern = '<place-template/>', replacement = place.template) %>% 
  gsub(pattern = '<state-template/>', replacement = state.template) %>% 
  cat(file = 'test.xml', sep = '\n')
 
states <- list(c('state-name'='Wisconsin','state-abbr'='WI'),
               c('state-name'='New Hampshire','state-abbr'='NH'))
