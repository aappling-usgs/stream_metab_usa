library(magrittr)
library(xml2)

#' @param file.out the file to write the template to
create_fgdc_template <- function(file.out, multiple_entities=FALSE){
  tempxml <- tempfile(fileext = '.xml')
  
  d <- xml_new_document()
  mt <-  xml_add_child(d, "metadata")
  m <- xml_add_child(mt, "idinfo")
  
  #### 1.1-1.4 Bibliodata ####
  m %>%  xml_add_child("citation") %>%
    xml_add_child("citeinfo") %>%
    xml_add_child("origin-template") %>%
    xml_add_sibling('pubdate', "{{pubdate}}") %>%
    xml_add_sibling('title', "{{title}}") %>%
    xml_add_sibling('geoform', "{{file-format}}") %>%
    xml_add_sibling('publish', "{{data-publisher}}") %>%
    xml_add_sibling('onlink', "{{doi}}") %>% 
    xml_add_sibling('lworkcit-template')
  
  m %>%
    xml_add_child('descript') %>%
    xml_add_child("abstract",'{{abstract}}') %>%
    xml_add_sibling("purpose", '{{purpose}}')
  
  ti <- m %>%
    xml_add_child('timeperd') %>%
    xml_add_child("timeinfo") 
  ti %>% 
    xml_add_child("rngdates") %>%
    xml_add_child('begdate','{{start-date}}') %>%
    xml_add_sibling('enddate','{{end-date}}')
  ti %>% 
    xml_add_sibling('current','model estimates')
  m %>%
    xml_add_child('status') %>%
    xml_add_child("progress", "Complete") %>% 
    xml_add_sibling('update','{{update}}') 
  
  #### 1.5 Spatial Domain ####
  m %>% 
    xml_add_child('spdom') %>% 
    xml_add_child('descgeog', '{{descgeog}}') %>% 
    xml_add_sibling('bounding') %>% 
    xml_add_child('westbc', "{{wbbox}}") %>% 
    xml_add_sibling('eastbc', "{{ebbox}}") %>% 
    xml_add_sibling('northbc', "{{nbbox}}") %>% 
    xml_add_sibling('southbc', "{{sbbox}}")
  
  #### 1.6 Keywords ####
  k <- xml_add_child(m, 'keywords') 
  
  k %>% 
    xml_add_child('theme') %>% 
    xml_add_child('themekt','USGS Biocomplexity Thesaurus') %>% 
    xml_add_sibling('themekey','Water courses') %>% 
    xml_add_sibling('themekey','Rivers') %>% 
    xml_add_sibling('themekey','Streams') %>% 
    xml_add_sibling('themekey','Energy metabolism') %>% 
    xml_add_sibling('themekey','Primary production') %>% 
    xml_add_sibling('themekey','Photosynthesis') %>% 
    xml_add_sibling('themekey','Aerobic respiration') %>% 
    xml_add_sibling('themekey','Gas exchange') %>% 
    xml_add_sibling('themekey','Carbon') %>% 
    xml_add_sibling('themekey','Carbon dioxide') %>% 
    xml_add_sibling('themekey','Oxygen') %>% 
    xml_add_sibling('themekey','Dissolved oxygen')
  
  k %>% 
    xml_add_child('theme') %>% 
    xml_add_child('themekt','ISO 19115 Topic Category') %>% 
    xml_add_sibling('themekey','environment') %>% 
    xml_add_sibling('themekey','inlandWaters') %>% 
    xml_add_sibling('themekey','007') %>% 
    xml_add_sibling('themekey','012') 
  
  k %>% 
    xml_add_child('place') %>% 
    xml_add_child('placekt',paste0(
      'Department of Commerce, 1995, Countries, Dependencies, Areas of Special Sovereignty, and ',
      'Their Principal Administrative Divisions,  Federal Information Processing Standard (FIPS) 10-4, ', 
      'Washington, D.C., National Institute of Standards and Technology')) %>% 
    xml_add_sibling('placekey','United States') %>% 
    xml_add_sibling('placekey','US')
  k %>% 
    xml_add_child('place-template')
  
  #### 1.7-1.8 Constraints ####
  p <- xml_add_child(m, 'accconst','none')
  p %>% 
    xml_add_sibling('useconst','{{usage-rules}}')
  
  
  #### 1.9 Point of Contact ####
  pt <-  xml_add_child(m,'ptcontac') 
  
  ci <- pt %>% 
    xml_add_child('cntinfo') %>% 
    xml_add_child('cntperp') 
  
  ci %>% 
    xml_add_child('cntper','{{contact-person}}') %>% 
    xml_add_sibling('cntorg','U.S. Geological Survey')
  
  
  adr <- ci %>% xml_add_sibling('cntaddr') 
  adr %>%
    xml_add_child('addrtype','Mailing and Physical') %>% 
    xml_add_sibling('address','{{contact-address}}') %>% 
    xml_add_sibling('city','{{contact-city}}') %>% 
    xml_add_sibling('state','{{contact-state}}') %>% 
    xml_add_sibling('postal','{{contact-zip}}') %>% 
    xml_add_sibling('country','U.S.A.')
  ci %>% 
    xml_add_sibling('cntpos','{{contact-position}}')
  adr %>% xml_add_sibling('cntvoice','{{contact-phone}}') %>% 
    xml_add_sibling('cntemail','{{contact-email}}')
  
  #### 1.11-1.14 Credit and Cross References ####
  m %>% 
    xml_add_child('datacred','{{funding-credits}}') %>% 
    xml_add_sibling('native','{{build-environment}}') %>% 
    xml_add_sibling('crossref-template')
  
  #### 2 Data Quality ####
  q <- xml_add_child(mt, 'dataqual')
  
  q %>% xml_add_child('attracc') %>% 
    xml_add_child('attraccr','{{accur-test}}')
  
  q %>% xml_add_child('logic','Not applicable') %>% 
    xml_add_sibling('complete','Not applicable')
  
  p <- xml_add_child(q, 'posacc') 
  p %>% xml_add_child('horizpa') %>% 
    xml_add_child('horizpar','A formal accuracy assessment of the horizontal positional information in the dataset was not conducted.')
  p %>% xml_add_child('vertacc') %>% 
    xml_add_child('vertaccr','A formal accuracy assessment of the vertical positional information in the dataset was not conducted.')
  
  #### 2.5 Lineage & Process Steps ####
  l <- xml_add_child(q, 'lineage')
  l %>% xml_add_child('srcinfo-template')
  l %>% xml_add_child('procstep') %>% 
    xml_add_child('procdesc','{{process-description}}') %>% 
    xml_add_sibling('procdate','{{process-date}}')
  
  #### 3 Spatial Data Organization ####
  xml_add_sibling(q, 'spdoinfo') %>% 
    xml_add_child('indspref','{{indirect-spatial}}') %>% 
    xml_add_sibling('direct','Point') %>% 
    xml_add_sibling('ptvctinf') %>% 
    xml_add_child('sdtsterm') %>% 
    xml_add_child('sdtstype','{{feature-type}}') %>% 
    xml_add_sibling('ptvctcnt','{{feature-count}}')
  
 #### 4 Spatial Reference Information ####
  s <- xml_add_child(mt, 'spref')
  h <- xml_add_child(s, 'horizsys') 
  h %>% 
    xml_add_child('geograph') %>% 
    xml_add_child('latres','{{latitude-res}}') %>% 
    xml_add_sibling('longres','{{longitude-res}}') %>% 
    xml_add_sibling('geogunit','Decimal degrees')
  
  h %>% xml_add_child('geodetic') %>% 
    xml_add_child('horizdn','North American Datum of 1983') %>% 
    xml_add_sibling('ellips','Geodetic Reference System 80') %>% 
    xml_add_sibling('semiaxis','6378137.0') %>% 
    xml_add_sibling('denflat','298.257')
  
  #### 5 Entity and Attribute Information ####
  if(multiple_entities) {
    xml_add_child(mt, 'eainfo') %>% 
      xml_add_child('ent-template')
  } else {
    dt <- xml_add_child(mt, 'eainfo') %>% 
      xml_add_child('detailed')
    dt %>% 
      xml_add_child('enttyp') %>% 
      xml_add_child('enttypl','{{data-name}}') %>% 
      xml_add_sibling('enttypd','{{data-description}}') %>% 
      xml_add_sibling('enttypds','Producer Defined')
    
    dt %>% xml_add_child('attr-template')
  }
  
  #### 6 Distribution ####
  ds <- xml_add_child(mt, 'distinfo')
  db <- xml_add_child(ds, 'distrib')
  ci <-  xml_add_child(db, 'cntinfo')
  ci %>% 
    xml_add_child('cntorgp') %>% 
    xml_add_child('cntorg','U.S. Geological Survey') %>%
    xml_add_sibling('cntper','GS ScienceBase')
  
  ci %>% 
    xml_add_child('cntaddr') %>% 
    xml_add_child('addrtype','mailing address') %>% 
    xml_add_sibling('address','Denver Federal Center, Building 810, Mail Stop 302') %>% 
    xml_add_sibling('city','Denver') %>% 
    xml_add_sibling('state','CO') %>% 
    xml_add_sibling('postal','80255') %>% 
    xml_add_sibling('country','United States')
  ci %>% xml_add_child('cntvoice','1-888-275-8747') %>% 
    xml_add_sibling('cntemail','sciencebase@usgs.gov')
  
  ds %>% xml_add_child('distliab','{{liability-statement}}')
  
  #### 6.4 Standard Order Process ####
  so <- xml_add_child(ds, 'stdorder')
  df <-  xml_add_child(so, 'digform')
  df %>% 
    xml_add_child('digtinfo') %>% 
    xml_add_child('formname','{{file-format}}')
  
  df %>% 
    xml_add_child('digtopt') %>% 
    xml_add_child('onlinopt') %>% 
    xml_add_child('computer') %>% 
    xml_add_child('networka') %>% 
    xml_add_child('networkr','{{doi}}')
  
  xml_add_child(so,'fees','None')
  
  #### 7 Metadata Reference Information ####
  mi <- xml_add_child(mt, 'metainfo')%>% 
    xml_add_child('metd','{{metadata-date}}')
  mt <- mi %>% xml_add_sibling('metc')
  cni <- xml_add_child(mt,'cntinfo')
  cni %>% 
    xml_add_child('cntperp') %>% 
    xml_add_child('cntper','{{metadata-person}}') %>% 
    xml_add_sibling('cntorg','U.S. Geological Survey')
  cni %>% 
    xml_add_child('cntpos','{{metadata-position}}') %>% 
    xml_add_sibling('cntaddr') %>%
    xml_add_child('addrtype','Mailing and Physical') %>% 
    xml_add_sibling('address','{{metadata-address}}') %>% 
    xml_add_sibling('city','{{metadata-city}}') %>% 
    xml_add_sibling('state','{{metadata-state}}') %>% 
    xml_add_sibling('postal','{{metadata-zip}}') %>% 
    xml_add_sibling('country','U.S.A.')
  cni %>% 
    xml_add_child('cntvoice','{{metadata-phone}}') %>% 
    xml_add_sibling('cntfax','{{metadata-fax}}') %>% 
    xml_add_sibling('cntemail','{{metadata-email}}')
  mt %>% 
    # https://www.fgdc.gov/standards/projects/metadata/biometadata/biodatap.pdf
    xml_add_sibling('metstdn','FGDC Biological Data Profile of the Content Standard for Digital Geospatial Metadata') %>% 
    xml_add_sibling('metstdv','FGDC-STD-001.1-1999')
  
  #### Write FGDC template file ####
  write_xml(d, file = tempxml)
  
  #### Section templates ####
  place.template = "<place>
        <placekt>U.S. Department of Commerce, 1987, Codes for the identification of the States, the District of Columbia and the outlying areas of the United States, and associated areas (Federal Information Processing Standard 5-2): Washington, D. C., NIST</placekt>
          {{#states}}
          <placekey>{{state-name}}</placekey>
          <placekey>{{state-abbr}}</placekey>
          {{/states}}
      </place>"
  
  abscontent.template = if(multiple_entities) {
    "{{#entities}}In {{data-name}}: {{#attributes}}{{attr-label}}, {{/attributes}}. {{/entities}}"
  } else {
    "{{#attributes}}{{attr-label}}, {{/attributes}}."
  }
  
  origin.template = "{{#authors}}
  <origin>{{.}}</origin>
  {{/authors}}"
  
  ent.template = "{{#entities}}<detailed>
      <enttyp>
        <enttypl>{{data-name}}</enttypl>
        <enttypd>{{data-description}}</enttypd>
        <enttypds>Producer Defined</enttypds>
      </enttyp>
      <attr-template/>
    </detailed>\n{{/entities}}"
  
  attr.template = "{{#attributes}}<attr>
        <attrlabl>{{attr-label}}</attrlabl>
        <attrdef>{{attr-def}}</attrdef>
        <attrdefs>{{attr-defs}}</attrdefs>
        <attrdomv>
          <rdom>
            <rdommin>{{data-min}}</rdommin>
            <rdommax>{{data-max}}</rdommax>
            <attrunit>{{data-units}}</attrunit>
          </rdom>
        </attrdomv>
      </attr>\n{{/attributes}}"
  
  lworkcit.template = "{{#larger-cites}}<lworkcit>
    <citeinfo>
      {{#authors}}
      <origin>{{.}}</origin>
      {{/authors}} 
      <pubdate>{{pubdate}}</pubdate>
      <title>{{title}}</title>
      {{#link}}
      <onlink>{{.}}</onlink>
      {{/link}} 
    </citeinfo>
  </lworkcit>\n{{/larger-cites}}"
  
  crossref.template = "{{#cross-cites}}<crossref>
    <citeinfo>
      {{#authors}}
      <origin>{{.}}</origin>
      {{/authors}} 
      <pubdate>{{pubdate}}</pubdate>
      <title>{{title}}</title>
      {{#link}}
      <onlink>{{.}}</onlink>
      {{/link}} 
    </citeinfo>
  </crossref>\n{{/cross-cites}}"
  
  srcinfo.template = paste(c(
    "{{#source-cites}}<srcinfo>",
    paste0("        ", c(
      "<srccite>",
      "  <citeinfo>",
      "    {{#authors}}",
      "    <origin>{{.}}</origin>",
      "    {{/authors}}",
      "    <pubdate>{{pubdate}}</pubdate>",
      "    <title>{{title}}</title>",
      "    <geoform>{{file-format}}</geoform>",
      # "    <pubinfo>",
      # "      <pubplace>{{pubplace}}</pubplace>",
      # "      <publish>{{publish}}</publish>",
      # "    </pubinfo>",
      "    {{#link}}",
      "    <onlink>{{.}}</onlink>",
      "    {{/link}}",
      "  </citeinfo>",
      "</srccite>",
      "<typesrc>online</typesrc>",
      "<srctime>",
      "  <timeinfo>",
      "    {{#sngdate}}<sngdate>",
      "      <caldate>{{caldate}}</caldate>",
      "    </sngdate>{{/sngdate}}",
      "    {{#rngdates}}<rngdates>",
      "      <begdate>{{begdate}}</begdate>",
      "      <enddate>{{enddate}}</enddate>",
      "    </rngdates>{{/rngdates}}",
      "  </timeinfo>",
      "  <srccurr>{{srccurr}}</srccurr>",
      "</srctime>",
      "<srccitea>{{srccitea}}</srccitea>",
      "<srccontr>{{srccontr}}</srccontr>"), collapse="\n"),
    "      </srcinfo>",
    "{{/source-cites}}"), collapse="\n")
    
  suppressWarnings(readLines(tempxml)) %>% 
    gsub(pattern = '&gt;',replacement = '>',.) %>% 
    gsub(pattern = '&lt;',replacement = '<',.) %>% 
    gsub(pattern = '<place-template/>', replacement = place.template) %>% 
    gsub(pattern = '<origin-template/>', replacement = origin.template) %>% 
    gsub(pattern = '<ent-template/>', replacement = ent.template) %>%
    gsub(pattern = '<attr-template/>', replacement = attr.template) %>% 
    gsub(pattern = '<lworkcit-template/>', replacement = lworkcit.template) %>% 
    gsub(pattern = '<crossref-template/>', replacement = crossref.template) %>% 
    gsub(pattern = '<abscontent-template/>', replacement = abscontent.template) %>% 
    gsub(pattern = '<srcinfo-template/>', replacement = srcinfo.template) %>% 
    cat(file = file.out, sep = '\n')
  
  return(file.out)
}