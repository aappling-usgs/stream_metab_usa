library(yaml)
library(whisker)
template.file <- 'test.xml'
template <- paste(readLines(template.file ),collapse = '\n')
text <- yaml.load_file('data_release/timeseries-text.yaml')

cat(whisker.render(template, text), file = 'test2.xml')
