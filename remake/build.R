wd=getwd(); setwd('remake'); remake::make(); setwd(wd) #remake_file='remake.yml'
wd=getwd(); setwd('remake'); remake::make(remake_file='1_site_data.yml'); setwd(wd)
wd=getwd(); setwd('remake'); remake::make(remake_file='1_ldas_host.yml'); setwd(wd)
wd=getwd(); setwd('remake'); remake::make(remake_file='1_timeseries.yml'); setwd(wd)
setwd(wd) # run if remake::make stops on error

# Each line is designed to be a complete script to run with a single Ctrl-Enter.
# If you set your Build | "Custom build script" to this file then it opens right
# away in your editor pane.
