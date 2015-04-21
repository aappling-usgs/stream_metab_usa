### makefile for continental-scale stream metabolism project

# Remember to edit with vim/Notepad++ rather than RStudio 
# because of tab-space substitution. RStudio may have a fix for this soon.

# Macros

CALL_R = R CMD BATCH  --no-save --no-restore --slave --no-timing

# Targets

all : p1_import

# Rules

#- p1_import

p1_import : init_sites

init_sites : p1_import/out/00_init_sites.Rout

p1_import/out/00_init_sites.Rout : p1_import/code/00_init_sites.R
	$(CALL_R) "--args sb_user=SBUSER sb_password=SBPASS outfile='$@'" p1_import/code/00_init_sites.R $@
#import_tsdata
#import_watershed
#import_landcover
#import_climate

#- p2_munge ?

#munge_tsdata
#munge_watershed
#munge_landcover
#munge_climate

#- p3_model

#model_metab
#model_light

#- p4_explore

#explore_metab
#map_metab
#predict_metab
