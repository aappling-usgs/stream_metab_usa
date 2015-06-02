### makefile for continental-scale stream metabolism project

# Remember to edit with vim/Notepad++ rather than RStudio 
# because of tab-space substitution. RStudio may have a fix for this soon.

# Macros

CALL_R = R CMD BATCH  --no-save --no-restore --slave --no-timing

# Targets

all : p2_metab

# Rules

## p1_import

#p1_import : init_sites

#init_sites : p1_import/out/00_init_sites.Rout

#p1_import/out/00_init_sites.Rout : p1_import/code/00_init_sites.R
#	$(CALL_R) "--args sb_user=SBUSER sb_password=SBPASS outfile='$@'" p1_import/code/00_init_sites.R $@

#import_tsdata
#import_watershed
#import_landcover
#import_climate

## p2_metab

p2_metab : model_metab

model_metab : p2_metab/out/model_metab.Rout

p2_metab/out/model_metab.Rout : 
	$(CALL_R) "--args sb_user=SBUSER sb_password=SBPASS outfile='$@'" p1_import/code/00_init_sites.R $@

## p3_auxvars

## p4_explore

#explore_metab
#map_metab
#predict_metab
