### makefile for continental-scale stream metabolism project

# Remember to edit with vim/Notepad++ rather than RStudio 
# because of tab-space substitution. RStudio may have a fix for this soon.

# Macros

CALL_R = R CMD BATCH  --no-save --no-restore --slave --no-timing

# Special targets

reinit_sites : p1_import/code/process_make_args.R p1_import/code/00_init_sites.R
	$(CALL_R) "--args sb_user=\"$(SBUSER)\" sb_password=\"$(SBPASS)\" outfile=$@ update_sitelist=TRUE replace_existing=FALSE delete_all=TRUE verbose=TRUE" p1_import/code/00_init_sites.R p1_import/out/00_reinit_sites.Rout

# Main targets

all : p1_import

#p2_metab

# Rules

## p1_import

p1_import : init_sites 
#add_nwis_data add_nldas_data add_calc_data

init_sites : p1_import/out/00_init_sites.Rout

p1_import/out/00_init_sites.Rout : p1_import/code/00_init_sites.R p1_import/code/process_make_args.R
	$(CALL_R) "--args sb_user=$(SBUSER) sb_password=$(SBPASS) outfile=$@ update_sitelist=TRUE replace_existing=FALSE delete_all=FALSE verbose=TRUE" p1_import/code/00_init_sites.R $@

#import_tsdata
#import_watershed
#import_landcover
#import_climate

## p2_metab

#p2_metab : model_metab

#model_metab : p2_metab/out/model_metab.Rout

#p2_metab/out/model_metab.Rout : 
#$(CALL_R) "--args sb_user=SBUSER sb_password=SBPASS outfile='$@'" p1_import/code/00_init_sites.R $@

## p3_auxvars

## p4_explore

#explore_metab
#map_metab
#predict_metab
