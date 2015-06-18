### makefile for continental-scale stream metabolism project

# Remember to edit with vim/Notepad++ rather than RStudio 
# because of tab-space substitution. RStudio may have a fix for this soon.

# You'll need to pretty much always pass in SBUSER and SBPASS on the command line, 
# e.g., in .Rproj.user/XXXXXXX/build_options, set 
# makefile_args="SBUSER=xxx@yyy.com SBPASS=qqq"

# Macros

CALL_R = R CMD BATCH  --no-save --no-restore --slave --no-timing

# Main targets

all : p1_import

#p2_metab

# Rules

## p1_import

p1_import : init_sites add_nwis_data
# add_nldas_data add_calc_data

init_sites : p1_import/out/00_init_sites.Rout
p1_import/out/00_init_sites.Rout : p1_import/code/00_init_sites.R p1_import/code/process_make_args.R
	$(CALL_R) "--args sb_user=$(SBUSER) sb_password=$(SBPASS) outfile=$@ update_sitelist=FALSE on_exists=skip delete_all=FALSE verbose=TRUE" p1_import/code/00_init_sites.R $@

add_nwis_data : init_sites $(addprefix p1_import/out/is_ready_nwis_,$(addsuffix .txt,doobs wtr disch stage par))
p1_import/out/is_ready_nwis_%.txt : p1_import/code/01_add_nwis_data.R
	$(CALL_R) "--args sb_user=$(SBUSER) sb_password=$(SBPASS) var=$* on_exists=skip verbose=TRUE" p1_import/code/01_add_nwis_data.R p1_import/out/01_add_nwis_data_$*.Rout

add_nldas_data : init_sites p1_import/out/01_add_nldas_data.Rout
p1_import/out/is_ready_nldas_%.txt : p1_import/code/01_add_nldas_data.R
	$(CALL_R) "--args sb_user=$(SBUSER) sb_password=$(SBPASS) on_exists=skip verbose=TRUE" p1_import/code/01_add_nldas_data.R p1_import/out/01_add_nldas_data_$*.Rout

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

# Special targets
# typing 'make' will invoke the first target entry in the file (all), 
# so these should only get run if you specify them explicitly

reinit_sites : p1_import/out/00_reinit_sites.Rout
p1_import/out/00_reinit_sites.Rout : p1_import/code/00_init_sites.R
	$(CALL_R) "--args sb_user=\"$(SBUSER)\" sb_password=\"$(SBPASS)\" outfile=$@ update_sitelist=TRUE on_exists=skip delete_all=TRUE verbose=TRUE" p1_import/code/00_init_sites.R $@

