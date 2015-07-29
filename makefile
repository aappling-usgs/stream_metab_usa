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

add_site_metadata : init_sites $(addprefix p1_import/out/is_ready_meta_,$(addsuffix .txt,basic))
p1_import/out/is_ready_meta_%.txt : p1_import/code/00_add_site_metadata.R
	$(CALL_R) "--args sb_user=$(SBUSER) sb_password=$(SBPASS) type=$* on_exists=skip verbose=TRUE outfile=$@" p1_import/code/00_add_site_metadata.R p1_import/out/00_add_site_metadata_$*.Rout

add_nwis_data : init_sites $(addprefix p1_import/out/is_ready_nwis_,$(addsuffix .txt,doobs wtr disch stage par airtemp))
p1_import/out/is_ready_nwis_%.txt : p1_import/code/01_add_nwis_data.R p1_import/in/date_range.tsv
	$(CALL_R) "--args sb_user=$(SBUSER) sb_password=$(SBPASS) var=$* on_exists=replace verbose=TRUE" p1_import/code/01_add_nwis_data.R p1_import/out/01_add_nwis_data_$*.Rout

add_nldas_data : init_sites $(addprefix p1_import/out/is_ready_nldas_,$(addsuffix .txt,baro sw))
p1_import/out/is_ready_nldas_%.txt : p1_import/code/02_add_nldas_data.R p1_import/in/date_range.tsv
	$(CALL_R) "--args sb_user=$(SBUSER) sb_password=$(SBPASS) var=$* on_exists=merge verbose=TRUE" p1_import/code/02_add_nldas_data.R p1_import/out/02_add_nldas_data_$*.Rout

add_calc_data : init_sites $(addprefix p1_import/out/is_ready_calc_,$(addsuffix .txt,suntime_calcLon par_calcLat depth_calcDisch dosat_calcGGbts sitetime_calcLon))
p1_import/out/is_ready_calc_%.txt : p1_import/code/04_add_calc_data.R
	$(CALL_R) "--args sb_user=$(SBUSER) sb_password=$(SBPASS) var_src=$* on_exists=skip verbose=TRUE" p1_import/code/04_add_calc_data.R p1_import/out/04_add_calc_data_$*.Rout

## p2_metab

#p2_metab : model_metab

model_metab : p2_metab/out/model_metab.Rout
p2_metab/out/01_model_metab.Rout : p2_metab/code/01_model_metab.R
	$(CALL_R) "--args sb_user=$(SBUSER) sb_password=$(SBPASS) tag=0.0.4 strategy=nighttime_k model=metab_night model_args=list() cluster=condor_cluster post_best=TRUE verbose=TRUE" p2_metab/code/01_model_metab.R $@

p2_metab/out/02_post_metab.Rout : p2_metab/code/02_post_metab.R
	$(CALL_R) "--args sb_user=$(SBUSER) sb_password=$(SBPASS) date=150714 tag=0.0.2 strategy=local_makefile_run on_run_exists=skip all_out_file= on_ts_exists=skip verbose=TRUE" p2_metab/code/02_post_metab.R $@

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

