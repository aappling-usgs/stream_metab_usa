### makefile for Appling et al. poster, AGU 2015

# You'll need to pretty much always pass in SBUSER and SBPASS on the command line, 
# e.g., in .Rproj.user/XXXXXXX/build_options, set 
# makefile_args="SBUSER=xxx@yyy.com SBPASS=qqq"

# Macros

CALL_R = R CMD BATCH  --no-save --no-restore --slave --no-timing

# Main targets

all : munge

#figures

# Rules

## munge

munge : cache/preds_1_PRK.RData cache/preds_2_K.RData cache/preds_3_PR.RData

cache/preds_1_PRK.RData : munge/preds_1_PRK.R munge/preds_lib.R
	$(CALL_R) "--args sb_user=$(SBUSER) sb_password=$(SBPASS) outfile=$@" munge/preds_1_PRK.R

cache/preds_2_K.RData : munge/preds_2_K.R munge/preds_lib.R
	$(CALL_R) "--args sb_user=$(SBUSER) sb_password=$(SBPASS) outfile=$@" munge/preds_2_K.R

cache/preds_3_PR.RData : munge/preds_3_PR.R munge/preds_lib.R
	$(CALL_R) "--args sb_user=$(SBUSER) sb_password=$(SBPASS) outfile=$@" munge/preds_3_PR.R

## figures

#figures : map