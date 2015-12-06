### makefile for Appling et al. poster, AGU 2015

# You'll need to pretty much always pass in SBUSER and SBPASS on the command line, 
# e.g., in .Rproj.user/XXXXXXX/build_options, set 
# makefile_args="SBUSER=xxx@yyy.com SBPASS=qqq"

# Macros

CALL_R = R CMD BATCH  --no-save --no-restore --slave --no-timing

# Main targets

all : munge figures numbers

#figures

# Rules

## munge

munge : cache/preds_1_PRK.RData cache/preds_2_K.RData cache/preds_3_PR.RData cache/siteyears_raw.RData

cache/preds_%.RData : munge/preds_%.R munge/preds_lib.R
	touch $@

#	$(CALL_R) "--args sb_user=$(SBUSER) sb_password=$(SBPASS) outfile=$@" munge/preds_$*.R log/preds_$*.Rout

cache/siteyears_%.RData : munge/siteyears_%.R munge/siteyears_lib.R
	$(CALL_R) "--args sb_user=$(SBUSER) sb_password=$(SBPASS) outfile=$@" munge/siteyears_$*.R log/siteyears_$*.Rout

## figures

figures : out/methods_1_MLE.png out/methods_2_Ksmooth.png

out/%.png : src/%.R
	$(CALL_R) "--args sb_user=$(SBUSER) sb_password=$(SBPASS) outfile=$@" src/$*.R log/$*.Rout

## numbers

numbers : out/intro_datasize.tsv

out/intro_datasize.tsv : src/intro_datasize.R cache/preds_3_PR.RData cache/siteyears_raw.RData
	$(CALL_R) "--args sb_user=$(SBUSER) sb_password=$(SBPASS) outfile=$@" src/intro_datasize.R log/intro_datasize.Rout

#out/%.tsv : src/%.R
#	$(CALL_R) "--args sb_user=$(SBUSER) sb_password=$(SBPASS) outfile=$@" src/$*.R log/$*.Rout

