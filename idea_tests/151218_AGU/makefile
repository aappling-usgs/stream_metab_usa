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

munge : cache/preds_1_PRK.RData cache/preds_2_K.RData cache/preds_3_PR.RData cache/siteyears_raw.RData cache/clean_1_filter.RData cache/clean_2_GAM.RData cache/siteyears_clean.RData cache/siteyears_GAM.RData cache/timing.RData

cache/preds_%.RData : munge/preds_%.R munge/preds_lib.R
	$(CALL_R) "--args sb_user=$(SBUSER) sb_password=$(SBPASS) outfile=$@" munge/preds_$*.R log/preds_$*.Rout

cache/siteyears_raw.RData : munge/siteyears_raw.R munge/siteyears_lib.R cache/preds_3_PR.RData
	$(CALL_R) "--args outfile=$@" munge/siteyears_raw.R log/siteyears_raw.Rout

cache/clean_1_filter.RData : munge/clean_1_filter.R cache/preds_3_PR.RData
	$(CALL_R) "--args outfile=$@" munge/clean_1_filter.R log/clean_1_filter.Rout

cache/clean_2_GAM.RData : munge/clean_2_GAM.R cache/clean_1_filter.RData
	$(CALL_R) "--args outfile=$@" munge/clean_2_GAM.R log/clean_2_GAM.Rout

cache/siteyears_clean.RData : munge/siteyears_clean.R munge/siteyears_lib.R cache/clean_2_GAM.RData
	$(CALL_R) "--args outfile=$@" munge/siteyears_clean.R log/siteyears_clean.Rout

cache/siteyears_GAM.RData : munge/siteyears_GAM.R munge/siteyears_lib.R cache/clean_2_GAM.RData
	$(CALL_R) "--args outfile=$@" munge/siteyears_GAM.R log/siteyears_GAM.Rout

cache/timing.RData : munge/timing.R munge/timing_lib.R cache/siteyears_GAM.RData
	$(CALL_R) "--args outfile=$@" munge/timing.R log/timing.Rout

## figures

figures : out/methods_1_MLE.png out/methods_2_Ksmooth.png out/methods_3_PRfixK.png out/methods_4_filter.png out/methods_5_GAM.png out/storms_1_example.png out/storms_2_regression.png out/seasons_1_example.png out/timing_1_durmap.png out/timing_2_durhist.png out/timing_3_durEvG.png out/timing_4_durvclim.png out/timing_5_peakhist.png out/timing_6_peakEvG.png out/timing_7_peakvsize.png

out/methods_1_MLE.png : src/methods_1_MLE.R
	$(CALL_R) "--args sb_user=$(SBUSER) sb_password=$(SBPASS) outfile=$@" src/methods_1_MLE.R log/methods_1_MLE.Rout

out/methods_2_Ksmooth.png : src/methods_2_Ksmooth.R cache/preds_2_K.RData
	$(CALL_R) "--args outfile=$@" src/methods_2_Ksmooth.R log/methods_2_Ksmooth.Rout

out/methods_3_PRfixK.png : src/methods_3_PRfixK.R cache/preds_1_PRK.RData cache/preds_3_PR.RData
	$(CALL_R) "--args outfile=$@" src/methods_3_PRfixK.R log/methods_3_PRfixK.Rout

out/methods_4_filter.png : src/methods_4_filter.R cache/clean_1_filter.RData
	$(CALL_R) "--args outfile=$@" src/methods_4_filter.R log/methods_4_filter.Rout

out/methods_5_GAM.png : src/methods_5_GAM.R cache/clean_2_GAM.RData
	$(CALL_R) "--args outfile=$@" src/methods_5_GAM.R log/methods_5_GAM.Rout

out/storms_1_example.png : src/storms_1_example.R src/example_lib.R cache/clean_2_GAM.RData
	$(CALL_R) "--args outfile=$@" src/storms_1_example.R log/storms_1_example.Rout

out/storms_2_regression.png : src/storms_2_regression.R src/example_lib.R cache/clean_2_GAM.RData
	$(CALL_R) "--args outfile=$@" src/storms_2_regression.R log/storms_2_regression.Rout

out/seasons_1_example.png : src/seasons_1_example.R src/example_lib.R cache/clean_2_GAM.RData
	$(CALL_R) "--args outfile=$@" src/seasons_1_example.R log/seasons_1_example.Rout

out/timing_%.png : src/timing_%.R src/timing_plots_lib.R cache/timing.RData
	$(CALL_R) "--args outfile=$@" src/timing_$*.R log/timing_$*.Rout

# out/timing_2_durhist.png # both er and gpp, overlapping histograms
# out/timing_3_durEvG.png
# out/timing_4_durvclim.png # or dur v fstfz-lstfz (durvfreeze)
# out/timing_5_peakhist.png # both er and gpp, overlapping
# out/timing_6_peakEvG.png
# out/timing_7_peakvsize.png # watershed area

## numbers

numbers : out/intro_datasize.tsv

out/intro_datasize.tsv : src/intro_datasize.R cache/preds_3_PR.RData cache/siteyears_raw.RData
	$(CALL_R) "--args sb_user=$(SBUSER) sb_password=$(SBPASS) outfile=$@" src/intro_datasize.R log/intro_datasize.Rout

#out/%.tsv : src/%.R
#	$(CALL_R) "--args sb_user=$(SBUSER) sb_password=$(SBPASS) outfile=$@" src/$*.R log/$*.Rout

