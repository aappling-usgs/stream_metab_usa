# Overview

Copy the contents of the yeti folder onto a corresponding folder on Yeti. Run
the models using those files.

```
sbatch array.batch
squeue -u aappling
```

# File setup on Yeti

* Our project directory on Yeti:
```
/cxfs/projects/usgs/water/owi/powstreams/stream_metab_usa
```

* Copy .Renviron, array.batch, and config.tsv from ./yeti into the above
directory

* Copy run_sdh_test.R, inputs.Rds, and run_job.R from ./ into the above
directory

* Might need to convert array.batch to UNIX line endings. This is cool:
```
dos2unix array.batch
```


# Reminder: bash ssh/scp commands

# connect to terminal
```
ssh yeti.cr.usgs.gov
```

# file transfer, local to yeti
```
scp array.batch aappling@yeti-dtn1.cr.usgs.gov:/cxfs/projects/usgs/water/owi/powstreams/stream_metab_usa
```

# file transfer, yeti to local w/ rename
```
scp aappling@yeti-dtn1.cr.usgs.gov:/cxfs/projects/usgs/water/owi/powstreams/stream_metab_usa/mydata.txt ./data/mydata2.txt
```

# Package setup on Yeti

* We need devtools, rstan, dplyr, tidyr, ggplot2, deSolve, LakeMetabolizer,
lazyeval, lubridate, magrittr, methods, tibble from CRAN

* We need unitted from GRAN

* We need streamMetabolizer from
`devtools::install_github('aappling-usgs/streamMetabolizer@develop')`