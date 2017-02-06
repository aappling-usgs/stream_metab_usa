#!/bin/bash
export PATH=/usr/local/bin:/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/sbin

chmod 777 unzip
./unzip bundle.zip

mkdir rLibs

Rscript $1 $2

zip job
