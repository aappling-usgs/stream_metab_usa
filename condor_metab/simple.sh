#!/bin/bash
export PATH=/usr/local/bin:/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/sbin:/usr/local/lib

mkdir rLibs

Rscript -e "parallel:::.slaveRSOCK()" MASTER=130.11.177.17 PORT=4043 OUT=log.log TIMEOUT=2592000 METHODS=TRUE XDR=TRUE 
