#!/bin/bash
export PATH=/usr/local/bin:/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/sbin

mkdir rLibs

Rscript -e "parallel:::.slaveRSOCK()" MASTER=130.11.177.196 PORT=4043 OUT=log.log TIMEOUT=2592000 METHODS=TRUE XDR=TRUE 
