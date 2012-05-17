#!/bin/sh

# compile and run nyota files

nyota=$1".nyota"
il=$1".il"
exe=$1".exe"

echo " >> -cp ../build/ nyota.Main "$nyota" "$il
scala -cp ../build/ nyota.Main $nyota $il

echo " >> ilasm "$il
ilasm $il

echo " >> mono "$exe
mono $exe
