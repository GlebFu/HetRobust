capture log close
log using jslhc2,replace
ts
version 5.0

clear
set memory 12m
set more off
* hc1 seed: 163564
set seed 163564
clear
* generate the base data
  hcbasebi 100000 jslhc2
log close


