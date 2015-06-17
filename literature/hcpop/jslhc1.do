capture log close
log using jslhc1,replace
ts
version 5.0
clear
set memory 12m
set more off
* hc1 seed: 163564
set seed 163564
clear
* generate the base data
  hcbase 100000 jslhc1
log close


