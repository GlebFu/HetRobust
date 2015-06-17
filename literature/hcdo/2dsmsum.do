capture log close
log using 2dsmsum, replace
ts
set more off
hcsum 2dsm B
hcsum 2dsm C
hcsum 2dsm G
hcsum 2dsm W
hcsum 2dsm all
set more on
log close
