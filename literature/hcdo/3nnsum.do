capture log close
log using 3nnsum, replace
ts
set more off
hcsum 3nn B
hcsum 3nn C
hcsum 3nn G
hcsum 3nn W
hcsum 3nn all
set more on
log close
