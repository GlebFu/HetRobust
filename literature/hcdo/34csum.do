capture log close
log using 34csum, replace
ts
set more off
hcsum 34c B
hcsum 34c C
hcsum 34c G
hcsum 34c W
hcsum 34c all
set more on
log close
