capture log close
log using 3c4sum, replace
ts
set more off
hcsum 3c4 B
hcsum 3c4 C
hcsum 3c4 G
hcsum 3c4 W
hcsum 3c4 all
set more on
log close
