capture log close
log using 1n4sum, replace
ts
set more off
hcsum 1n4 B
hcsum 1n4 C
hcsum 1n4 G
hcsum 1n4 W
hcsum 1n4 all
set more on
log close
