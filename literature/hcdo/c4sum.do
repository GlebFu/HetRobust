capture log close
log using c4sum, replace
ts
set more off
hcsum c4 B
hcsum c4 C
hcsum c4 G
hcsum c4 W
hcsum c4 all
set more on
log close
