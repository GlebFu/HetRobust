capture log close
log using 34nsum, replace
ts
set more off
hcsum 34n B
hcsum 34n C
hcsum 34n G
hcsum 34n W
hcsum 34n all
set more on
log close
