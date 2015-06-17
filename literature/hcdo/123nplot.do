capture log close
log using 123nplot, replace
ts

hcplt 123n 05 10 .15 P

* to get both 05 and 10: hcplt 123n 10 10 .15

log close
