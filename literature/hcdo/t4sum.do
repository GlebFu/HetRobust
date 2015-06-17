capture log close
log using t4sum, replace
ts
    hcsum t4 B
    hcsum t4 C
    hcsum t4 G
    hcsum t4 W
    hcsum t4 all
log close
