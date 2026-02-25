# Script to create wagepan data files in various formats
# Run this once to populate the data/ folder with the needed files.

library(tidyverse)
library(haven)
library(wooldridge)
library(labelled)

data(wagepan)

# Select a subset of variables for examples
wagepan_eksempel <- wagepan %>%
  select(nr, year, hours, lwage, educ, black, hisp, married, union, exper)

# Add variable labels (for Stata/SPSS-like labelled data)
var_label(wagepan_eksempel) <- list(
  nr      = "Person identifier",
  year    = "Year",
  hours   = "Annual hours worked",
  lwage   = "Log hourly wage",
  educ    = "Years of education",
  black   = "Black",
  hisp    = "Hispanic",
  married = "Married",
  union   = "Union member",
  exper   = "Years of experience"
)

# Save in all needed formats
saveRDS(wagepan_eksempel, "data/wagepan_eksempel.rds")

wagepan_eksempel_rdata <- wagepan_eksempel
save(wagepan_eksempel_rdata, file = "data/wagepan_eksempel.Rdata")

write_csv(wagepan_eksempel, "data/wagepan_eksempel.csv")

openxlsx::write.xlsx(wagepan_eksempel, file = "data/wagepan_eksempel.xlsx")

write_dta(wagepan_eksempel, path = "data/wagepan_eksempel.dta")

write_sav(wagepan_eksempel, path = "data/wagepan_eksempel.sav")

write_sas(wagepan_eksempel, path = "data/wagepan_eksempel.sas7bdat")

cat("All wagepan data files created successfully in data/\n")
