## ----setup, echo=FALSE--------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)
knitr::opts_chunk$set(echo = TRUE, collapse = TRUE, comment = "#>")
library(RMODFLOW)

## -----------------------------------------------------------------------------
rma <- rmf_example_file("rocky-mountain-arsenal.nam") %>%
  rmf_read()

## -----------------------------------------------------------------------------
modflow <- rmf_create(rma$dis, rma$bas, rma$lpf, rma$oc, rma$pcg, rma$wel, rma$chd, cbc = 88)
str(modflow)

## ---- eval = FALSE------------------------------------------------------------
#  rmf_write(modflow, file = "input.nam", verbose = FALSE)

## ---- eval = FALSE------------------------------------------------------------
#  rmf_write(modflow, file = "input.nam", verbose = FALSE, exclude = c("wel", "chd"))

