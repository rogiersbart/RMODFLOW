## ----setup, echo=FALSE--------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)
knitr::opts_chunk$set(echo = TRUE, collapse = TRUE, comment = "#>")
library(RMODFLOW)

## -----------------------------------------------------------------------------
dis <- rmf_example_file('water-supply-problem.dis') %>%
  rmf_read_dis()
riv <- rmf_example_file('water-supply-problem.riv') %>%
  rmf_read_riv(dis = dis)


## -----------------------------------------------------------------------------
str(riv)

## ---- eval=FALSE--------------------------------------------------------------
#  rmf_write_riv(riv, dis = dis, file = 'input.riv')

## -----------------------------------------------------------------------------
riv_east <- data.frame(i = 1:dis$nrow, j = dis$ncol, k = 1, stage = 0, conductance = 0.02, rbot = -10) %>%
  rmf_create_list(kper = 1)

riv_west <- data.frame(i = 1:dis$nrow, j = 1, k = 1, stage = 0, conductance = 0.02, rbot = -10) %>%
  rmf_create_list(kper = 1)

riv_new <- rmf_create_riv(riv_east, riv_west, dis = dis)
str(riv_new)

## -----------------------------------------------------------------------------
# Currently, no example models with RCH or EVT packages

## -----------------------------------------------------------------------------
dis <- rmf_create_dis(nper = 2)

rch_summer <- rmf_create_array(0.0002, dim = c(dis$nrow, dis$ncol), kper = 1)
rch_winter <- rmf_create_array(0.0003, dim = c(dis$nrow, dis$ncol), kper = 2)

rch <- rmf_create_rch(list(rch_summer, rch_winter), dis = dis)


## -----------------------------------------------------------------------------
str(rch)

