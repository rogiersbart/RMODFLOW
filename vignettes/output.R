## ----setup, echo=FALSE--------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)
knitr::opts_chunk$set(echo = TRUE, collapse = TRUE, comment = "#>")
library(RMODFLOW)

## -----------------------------------------------------------------------------
dis <- rmf_example_file("water-supply-problem.dis") %>%
  rmf_read_dis()

head <- rmf_example_file("water-supply-problem.fhd") %>% 
  rmf_read_hed(dis = dis, binary = FALSE)


## -----------------------------------------------------------------------------
ddn <- rmf_example_file("water-supply-problem.fdn") %>% 
  rmf_read_ddn(dis = dis, binary = FALSE)

## -----------------------------------------------------------------------------
str(head)

## -----------------------------------------------------------------------------
rmf_example_file("water-supply-problem.fhd") %>% 
  rmf_read_hed(dis = dis, binary = FALSE, timesteps = c(1,16)) %>%
  str()

## -----------------------------------------------------------------------------
# read bas to remove no-flow values from plot.
# Alternatively, this could have been done by supplying a bas object to rmf_read_hed
bas <- rmf_example_file("water-supply-problem.bas") %>% 
  rmf_read_bas(dis = dis)

rmf_plot(head, dis = dis, bas = bas, k = 1)

## -----------------------------------------------------------------------------
rmf_plot(head, dis = dis, bas = bas, k = 1, kper = 1, kstp = 16)
rmf_plot(head, dis = dis, bas = bas, k = 1, nstp = 12)

## -----------------------------------------------------------------------------
m <- rmf_example_file('rocky-mountain-arsenal.nam') %>%
  rmf_read(output = TRUE, verbose = FALSE)

rmf_plot(m$head, dis = m$dis, bas = m$bas, j = 25, grid = TRUE, saturated = TRUE) # default
rmf_plot(m$head, dis = m$dis, bas = m$bas, j = 25, grid = TRUE, saturated = FALSE)

## -----------------------------------------------------------------------------
# same results
rmf_plot(m$head, dis = m$dis, bas = m$bas, k = 1, saturated = TRUE) # default
rmf_plot(m$head, dis = m$dis, bas = m$bas, k = 1, saturated = FALSE)

## -----------------------------------------------------------------------------
wt <- rmf_convert_hed_to_water_table(head, l = 10)
str(wt)

## -----------------------------------------------------------------------------
cbc <- rmf_example_file("water-supply-problem.cbc") %>%
  rmf_read_cbc(dis = dis)
str(cbc)

## -----------------------------------------------------------------------------
rmf_example_file("water-supply-problem.cbc") %>%
  rmf_read_cbc(dis = dis, fluxes = c("wells", "flow_right_face")) %>%
  str()

## -----------------------------------------------------------------------------
rmf_plot(cbc, dis = dis, bas = bas, k = 1, flux = "storage")
rmf_plot(cbc, dis = dis, bas = bas, k = 1, flux = "river_leakage")

## -----------------------------------------------------------------------------
rmf_plot(head, dis = dis, bas = bas, k = 1, l = 16) +
  rmf_plot(cbc, dis = dis, bas = bas, k = 1, l = 16, flux = "darcy", type = "vector", add = TRUE)

## -----------------------------------------------------------------------------
bud <- rmf_example_file("water-supply-problem.lst") %>%
  rmf_read_bud()
str(bud)

## -----------------------------------------------------------------------------
rmf_plot(bud, dis = dis)

## -----------------------------------------------------------------------------
rmf_plot(bud, dis = dis, fluxes = c("wells", "river_leakage"))

## -----------------------------------------------------------------------------
rmf_plot(bud, dis = dis, net = TRUE)

## -----------------------------------------------------------------------------
rmf_plot(bud, dis = dis, what = "cumulative")
rmf_plot(bud, dis = dis, what = "discrepancy")

## -----------------------------------------------------------------------------
rmf_plot(bud, dis = dis, type = "bar")

## -----------------------------------------------------------------------------
rmf_plot(bud, dis = dis, type = "bar", timesteps = c(1, 16))

## -----------------------------------------------------------------------------
dis_ss <- rmf_example_file("example-model.dis") %>%
  rmf_read_dis()
bud_ss <- rmf_example_file("example-model.lst") %>%
  rmf_read_bud()

rmf_plot(bud_ss, dis = dis_ss)

## -----------------------------------------------------------------------------
hpr <- rmf_example_file("water-supply-problem.hob_out") %>%
  rmf_read_hpr()
str(hpr)

## -----------------------------------------------------------------------------
rmf_plot(hpr)

## -----------------------------------------------------------------------------
rmf_plot(hpr, type = "histogram")

## -----------------------------------------------------------------------------
rmf_plot(hpr, type = "residual") + ggplot2::theme(axis.text.x.bottom = ggplot2::element_text(angle = 90))

## -----------------------------------------------------------------------------
rmf_performance(hpr)

## -----------------------------------------------------------------------------
oc <- rmf_example_file("water-supply-problem.oc") %>%
  rmf_read_oc(dis = dis)
str(oc)

## -----------------------------------------------------------------------------
rmf_create_oc(dis = dis, print_head = TRUE, 
              save_head = c(rep(FALSE, sum(dis$nstp)/2), rep(TRUE, sum(dis$nstp)/2) )) %>%
  str()

## -----------------------------------------------------------------------------
rmf_create_oc(dis = dis,
              ihedun = 50,
              save_drawdown = TRUE, iddnun = 60) %>% 
  str()

## -----------------------------------------------------------------------------
rmf_create_oc(dis = dis, chedfm = 0) %>% 
  str()

## -----------------------------------------------------------------------------
oc <- rmf_create_oc(dis = dis,
                    save_drawdown = TRUE, cddnfm = 0, iddnun = 50, 
                    save_head = FALSE, 
                    print_budget = rep(c(FALSE, TRUE), sum(dis$nstp)/2))
str(oc)

