## ----setup, echo=FALSE--------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)
knitr::opts_chunk$set(echo = TRUE, collapse = TRUE, comment = "#>")
library(RMODFLOW)

## -----------------------------------------------------------------------------
dis <- rmf_example_file("example-model.dis") %>%
  rmf_read_dis()

# chd <- rmf_example_file("example-model.chd") %>%
#   rmf_read_chd(dis = dis) # FIX failing for me at the moment
# str(chd)

## -----------------------------------------------------------------------------
mlt <- rmf_example_file("example-model.mlt") %>%
  rmf_read_mlt(dis = dis)

zon <- rmf_example_file("example-model.zon") %>%
  rmf_read_zon(dis = dis)

lpf <- rmf_example_file("example-model.lpf") %>%
  rmf_read_lpf(dis = dis, mlt = mlt, zon = zon)
str(lpf)

## -----------------------------------------------------------------------------
# No example with continuous boundary package defined by parameters

## -----------------------------------------------------------------------------
rmf_calculate_array(dis = dis, 
                    layer = 1:2,
                    mltarr = mlt$rmlt[c(1, 9)],
                    zonarr = NULL,
                    parval = 1.5)

## -----------------------------------------------------------------------------
pvl <- rmf_example_file("example-model.pval") %>%
  rmf_read_pval()
str(pvl)

## -----------------------------------------------------------------------------
lst <- data.frame(i = 1,
                  j = 1:dis$ncol,
                  k = 1,
                  q = 500) %>%
  rmf_create_list(kper = 1)

lst_p <- rmf_create_parameter(lst, parnam = "WELLS", parval = 1.5)
str(lst_p)


## -----------------------------------------------------------------------------
ar <- rmf_create_array(12e-1, dim = c(dis$nrow, dis$ncol)) %>%
  rmf_create_parameter(parnam = "RCH_1", parval = 1.5, kper = 1) 
ar

ar2 <- rnorm(dis$nrow*dis$ncol, mean = 12e-1, sd = 0.5) %>%
  rmf_create_array(dim = c(dis$nrow, dis$ncol)) %>%
  rmf_create_parameter(parnam = "RCH_2", parval = 1.5, kper = 1) 
ar2

## -----------------------------------------------------------------------------
mlt <- rmf_create_mlt(mltnam = "RCH", rmlt = rmf_create_array(0.0002, dim = c(dis$nrow, dis$ncol)))
zon <- rmf_create_zon(zonnam = "RCH", izon = rmf_create_array(1:4, dim = c(dis$nrow, dis$ncol)))
p <- rmf_create_parameter(dis = dis, kper = 1, parnam = "RCH_1", parval = 1.5,
                          mltnam = "RCH", zonnam = "RCH", iz = list(c(1,2)),
                          mlt = mlt, zon = zon)
p

## -----------------------------------------------------------------------------
hk <- rmf_create_array(10, dim = c(dis$nrow, dis$ncol, dis$nlay)) %>%
  rmf_create_parameter(parnam = "HK_1", parval = 0.5, layer = 1:dis$nlay,
                       partyp = "HK")
str(hk)

## -----------------------------------------------------------------------------
mlt <- rmf_create_mlt(mltnam = "HK", rmlt = rmf_create_array(rnorm(dis$nrow*dis$ncol, 8, 8), dim = c(dis$nrow, dis$ncol)))
zon <- rmf_create_zon(zonnam = "HK", izon = rmf_create_array(1, dim = c(dis$nrow, dis$ncol)))
p <- rmf_create_parameter(dis = dis, layer = 1:dis$nlay, parnam = "HK_1", parval = 1.5,
                          mltnam = "HK", zonnam = "HK", iz = list(1),
                          partyp = "HK", mlt = mlt, zon = zon)
str(p)

## -----------------------------------------------------------------------------
rmf_create_pval(parnam = c("HK_1", "HK_2"),
               parval = c(12, 0.01))

## -----------------------------------------------------------------------------
hk_fact <- 1.5
hk <- rmf_create_array(10, dim = c(dis$nrow, dis$ncol, dis$nlay)) * hk_fact
lpf <- rmf_create_lpf(dis = dis, hk = hk)


