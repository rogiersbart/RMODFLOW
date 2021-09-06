## ----setup, echo=FALSE--------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)
knitr::opts_chunk$set(echo = TRUE, collapse = TRUE, comment = "#>")
library(RMODFLOW)

## -----------------------------------------------------------------------------
dis <- rmf_example_model("example-model.dis") %>% 
  rmf_read_dis()

rmf_plot(dis$top, dis = dis)

## -----------------------------------------------------------------------------
ds <- rmf_create_array(0, dim = c(dis$nrow, dis$ncol))
ds[, 1] <- 1
ds[, dis$ncol] <- -1

rmf_plot(ds, dis = dis, type = "factor")

# not named; labels are assigned to sorted factor levels 
lv <- c("Positive", "Zero", "Negative")
rmf_plot(ds, dis = dis, type = "factor", levels = lv)

# named
lv <- c("1" = "Positive", "0" = "Zero", "-1" = "Negative")
rmf_plot(ds, dis = dis, type = "factor", levels = lv)

## -----------------------------------------------------------------------------
rmf_plot(dis$top, dis = dis, type = "grid")

## -----------------------------------------------------------------------------
rmf_plot(dis$top, dis = dis, gridlines = TRUE)
rmf_plot(dis$top, dis = dis, gridlines = "pink")

## -----------------------------------------------------------------------------
rmf_plot(dis$top, dis = dis, type = "contour")
rmf_plot(dis$top, dis = dis, type = "contour", binwidth = 2, label = FALSE)

## -----------------------------------------------------------------------------
rmf_plot(dis$top, dis = dis, type = "vector")
rmf_plot(dis$top, dis = dis, type = "vector", vecint = 3)

u <- rmf_create_array(0.5, dim = c(dis$nrow, dis$ncol))
v <- rmf_create_array(-1, dim = c(dis$nrow, dis$ncol))
uvw <- list(u = u, v = v)

rmf_plot(dis$top, dis = dis, type = "vector", uvw = uvw)


## -----------------------------------------------------------------------------
rmf_plot(dis$top, dis = dis, colour_palette = heat.colors, nlevels = 8)
rmf_plot(dis$top, dis = dis, colour_palette = heat.colors, nlevels = 8, type = 'vector')

## -----------------------------------------------------------------------------
mask <- rmf_create_array(0, dim = c(dis$nrow, dis$ncol))
mask[1:dis$nrow/2, ] <- 1

rmf_plot(dis$top, dis = dis, mask = mask)

# water-supply problem
dis_ws <- rmf_example_file("water-supply-problem.dis") %>%
  rmf_read_dis()
bas_ws <- rmf_example_file("water-supply-problem.bas") %>%
  rmf_read_bas(dis = dis_ws)

# without bas
rmf_plot(dis_ws$top, dis = dis_ws)

# with bas
rmf_plot(dis_ws$top, dis = dis_ws, bas = bas_ws)


## -----------------------------------------------------------------------------
rmf_plot(dis$top, dis = dis) +
  rmf_plot(dis$top, dis = dis, type = "vector", add = TRUE)

rmf_plot(dis$top, dis = dis) +
  ggplot2::scale_fill_viridis_c()

## -----------------------------------------------------------------------------
rmf_plot(dis$top, dis = dis, legend = FALSE)
rmf_plot(dis$top, dis = dis, legend = "Topography (m)")

# This also works for added layers:
rmf_plot(dis$top, dis = dis) +
  rmf_plot(dis$top, dis = dis, type = "vector", add = TRUE, colour_palette = heat.colors, legend = TRUE)

## -----------------------------------------------------------------------------
rmf_plot(dis$botm, dis = dis, k = 1)
rmf_plot(dis$botm, dis = dis, j = 5, gridlines = TRUE)

## -----------------------------------------------------------------------------
# layer 1 + column 5
rmf_plot(dis$botm, dis = dis, k = 1) +
  rmf_plot(dis$botm, dis = dis, j = 5, add = TRUE)

## -----------------------------------------------------------------------------
hed <- rmf_example_file("water-supply-problem.fhd") %>%
  rmf_read_hed(dis = dis_ws, binary = FALSE)

rmf_plot(hed, dis = dis_ws, bas = bas_ws, k = 1)
rmf_plot(hed, dis = dis_ws, bas = bas_ws, j = 5, l = 10)

## -----------------------------------------------------------------------------
rmf_plot(hed, dis = dis_ws, i = 20, j = 25, k = 1)

## -----------------------------------------------------------------------------
ls <- data.frame(i = c(5, 5, 6), 
                 j = 5, 
                 k = 1, 
                 q = c(-500, -500, -1500)) %>%
  rmf_create_list()

rmf_plot(ls, dis = dis, k = 1)
rmf_plot(ls, dis = dis, k = 1, active_only = TRUE)
rmf_plot(ls, dis = dis, k = 1, variable = 'q', active_only = TRUE, gridlines = TRUE)

# fun = mean, notice change in scale
rmf_plot(ls, dis = dis, k = 1, variable = 'q', active_only = TRUE, fun = mean)


## -----------------------------------------------------------------------------
rch <- rnorm(dis_ws$nrow*dis_ws$ncol, mean = 0.0002, sd = 0.00008) %>%
  rmf_create_array(dim = c(dis_ws$nrow, dis_ws$ncol), kper = 1) %>%
  rmf_create_rch(dis = dis_ws)

rmf_plot(rch, dis = dis_ws)

## -----------------------------------------------------------------------------
riv_ws <- rmf_example_file("water-supply-problem.riv") %>%
   rmf_read_riv(dis = dis_ws)

rmf_plot(riv_ws, dis = dis_ws, k = 1, variable = 'conductance')

## -----------------------------------------------------------------------------
lpf_ws <- rmf_example_file("water-supply-problem.lpf") %>%
  rmf_read_lpf(dis = dis_ws)

rmf_plot(lpf_ws$hk, dis = dis_ws, k = 1)

## -----------------------------------------------------------------------------
# No example problem with HUF package atm


## -----------------------------------------------------------------------------
m <- rmf_example_file("rocky-mountain-arsenal.nam") %>%
  rmf_read(verbose = FALSE)

rmf_plot(m, k = 1)


## -----------------------------------------------------------------------------
rmf_plot(m, k = 1, omit = 'CHD', gridlines = FALSE)

## -----------------------------------------------------------------------------
m <- rmf_example_file("example-model.nam") %>%
  rmf_read(verbose = FALSE)

# nothing in layer 2
rmf_plot(m, k = 2)

# force showing all 
rmf_plot(m, k = 2, to_k = TRUE)

