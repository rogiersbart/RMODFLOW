## ----setup, echo = FALSE------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)
knitr::opts_chunk$set(
  echo = TRUE,
  collapse = TRUE,
  comment = "#>"
)
library(RMODFLOW)

## -----------------------------------------------------------------------------
(prj <- rmf_create_prj(origin = c(152082, 168000.2), rotation = -12, crs = 31370))

## -----------------------------------------------------------------------------
dis <- rmf_create_dis(prj = prj)
dis <- rmf_set_prj(dis, prj)

## -----------------------------------------------------------------------------
rmf_plot(dis$top, dis)

## -----------------------------------------------------------------------------
rmf_plot(dis$top, dis, prj = NULL)

## ---- echo = FALSE------------------------------------------------------------
extent <- rmf_extent(dis)
cat('#', 'Start RMODFLOW projection information', '\n')
cat('#', 'Upper left corner:', paste0('(', paste(extent$corners['ul',], collapse = ', '), ')'), '\n')
cat('#', 'Lower left corner:', paste0('(', paste(extent$corners['ll',], collapse = ', '), ')'), '\n')
cat('#', 'Upper right corner:', paste0('(', paste(extent$corners['ur',], collapse = ', '), ')'), '\n')
cat('#', 'Lower right corner:', paste0('(', paste(extent$corners['lr',], collapse = ', '), ')'), '\n')
cat('#', 'Grid angle (in degrees counterclockwise):', ifelse(is.null(prj), 0, prj$rotation), '\n')
cat('#', 'Z coordinate lower left corner:', ifelse(is.null(prj) || is.na(prj$origin[3]), 0, prj$origin[3]), '\n')
if(is.null(prj$crs) || is.na(prj$crs)) {
  cat('#', 'proj4string:', 'NA', '\n')
} else { # if crs is present it should have either epsg, proj4string or wkt (for sf >= 0.9)
  if(!is.na(prj$crs$epsg)) {
    cat('#', 'epsg:', prj$crs$epsg, '\n')
  } else if(!is.na(prj$crs$proj4string)) {
    cat('#', 'proj4string:', prj$crs$proj4string, '\n')
  } else {
    if(packageVersion('sf') >= '0.9') {
      cat('#', 'wkt:', '\n')
      cat(paste('#', prj$crs$wkt), sep = '\n')
    }
  }
}
cat('#', 'End RMODFLOW projection information', '\n')

## -----------------------------------------------------------------------------
dis$lenuni <- 1 # ft
rmf_plot(dis$top, dis, prj = prj) # crs in prj is defined in m

## -----------------------------------------------------------------------------
rmf_as_sf(dis$top, dis)

(sf <- rmf_as_sf(dis$top, dis, as_points = TRUE, id = 'modflow'))
plot(sf)

## -----------------------------------------------------------------------------
rmf_as_stars(dis$botm, dis)

## ---- error = TRUE------------------------------------------------------------
rmf_as_raster(dis$botm, dis)

## -----------------------------------------------------------------------------
rmf_as_raster(dis$top, dis, prj = rmf_create_prj(origin = c(152082, 168000.2), crs = 31370))

## -----------------------------------------------------------------------------
dis <- rmf_create_dis()

# point
pts <- sf::st_sfc(list(sf::st_point(c(150, 312)), sf::st_point(c(500, 500)), sf::st_point(c(850, 566))))
obj <- sf::st_sf(q = c(-500, -400, -300), geom = pts)

# convert to rmf_list
(rlst <- rmf_as_list(obj, dis))

# polygon
p1 <- rbind(c(120, 120), c(120, 760), c(800, 800), c(120, 120))
pol1 <- sf::st_polygon(list(p1))

obj <- sf::st_sf(head = 15, geom = sf::st_sfc(pol1))

# convert to rmf_list and plot
# op = sf::st_intersects
rmf_as_list(obj, dis) %>%
  rmf_plot(dis, k = 1, grid = TRUE) +
  ggplot2::geom_sf(data = obj, inherit.aes = FALSE, alpha = 0.4, fill = 'yellow')

# op = sf::st_covers
rmf_as_list(obj, dis, op = sf::st_covers) %>%
  rmf_plot(dis, k = 1, grid = TRUE) +
  ggplot2::geom_sf(data = obj, inherit.aes = FALSE, alpha = 0.4, fill = 'yellow')

## -----------------------------------------------------------------------------
# 4 cells selected for second point on cell edges
obj <- sf::st_sf(q = c(-500, -400, -300), geom = pts)

rmf_as_list(obj, dis) %>% 
  rmf_plot( dis, k = 1, grid = TRUE) +
  ggplot2::geom_sf(data = obj, inherit.aes = FALSE)

## -----------------------------------------------------------------------------
sfc <- sf::st_sfc(list(sf::st_point(c(100,200)), sf::st_point(c(750, 800)), sf::st_point(c(700, 850))))
obj <- sf::st_sf(q = c(-500, -400, -300), geom = sfc)
dis <- rmf_create_dis()

rmf_as_array(obj, dis = dis, select = 'q')

# add values at same location
rmf_as_array(obj, dis = dis, select = 'q', options = c('MERGE_ALG=ADD'))


## -----------------------------------------------------------------------------
# alternative
rmf_as_list(obj, dis = dis, select = 'q') %>%
  rmf_as_array(dis = dis)

## -----------------------------------------------------------------------------
r <- rmf_create_array(1:prod(dis$nrow, dis$ncol), dim = c(dis$nrow, dis$ncol))
s <- rmf_as_stars(r, dis = dis)

rmf_as_array(s, dis = dis, resample = FALSE)

## -----------------------------------------------------------------------------
# linestring
s1 <- rbind(c(150,312), c(500, 500), c(850, 566))
ls1 <- sf::st_linestring(s1)
s2 <- rbind(c(100,100), c(500, 555))
ls2 <- sf::st_linestring(s2)

sf::st_sf(conductance = c(500, 1000), stage = c(-5, -2), rbot = c(-6, -5), geom = sf::st_sfc(ls1, ls2)) %>%
  rmf_as_list(dis = dis, kper = 1) %>%
  rmf_create_riv(dis = dis)


## -----------------------------------------------------------------------------
prj <- rmf_create_prj(origin = c(152082, 168000.2), rotation = -12, crs = 31370)
dis <- rmf_create_dis(prj = prj)
rmf_has_prj(dis)

rmf_transform_prj(prj, crs = 3812)

## -----------------------------------------------------------------------------
rmf_extent(dis)

