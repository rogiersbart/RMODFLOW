## -----------------------------------------------------------------------------
library(RMODFLOW)
rmf_example_models()

## -----------------------------------------------------------------------------
rmf_example_files()

## -----------------------------------------------------------------------------
rmf_example_file("example-model.dis")

## -----------------------------------------------------------------------------
rmf_example_model("example-model")

## -----------------------------------------------------------------------------
dis <- rmf_read_dis(rmf_example_file("example-model.dis"))
class(dis)

## -----------------------------------------------------------------------------
bas <- rmf_read_bas(rmf_example_file("example-model.bas"), dis = dis)
class(bas)

## -----------------------------------------------------------------------------
hed <- rmf_read_fhd(rmf_example_file("example-model.fhd"), dis = dis, bas = bas)
class(hed)

## -----------------------------------------------------------------------------
str(dis)
str(bas)
str(hed)

## ---- eval = FALSE------------------------------------------------------------
#  rmf_copy_to_wd(rmf_example_model("example-model"))

## -----------------------------------------------------------------------------
rmf_plot(dis$top, dis = dis)

## -----------------------------------------------------------------------------
df <- rmf_as_tibble(dis$top, dis = dis)
library(ggplot2)
ggplot(df, aes(x, y, fill = value, group = id)) +
  geom_polygon() +
  scale_fill_gradientn(colours = rev(rainbow(7))) +
  coord_equal()

## -----------------------------------------------------------------------------
rmf_plot(dis$botm, dis = dis, k = 1)
rmf_plot(dis$botm, dis = dis, i = 5)
rmf_plot(dis$botm, dis = dis, j = 5)

## -----------------------------------------------------------------------------
df <- dis$botm %>%
  rmf_as_tibble(dis = dis, k = 1)
ggplot(df, aes(x, y, fill = value, group = id)) +
  geom_polygon() +
  scale_fill_gradientn(colours = rev(rainbow(7))) +
  coord_equal()

## -----------------------------------------------------------------------------
rmf_plot(hed, dis = dis, k = 1, l = 1)
rmf_plot(hed, dis = dis, i = 5, l = 1)
rmf_plot(hed, dis = dis, j = 5)

## -----------------------------------------------------------------------------
df <- rmf_as_tibble(hed, dis = dis, k = 1, l = 1)
ggplot(df, aes(x, y, fill = value, group = id)) +
  geom_polygon() +
  scale_fill_gradientn(colours = rev(rainbow(7))) +
  coord_equal()

## ---- eval = FALSE------------------------------------------------------------
#  df <- rmf_as_sf(dis$top, dis = dis, prj = list(projection = "+init=epsg:31370", origin = c(0,0,0), rotation = 0))
#  library(mapview)
#  mapview(df)

## -----------------------------------------------------------------------------
rmf_example_file("example-model.dis") %>% 
  rmf_read_dis() %>% 
  rmf_plot(.$top, dis = .)

