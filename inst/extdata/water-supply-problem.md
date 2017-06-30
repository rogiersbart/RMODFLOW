The water supply problem MODFLOW example is derived from the [corresponding ModelMuse example](https://water.usgs.gov/nrp/gwsoftware/ModelMuse/Help/index.html?water_supply_problem.htm). Note some MODFLOW files were renamed after the file export in ModelMuse, to make file naming more consistent with RMODFLOW conventions (although ModelMuse extensions are/will be supported as well):

``` r
file.rename("water-supply-problem.hob_out", "water-supply-problem.hpr")
file.rename("water-supply-problem.ob_hob", "water-supply-problem.hob")
x <- readLines("water-supply-problem.nam")
y <- gsub("water-supply-problem.hob_out", "water-supply-problem.hpr", x, fixed = TRUE)
y <- gsub("water-supply-problem.ob_hob", "water-supply-problem.hob", y, fixed = TRUE)
cat(y, file = "rocky-mountain-arsenal.nam", sep = "\n")
```
