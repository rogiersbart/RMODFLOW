
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The {RMODFLOW} R package<br><small><font color="#999">Pre- and post-processing of MODFLOW files</font></small>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/RMODFLOW.svg)](https://CRAN.R-project.org/package=RMODFLOW)
<!-- badges: end -->

The {[RMODFLOW](https://rogiersbart.github.io/RMODFLOW/)} R package
provides a set of tools for groundwater flow modelling with
[MODFLOW](https://www.usgs.gov/mission-areas/water-resources/science/modflow-and-related-programs).
The functionality is targeted at feature-parity with that of the
[ModelMuse](https://www.usgs.gov/software/modelmuse-a-graphical-user-interface-groundwater-models)
GUI and friends, albeit restricted to the
[MODFLOW-2005](https://www.usgs.gov/software/modflow-2005-usgs-three-dimensional-finite-difference-ground-water-model)
family of codes. Related developments are the
{[RMT3DMS](https://rogiersbart.github.io/RMT3DMS/)} package for solute
transport modelling, and
{[RMODPATH](https://github.com/cneyens/RMODPATH)} for particle tracking.

# Install

You can install the latest version of
{[RMODFLOW](https://rogiersbart.github.io/RMODFLOW/)} with any of the
following:

``` r
renv::install("rogiersbart/RMODFLOW")
pak::pkg_install("rogiersbart/RMODFLOW")
remotes::install_github("rogiersbart/RMODFLOW")
```

If you don’t have {[renv](https://rstudio.github.io/renv/)},
{[pak](https://pak.r-lib.org/)}, or
{[remotes](https://remotes.r-lib.org/)} installed, try this instead:

``` r
install.packages("remotes")
remotes::install_github("rogiersbart/RMODFLOW")
```

# Use

To get started, have a look at the [introduction to
{RMODFLOW}](https://rogiersbart.github.io/RMODFLOW/docs/articles/RMODFLOW.html).
A series of articles is available if you want to dive deeper into the
{RMODFLOW} functionality:

-   [Boundary
    conditions](https://rogiersbart.github.io/RMODFLOW/articles/boundary_conditions.html)
-   [Structure of input
    data](https://rogiersbart.github.io/RMODFLOW/articles/input_data_structure.html)
-   [Handling MODFLOW
    output](https://rogiersbart.github.io/RMODFLOW/articles/output.html)
-   [MODFLOW
    parameters](https://rogiersbart.github.io/RMODFLOW/articles/parameters.html)
-   [Plotting in
    RMODFLOW](https://rogiersbart.github.io/RMODFLOW/articles/plotting.html)
-   [Spatial
    functions](https://rogiersbart.github.io/RMODFLOW/articles/spatial.html)
-   [Top-level
    functions](https://rogiersbart.github.io/RMODFLOW/articles/top_level.html)
