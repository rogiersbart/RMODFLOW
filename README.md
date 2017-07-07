RMODFLOW<br><small><font color="#333333">Pre- and post-processing of MODFLOW files in R</font></small>
====================================================================

[![Travis-CI Build Status](https://travis-ci.org/rogiersbart/RMODFLOW.svg?branch=master)](https://travis-ci.org/rogiersbart/RMODFLOW)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/RMODFLOW)](https://cran.r-project.org/package=RMODFLOW)
[![Coverage Status](https://img.shields.io/codecov/c/github/rogiersbart/RMODFLOW/master.svg)](https://codecov.io/github/rogiersbart/RMODFLOW?branch=master)

The [RMODFLOW](https://rogiersbart.github.io/RMODFLOW/) package provides a set of tools for groundwater flow modelling with [MODFLOW](https://water.usgs.gov/ogw/modflow/). A related package is the [RMT3DMS](https://rogiersbart.github.io/RMT3DMS/) package for solute transport modelling.

# To install

1. install the devtools package: `install.packages("devtools")`
2. load the devtools package: `library(devtools)`
3. install the latest development version: `install_github("rogiersbart/RMODFLOW")`

# For users

The package is still in development, although different people have already made good use of it. This means that breaking changes might still happen more than you would like. Once we are close to a first major release (*i.e.* a first version on CRAN hopefully), this will of course not be the case anymore. A good place to start is the vignette "[A brief overview](https://rogiersbart.github.io/RMODFLOW/articles/RMODFLOW.html)".

# For contributors

Using R and MODFLOW, and wanting to contribute? Please do so! The most straightforward contributions at the moment are in the form of file reading, writing and object creation functions for the many still-not-supported MODFLOW packages. I intend to create a vignette for contributors, listing what packages we are currently supporting *etc.*, but it is not there yet. It is probably not too hard to find out yourself however. Just make sure you do things the way we do, and you follow the [online guide to MODFLOW](https://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/) meticulously (note we use lowercase versions of all parameters here). A good starting point is probably to have a close look at `rmf_read_dis()` and the [corresponding page](https://water.usgs.gov/nrp/gwsoftware/modflow2000/MFDOC/index.html?dis.htm) in the online guide. Note also that we are first focussing on MODFLOW-2005 packages. Later on the other derived versions of MODFLOW-2005 might be supported as well. MODFLOW-2000 and MODFLOW 6 are out of scope.

# Version history

* 0.4.0 - Version used by Neyens (2016).
* 0.3.0 - Version presented by Rogiers (2015a).
* 0.2.0 - Version presented by Rogiers (2015b).
* 0.1.0 - Different modifications to the code for use in reproducible reporting. 

# What's next?

There are many ideas for further development. Here are a few to give you a taste of where this is going:

- S3 methods for several [htmlwidgets](http://www.htmlwidgets.org/) enabling interactive visualization.
- Extend animation and 3D visualization support.
- More compatibility with the [tidyverse](http://tidyverse.org/), possibly resulting in something like an RMODFLOW model manipulation language, which would allow to make use of *e.g.* [dplyr](http://dplyr.tidyverse.org/) verbs.
- Functions for downloading different MODFLOW versions.
- Support more MODFLOW packages
- Add access to external, larger model examples.

# References

* Rogiers B. 2017. Reproducible research in computational subsurface hydrology - First steps in R with RMODFLOW and RMT3DMS. useR!2017. 04-07 July 2017. Brussels, Belgium. [presentation](https://drive.google.com/file/d/0B4xr2UZeAf_mZVJHSGhYek52bm8/view?usp=sharing)
* Cas Neyens. 2016. Analysis of pumping tests in a sandy aquifer in Northern Belgium. MSc thesis, Faculty of Science, KU Leuven, 163 pp. [thesis](https://drive.google.com/file/d/0B4xr2UZeAf_mY2lzLU1xbDBWeHc/view?usp=sharing)
* Rogiers B. 2015a. Groundwater flow and solute transport modelling from within R: The RMODFLOW and RMT3DMS packages. MODFLOW AND MORE 2015: MODELING A COMPLEX WORLD, Golden, Colorado, 31 May - 03 June, 2015. [poster](https://drive.google.com/file/d/0B4xr2UZeAf_mUEJuRUhCM3JtQlE/view?usp=sharing)
* Rogiers B. 2015b. Groundwater flow and solute transport modelling from within R: Development of the RMODFLOW and RMT3DMS packages. Geophysical Research Abstracts 17: EGU2015-11879, EGU General Assembly 2015, Vienna, 12-17 April, 2015. [pico](https://drive.google.com/file/d/0B4xr2UZeAf_mczZkcl9MbGlib28/view?usp=sharing)