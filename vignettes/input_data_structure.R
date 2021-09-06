## ----setup, echo=FALSE--------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)
knitr::opts_chunk$set(echo = TRUE, collapse = TRUE, comment = "#>")
library(RMODFLOW)

## -----------------------------------------------------------------------------
ar <- matrix(42, nrow = 4, ncol = 5)
rmf_create_array(ar)

rmf_create_array(42, dim = c(4, 5))

## -----------------------------------------------------------------------------
rmf_create_array(10, dim = c(2, 5), dimlabels = c('rows', 'columns'))

## -----------------------------------------------------------------------------
rmf_create_array("my_value", dim = c(2, 2, 1, 3))

## -----------------------------------------------------------------------------
# R array drops dims of length 1
ar <- array(1, dim = c(2,2,1,3))
dim(ar[,,,2])

# rmf_array keeps dims of length 1
rmf_ar <- rmf_create_array(ar)
dim(rmf_ar[,,,2])

# collapses into atomic vector
class(ar[1,1,,])

# remains 4D array since 4th dimension is not subsetted
class(rmf_ar[1,1,,])

# collapses
class(rmf_ar[1,1,,, drop = TRUE])

# drops class
ar <- structure(array(12, dim = c(4, 4, 2)), class = 'not_rmf')
class(ar[,,1])

# subsetting a rmf_array always returns a rmf_array...
class(rmf_ar)
class(rmf_ar[,,1,1:2])
class(rmf_ar[,,,1])
class(rmf_ar[,,1,1])

# ... unless return is 1D
class(rmf_ar[,1,1,1])


## -----------------------------------------------------------------------------
# extracting a single layer returns a rmf_array
dis <- rmf_example_file("example-model.dis") %>% 
  rmf_read_dis()

dis$botm[,,5]


## -----------------------------------------------------------------------------
rmf_create_array(1.5, dim = c(2, 2), kper = c(1,4))

## ---- eval=FALSE--------------------------------------------------------------
#  dis <- rmf_example_file("example-model.dis") %>% rmf_read_dis()
#  nam <- rmf_example_file("example-model.nam") %>% rmf_read_nam()
#  
#  # this will create separate files for the 3D botm array and for the 2D top array using the EXTERNAL header
#  # it will also generate a warning as a reminder to add Nunit to the NAME file
#  rmf_write_dis(dis = dis, file = "input.dis", external = c("botm", "top"), nam = nam)
#  
#  # this will create separate binary files for each layer in botm using the OPEN/CLOSE header
#  rmf_write_dis(dis = dis, file = "input.dis", fname = c("botm"), binary = c("botm"))

## -----------------------------------------------------------------------------
wells <- tibble::tribble(~i, ~j, ~k, ~q,
                1,   1,  1, -25,
                2,   1,  1, -50) %>% rmf_create_list()
str(wells)


## -----------------------------------------------------------------------------
wel <- matrix(c(1, 1, 2, -25), ncol = 4) 
colnames(wel) <- c("i", "j", "k", "q")

rmf_create_list(wel, kper = c(1,4)) %>% str()

## -----------------------------------------------------------------------------
dis <- rmf_create_dis(nrow = 3, ncol = 2, nlay = 1)
ar <- rmf_create_array(dim = c(dis$nrow, dis$ncol, dis$nlay, 2))
ar[] <- 1:prod(dim(ar))
ar

rmf_as_list(ar[,,,1])

# a rmf_as_list.rmf_4d_array needs a l index for the 4th dimension
lst <- rmf_as_list(ar, l = 2)
lst 

# rmf_as_array needs a dis object to guess the dimensions of the array
rmf_as_array(lst, dis = dis)



