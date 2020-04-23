#========================================================================================
#======= creating an R package structure with some dummy code to delete later ===========
#========================================================================================

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set wd to GitHub repo

library(usethis)
library(devtools)
if(!("group3project" %in% list.files())){
  create_package("group3project", rstudio=FALSE, open = FALSE) # creates a package structure
}

setwd("group3project")
# add a function in ./R/sample-functions.R before building
setwd("../")
#build("group3project")
document("group3project")  # creates NAMESPACE and ./man/ folder with roxygen files
load_all()

# needs to be done in the group3project.Rproj:
  # use_test("test-sample-functions")  # to create ./tests/testthat/test-sample-functions.R
# then add some tests to test-sample-functions.R to test the functions in ./R/sample-functions.R

library(testthat)
library(group3project)
#load_all()
test_file("group3project/tests/testthat/test-calc_acc.R")
test_package("group3project") # run all tests in package
