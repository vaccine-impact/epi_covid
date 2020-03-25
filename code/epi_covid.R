# epi_covid.R

# To assess the health benefits of vaccination versus the risks of sustaining or 
# suspending immunisation programmes in Africa during the COVID-19 pandemic. 

# load libraries
library (data.table)
library (ggplot2)
library (tictoc)

# remove all objects from workspace
rm (list = ls ())

# start time
print (Sys.time ())
tic ()

# ------------------------------------------------------------------------------
# functions
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Extract vaccine coverage estimates for 2018 from WUENIC 2018
# ------------------------------------------------------------------------------
wuenic_vaccine_coverage <- function () {

} # end of function -- combine_burden_estimate
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# main program
# ------------------------------------------------------------------------------

# start of program
print (Sys.time ())

# move to base directory (run code from source directory)
source_wd <- getwd ()
setwd ("../")

# extract vaccine coverage estimates for 2018 from WUENIC 2018
wuenic <- wuenic_vaccine_coverage ()

# return to source directory
setwd (source_wd)

# end of program
print (Sys.time ())
toc ()
# ------------------------------------------------------------------------------
