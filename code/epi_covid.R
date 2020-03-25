# epi_covid.R

# To assess the health benefits of vaccination versus the risks of sustaining or 
# suspending immunisation programmes in Africa during the COVID-19 pandemic. 

# load libraries
library (data.table)
library (ggplot2)
library (readxl)
library (tictoc)
library (tidyverse)

# remove all objects from workspace
rm (list = ls ())

# start time
print (Sys.time ())
tic ()

# ------------------------------------------------------------------------------
# functions
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Extract vaccine coverage estimates for 2018 from WUENIC (Last update: 15-July-2019)
# ------------------------------------------------------------------------------
wuenic_vaccine_coverage <- function () {
  
  setwd ("C:/Users/kajam/OneDrive/Documents/GitHub/epi_covid")  # debug #
  
  # wuenic excel file containing vaccine coverage data
  wuenic_file <- "data/wuenic_15Jul2019.xls"
  
  # read wuenic vaccine coverage data
  bcg <- read_excel (wuenic_file, sheet = "BCG")
  dtp1 <- read_excel (wuenic_file, sheet = "DTP1")
  dtp3 <- read_excel (wuenic_file, sheet = "DTP3")
  hepb_bd <- read_excel (wuenic_file, sheet = "HepB_BD")
  hepb3 <- read_excel (wuenic_file, sheet = "HepB3")
  hib3 <- read_excel (wuenic_file, sheet = "Hib3")
  ipv1 <- read_excel (wuenic_file, sheet = "IPV1")
  
  setDT (bcg)
  
  
  mad <- wuenic_file %>%
    excel_sheets() %>%
    set_names() %>%
    map_df(read_excel,
           path = wuenic_file)
  
  mad

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

# extract vaccine coverage estimates for 2018 from WUENIC
wuenic <- wuenic_vaccine_coverage ()

# return to source directory
setwd (source_wd)

# end of program
print (Sys.time ())
toc ()
# ------------------------------------------------------------------------------
