## Bill Calculation Formula
####To be used as an external call for Shiny Dashboard

library(ggplot2)
library(reshape2)
library(gridExtra)
library(grid)
library(gtable)
library(scales)
library(forcats)
library(RateParser)
library(ggmap)

fnAddress <- function(district, address){
  district_all <- c("Beverly Hills City of - 239/07-03-2017.owrs", 
                    "Santa Monica City of - 364/smc-2017-01-01.owrs", 
                    "Moulton Niguel Water District - 147/mnwd-2016-01-01.owrs",
                    "Amador Water Agency - 71/10-01-2017.owrs",
                    "Apple Valley Ranchos Water Company - 379/AVRWC-2017-01-01.owrs",
                    "Brentwood City of - 275/Brentwood-2016-07-01.owrs",
                    "Burbank City of - 270/bc-2017-01-02.owrs",
                    "California Water Service Company Antelope Valley - 406/CWSCAV-2017-01-01(1).owrs"
                    )
  names(district_all) <- c("239", "364", "147", "71", "379", "275", "270", "406")
  owrs_file <<- read_owrs_file(paste("California/", district_all[as.character(district)], sep = ""))
  #owrs_file <- read_owrs_file("examples/mnwd-2016-01-01.owrs")
  #mygeocode <- geocode(as.character(address), output = "latlon")
  return(district)
}