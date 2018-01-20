library(shiny)
library(DT)
source("fnBillCalc_Shiny.R")

`%then%` <- shiny:::`%OR%`

DistrictList <- list.files("California")
# DistrictList <- gsub("[0-9\\-]", "", list.files("California"))
# DistrictList <- trimws(DistrictList, which = c("both", "left", "right"))