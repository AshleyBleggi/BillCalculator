library(shiny)
library(DT)
#library(shinyjqui)

shinyUI(fluidPage(
  tags$head(tags$style(
    type="text/css",
    "#logo img {max-width: 100%; width: 100%; height: auto}"
  )),
  
  sidebarLayout(#column(3, wellPanel(
    sidebarPanel(
      tags$a(imageOutput("logo", height = "100%", width = "100%"), href="http://californiadatacollaborative.org/"),
      h5(em("Please refer to your bill for the required inputs")),
      
      # selectInput("district",
      #             label = h5("Water District"),
      #             choices = c(DistrictList
      #             ),
      #             selected = "Moulton Niguel Water District"),
      
      #h4(""),
      h5("Address"),
      textInput("address", label = NULL, value = ""),
      
      actionButton("go", label = "Update"),
      
      # selectInput("cust_class", 
      #             label = h5("Customer Class"), 
      #             choices = c("Single Family Residential" = "RESIDENTIAL_SINGLE",
      #                         "Multi Family Residential" = "RESIDENTIAL_MULTI"
      # )),
      
      h4(""),icon("tint"),
      h5("Billing Units Used"),
      h6("(1 BU = 748 gallons)"),
      numericInput("usage", label = NULL, value = 15, min = 0, max = 999),
      
      uiOutput("rateInputs"),
      
      width = 3
    ),
    
    mainPanel(#column(9,
      h2("Water Budget Bill Calculator and Bill Comparison",
         style = "font-family: 'Arial Narrow'; margin-top: 0.83 em; margin-bottom: 0em; font-weight:900 ; color:rgb(0,51,127)",
         align = "center"),
      
      HTML('<h4 style="font-family: &#39;Arial Narrow&#39;">
                Use the bill calculator to see how changes in your water usage will
                affect your bill.  For more information on our water budget based rate structure,
                visit 
                <a href="https://www.mnwd.com/waterbudgetbasedrates/", target="_parent">www.mnwd.com/waterbudgetbasedrates/</a><br>
                To determine the Evapotranspiration amount for your microzone, refer to your bill statement or go
                to our <a href="https://www.mnwd.com/watersavingtools/", target="_blank">water saving tools page</a> and use the Evapotranspiration Map.
                
                </h4>'),
      
      h4(em("Please note: The bill calculator is to be used for informational purposes only. 
                 Billed amounts may vary based on actual water usage, actual evapotranspiration rates, 
                 any changes in household size or irrigable area, and/or days in the billing cycle.
                 For Single Family and Multi Family Residentials, Tier 1 and Tier 2 account to Indoor and Outdoor
                 Budgets respectively.",
            style = "font-family: 'Arial Narrow'; margin-top: 0em")
      ),
      
      splitLayout(cellWidths = c("43%", "43%", "14%"), 
                  plotOutput("use"), 
                  plotOutput("charge"),
                  plotOutput("legend")
      ),
      
      dataTableOutput("vol_table"),
      width = 9
    )#end mainPanel
  )
))