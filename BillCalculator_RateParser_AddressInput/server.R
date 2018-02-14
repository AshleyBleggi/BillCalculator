library(shiny)
library(DT)
source("R/fnBillCalc_Shiny.R")

`%then%` <- shiny:::`%OR%`

ET <- data.frame(Months = month.name,ETF = c(2.26,2.53,3.08,3.74,4.54,4.44,4.71,4.56,4.97,3.88,3.15,2.29))
cust_class <- "RESIDENTIAL_SINGLE"


shinyServer(
  function(input, output) {
    
    # Geocode address. Depends on entering address and clicking "Update" button.
    geocode_data <- eventReactive(input$go,{
        # validate(need(input$address, "-Please specify address"))
        # geocode(input$address, output = "latlon")
        data.frame("lon"=-117.7075, "lat"=33.56379)
      }
    )
    
    # Spatial join the geocoded address points with the Water District boundary shapefile
    # After finding the distict, read the OWRS file for that district and return it
    # Depends on geocode_data
    owrs_file <- reactive({
      districtshp <- readOGR("shp", "water_district",verbose=FALSE)
      points_sp <- SpatialPoints(geocode_data(), proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
      df_add <- over(points_sp, districtshp)
      filePath <- Sys.glob(file.path("California", paste0(df_add$Agency_Nam, '*', collapse ='')))
      
      #filePath <- file.path("California", df1$district)
      fileName <- list.files(filePath, pattern="*.owrs", full.name=TRUE)
      if (length(fileName) >= 1){
        fileName <- tail(fileName, n=1)
      }
      owrs_file <- read_owrs_file(fileName)
      owrs_file
    })
    
    # Assemble the inputs into a dataframe and create plots
    # Depends on all inputs
    use_by_tier_data <- reactive({
      validate(
        #need(input$district, ""),
        # need(input$address, "-Please specify address"),
        
        need(input$usage, "-Please specify water usage") %then%
          need(input$usage < 999 & input$usage > 0, "-Please enter usage between 0 and 999 BU"),
        
        need(input$bill_days, "-Please specify the number of days in this billing cycle") %then%
          need(input$bill_days < 99 & input$bill_days > 0, "-Please enter between 0 and 99 days"),
        
        if(TRUE){
          need(input$homesize, "-Please specify your household size")} %then% 
          need(input$homesize < 99 & input$homesize > 0, "-Please enter a household size less than 99"),
        
        need(input$irr_area, "-Please specify the irrigable area associated with your account") %then%
          need(input$irr_area < 99999 & input$irr_area > 0, "-Please enter an irrigable area less than 99,999 and greater than 0"),
        
        need(input$et, "-Please specify the evapotranspiration factor associated with this bill:
             ET varies daily by microzone. There are 110 microzones within the MNWD service area.
             Your water budget is calculated based on the actual ET during the billing cycle;
             however, it is possible to estimate your water budget based on historical ET.
             The 7-year historical average ET values for the MNWD service area are:
             Jan = 2.26; Feb = 2.53; Mar=3.08; Apr=3.74;
             May=4.54; Jun=4.44; Jul=4.71; Aug=4.56;
             Sep=4.97; Oct=3.88; Nov=3.15; Dec=2.29") %then% 
          need(input$et < 99 & input$et > 0, "-Please enter an evapotranspiration factor less than 99")
      )
      
      input_df <- data.frame(
        usage_ccf = input$usage, 
        hhsize = input$homesize,
        days_in_period = input$bill_days, 
        irr_area = input$irr_area, 
        et_amount = input$et, 
        meter_size = input$meter,
        cust_class = cust_class
        # city_limits = input$city_limits,
        # usage_month = input$usage_month,
        # usage_zone = input$usage_zone,
        # pressure_zone = input$pressure_zone,
        # water_font = input$water_font,
        # elevation_zone = input$elevation_zone,
        # tax_exemption = input$tax_exemption,
        # season = input$season,
        # turbine_meter = input$turbine_meter,
        # meter_type = input$meter_type,
        # senior = input$senior,
        # tariff_area = input$tariff_area,
        # block = input$block,
        # lot_size_group = input$lot_size_group,
        # temperature_zone = input$temperature_zone
      )
      
      #call plots
      plotList<-fnUseByTier(input_df, owrs_file())
      plotList
    })
    
    # Generate those inputs that are required by the OWRS file
    # depends on owrs_file
    output$rateInputs <- renderUI({
      
      owrs_str <- jsonlite::toJSON(owrs_file()$rate_structure[[cust_class]])
      widgetList <- list()
      
      if(grepl("hhsize", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),icon("users"),
                                     h5("Number of Persons in Household"),
                                     numericInput("homesize", label = NULL, value = 4, min = 0, max = 99))
        )
      }
      
      if(grepl("irr_area", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),icon("tree"),
                                     h5("Irrigable Area (sq. ft.)"),
                                     numericInput("irr_area", label = NULL, value = 2000, min = 0, max = 99999))
        )
      }
      
      if(grepl("days_in_period", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),icon("calendar"),
                                     h5("Days in Billing Cycle"),
                                     h6("(Typically 28-35 days)"),
                                     numericInput("bill_days", label = NULL, value = 30, min = 0, max = 99) )
        )
      }
      
      if(grepl("et_amount", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),icon("leaf"),
                                     h5("Evapotranspiration"),
                                     h6("(This month's average ET - ",ET$ETF[ET$Months == months.Date(Sys.Date())],")"),
                                     numericInput("et", label = NULL, value = 5, min = 0, max = 99) )
        )
      }
      
      if(grepl("meter_size", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),
                                     h5("Meter Size"),
                                     h6("(Typically 3/4 in. for residential customers)"),
                                     selectInput("meter", 
                                                 label = NULL,
                                                 choices = c("5/8 in." = '5/8"', "3/4 in." = '3/4"',
                                                             "1 in." = '1"', "1 1/2 in." = '1 1/2"',
                                                             "2 in." = '2"', "3 in." = '3"',
                                                             "4 in." = '4"', "6 in." = '6"',
                                                             "8 in." = '8"', "10 in." = '10"'),
                                                 selected = '3/4"'),
                                     textOutput("budget") )
        )
      }
      
      if(grepl("city_limits", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),
                                     h5("Are you located within the city limits?"),
                                     selectInput("city_limits", 
                                                 label = NULL, 
                                                 choices = c("inside_city","outside_city"),
                                                 selected = 'inside_city' ) )
        )
      }
      
      if(grepl("usage_month", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),
                                     h5("Month (1-12)"),
                                     numericInput("usage_month", 
                                                  label = NULL,
                                                  value = as.integer(format(Sys.Date(),"%m")), 
                                                  min = 1, max = 12) )
        )
      }
      
      if(grepl("usage_zone", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),
                                     h5("Usage Zone"),
                                     numericInput("usage_zone", 
                                                  label = NULL,
                                                  value = 1, 
                                                  min = 1, max = 5) )
        )
      }

      if(grepl("pressure_zone", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),
                                     h5("Pressure Zone"),
                                     numericInput("pressure_zone", 
                                                  label = NULL,
                                                  value = 1, 
                                                  min = 1, max = 4) )
        )
      }
      
      if(grepl("elevation_zone", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),
                                     h5("Elevation Zone"),
                                     numericInput("elevation_zone", 
                                                  label = NULL,
                                                  value = 1, 
                                                  min = 1, max = 6) )
        )
      }
      
      if(grepl("lot_size_group", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),
                                     h5("Lot Size Group (1-5"),
                                     numericInput("lot_size_group", 
                                                  label = NULL,
                                                  value = 1, 
                                                  min = 1, max = 6) )
        )
      }

      if(grepl("temperature_zone", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),
                                     h5("Temperature Zone"),
                                     selectInput("temperature_zone", 
                                                 label = NULL, 
                                                 value = "Medium",
                                                 choices = c("Low","Medium","High")) )
        )
      }

      if(grepl("senior", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),
                                     h5("Senior citizen?"),
                                     selectInput("senior", 
                                                 label = NULL, 
                                                 value = "no",
                                                 choices = c("yes","no")) )
        )
      }

      if(grepl("water_font", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),
                                     h5("What is your water source?"),
                                     selectInput("water_font", 
                                                 label = NULL, 
                                                 value = "city_delivered",
                                                 choices = c("city_delivered","private_wells")) )
        )
      }
      
      if(grepl("tax_exemption", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),
                                     h5("Tax exempt?"),
                                     selectInput("tax_exemption", 
                                                 label = NULL, 
                                                 value = "not_granted",
                                                 choices = c("granted","not_granted")) )
        )
      }

#       
#       conditionalPanel(
#         condition = "input.district == 'Long Beach City of - 1656'",
#         selectInput("tax_exemption", label = h5("Tax Exemption"), choices = c("granted",
#                                                                               "not_granted"
#         )
#         )),
#       
#       conditionalPanel(
#         condition = "input.district == 'El Dorado Irrigation District - Main - 934'",
#         selectInput("turbine_meter", label = h5("Turbine Meter"), choices = c("Yes",
#                                                                               "No"
#         )
#         )),
#       
#       conditionalPanel(
#         condition = "input.district == 'Huntington Beach City of - 1376'||input.district == 'Livermore  City of - 1631'",
#         selectInput("meter_type", label = h5("Meter Type"), choices = c("compound",
#                                                                         "FM",
#                                                                         "Turbine",
#                                                                         "Displacement"
#         )
#         )),
#       
#       conditionalPanel(
#         condition = "input.district == 'Long Beach City of - 1656'||input.district == 'Arcadia  City Of - 132'||input.district == 'Pasadena  City Of - 2136'||input.district == 'Los Angeles Department of Water and Power - 1665'",
#         selectInput("season", label = h5("Season"), choices = c("Winter",
#                                                                 "Summer"
#         )
#         )),
#       
#       conditionalPanel(
#         condition = "input.district == 'Suburban Water Systems San Jose Hills - 16'",
#         selectInput("tariff_area", label = h5("Tariff Area"), choices = c(1,2,3)
#         )),
#       
#       conditionalPanel(
#         condition = "input.district == 'Suburban Water Systems San Jose Hills - 16'",
#         selectInput("block", label = h5("Block"), choices = c(1,2)
#         )),
      
      tagList(widgetList)
    })
    
    #Usage plot
    output$use <- renderPlot({
      use_by_tier_data()[[1]]
    },
    height = "auto",
    #height = 650,
    width = "auto"
    )
    
    #Charges plot
    output$charge<-renderPlot({
      use_by_tier_data()[[2]]
    },
    height = "auto",
    #height = 650,
    width = "auto"
    )

    #Legend
    output$legend<-renderPlot({
      use_by_tier_data()[[3]]
    })

    #call vol table
    output$vol_table <- renderDataTable({
      datatable(use_by_tier_data()[[4]])
      
      
      # datatable(fnUseByTier(input_df, tablemode = TRUE),#extensions = 'Buttons',
      # 
      #           options = list(pageLength = 15, columnDefs = list(list(className = 'dt-center',
      #                                                                  targets = 1:7)),dom = 't'
      #                          #buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      # 
      #           ))
    })
    
    
    #Logo
    output$logo <- renderImage({
      return(list(
        src = "logo/logo.png",
        filetype = "image/png",
        alt = NULL,
        height = 120,
        width = 171
      ))
    }, deleteFile = FALSE
    )
    
  })