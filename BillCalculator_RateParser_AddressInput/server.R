library(shiny)
library(DT)
source("fnBillCalc_Shiny.R")

`%then%` <- shiny:::`%OR%`

shinyServer(
  function(input, output) {
    
    geocode_data <- eventReactive(input$go,{
        # validate(need(input$address, "-Please specify address"))
        geocode(input$address, output = "latlon")
      }
    )
    
    use_by_tier_data <- reactive({
      validate(
        #need(input$district, ""),
        # need(input$address, "-Please specify address"),
        
        need(input$usage, "-Please specify water usage") %then%
          need(input$usage < 999 & input$usage > 0, "-Please enter usage between 0 and 999 BU"),
        
        need(input$bill_days, "-Please specify the number of days in this billing cycle") %then%
          need(input$bill_days < 99 & input$bill_days > 0, "-Please enter between 0 and 99 days"),
        
        if(input$cust_class %in% c("Single Family Residential","Multi Family Residential")){
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
        cust_class = input$cust_class,
        city_limits = input$city_limits,
        usage_month = input$usage_month,
        usage_zone = input$usage_zone,
        pressure_zone = input$pressure_zone,
        water_font = input$water_font,
        elevation_zone = input$elevation_zone,
        tax_exemption = input$tax_exemption,
        season = input$season,
        turbine_meter = input$turbine_meter,
        meter_type = input$meter_type,
        senior = input$senior,
        tariff_area = input$tariff_area,
        block = input$block,
        lot_size_group = input$lot_size_group,
        temperature_zone = input$temperature_zone
      )
      
      #call plots
      plotList<-fnUseByTier(input_df, geocode_data())
      plotList
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