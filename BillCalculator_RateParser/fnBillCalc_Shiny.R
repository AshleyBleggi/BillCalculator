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



#library(DT)

#not used anymore
find_cell <- function(table, row, col, name="core-fg"){
  l <- table$layout
  which(l$t==row & l$l==col & l$name==name)
}
#helper rounding fn
round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

#To center the title by default for all ggplots
theme_update(plot.title = element_text(hjust = 0.5))
owrs_file <- read_owrs_file("examples/mnwd-2016-01-01.owrs")
#Function to Calculate Usage by Tier and Plot
#************One-shot Version********************  
fnUseByTier <- function(df1, tablemode){
  ##########################################5 tier############################################### 
  calced <- calculate_bill(df1, owrs_file)
  custclass <- df1$cust_class
  commodity_id <- as.character(owrs_file$rate_structure[[as.character(custclass)]]$commodity_charge)
  #View(commodity_id)
  
  if((commodity_id == "Budget")  || (commodity_id == "Tiered")){
  len <- length(owrs_file$rate_structure[[as.character(custclass)]]$tier_starts)
  if(len == 5){
    ##******Initial Use and Rate Arrays******
    #Tier Use and Rate Arrays
    inputs <- data.frame(
      tiers = c("Tier 1","Tier 2","Tier 3","Tier 4","Tier 5"),
      #tier_breaks = c(0,0,0,0,0),
      tier_use = numeric(len),
      tier_rates = numeric(len),  # Stage 1 Rates
      # tier_rates = c(1.56,1.78,9.21,9.21,9.21),  # Stage 2 Rates
      #tier_rates_drought = c(0,0,6.55,4.79,0), #drought rates
      tier_charge =  numeric(len)
      #tier_charge_drought =  c(0,0,0,0,0),
      #new_tier_breaks = c(0,0,0,0,0),
      #new_tier_use = c(0,0,0,0,0),
      #new_tier_rates = c(1.69,1.94,3.32,5.12,9.59),
      #new_tier_charge = c(0,0,0,0,0)
    )
    rownames(inputs) <- c("Tier 1","Tier 2","Tier 3","Tier 4","Tier 5")
    
    ##*************************************   
    #determine use per tier given customer usage
    
    
    for(i in c(1:len)) {
      col_name <- sprintf("X%s", i)
      inputs$tier_use[i] <- calced[[col_name]]
    }
    # inputs$tier_use[1] <- calced$X1
    # inputs$tier_use[2] <- calced$X2
    # inputs$tier_use[3] <- calced$X3
    # inputs$tier_use[4] <- calced$X4
    # inputs$tier_use[5] <- calced$X5
    new_usage <- sum(inputs$tier_use)
    
    #determine charge per tier
    
    for(i in c(1:len)) {
      col_name <- sprintf("XR%s", i)
      inputs$tier_charge[i] <- calced[[col_name]]
    }
    # inputs$tier_charge[1] <- calced$XR1
    # inputs$tier_charge[2] <- calced$XR2
    # inputs$tier_charge[3] <- calced$XR3
    # inputs$tier_charge[4] <- calced$XR4
    # inputs$tier_charge[5] <- calced$XR5
    inputs$tier_charge
    use_charge <- sum(inputs$tier_charge[1:len])
    
    total_vol_charge <- calced$commodity_charge #total_vol_charge <- sum(use_charge,penalty_charge)
    
    #use reshape's "melt" function to change 'tier_use' to long-form
    df <- melt(inputs, id.vars = "tiers")
    
    #***************graphs**************************    
    #use ggplot to create stacked bar graph of use by tier
    use_breaks <- pretty(c(1:new_usage),n = len)
    p1 <- ggplot(subset(df, variable %in% c("tier_use")), aes(x=variable, y=value, fill=fct_rev(tiers))) + 
      geom_bar(stat="identity", width=0.25) +
      ggtitle("Water Usage by Tier\n") +
      xlab("") +
      ylab("Billing Units (ccf)\n") +
      scale_y_continuous(breaks = use_breaks, labels = use_breaks) +
      scale_x_discrete(labels=c("tier_use" = "Current\n Usage"
      ))+
      scale_fill_manual(values = rev(c("blue", "green3", "yellow", "orange", "red2"))
      ) +
      theme(axis.text.x = element_text(face="bold", color="#993333", 
                                       size=14),
            axis.text.y = element_text(face="bold", color="#993333", 
                                       size=12),
            #panel.grid.major.x = element_blank(),
            #panel.grid.minor.x = element_blank(),
            legend.position = "none")+
      guides(fill=guide_legend(title=NULL))
    p1
    
    #use ggplot to create stacked bar graph of charge by tier  
    bill_breaks <- pretty(c(1:use_charge),len)
    p2 <- ggplot(subset(df, variable %in% c("tier_charge")), aes(x=variable, y=value, fill=fct_rev(tiers))) +
      geom_bar(stat="identity", width=0.25) +
      ggtitle("Water Charge by Tier\n") +
      xlab("") +
      ylab("Dollars\n") +
      scale_fill_manual(values = rev(c("blue", "green3", "yellow", "orange", "red2"))
      ) +
      scale_y_continuous(labels = dollar_format(),
                         breaks = bill_breaks)+
      scale_x_discrete(labels=c("tier_charge" = "Current\n Charge"
      ))+
      theme(axis.text.x = element_text(face="bold", color="#993333", 
                                       size=14),
            axis.text.y = element_text(face="bold", color="#993333", 
                                       size=12),
            #panel.grid.major.x = element_blank(),
            #panel.grid.minor.x = element_blank(),
            legend.position = "none")+
      guides(fill=guide_legend(title=NULL))
    p2
    
    #use ggplot to create stacked bar graph of charge by tier  
    plegd_breaks <- c(0:6)
    plegd <- ggplot(subset(df, variable == "tier_charge"), aes(x=variable, y=value, fill=factor(tiers))) + 
      geom_bar(stat="identity", width=0) +
      xlab("") +
      ylab("") +
      scale_fill_manual(values = c("red2", "orange", "yellow", "green3", "blue"),
                        limits = c("1","2","3","4","5"),
                        labels = c("Tier 5","Tier 4","Tier 3","Tier 2","Tier 1")
      ) +
      scale_y_discrete(breaks = plegd_breaks) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.background =element_rect(fill = NA),
            legend.position = c(0.5,0.5))+
      guides(fill=guide_legend(title=NULL))
    plegd
    
    #Sum of Current Charges
    total_bill = calced$bill
    
    #Table Preparation
    vol_table <- data.frame(`Usage Tier` = c("Tier 1","Tier 2",
                                             "Tier 3","Tier 4","Tier 5",
                                             # "Tier 3*","Tier 4*","Tier 5*",  # asterix for penalty fee note
                                             "Total Water\nUsage Charge",
                                             "Total Bill"),
                            `Usage Per Tier` = c(inputs$tier_use,"",""),
                            
                            #`New Usage Per Tier` = c(inputs$new_tier_use,"","","","",""),
                            
                            # Price = c(paste0("$",
                            #                  formatC(as.numeric(inputs$tier_rates),
                            #                          format="f",
                            #                          digits=2,
                            #                          big.mark=",")
                            # ),"","","","",""),
                            # 
                            # `Proposed Price` = c(paste0("$",
                            #                             formatC(as.numeric(inputs$new_tier_rates),
                            #                                     format="f",
                            #                                     digits=2,
                            #                                     big.mark=",")
                            # ),"","","","$4.31",""),
                            
                            `Current Charge` = paste0("$",
                                                      formatC(as.numeric(c(inputs$tier_charge,total_vol_charge,
                                                                           total_bill)),
                                                              format="f",
                                                              digits=2,
                                                              big.mark=","))
                            
                            # `Proposed Charge` = paste0("$",
                            #                            formatC(as.numeric(c(inputs$new_tier_charge,new_total_vol_charge,
                            #                                                 new_total_fix_charge,
                            #                                                 new_total_ww_service_charge, per_person_sewer_charge,
                            #                                                 new_total_bill)),
                            #                                    format="f",
                            #                                    digits=2,
                            #                                    big.mark=","))
                            
    )
    colnames(vol_table) <- c("Usage Tier","Usage Per Tier",
                             "Current Charge")
    
    
    # bill_table <- data.frame(Bill.Components = c("Water Usage Charge","Basic Water Charge","Basic Sewer Charge",
    #                                     "Per Person Sewer Charge","Total Bill"),
    #                          Current = paste0("$",
    #                                          formatC(as.numeric(current_charges),
    #                                                  format="f",
    #                                                  digits=2,
    #                                                  big.mark=",")),
    #                          Proposed = paste0("$",
    #                                            formatC(as.numeric(proposed_charges),
    #                                                    format="f",
    #                                                    digits=2,
    #                                                    big.mark=",")))
    # colnames(bill_table) <- c("Bill Components","Total Current Charges",
    #                           "Total Proposed Charges")
    
    #Fn ouputs plot/table
    if(tablemode == FALSE){
      plotList<-list(p1,p2,plegd)
      return(plotList)
      #grid.arrange(arrangeGrob(p1, p2,plegd, ncol = 3, widths = unit(c(2/5,2/5,1/5),"npc")),
       #nullGrob()
                   #arrangeGrob(t1, nullGrob(),t2, ncol = 1, heights = unit(c(4/10,2/10,4/10),"npc")),
                   ## Information on Stage 2 penalties
                   # textGrob("*$7.43 of the $9.21 per BU is a penalty for using water in excess of your water budget.",
                   #          gp=gpar(fontsize=12,fontface="italic")),
                   #nrow=4, ncol=1,
                   #heights=c(3, 1/8, 3, 1)
       #)
    }else if(tablemode == TRUE){
      vol_table
    }  
    # }else
    #   bill_table
    
    ##########################################4 tier################################################# 
  } 

#************************************* 
else if(len == 4){
  ##******Initial Use and Rate Arrays******
  #Tier Use and Rate Arrays  
  inputs <- data.frame(
    tiers = c("Tier 1","Tier 2","Tier 3","Tier 4"),
    #tier_breaks = c(0,0,0,0,0),
    tier_use = numeric(len),
    tier_rates = numeric(len),  # Stage 1 Rates
    # tier_rates = c(1.56,1.78,9.21,9.21,9.21),  # Stage 2 Rates
    #tier_rates_drought = c(0,0,6.55,4.79,0), #drought rates
    tier_charge =  numeric(len)
    #tier_charge_drought =  c(0,0,0,0,0),
    #new_tier_breaks = c(0,0,0,0,0),
    #new_tier_use = c(0,0,0,0,0),
    #new_tier_rates = c(1.69,1.94,3.32,5.12,9.59),
    #new_tier_charge = c(0,0,0,0,0)
  )
  rownames(inputs) <- c("Tier 1","Tier 2","Tier 3","Tier 4")
  
  ##*************************************   
  #determine use per tier given customer usage
  
  
  for(i in c(1:len)) {
    col_name <- sprintf("X%s", i)
    inputs$tier_use[i] <- calced[[col_name]]
  }
  # inputs$tier_use[1] <- calced$X1
  # inputs$tier_use[2] <- calced$X2
  # inputs$tier_use[3] <- calced$X3
  # inputs$tier_use[4] <- calced$X4
  # inputs$tier_use[5] <- calced$X5
  
  new_usage <- sum(inputs$tier_use)
  
  
  #determine charge per tier
  
  for(i in c(1:len)) {
    col_name <- sprintf("XR%s", i)
    inputs$tier_charge[i] <- calced[[col_name]]
  }
  # inputs$tier_charge[1] <- calced$XR1
  # inputs$tier_charge[2] <- calced$XR2
  # inputs$tier_charge[3] <- calced$XR3
  # inputs$tier_charge[4] <- calced$XR4
  # inputs$tier_charge[5] <- calced$XR5
  inputs$tier_charge
  use_charge <- sum(inputs$tier_charge[1:len])
  
  total_vol_charge <- calced$commodity_charge #total_vol_charge <- sum(use_charge,penalty_charge)
  
  #use reshape's "melt" function to change 'tier_use' to long-form
  df <- melt(inputs, id.vars = "tiers")
  
  #***************graphs**************************    
  #use ggplot to create stacked bar graph of use by tier
  use_breaks <- pretty(c(1:new_usage),n = len)
  p1 <- ggplot(subset(df, variable %in% c("tier_use")), aes(x=variable, y=value, fill=fct_rev(tiers))) + 
    geom_bar(stat="identity", width=0.25) +
    ggtitle("Water Usage by Tier\n") +
    xlab("") +
    ylab("Billing Units (ccf)\n") +
    scale_y_continuous(breaks = use_breaks, labels = use_breaks) +
    scale_x_discrete(labels=c("tier_use" = "Current\n Usage"
    ))+
    scale_fill_manual(values = rev(c("blue", "green3", "yellow", "red2"))
    ) +
    theme(axis.text.x = element_text(face="bold", color="#993333", 
                                     size=14),
          axis.text.y = element_text(face="bold", color="#993333", 
                                     size=12),
          #panel.grid.major.x = element_blank(),
          #panel.grid.minor.x = element_blank(),
          legend.position = "none")+
    guides(fill=guide_legend(title=NULL))
  p1
  
  #use ggplot to create stacked bar graph of charge by tier  
  bill_breaks <- pretty(c(1:use_charge),len)
  p2 <- ggplot(subset(df, variable %in% c("tier_charge")), aes(x=variable, y=value, fill=fct_rev(tiers))) +
    geom_bar(stat="identity", width=0.25) +
    ggtitle("Water Charge by Tier\n") +
    xlab("") +
    ylab("Dollars\n") +
    scale_fill_manual(values = rev(c("blue", "green3", "yellow", "red2"))
    ) +
    scale_y_continuous(labels = dollar_format(),
                       breaks = bill_breaks)+
    scale_x_discrete(labels=c("tier_charge" = "Current\n Charge"
    ))+
    theme(axis.text.x = element_text(face="bold", color="#993333", 
                                     size=14),
          axis.text.y = element_text(face="bold", color="#993333", 
                                     size=12),
          #panel.grid.major.x = element_blank(),
          #panel.grid.minor.x = element_blank(),
          legend.position = "none")+
    guides(fill=guide_legend(title=NULL))
  p2
  
  #use ggplot to create stacked bar graph of charge by tier  
  plegd_breaks <- c(0:5)
  plegd <- ggplot(subset(df, variable == "tier_charge"), aes(x=variable, y=value, fill=factor(tiers))) + 
    geom_bar(stat="identity", width=0) +
    xlab("") +
    ylab("") +
    scale_fill_manual(values = c("red2", "yellow", "green3", "blue"),
                      limits = c("1","2","3","4"),
                      labels = c("Tier 4","Tier 3","Tier 2","Tier 1")
    ) +
    scale_y_discrete(breaks = plegd_breaks) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.background =element_rect(fill = NA),
          legend.position = c(0.5,0.5))+
    guides(fill=guide_legend(title=NULL))
  plegd
 
  #Sum of Current Charges
  total_bill = calced$bill
  
  #Table Preparation
  vol_table <- data.frame(`Usage Tier` = c("Tier 1","Tier 2",
                                           "Tier 3","Tier 4",
                                           # "Tier 3*","Tier 4*","Tier 5*",  # asterix for penalty fee note
                                           "Total Water\nUsage Charge",
                                           "Total Bill"),
                          `Usage Per Tier` = c(inputs$tier_use,"",""),
                          
                          #`New Usage Per Tier` = c(inputs$new_tier_use,"","","","",""),
                          
                          # Price = c(paste0("$",
                          #                  formatC(as.numeric(inputs$tier_rates),
                          #                          format="f",
                          #                          digits=2,
                          #                          big.mark=",")
                          # ),"","","","",""),
                          # 
                          # `Proposed Price` = c(paste0("$",
                          #                             formatC(as.numeric(inputs$new_tier_rates),
                          #                                     format="f",
                          #                                     digits=2,
                          #                                     big.mark=",")
                          # ),"","","","$4.31",""),
                          
                          `Current Charge` = paste0("$",
                                                    formatC(as.numeric(c(inputs$tier_charge,total_vol_charge,
                                                                         total_bill)),
                                                            format="f",
                                                            digits=2,
                                                            big.mark=","))
                          
                          # `Proposed Charge` = paste0("$",
                          #                            formatC(as.numeric(c(inputs$new_tier_charge,new_total_vol_charge,
                          #                                                 new_total_fix_charge,
                          #                                                 new_total_ww_service_charge, per_person_sewer_charge,
                          #                                                 new_total_bill)),
                          #                                    format="f",
                          #                                    digits=2,
                          #                                    big.mark=","))
                          
  )
  colnames(vol_table) <- c("Usage Tier","Usage Per Tier",
                           "Current Charge")
  
  
  # bill_table <- data.frame(Bill.Components = c("Water Usage Charge","Basic Water Charge","Basic Sewer Charge",
  #                                     "Per Person Sewer Charge","Total Bill"),
  #                          Current = paste0("$",
  #                                          formatC(as.numeric(current_charges),
  #                                                  format="f",
  #                                                  digits=2,
  #                                                  big.mark=",")),
  #                          Proposed = paste0("$",
  #                                            formatC(as.numeric(proposed_charges),
  #                                                    format="f",
  #                                                    digits=2,
  #                                                    big.mark=",")))
  # colnames(bill_table) <- c("Bill Components","Total Current Charges",
  #                           "Total Proposed Charges")
  
  #Fn ouputs plot/table
  if(tablemode == FALSE){
    plotList<-list(p1,p2,plegd)
    return(plotList)
    #grid.arrange(arrangeGrob(p1, p2,plegd, ncol = 3, widths = unit(c(2/5,2/5,1/5),"npc")),
    #nullGrob()
    #arrangeGrob(t1, nullGrob(),t2, ncol = 1, heights = unit(c(4/10,2/10,4/10),"npc")),
    ## Information on Stage 2 penalties
    # textGrob("*$7.43 of the $9.21 per BU is a penalty for using water in excess of your water budget.",
    #          gp=gpar(fontsize=12,fontface="italic")),
    #nrow=4, ncol=1,
    #heights=c(3, 1/8, 3, 1)
    #)
  }else if(tablemode == TRUE){
    vol_table
  }  
  # }else
  #   bill_table
  
  ##########################################No tier################################################# 
}
  }
  else{
    total_vol_charge <- calced$commodity_charge
    total_bill = calced$bill
    vol_table <- data.frame(`Usage Tier` = c("Total Water\nUsage Charge",
                                             "Total Bill"),
                            `Current Charge` = paste0("$",
                                                      formatC(as.numeric(c(total_vol_charge,
                                                                           total_bill)),
                                                              format="f",
                                                              digits=2,
                                                              big.mark=","))
                            
    )
    colnames(vol_table) <- c("Usage",
                             "Current Charge")
    if(tablemode == FALSE){
      plotList<-list(NULL,NULL,NULL)
      return(plotList)
    }else if(tablemode == TRUE){
      vol_table
    }  
  }
}

# #Fn to Calculate and Display Current and Proposed Budgets
# #************Water Budget Text********************  
# fnBudget <- function(usage = 70, gpcd = 60, hh_size = 4, days = 30, conv_gal2ccf = 748,
#                      lot_size = 6000, plant_factor = 0.7, evapo_trans = 3.43, conv_in2gal = 0.62,
#                      meter_selection = 0.625, cust_class, typical_usage){
#   
#   if(cust_class == "Single Family Residential"|cust_class == "Multi Family Residential"){
#     
#     indoor_budget <- round(gpcd*hh_size*days/conv_gal2ccf, digits = 0)
#     new_indoor_budget <- round(55*hh_size*days/conv_gal2ccf, digits = 0)
#     outdoor_budget <- round(lot_size*plant_factor*evapo_trans*conv_in2gal/conv_gal2ccf, digits = 0)
#     
#     msg <- paste("Your Current Calculated Water Budget = ",
#                  round(sum(indoor_budget,outdoor_budget), digits = 0)," BU and",
#                  "Your Proposed Water Budget = ",
#                  round(sum(new_indoor_budget,outdoor_budget), digits = 0)," BU")
#     return(noquote(strsplit(msg, "\n")[[1]]))
#     
#     
#   }else if(cust_class == "Commercial"|cust_class == "Irrigation"){
#     
#     budget <- round(lot_size*plant_factor*evapo_trans*conv_in2gal/conv_gal2ccf, digits = 0)
#     
#     msg <- paste("Your Current Calculated Water Budget = ",
#                  round(budget, digits = 0)," BU and",
#                  "Your Proposed Water Budget = ",
#                  round(budget, digits = 0)," BU")
#     return(noquote(strsplit(msg, "\n")[[1]]))
#     
#   }else if(cust_class == "Recycled"){ #Recycled Budget
#     
#     plant_factor <- 0.8
#     
#     budget <- round(lot_size*plant_factor*evapo_trans*conv_in2gal/conv_gal2ccf, digits = 0)
#     
#     msg <- paste("Your Current Calculated Water Budget = ",
#                  round(budget, digits = 0)," BU and",
#                  "Your Proposed Water Budget = ",
#                  round(budget, digits = 0)," BU")
#     return(noquote(strsplit(msg, "\n")[[1]]))
#     
#   }else if(cust_class == "Commercial Non-Irrigation"){ #Commercial Non-Irrigation Budget
#     
#     budget <- round((typical_usage+usage)/2, digits = 0)
#     
#     msg <- paste("Your Current Calculated Water Budget = ",
#                  round(budget, digits = 0)," BU and",
#                  "Your Proposed Water Budget = ",
#                  round(budget, digits = 0)," BU")
#     return(noquote(strsplit(msg, "\n")[[1]]))
#   }
# }
# 
# #************************************* 
