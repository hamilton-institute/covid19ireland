library(readxl)
library(dplyr)
library(lubridate)
library(Jmisc)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(cowplot)
library(scales)
library(shiny)
library(shinyWidgets)
library(plotly)
library("survival")
library(shinycssloaders)
library(sf)


rm(list = ls())

load("RIP_rk_aggregated_data_merged_28Aug.RData") 
load("rk_groupings.Rdata")
load("ire.Rdata")

ui <- fluidPage(
  
  titlePanel("Excess postings to RIP.ie in Eircode regions"),
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(width=12,
               
               setSliderColor(c(rep("#b2df8a", 3)), sliderId=c(8,9,10)),
               # Input: Selector for choosing dataset ----
               
               selectInput("VarShowInt",
                           label = "Select region to show:",
                           choices = c("DUBLIN NORTH"       ,"WICKLOW"            ,"SOUTH MEATH"       ,
                                       "DUNDALK"            ,"DROGHEDA"           ,"DUBLIN SOUTH"      ,
                                       "BRAY"               ,"NAVAN"              ,"INNER CITY DUBLIN" ,
                                       "NORTH INNER DUBLIN" ,"SOUTH INNER DUBLIN" ,"WEST INNER DUBLIN" ,
                                       "DUBLIN WEST"        ,"TIPPERARY NORTH"    ,"ROSCREA"           ,
                                       "TIPPERARY SOUTH"    ,"WEST MAYO"          ,"SOUTH MAYO"        ,
                                       "ROSCOMMON"          ,"BOYLE"              ,"LIFFORD"           ,
                                       "CAVAN NORTH"        ,"MONAGHAN NORTH"     ,"BALLINASLOE"       ,
                                       "WEST GALWAY"        ,"LOUGHREA"           ,"ATHLONE"           ,
                                       "CORK CENTRAL"       ,"WEST CORK"          ,"CORK EAST"         ,
                                       "CORK CITY SOUTH"    ,"CORK NORTH"         ,"CARLOW"            ,
                                       "PORTLAOISE"         ,"OFFALY NORTH"       ,"BIRR"              ,
                                       "KILDARE WEST"       ,"KILKENNY"           ,"CLARE EAST"        ,
                                       "KILRUSH"            ,"KERRY NORTH"        ,"LIMERICK SOUTH"    ,
                                       "MAYNOOTH"           ,"NAAS"               ,"WATERFORD WEST"    ,
                                       "WATERFORD"          ,"ENNISCORTHY"        ,"GOREY"             ,
                                       "SOUTH WEXFORD"      ,"MONAGHAN SOUTH"     ,"SLIGO"             ,
                                       "SOUTH KERRY"        ,"LETTERKENNY"        ,"DONEGAL"           ,
                                       "BALLINA"            ,"CASTLEREA"          ,"LONGFORD"          ,
                                       "CARRICK-ON-SHANNON" ,"LIMERICK"           ,"MULLINGAR"         ,
                                       "ARKLOW"             ,"KELLS") , 
                           
                           # choices =  trimws(c('CARLOW','KELLS','NORTH WEST CAVAN',
                           #             'ENNIS & SHANNON','KILRUSH                  ','CORK-BALLINHASSIG       ',
                           #             'EAST CORK','MALLOW','SOUTH/WEST CORK',
                           #             'WEST CORK              ','DONEGAL                  ','LETTERKENNY             ',
                           #             'LIFFORD                ','INNER CITY DUBLIN        ','NORTH COUNTY DUBLIN     ',
                           #             'SOUTH COUNTY DUBLIN    ','WEST COUNTY DUBLIN       ','BALLINASLOE             ',
                           #             'LOUGHREA, ATHENRY, TUAM','WEST GALWAY              ','NORTH KERRY             ',
                           #             'SOUTH KERRY            ','ATHY                     ','KILDARE                 ',
                           #             'MAYNOOTH               ','NAAS                     ','NEWBRIDGE & CURRAGH CAMP',
                           #             'KILKENNY               ','PORTLAOISE               ','CARRICK-ON-SHANNON      ',
                           #             'LIMERICK               ','SOUTH LIMERICK           ','LONGFORD                ',
                           #             'DROGHEDA               ','DUNDALK                  ','BALLINA                 ',
                           #             'SOUTH MAYO             ','WEST MAYO                ','ENFIELD                 ',
                           #             'NAVAN                  ','SOUTH-EAST COUNTY MEATH  ','NORTH MONAGHAN          ',
                           #             'SOUTH MONAGHAN         ','BIRR                     ','TULLAMORE               ',
                           #             'BOYLE                  ','CASTLEREA                ','ROSCOMMON               ',
                           #             'SLIGO                  ','NORTH TIPPERARY          ','SOUTH-EAST TIPPERARY    ',
                           #             'THURLES                ','WEST TIPPERARY           ','SOUTH-WEST WATERFORD    ',
                           #             'WATERFORD              ','ATHLONE                  ','MULLINGAR               ',
                           #             'ENNISCORTHY            ','GOREY                    ','SOUTH WEXFORD           ',
                           #             'ARKLOW                 ','BRAY                     ','WICKLOW')),
                           selected = c("MAYNOOTH")
               ),
               h4(div(HTML("<em>Figure guide:</em>"))),
               h5(div(HTML("<em>The solid  line shows the 28-day centered sum of Notices Posted to RIP.ie for the selected region. The dashed line shows the average for the same period from 2015--2019. The dotted line shows the maximum for the same period.<em>"))),
        )
      )),
    
    
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      #Output: HTML table with requested number of observations ----
      # navbarPage("Output:",
      #          tabPanel("Spread",
               fluidPage(
                 fluidRow(
                   
                    plotOutput("plot1") %>% withSpinner(color="#1E90FF"),
                    
                    plotOutput("plot2")
          
                   
                    
                     )
                    
                    # uiOutput("HospBedper")
                    

    )
  )
#)
#,
      
               # tabPanel("Extinction",
               #          fluidPage(
               #            fluidRow(
               # 
               #              plotOutput("plot2")
               # 
               #            )
               #          )
               #        )


    #     )
     )
    
    
    ########################
  ) 





server <- function(input, output) {

  
  # load rebeccas merged data
 

  
  # calculate reference levels:
  ref_level <- merged_rk_data %>% filter(Year < 2020 & Year >=2015 ) %>% 
    ungroup() %>%
    group_by(Group,Date) %>% 
    summarize(Monthly_Notices = sum(Monthly_Notices)) %>%
    mutate(DOY=yday(Date)) %>%
    group_by(Group,DOY) %>%
    mutate(Ref_Level = mean(Monthly_Notices), 
           Prev_Max = max(Monthly_Notices))
  
  
  output$plot1 <- renderPlot({
    
      # choose group to plot
    ths_group <- input$VarShowInt
    
    x <- rk_grouped$RoutingKey[which(rk_grouped$Group==ths_group)]
    x <- knitr::combine_words(x)
    
    ggplot()+
      geom_line(data = merged_rk_data %>% 
                  filter(Group == ths_group) %>% 
                  filter(Year == 2020), 
                aes(x=Date,y=Monthly_Notices, linetype="2020"))+
      geom_line(data = ref_level %>% 
                  filter(Group == ths_group),
                aes(x=as.Date(DOY,origin="2020-01-01"),y=Prev_Max, linetype="Previous years max")) +
      geom_line(data = ref_level %>% 
                  filter(Group == ths_group),
                aes(x=as.Date(DOY,origin="2020-01-01"),y=Ref_Level, linetype="Previous years mean")) +
      facet_wrap(facets = vars(Group)) +
      ggtitle(paste0("Notices Posted in 2020 - Eircode: ", x)) +
      labs(x="",y="Monthly Notices") +
      theme(axis.text.x = element_text(angle = 90), legend.position = c(0.89, 0.85))  +
      scale_x_date(date_breaks = "1 month", date_labels = "%b",limits=c(as.Date("2020-01-01"),as.Date("2020-09-01"))) +
      labs(linetype = "") + 
      scale_linetype_manual(values=c("solid", "dotted", "dashed"))
    
   # ggplot()+
   #    geom_line(data = merged_rk_data %>% 
   #                filter(Group == ths_group) %>% 
   #                filter(Year == 2020), 
   #              aes(x=Date,y=Monthly_Notices)) +
   #    geom_line(data = ref_level %>% 
   #                filter(Group == ths_group),
   #              aes(x=as.Date(DOY,origin="2020-01-01"),y=Ref_Level),linetype="dashed") +
   #    geom_line(data = ref_level %>% 
   #                filter(Group == ths_group),
   #              aes(x=as.Date(DOY,origin="2020-01-01"),y=Prev_Max),linetype="dotted") + 
   #    facet_wrap(facets = vars(Group)) +
   #    ggtitle(paste0("Notices Posted in 2020 - Eircode: ", rk_grouped$RoutingKey[which(rk_grouped$Group==ths_group)])) +
   #    labs(x="",y="Monthly Notices") +
   #    theme(axis.text.x = element_text(angle = 90))  +
   #    scale_x_date(date_breaks = "1 month", date_labels = "%b",limits=c(as.Date("2020-01-01"),as.Date("2020-09-01")))
   #  
    #, rk_grouped$RoutingKey[which(rk_grouped$Group==ths_group)]
 
  })
  

  output$plot2 <- renderPlot({
    
    slct <- input$VarShowInt
    
    if(slct == 'DUBLIN NORTH'|| slct == 'DUBLIN SOUTH'|| slct == 'INNER CITY DUBLIN' || slct == 'NORTH INNER DUBLIN'|| slct == 'SOUTH INNER DUBLIN'|| slct == 'WEST INNER DUBLIN' || slct == 'DUBLIN WEST'){
      
      p <- ggplot()+
        geom_sf(data = ire %>% filter(name %in% c("Dún Laoghaire–Rathdown","South Dublin","Dublin","Fingal")),
                aes())+
        geom_sf(data = rk_grouped %>% filter(Group == slct), aes(), fill = "red")+
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(), 
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.background = element_blank()) +
              labs(title = "                     Dublin county")
      
    }
    else {
      
      p <- ggplot()+
      geom_sf(data = ire,aes()) +
      geom_sf(data = rk_grouped %>% filter(Group == slct), aes(), fill = "red") +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.background = element_blank())
      }

  p
  
  })
  
  # output$HospBedper <- renderUI({
  # 
  #   slct <- input$VarShowInt
  #   paste0("The group ", slct, " is comprised of: ", rk_grouped$Descriptor[which(rk_grouped$Group==slct)],
  #          " (", rk_grouped$RoutingKey[which(rk_grouped$Group==slct)], ")","The solid  line shows the 28-day centered sum of Notices Posted to RIP.ie for this region.",
  #   "The dashed line shows the average for the same period from 2015--2019. ",
  #   "The dotted line shows the maximum for the same period")
  # 
  # })
  
  
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)

