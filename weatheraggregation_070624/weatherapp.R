################################################################################
#                   Author: Joshua Thompson
#   O__  ----       Email:  joshua.thompson@ofwat.gov.uk
#  c/ /'_ ---
# (*) \(*) --
# ======================== Script  Information =================================
# PURPOSE: weather aggregator tool 
#
# PROJECT INFORMATION:
#   Name: weather aggregator tool
#
# HISTORY:----
#   Date		        Remarks
#	-----------	   ---------------------------------------------------------------
#	 30/05/2024    Created script                                   JThompson (JT)
#===============================  Environment Setup  ===========================
#==========================================================================================


# Load libraries
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(wesanderson) 
library(shinycssloaders)
library(shinyjs)
library(shinyBS)
library(httr)
library(jsonlite)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(extrafont)
library(stringr)
library(RColorBrewer)
library(plotly)
library(DT)
library(Cairo)
library(writexl)

#'[================================================]
#'[================================================]
#'##################################################
# Load data
####################################################
#'[================================================]
#'[================================================]

# Water company data and rainfall stations 
WCPR_Companies <- readRDS("WCPR_Companies.rds")
station_sf <- readRDS("station_sf.rds")
area_cents <- readRDS("area_cents.rds")

# Water company data and temperature stations 
WCPR_Companies_temp <- readRDS("WCPR_Companies_temp.rds")
temp_stations_sf <- readRDS("temp_station_sf.rds")

# Streamflow stations 
station_sf_stream <- readRDS("station_sf_stream.rds")

# Sewer overflow data and wastewater rainfall stations  
EDM_23 <- readRDS("EDM_23.rds")
EDM_22 <- readRDS("EDM_22.rds")
EDM_21 <- readRDS("EDM_21.rds")
WWRainfall_tbl <- readRDS("WWRainfall_tbl.rds")

#'[================================================]
#'[================================================]
#'##################################################
# Create UI
####################################################
#'[================================================]
#'[================================================]

ui <- fluidPage(
  useShinyjs(),
  # initial Ofwat logo loader 
  tags$head(
    tags$style(HTML("
      #initial_loader {
        position: fixed;
        width: 100%;
        height: 100%;
        background: white;
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        z-index: 10000;
        text-align: center;
      }
      #initial_loader img {
        width: 400px;
        height: auto;
      }
      .loading-bar {
        width: 400px;
        margin-top: 20px;
        height: 20px;
        background-color: #f3f3f3;
        border: 1px solid #ccc;
        border-radius: 5px;
        overflow: hidden;
        position: relative;
      }
      .loading-bar div {
        position: absolute;
        width: 100%;
        height: 100%;
        background-color: #003296;
        animation: loading 2s ease-in-out infinite;
      }
      @keyframes loading {
        0% { left: -100%; }
        50% { left: 0; }
        100% { left: 100%; }
      }
    "))
  ),
  
  div(id = "initial_loader",
      img(src = "Ofwat_logo.png", alt = "Loading..."),
      div(class = "loading-bar",
          div()
      )
  ),
  
  # set up UI style 
  tags$link(rel = "stylesheet", type = "text/css", href = "css/krub.css"),
  tags$head(HTML("<title>Weather Aggregator Tool</title> <link rel='icon' type='image/gif/png' href='Ofwat_logo.png'>"),
            tags$style(HTML(
              "
              .navbar{background-color: #FFFFFF !important; padding-left: 0px; margin-left:0px; padding-right: 0px; margin-right:0px;padding-top: 0px; margin-top:0px;}
              .navbar-default .navbar-brand:hover {color: blue;}
              .navbar { background-color: gray;}
              .navbar-default .navbar-nav > li > a {color:black;}
              .navbar-default .navbar-nav > .active > a,
              .navbar-default .navbar-nav > .active > a:focus,
              .navbar-default .navbar-nav > .active > a:hover {color: white;background-color: #505250;}
              .navbar-default .navbar-nav > li > a:hover {color: white;background-color:#505250;text-decoration:underline;}
              .butt{background-color:#505250; color:#FFFFFF; border-color:#080808;}
              .btn-file{background-color:#505250; color:#FFFFFF; border-color:#080808;}
              .action-button{background-color:#505250; color:#FFFFFF; border-color:#080808;}
              .radio input{background-color:#505250; color:#FFFFFF; border-color:#080808;}
              input[type='radio'] {filter: saturate(0);}
              input[type='checkbox'] {filter: saturate(0);}
              * {font-family: 'Krub', sans-serif;}
              /*html {overflow:   scroll;}
              ::-webkit-scrollbar {width: 0px; background: transparent; */}
              "
            )), 
            tags$script('
      function copyToClipboard() {
        var textArea = document.createElement("textarea");
        textArea.value = "joshua.thompson@ofwat.gov.uk";
        document.body.appendChild(textArea);
        textArea.select();
        document.execCommand("Copy");
        document.body.removeChild(textArea);
        alert("Email address copied to clipboard!");
      }
    ')),
  
  # define nheader components 
  titlePanel(
    fluidRow(style = "background-color:#ffffff; padding-top: 0px; margin-top:0px; padding-bottom: 20px; margin-bottom:0px",
             column(9,h2(("Weather Aggregator Tool"),style="font-size:26px;font-style:bold; font-weight: 600; color:black;"),
                    p("A tool to aggregate weather data by company area.",style="font-size:18px;font-style:normal; font-weight: 400; color:black;"),
                    a(actionButton(inputId = "email2", label = "   Help!",icon = icon("envelope", lib = "font-awesome"),
                                   style = "background-color:#505250; color:#FFFFFF; border-color:#080808",
                                   onclick = "copyToClipboard();")),
                    a(actionButton(inputId = "github1", label = "  Developer",icon = icon("github", lib = "font-awesome"),
                                   style = "background-color:#505250; color:#FFFFFF; border-color:#080808"),href="https://github.com/joshuajdthompson",target="_blank")),
             column(3, tags$a(img(src='Ofwat_logo.png', align = "right", width = '300px', height = 'auto', style="padding: 0px")))
    )),
  navbarPage("",
             #'*================================================*
             ###################################################
             # Rainfall
             ###################################################
             #'*================================================*
             tabPanel("Rainfall", value = "1", fluid = TRUE,icon=icon("cloud-showers-heavy"),
                      sidebarLayout(
                        sidebarPanel(
                          selectizeInput(inputId = "company", 
                                         label = "Select Company:",
                                         choices = unique(WCPR_Companies$COMPANY),
                                         selected = NULL,
                                         multiple = TRUE, 
                                         options = list(maxItems = 1)),
                          selectizeInput(inputId = "areaServed",
                                         label = "Region:",
                                         choices = unique(WCPR_Companies$AreaServed),
                                         selected = NULL,
                                         multiple = TRUE, 
                                         options = list(maxItems = 1)),
                          selectizeInput("site_selected", "Selected sites:", choices = NULL, multiple = TRUE, options = list(plugins = list('remove_button'))),
                          div(
                            style = "display: flex; align-items: center;",
                            actionButton("apinplot_button", "Generate API call and plot"),
                          ),
                          br(),
                          conditionalPanel(
                            condition = "input.apinplot_button > 0",
                            h4(strong("Download data"), style = "font-size: 22px; font-style: normal; font-weight: 400; color: black;"),
                            radioButtons("dataFormat", "Choose data format:", choices = c("Aggregated by company area" = "aggregated", "Individual gauges in company area" = "raw")),
                            downloadButton("downloadData", "Download data", class = "butt")
                          )
                        ),
                        mainPanel(
                          h4(strong("Map of company areas and rainfall gauges"),style="font-size:22px;font-style:normal; font-weight: 400; color: black;"),
                          leafletOutput("mainmap")%>% withSpinner(color="black"), 
                          br(), 
                          conditionalPanel(
                            condition = "input.apinplot_button > 0",
                            h4(strong("Rainfall plot"), style = "font-size: 22px; font-style: normal; font-weight: 400; color: black;"),
                            plotOutput("rainplot")
                          )
                        )
                      )
                      
             ),
             #'*================================================*
             ###################################################
             # Temperatire
             ###################################################
             #'*================================================*
             tabPanel("Temperature", value = "2", fluid = TRUE,icon=icon("temperature-high"),
                      sidebarLayout(
                        sidebarPanel(
                          selectizeInput(inputId = "companytemp", 
                                         label = "Select Company:",
                                         choices = unique(WCPR_Companies$COMPANY),
                                         selected = NULL,
                                         multiple = TRUE, 
                                         options = list(maxItems = 1)),
                          selectizeInput(inputId = "areaServedtemp",
                                         label = "Region:",
                                         choices = unique(WCPR_Companies$AreaServed),
                                         selected = NULL,
                                         multiple = TRUE, 
                                         options = list(maxItems = 1)),
                          selectizeInput("site_selectedtemp", "Selected sites (nearest):", choices = NULL, multiple = TRUE, options = list(plugins = list('remove_button'))),
                          div(
                            style = "display: flex; align-items: center;",
                            actionButton("apinplot_buttontemp", "Generate API call and plot"),
                          ),
                          br(),
                          conditionalPanel(
                            condition = "input.apinplot_buttontemp > 0",
                            h4(strong("Download data"), style = "font-size: 22px; font-style: normal; font-weight: 400; color: black;"),
                            radioButtons("dataFormattemp", "Choose data format:", choices = c("Aggregated by company area" = "aggregated", "Individual gauges in company area" = "raw")),
                            downloadButton("downloadDatatemp", "Download data", class = "butt")
                          )
                        ),
                        mainPanel(
                          h4(strong("Map of company areas and temperature gauges"),style="font-size:22px;font-style:normal; font-weight: 400; color: black;"),
                          leafletOutput("mainmaptemp")%>% withSpinner(color="black"), 
                          br(), 
                          conditionalPanel(
                            condition = "input.apinplot_buttontemp > 0",
                            h4(strong("Temperature plot"), style = "font-size: 22px; font-style: normal; font-weight: 400; color: black;"),
                            plotOutput("tempplot")
                          )
                        )
                      )
             ),
             #'*================================================*
             ###################################################
             # Stream flow
             ###################################################
             #'*================================================*
             tabPanel("Stream Flow", value = "3", fluid = TRUE,icon=icon("water"),
                      sidebarLayout(
                        sidebarPanel(
                          selectizeInput(inputId = "companystream", 
                                         label = "Select Company:",
                                         choices = unique(WCPR_Companies$COMPANY),
                                         selected = NULL,
                                         multiple = TRUE, 
                                         options = list(maxItems = 1)),
                          selectizeInput(inputId = "areaServedstream",
                                         label = "Region:",
                                         choices = unique(WCPR_Companies$AreaServed),
                                         selected = NULL,
                                         multiple = TRUE, 
                                         options = list(maxItems = 1)),
                          selectizeInput("site_selectedstream", "Selected sites:", choices = NULL, multiple = TRUE, options = list(plugins = list('remove_button'))),
                          div(
                            style = "display: flex; align-items: center;",
                            actionButton("apinplot_buttonstream", "Generate API call and plot"),
                          ),
                          br(),
                          conditionalPanel(
                            condition = "input.apinplot_buttonstream > 0",
                            h4(strong("Download data"), style = "font-size: 22px; font-style: normal; font-weight: 400; color: black;"),
                            radioButtons("dataFormatstream", "Choose data format:", choices = c("Aggregated by company area" = "aggregated", "Individual gauges in company area" = "raw")),
                            downloadButton("downloadDatastream", "Download data", class = "butt")
                          )
                        ),
                        mainPanel(
                          h4(strong("Map of company areas and stream flow gauges"),style="font-size:22px;font-style:normal; font-weight: 400; color: black;"),
                          leafletOutput("streammap")%>% withSpinner(color="black"), 
                          br(), 
                          conditionalPanel(
                            condition = "input.apinplot_buttonstream > 0",
                            h4(strong("Stream plot"), style = "font-size: 22px; font-style: normal; font-weight: 400; color: black;"),
                            plotOutput("streamplot")
                          )
                        )
                      )
             ),
             #'*================================================*
             ###################################################
             # Storm overflows
             ###################################################
             #'*================================================*
             tabPanel("Storm Overflows", value = "4", fluid = TRUE,icon=icon("circle-dot"),
                      sidebarLayout(
                        sidebarPanel(
                          selectizeInput("year", "Year of data to map:", 
                                         choices = c("2023", "2022", "2021"),
                                         selected = F,
                                         options = list(maxItems = 1)),
                          selectizeInput("waterCompanyName", "Water company:", choices = NULL,selected = NULL),  # Choices will be updated based on year selection
                          div(
                            style = "display: flex; align-items: center;",
                            actionButton("plotoverflow", "Aggregate sewer overflow data and plot"),
                          ), br(),
                          conditionalPanel(
                            condition = "input.plotoverflow > 0",
                            h4(strong("Gather rainfall data"), style = "font-size: 22px; font-style: normal; font-weight: 400; color: black;"),
                            actionButton("apinplot_buttonsewerrain", "Generate API call and plot"),
                          ),
                          br(),
                          conditionalPanel(
                            condition = "input.apinplot_buttonsewerrain > 0",
                            h4(strong("Download data"), style = "font-size: 22px; font-style: normal; font-weight: 400; color: black;"),
                            radioButtons("dataFormatsewer", "Choose data format:", choices = c("Aggregated by company area" = "aggregated", "Individual gauges in company area" = "raw")),
                            downloadButton("downloadDatasewer", "Download data", class = "butt")
                          )
                        ),
                        mainPanel(
                          h4(strong("Map of storm overflows by spill hours"),style="font-size:22px;font-style:normal; font-weight: 400; color: black;"),
                          leafletOutput("overflowmap")%>% withSpinner(color="black"), 
                          br(), 
                          conditionalPanel(
                            condition = "input.plotoverflow > 0",
                            h4(strong("Storm overflow plot"), style = "font-size: 22px; font-style: normal; font-weight: 400; color: black;"),
                            plotOutput("overflowplot")
                          ),
                          br(), 
                          conditionalPanel(
                            condition = "input.apinplot_buttonsewerrain > 0",
                            h4(strong("Storm overflow rainfall plot"), style = "font-size: 22px; font-style: normal; font-weight: 400; color: black;"),
                            plotOutput("rainplotsewer")
                          )
                        )
                      )
             )
  )
)

#'[================================================]
#'[================================================]
#'##################################################
# Create server logic
####################################################
#'[================================================]
#'[================================================]
server <- function(input, output, session) {
  
  # hide initial loader
  observe({
    invalidateLater(3000, session)
    removeUI(selector = "#initial_loader")
  })
  
  # set up reactives to hold data values 
  data_values <- reactiveValues(rainfall = NULL, monthly_rainfall = NULL, temperature = NULL, monthly_temp = NULL,
                                discharge = NULL, monthly_discharge = NULL,annual_rainfallsewer = NULL, rainfallsewer = NULL)
    
  #'*================================================*
  ###################################################
  # Rainfall
  ###################################################
  #'*================================================*

  #######################
  # update dropdowns
  #######################
  observeEvent(input$company, {
    area=unique(WCPR_Companies %>%
                  dplyr::filter(COMPANY==input$company) %>%
                  pull(AreaServed))
    updateSelectizeInput(session,
                         "areaServed",
                         selected = NULL,
                         choices = area)
  }) 
  
  #===================================================
  # company Map
  #===================================================
  output$mainmap <-renderLeaflet({
    leaflet() %>%
      setView(lng = -0.8556470749832616, lat = 52.55857268224709, zoom = 6) %>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      addPolygons(data = WCPR_Companies, 
                  fill = TRUE,
                  color = "black", 
                  weight =1,
                  fillColor = "lightgrey",
                  fillOpacity = 0.35,
                  popup = paste("Company: ", WCPR_Companies$COMPANY, "<br>",
                                "Area served: ", WCPR_Companies$AreaServed,"<br>"),
                  highlightOptions = highlightOptions(color = "navy",
                                                      opacity = 0.8,
                                                      weight = 5,
                                                      bringToFront = FALSE),
                  group = "companies")%>%
      addCircles(data = station_sf,
                 radius = 1500,
                 fillColor = "red",
                 fillOpacity = 1,
                 color = "black",
                 weight = 1,
                 stroke = T,
                 popup = paste("Rainfall gauge name: ", station_sf$stationName),
                 highlightOptions = highlightOptions(color = "black",
                                                     opacity = 1.0,
                                                     weight = 3,
                                                     bringToFront = TRUE),
                 group = "gauges") %>%
      addMiniMap(
        tiles = providers$CartoDB.Positron,
        position = 'topright', 
        width = 200, height = 200,
        toggleDisplay = FALSE,
        aimingRectOptions = list(color = "red", weight = 1, clickable = FALSE),
        zoomLevelOffset=-3)
    
  })
  
  # create a proxy of the map
  mainmapProxy <- leafletProxy("mainmap")
  
  observe({
    if(!is.null(input$company) & is.null(input$areaServed)){
      
      #get the selected comany polygons
      selected_area <- WCPR_Companies %>% filter(COMPANY==input$company)#%>% head(1)
      selected_gauges <- station_sf %>% filter(COMPANY==input$company)
      
      #remove any previously highlighted polygons
      mainmapProxy %>% clearGroup(group="highlighted_polygon") %>% 
        clearGroup(group="highlighted_points") %>% 
        showGroup("companies") %>% showGroup("gauges")

      
      #add a slightly thicker polygon on top of the selected one
      mainmapProxy %>% 
        addPolylines(stroke=TRUE, weight = 5,color="black",data=selected_area,group="highlighted_polygon") %>% 
        addCircles(stroke=TRUE, radius = 1500, weight = 5,color="black",fillColor = "black",
                   data=selected_gauges,group="highlighted_points",
                   popup = paste("Rainfall gauge name: ", selected_gauges$stationName))
      
      #update site input
      updateSelectizeInput(session, "site_selected", choices = selected_gauges$stationName, selected = selected_gauges$stationName)
      
    }else if(!is.null(input$company) & !is.null(input$areaServed)){
      
      #get the selected comany polygons
      selected_area <- WCPR_Companies %>% filter(AreaServed==input$areaServed)#%>% head(1)
      selected_gauges <- station_sf %>% filter(AreaServed==input$areaServed)
      
      #remove any previously highlighted polygons
      mainmapProxy %>% clearGroup(group="highlighted_polygon") %>% 
        clearGroup(group="highlighted_points") %>% 
        showGroup("companies") %>% showGroup("gauges")
      
      # get centroid lat and long of selected area polygon
      selected_area_centroids <- area_cents %>% filter(AreaServed == input$areaServed) %>% head(1) 
      
      #center the view on the selected area polygon 
      mainmapProxy %>% setView(lng=selected_area_centroids$long,lat=selected_area_centroids$lat,zoom=8)
      
      #add a slightly thicker red polygon on top of the selected one
      mainmapProxy %>% 
        addPolylines(stroke=TRUE, weight = 5,color="black",data=selected_area,group="highlighted_polygon") %>% 
        addCircles(stroke=TRUE, radius = 700, weight = 5,color="black",fillColor = "black",
                   data=selected_gauges,group="highlighted_points",
                   popup = paste("Rainfall gauge name: ", selected_gauges$stationName))
      
      #update site input
      updateSelectizeInput(session, "site_selected", choices = selected_gauges$stationName, selected = selected_gauges$stationName)
      
    }
  })
  
  # button generate API call and plot
  observeEvent(input$apinplot_button, {
    req(input$site_selected)  
    showModal(modalDialog(
      title = NULL,
      size = "l",  
      tags$div(
        style = "text-align: center; font-family: 'Krub', sans-serif; font-weight: 600; font-size: 30px;",  
        tags$img(src = "ea_logo.png", alt = "EA Logo", style = "width: 400px; height: auto;"),
        br(),
        "Connecting with the Environment Agency API and aggregating data... please wait",
        br(),br(),
        div(
          class = "loader",
          style = "margin: auto; border: 5px solid #f3f3f3; border-top: 5px solid #009f41; border-radius: 80%; width: 100px; height: 100px; animation: spin 2s linear infinite;"
        ), br(),
      ),
      easyClose = FALSE,
      footer = NULL,
      style = "border: none; box-shadow: none; background-color: rgba(255, 255, 255, 0.5); padding: 0;",
      tags$head(
        tags$style(HTML("
      @keyframes spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
    "))
      )
    ))
    stations <- station_sf %>% filter(stationName %in% input$site_selected) %>% pull(wiskiID)
    
    rainfall <- tibble()
    
    for(i in stations){
      url.rain <- paste0("https://environment.data.gov.uk/hydrology/data/readings.json?max-date=2024-04-01&mineq-date=2012-04-01&period=86400&station.wiskiID=",as.character(i))
      
      # send API request
      response.rain <- GET(url.rain)
      
      if (http_status(response.rain)$category == "Success") {
        # parse JSON 
        data.rain <- tryCatch({
          fromJSON(content(response.rain, "text"), flatten = TRUE)
        }, error = function(e) {
          message("Error parsing JSON: ", e)
          return(NULL)
        })
        
        # check if it worked
        if (!is.null(data.rain)) {
          # extract relevant bits we are interested in
          station_data.rain <- data.rain$items
          
          # check if station data is available
          if (!is.null(station_data.rain) && length(station_data.rain) > 0) {
            
            statName <- station_sf %>% filter(wiskiID == i) %>% pull(stationName)
            # make tibble
            station_df.rain <- tibble(
              date = as.Date(station_data.rain$date),
              rain_mm = station_data.rain$value,
              station = statName,
              wiskiID = i,
            ) %>% filter(complete.cases(.))
            
          } else {
            message("No data available for the specified station and date range.")
          }
        } else {
          message("Failed to parse JSON response.")
        }
      } else {
        message("Error: API request failed with status ", http_status(response.rain)$reason)
      }
      rainfall <- bind_rows(rainfall,station_df.rain) 
      
      
    }
    
    # save data to the reactiveValues
    data_values$rainfall <- rainfall
    print(head(data_values$rainfall))
    
    # plot
    output$rainplot <- renderPlot({
      monthly_rainfall <- rainfall %>% filter(complete.cases(.))  %>%
        mutate(year = year(date), month = month(date)) %>%
        group_by(year, month,station) %>%
        summarize(total_rain_mm = sum(rain_mm, na.rm = TRUE), .groups = 'drop') %>%
        ungroup() %>%
        group_by(year, month) %>%
        summarize(average_rain_mm = mean(total_rain_mm, na.rm = TRUE), .groups = 'drop') %>%
        mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>% 
        filter(complete.cases(.))
      
      # update the reactiveValues
      data_values$monthly_rainfall <- monthly_rainfall
      print(head(data_values$monthly_rainfall))
      
      ggplot(monthly_rainfall %>% mutate(Title = str_wrap(paste0('Average monthly rainfall (mm) observed for ', input$company, ifelse(!is.na(input$areaServed),paste0(' in the ', input$areaServed , ' region'),"")),width=50)), aes(x = date, y = 1, fill = average_rain_mm)) +
        facet_wrap(~Title,ncol=1) +
        geom_tile() +
        scale_x_date(expand = c(0, 0), date_breaks = "1 years", date_labels = "%Y") +
        scale_y_continuous(expand = c(0, 0)) +
        scale_fill_gradientn(colours = brewer.pal(9, "Blues"),
                             guide = guide_colorbar(
                               barheight = unit(3, units = "mm"),
                               barwidth = unit(50, units = "mm"),
                               title.vjust = 1)) +
        labs(x = "Year", y = "",fill = "Monthly Rainfall (mm)") +
        theme(text = element_text(family = "Krub"), 
              axis.text.x = element_text(angle = 0, vjust = 0, hjust=0.5,colour="black"),
              strip.background = element_rect(fill="black"),
              strip.text = element_text(colour = 'white',face="bold",size = 12),
              panel.background = element_blank(),
              panel.grid.major.x = element_line(size = 0.5, linetype = 'solid',
                                                colour = "lightgrey"),
              panel.border=element_rect(colour="black",size=1,fill=NA),
              axis.text.y=element_blank(),axis.ticks.y = element_blank(),
              legend.position = "bottom")
    }, res = 128)
    
    # hide modal
    removeModal()
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      if (input$dataFormat == "raw") {
        paste0("rainfall_raw",input$company,"_",Sys.Date(),".xlsx")
      } else {
        paste0("monthly_rainfall_aggregated",input$company,"_",Sys.Date(),".xlsx")
      }
    },
    content = function(file) {
      if (input$dataFormat == "raw") {
        write_xlsx(data_values$rainfall, file)
      } else {
        write_xlsx(data_values$monthly_rainfall, file)
      }
    }
  )
  
  #'*================================================*
  ###################################################
  # Temperature
  ###################################################
  #'*================================================*
  
  data_valuestemp <- reactiveValues(temperature = NULL)
  
  #######################
  # update dropdowns
  #######################
  observeEvent(input$companytemp, {
    area=unique(WCPR_Companies %>%
                  dplyr::filter(COMPANY==input$companytemp) %>%
                  pull(AreaServed))
    updateSelectizeInput(session,
                         "areaServedtemp",
                         selected = NULL,
                         choices = area)
  }) 
  
  #===================================================
  # company Map
  #===================================================
  output$mainmaptemp <-renderLeaflet({
    leaflet() %>%
      setView(lng = -0.8556470749832616, lat = 52.55857268224709, zoom = 6) %>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      addPolygons(data = WCPR_Companies, 
                  fill = TRUE,
                  color = "black", 
                  weight =1,
                  fillColor = "lightgrey",
                  fillOpacity = 0.35,
                  popup = paste("Company: ", WCPR_Companies$COMPANY, "<br>",
                                "Area served: ", WCPR_Companies$AreaServed,"<br>"),
                  highlightOptions = highlightOptions(color = "navy",
                                                      opacity = 0.8,
                                                      weight = 5,
                                                      bringToFront = FALSE),
                  group = "companies")%>%
      addCircles(data = temp_stations_sf,
                 radius = 10000,
                 fillColor = "red",
                 fillOpacity = 1,
                 color = "black",
                 weight = 1,
                 stroke = T,
                 popup = paste("Temperature gauge name: ", temp_stations_sf$Name),
                 highlightOptions = highlightOptions(color = "black",
                                                     opacity = 1.0,
                                                     weight = 3,
                                                     bringToFront = TRUE),
                 group = "gauges") %>%
      addMiniMap(
        tiles = providers$CartoDB.Positron,
        position = 'topright', 
        width = 200, height = 200,
        toggleDisplay = FALSE,
        aimingRectOptions = list(color = "red", weight = 1, clickable = FALSE),
        zoomLevelOffset=-3)
    
  })
  
  # create a proxy of the map
  mainmapProxytemp <- leafletProxy("mainmaptemp")
  
  observe({
    if(!is.null(input$companytemp) & is.null(input$areaServedtemp)){
      
      #get the selected comany polygons
      selected_area_temp <- WCPR_Companies_temp %>% filter(COMPANY==input$companytemp)#%>% head(1)
      selected_gauges_temp <- temp_stations_sf %>% slice(c(WCPR_Companies_temp %>% filter(COMPANY==input$companytemp) %>% 
                                                             pull(nearest_temp_station) %>% unique()))

      
      #remove any previously highlighted polygons
      mainmapProxytemp %>% clearGroup(group="highlighted_polygon") %>% 
        clearGroup(group="highlighted_points") %>% 
        showGroup("companies") %>% showGroup("gauges")

      #add a slightly thicker polygon on top of the selected one
      mainmapProxytemp %>% 
        addPolylines(stroke=TRUE, weight = 5,color="black",data=selected_area_temp,group="highlighted_polygon") %>% 
        addCircles(stroke=TRUE, radius = 10000, weight = 5,color="black",fillColor = "black",
                   data=selected_gauges_temp,group="highlighted_points",
                   popup = paste("Temperature gauge name: ", selected_gauges_temp$Name))
      
      #update site input
      updateSelectizeInput(session, "site_selectedtemp", choices = selected_gauges_temp$Name, selected = selected_gauges_temp$Name)
      
    }else if(!is.null(input$companytemp) & !is.null(input$areaServedtemp)){
      
      #get the selected comany polygons
      selected_area_temp <- WCPR_Companies_temp %>% filter(AreaServed==input$areaServedtemp)#%>% head(1)
      selected_gauges_temp <- temp_stations_sf %>% slice(c(WCPR_Companies_temp %>% filter(AreaServed==input$areaServedtemp) %>% 
                                                             pull(nearest_temp_station) %>% unique()))
      print(selected_gauges_temp)
      
      #remove any previously highlighted polygons
      mainmapProxytemp %>% clearGroup(group="highlighted_polygon") %>% 
        clearGroup(group="highlighted_points") %>% 
        showGroup("companies") %>% showGroup("gauges")
      
      # get centroid lat and long of selected area polygon
      selected_area_centroids_temp <- area_cents %>% filter(AreaServed == input$areaServedtemp) %>% head(1) 
      
      #center the view on the selected area polygon 
      mainmapProxytemp %>% setView(lng=selected_area_centroids_temp$long,lat=selected_area_centroids_temp$lat,zoom=8)
      
      #add a slightly thicker red polygon on top of the selected one
      mainmapProxytemp %>% 
        addPolylines(stroke=TRUE, weight = 5,color="black",data=selected_area_temp,group="highlighted_polygon") %>% 
        addCircles(stroke=TRUE, radius = 7000, weight = 5,color="black",fillColor = "black",
                   data=selected_gauges_temp,group="highlighted_points",
                   popup = paste("Temperature gauge name: ", selected_gauges_temp$Name))
      
      #update site input
      updateSelectizeInput(session, "site_selectedtemp", choices = selected_gauges_temp$Name, selected = selected_gauges_temp$Name)
      
    }
  })
  
  # button to generate API call and plot
  observeEvent(input$apinplot_buttontemp, {
    req(input$site_selectedtemp)  
    showModal(modalDialog(
      title = NULL,
      size = "l",  
      tags$div(
        style = "text-align: center; font-family: 'Krub', sans-serif; font-weight: 600; font-size: 30px;",  
        tags$img(src = "Met_Office_logo.png", alt = "Met Office Logo", style = "width: 400px; height: auto;"),
        br(),
        "Connecting with the Met Office API and gathering data... please wait",
        br(),br(),
        div(
          class = "loader",
          style = "margin: auto; border: 5px solid #f3f3f3; border-top: 5px solid #d7e01e; border-radius: 80%; width: 100px; height: 100px; animation: spin 2s linear infinite;"
        ), br(),
      ),
      easyClose = FALSE,
      footer = NULL,
      style = "border: none; box-shadow: none; background-color: rgba(255, 255, 255, 0.5); padding: 0;",
      tags$head(
        tags$style(HTML("
      @keyframes spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
    "))
      )
    ))
    
    stations <- temp_stations_sf %>% filter(Name %in% input$site_selectedtemp) %>% pull(Name)
    temperature <- tibble()
    
    for(i in stations){
      #print(i)
      raw <- read_table(temp_stations_sf %>% filter(Name == i) %>% pull(link),
                        col_names = FALSE, skip = 20,
                        col_types = cols(
                          .default = col_character()
                        )) %>% 
        slice(-1) %>%
        mutate(across(everything(), as.numeric)) %>%
        rename(
          yyyy = X1,
          mm = X2,
          tmax = X3,
          tmin = X4,
          af = X5,
          rain = X6,
          sun = X7
        )
      
      #print(raw)
      
      df <- raw %>%
        mutate(yyyy = ymd(str_c(yyyy, mm,"-01", sep = "-"))) 
      
      station_df.temp <- df %>% 
        group_by(yyyy) %>% 
        summarise(average = (mean(tmax, na.rm = TRUE) + mean(tmin, na.rm = TRUE)) / 2) %>%
        mutate(station = i)
      
      temperature <- bind_rows(temperature,station_df.temp)
    }
    
    # Update the reactiveValues
    data_values$temperature <- temperature
   #print(head(data_values$temperature))
    
    # plot 
    output$tempplot <- renderPlot({
      monthly_temp <- temperature %>% filter(complete.cases(.))  %>%
        group_by(yyyy, station) %>%
        summarize(average_temp_c = mean(average, na.rm = TRUE), .groups = 'drop') %>%
        ungroup() %>% filter(complete.cases(.))
      
      # save data to the reactiveValues
      data_values$monthly_temp <- monthly_temp
      
      ggplot(data_values$monthly_temp %>% filter(yyyy >= as.Date("2012-04-01")) %>% mutate(Title = str_wrap(paste0('Average monthly temperature (C) observed for ', input$companytemp, ifelse(!is.na(input$areaServedtemp),paste0(' in the ', input$areaServedtemp , ' region'),"")),width=50)), aes(x = yyyy, y = 1, fill = average_temp_c)) +
        facet_wrap(~Title,ncol=1) +
        geom_tile() +
        scale_x_date(expand = c(0, 0), date_breaks = "1 years", date_labels = "%Y") +
        scale_y_continuous(expand = c(0, 0)) +
        scale_fill_gradientn(colours = rev(brewer.pal(11, "RdBu")),
                             guide = guide_colorbar(
                               barheight = unit(3, units = "mm"),
                               barwidth = unit(50, units = "mm"),
                               title.vjust = 1)) +
        labs(x = "Year", y = "",fill = "Average Monthly Temperature (c)") +
        theme(text = element_text(family = "Krub"), 
              axis.text.x = element_text(angle = 0, vjust = 0, hjust=0.5,colour="black"),
              strip.background = element_rect(fill="black"),
              strip.text = element_text(colour = 'white',face="bold",size = 12),
              panel.background = element_blank(),
              panel.grid.major.x = element_line(size = 0.5, linetype = 'solid',
                                                colour = "lightgrey"),
              panel.border=element_rect(colour="black",size=1,fill=NA),
              axis.text.y=element_blank(),axis.ticks.y = element_blank(),
              legend.position = "bottom")

    }, res = 128)
    
    # hide loader
    removeModal()
    
  })
  
  output$downloadDatatemp <- downloadHandler(
    filename = function() {
      if (input$dataFormattemp == "raw") {
        paste0("temperature_raw",input$companytemp,"_",Sys.Date(),".xlsx")
      } else {
        paste0("monthly_temperature_aggregated",input$companytemp,"_",Sys.Date(),".xlsx")
      }
    },
    content = function(file) {
      if (input$dataFormattemp == "raw") {
        write_xlsx(data_values$temperature, file)  
      } else {
        write_xlsx(data_values$monthly_temp, file)  
      }
    }
  )
  
  #'*================================================*
  ###################################################
  # Stream
  ###################################################
  #'*================================================*
  
  #######################
  # update dropdowns
  #######################
  observeEvent(input$companystream, {
    area=unique(WCPR_Companies %>%
                  dplyr::filter(COMPANY==input$companystream) %>%
                  pull(AreaServed))
    updateSelectizeInput(session,
                         "areaServedstream",
                         selected = NULL,
                         choices = area)
  }) 
  
  #===================================================
  # company Map
  #===================================================
  output$streammap <-renderLeaflet({
    leaflet() %>%
      setView(lng = -0.8556470749832616, lat = 52.55857268224709, zoom = 6) %>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      addPolygons(data = WCPR_Companies, 
                  fill = TRUE,
                  color = "black", 
                  weight =1,
                  fillColor = "lightgrey",
                  fillOpacity = 0.35,
                  popup = paste("Company: ", WCPR_Companies$COMPANY, "<br>",
                                "Area served: ", WCPR_Companies$AreaServed,"<br>"),
                  highlightOptions = highlightOptions(color = "navy",
                                                      opacity = 0.8,
                                                      weight = 5,
                                                      bringToFront = FALSE),
                  group = "companies")%>%
      addCircles(data = station_sf_stream,
                 radius = 1500,
                 fillColor = "red",
                 fillOpacity = 1,
                 color = "black",
                 weight = 1,
                 stroke = T,
                 popup = paste("Stream gauge name: ", station_sf_stream$stationName),
                 highlightOptions = highlightOptions(color = "black",
                                                     opacity = 1.0,
                                                     weight = 3,
                                                     bringToFront = TRUE),
                 group = "gauges") %>%
      addMiniMap(
        tiles = providers$CartoDB.Positron,
        position = 'topright', 
        width = 200, height = 200,
        toggleDisplay = FALSE,
        aimingRectOptions = list(color = "red", weight = 1, clickable = FALSE),
        zoomLevelOffset=-3)
    
  })
  
  # create a proxy of the map
  streammapProxy <- leafletProxy("streammap")
  
  observe({
    if(!is.null(input$companystream) & is.null(input$areaServedstream)){
      
      #get the selected comany polygons
      selected_areastream <- WCPR_Companies %>% filter(COMPANY==input$companystream)#%>% head(1)
      selected_gaugesstream <- station_sf_stream %>% filter(COMPANY==input$companystream)
      
      #remove any previously highlighted polygons
      streammapProxy %>% clearGroup(group="highlighted_polygon") %>% 
        clearGroup(group="highlighted_points") %>% 
        showGroup("companies") %>% showGroup("gauges")

      #add a slightly thicker red polygon on top of the selected one
      streammapProxy %>% 
        addPolylines(stroke=TRUE, weight = 5,color="black",data=selected_areastream,group="highlighted_polygon") %>% 
        addCircles(stroke=TRUE, radius = 1500, weight = 5,color="black",fillColor = "black",
                   data=selected_gaugesstream,group="highlighted_points",
                   popup = paste("Stream gauge name: ", selected_gaugesstream$stationName))
      
      #update site input
      updateSelectizeInput(session, "site_selectedstream", choices = selected_gaugesstream$stationName, selected = selected_gaugesstream$stationName)
      
    }else if(!is.null(input$companystream) & !is.null(input$areaServedstream)){
      
      #get the selected comany polygons
      selected_areastream <- WCPR_Companies %>% filter(AreaServed==input$areaServedstream)#%>% head(1)
      selected_gaugesstream <- station_sf_stream %>% filter(AreaServed==input$areaServedstream)
      
      #remove any previously highlighted polygons
      streammapProxy %>% clearGroup(group="highlighted_polygon") %>% 
        clearGroup(group="highlighted_points") %>% 
        showGroup("companies") %>% showGroup("gauges")
      
      # get centroid lat and long of selected area polygon
      selected_area_centroids <- area_cents %>% filter(AreaServed == input$areaServedstream) %>% head(1) 
      
      #center the view on the selected area polygon 
      streammapProxy %>% setView(lng=selected_area_centroids$long,lat=selected_area_centroids$lat,zoom=8)
      
      #add a slightly thicker  polygon on top of the selected one
      streammapProxy %>% 
        addPolylines(stroke=TRUE, weight = 5,color="black",data=selected_areastream,group="highlighted_polygon") %>% 
        addCircles(stroke=TRUE, radius = 700, weight = 5,color="black",fillColor = "black",
                   data=selected_gaugesstream,group="highlighted_points",
                   popup = paste("Stream gauge name: ", selected_gaugesstream$stationName))
      
      #update site input
      updateSelectizeInput(session, "site_selectedstream", choices = selected_gaugesstream$stationName, selected = selected_gaugesstream$stationName)
      
    }
  })
  
  
  # button action to generate API call and plot
  observeEvent(input$apinplot_buttonstream, {
    req(input$site_selectedstream)  
    showModal(modalDialog(
      title = NULL,
      size = "l",  
      tags$div(
        style = "text-align: center; font-family: 'Krub', sans-serif; font-weight: 600; font-size: 30px;",  
        tags$img(src = "ea_logo.png", alt = "EA Logo", style = "width: 400px; height: auto;"),
        br(),
        "Connecting with the Environment Agency API and aggregating data... please wait",
        br(),br(),
        div(
          class = "loader",
          style = "margin: auto; border: 5px solid #f3f3f3; border-top: 5px solid #009f41; border-radius: 80%; width: 100px; height: 100px; animation: spin 2s linear infinite;"
        ), br(),
      ),
      easyClose = FALSE,
      footer = NULL,
      style = "border: none; box-shadow: none; background-color: rgba(255, 255, 255, 0.5); padding: 0;",
      tags$head(
        tags$style(HTML("
      @keyframes spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
    "))
      )
    ))
    stationsstream <- station_sf_stream %>% filter(stationName %in% input$site_selectedstream) %>% pull(wiskiID)
    
    discharge <- tibble()
    
    for(i in stationsstream){
      url.stream <- paste0("https://environment.data.gov.uk/hydrology/data/readings.json?max-date=2024-04-01&mineq-date=2012-04-01&period=86400&valueType=mean&station.wiskiID=",as.character(i))

      catchmentArea <- station_sf_stream %>% filter(wiskiID %in% i) %>% pull(catchmentArea)

          # send API request
      response.stream <- GET(url.stream)
      
      if (http_status(response.stream)$category == "Success") {
        # parse JSON 
        data.stream <- tryCatch({
          fromJSON(content(response.stream, "text"), flatten = TRUE)
        }, error = function(e) {
          message("Error parsing JSON: ", e)
          return(NULL)
        })
        
        # check if it worked
        if (!is.null(data.stream)) {
          # extract relevant bits we are interested in
          station_data.stream <- data.stream$items
          
          # check if station data is available
          if (!is.null(station_data.stream) && length(station_data.stream) > 0) {
            
            statName <- station_sf_stream %>% filter(wiskiID == i) %>% pull(stationName)
            # make tibble
            station_df.stream <- tibble(
              date = as.Date(station_data.stream$date),
              discharge_m3s = station_data.stream$value,
              station = statName,
              wiskiID = i,
              catchmentArea = catchmentArea
            ) %>% filter(complete.cases(.))
            
          } else {
            message("No data available for the specified station and date range.")
          }
        } else {
          message("Failed to parse JSON response.")
        }
      } else {
        message("Error: API request failed with status ", http_status(response.stream)$reason)
      }
      discharge <- bind_rows(discharge,station_df.stream) 
      
      
    }
    
    # save data to the reactiveValues
    data_values$discharge <- discharge
    print(head(data_values$discharge))
    
    # Plot the data
    output$streamplot <- renderPlot({
      monthly_discharge <- discharge %>% filter(complete.cases(.))  %>%
        mutate(year = year(date), month = month(date)) %>%
        group_by(year, month,station) %>%
        summarize(total_discharge_mm = sum((discharge_m3s*86.4)/(catchmentArea), na.rm = TRUE),
                  catchmentArea = unique(catchmentArea), .groups = 'drop') %>%
        ungroup() %>%
        group_by(year, month) %>%
        summarize(average_discharge_mm = mean(total_discharge_mm, na.rm = TRUE), .groups = 'drop') %>%
        mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>% 
        filter(complete.cases(.))
      
      # Update the reactiveValues
      data_values$monthly_discharge <- monthly_discharge
      print(head(data_values$monthly_discharge))
      
      ggplot(monthly_discharge %>% mutate(Title = str_wrap(paste0('Average monthly discharge (mm) observed for ', input$companystream, ifelse(!is.na(input$areaServedstream),paste0(' in the ', input$areaServedstream , ' region'),"")),width=50)), aes(x = date, y = 1, fill = average_discharge_mm)) +
        facet_wrap(~Title,ncol=1) +
        geom_tile() +
        scale_x_date(expand = c(0, 0), date_breaks = "1 years", date_labels = "%Y") +
        scale_y_continuous(expand = c(0, 0)) +
        scale_fill_gradientn(colours = brewer.pal(9, "Blues"),
                             guide = guide_colorbar(
                               barheight = unit(3, units = "mm"),
                               barwidth = unit(50, units = "mm"),
                               title.vjust = 1)) +
        labs(x = "Year", y = "",fill = "Monthly Discharge (mm)") +
        theme(text = element_text(family = "Krub"), 
              axis.text.x = element_text(angle = 0, vjust = 0, hjust=0.5,colour="black"),
              strip.background = element_rect(fill="black"),
              strip.text = element_text(colour = 'white',face="bold",size = 12),
              panel.background = element_blank(),
              panel.grid.major.x = element_line(size = 0.5, linetype = 'solid',
                                                colour = "lightgrey"),
              panel.border=element_rect(colour="black",size=1,fill=NA),
              axis.text.y=element_blank(),axis.ticks.y = element_blank(),
              legend.position = "bottom")

    }, res = 128)
    
    # Hide the modal dialog
    removeModal()
    
  })
  
  output$downloadDatastream <- downloadHandler(
    filename = function() {
      if (input$dataFormatstream == "raw") {
        paste0("discharge_raw",input$companystream,"_",Sys.Date(),".xlsx")
      } else {
        paste0("monthly_discharge_aggregated",input$companystream,"_",Sys.Date(),".xlsx")
      }
    },
    content = function(file) {
      if (input$dataFormatstream == "raw") {
        write_xlsx(data_values$discharge, file)  
      } else {
        write_xlsx(data_values$monthly_discharge, file)  
      }
    }
  )
  
  #'*================================================*
  ###################################################
  # Sewer Overflows
  ###################################################
  #'*================================================*
   
  observeEvent(input$year, {
    # Update data display  based on year selection
    selected_data <- switch(input$year,
                            "2023" = EDM_23,
                            "2022" = EDM_22,
                            "2021" = EDM_21)
    updateSelectizeInput(session, "waterCompanyName", choices = unique(selected_data$waterCompanyName), selected = NULL)
  })
  
  
  output$overflowmap <-renderLeaflet({
    selected_data <- switch(input$year,
                            "2023" = EDM_23,
                            "2022" = EDM_22,
                            "2021" = EDM_21)
    leaflet() %>%
      setView(lng = -0.8556470749832616, lat = 52.55857268224709, zoom = 6) %>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      addCircles(data = selected_data, 
                 radius = as.numeric(selected_data$totalDurationAllSpillsHrs),
                 fillColor = "blue",
                 fillOpacity = 0.5,
                 color = "black",
                 weight = 1,
                 stroke = T,
                 popup = paste("Company: ", selected_data$waterCompanyName, "<br>",
                               "Site Name: ", selected_data$siteNameEA,"<br>",
                               "Spill Count: ", selected_data$countedSpills,"<br>",
                               "Spill Duration (hrs): ", selected_data$totalDurationAllSpillsHrs,"<br>"),
                 highlightOptions = highlightOptions(color = "navy",
                                                     opacity = 0.8,
                                                     weight = 5,
                                                     bringToFront = FALSE),
                 group = "companies")%>%
      addMiniMap(
        tiles = providers$CartoDB.Positron,
        position = 'topright', 
        width = 200, height = 200,
        toggleDisplay = FALSE,
        aimingRectOptions = list(color = "red", weight = 1, clickable = FALSE),
        zoomLevelOffset=-3)
  })
  
  observeEvent(input$plotoverflow, {
    req(input$waterCompanyName)  
    
    t  = bind_rows(
      EDM_21 %>% filter(waterCompanyName == input$waterCompanyName) %>% 
        select(c("acronym","waterCompanyName","assetType",
                 "countedSpills","totalDurationAllSpillsHrs", 
                 "country","localAuthority")) %>% 
        mutate(Year = 2021),
      EDM_22 %>% filter(waterCompanyName == input$waterCompanyName) %>% 
        select(c("acronym","waterCompanyName","assetType",
                 "countedSpills","totalDurationAllSpillsHrs", 
                 "country","localAuthority")) %>% 
        mutate(Year = 2022),
      EDM_23 %>% filter(waterCompanyName == input$waterCompanyName) %>% 
        select(c("acronym","waterCompanyName","assetType",
                 "countedSpills","totalDurationAllSpillsHrs", 
                 "country","localAuthority")) %>% 
        mutate(Year = 2023)
    )%>% group_by(Year) %>%
      summarize(`Total Spills (Count)`=sum(countedSpills,na.rm = T),
                `Total Duration (Hours)`=sum(totalDurationAllSpillsHrs, na.rm = T)) %>% 
      tibble() %>% select(-c(geoms)) %>%
      mutate(Company = input$waterCompanyName) %>%
      pivot_longer(cols = c(`Total Spills (Count)`, `Total Duration (Hours)`), 
                   names_to = "Metric", 
                   values_to = "Value")
    
    # Plot the data
    output$overflowplot <- renderPlot({
 
      ggplot(t, aes(x = factor(Year), y = Value, fill = Metric)) +
        geom_bar(stat = "identity", position = "dodge") +
        facet_wrap(~ Company, scales = "free") +
        labs(x = "Year",
             y = "No. Spills/Duration (hrs)",
             fill = "Storm Overflow Measurement")+
        scale_fill_manual(values = c(`Total Spills (Count)` = "#3B9AB2", `Total Duration (Hours)` = "#F21A00")) +
        theme(text = element_text(family = "Krub"), 
              axis.text.x = element_text(angle = 0, vjust = 0, hjust=0.5,colour="black"),
              strip.background = element_rect(fill="black"),
              strip.text = element_text(colour = 'white',face="bold",size = 12),
              panel.background = element_blank(),
              panel.grid.major.x = element_line(size = 0.5, linetype = 'solid',
                                                colour = "lightgrey"),
              panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                                colour = "lightgrey"),
              panel.border=element_rect(colour="black",size=1,fill=NA),
              axis.text.y=element_text(colour="black"),
              legend.box.background = element_blank(),
              legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'), legend.spacing.x = unit(0, "pt"),legend.margin=margin(t=-0.5,l=0.05,b=0.05,r=0.05, unit='cm'))
    }, res = 128)
    
    
  })
  
  # button to generate API call and plot
  observeEvent(input$apinplot_buttonsewerrain, {
    req(input$waterCompanyName)  
    showModal(modalDialog(
      title = NULL,
      size = "l",  
      tags$div(
        style = "text-align: center; font-family: 'Krub', sans-serif; font-weight: 600; font-size: 30px;",  # Text styles
        tags$img(src = "ea_logo.png", alt = "EA Logo", style = "width: 400px; height: auto;"),
        br(),
        "Connecting with the Environment Agency API and aggregating data... please wait",
        br(),br(),
        div(
          class = "loader",
          style = "margin: auto; border: 5px solid #f3f3f3; border-top: 5px solid #009f41; border-radius: 80%; width: 100px; height: 100px; animation: spin 2s linear infinite;"
        ), br(),
      ),
      easyClose = FALSE,
      footer = NULL,
      style = "border: none; box-shadow: none; background-color: rgba(255, 255, 255, 0.5); padding: 0;",
      tags$head(
        tags$style(HTML("
      @keyframes spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
    "))
      )
    ))
    stationssewer <- WWRainfall_tbl %>% filter(waterCompanyName %in% input$waterCompanyName) %>% pull(wiskiID)
    
    rainfallsewer <- tibble()
    
    for(i in stationssewer){
      url.rainsewer <- paste0("https://environment.data.gov.uk/hydrology/data/readings.json?max-date=2023-12-31&mineq-date=2021-01-01&period=86400&station.wiskiID=",as.character(i))
      
      # send API request
      response.rainsewer <- GET(url.rainsewer)
      
      if (http_status(response.rainsewer)$category == "Success") {
        # parse JSON 
        data.rainsewer <- tryCatch({
          fromJSON(content(response.rainsewer, "text"), flatten = TRUE)
        }, error = function(e) {
          message("Error parsing JSON: ", e)
          return(NULL)
        })
        
        # check if it worked
        if (!is.null(data.rainsewer)) {
          # extract relevant bits we are interested in
          station_data.rainsewer <- data.rainsewer$items
          
          # check if station data is available
          if (!is.null(station_data.rainsewer) && length(station_data.rainsewer) > 0) {
            
            statNamesewer <- WWRainfall_tbl %>% filter(wiskiID == i) %>% pull(stationName)
            # make tibble
            station_df.rainsewer <- tibble(
              date = as.Date(station_data.rainsewer$date),
              rain_mm = station_data.rainsewer$value,
              station = statNamesewer,
              wiskiID = i,
            ) %>% filter(complete.cases(.))
            
          } else {
            message("No data available for the specified station and date range.")
          }
        } else {
          message("Failed to parse JSON response.")
        }
      } else {
        message("Error: API request failed with status ", http_status(response.rainsewer)$reason)
      }
      rainfallsewer <- bind_rows(rainfallsewer,station_df.rainsewer) 
      
      
    }
    
    # Update the reactiveValues
    data_values$rainfallsewer <- rainfallsewer
    print(head(data_values$rainfallsewer))
    
    # Plot the data
    output$rainplotsewer <- renderPlot({
      annual_rainfallsewer <- rainfallsewer %>% filter(complete.cases(.))  %>%
        mutate(year = year(date), month = month(date)) %>%
        group_by(year) %>%
        summarize(total_rain_mm = sum(rain_mm, na.rm = TRUE), .groups = 'drop') %>%
        ungroup() %>%
        group_by(year) %>%
        summarize(average_rain_mm = mean(total_rain_mm, na.rm = TRUE), .groups = 'drop') %>%
        filter(complete.cases(.))
      
      # save data to the reactiveValues
      data_values$annual_rainfallsewer <- annual_rainfallsewer
      print(head(data_values$annual_rainfallsewer))
      
      ggplot(annual_rainfallsewer %>% mutate(Title = paste0("Average rainfall (mm) for ",input$waterCompanyName)), aes(x = factor(year), y = average_rain_mm)) +
        geom_bar(stat = "identity", position = "dodge", fill = "#3B9AB2") +
        facet_wrap(~ Title, scales = "free") +
        labs(x = "Year",
             y = "Average Rainfall (mm)")+
        theme(text = element_text(family = "Krub"), 
              axis.text.x = element_text(angle = 0, vjust = 0, hjust=0.5,colour="black"),
              strip.background = element_rect(fill="black"),
              strip.text = element_text(colour = 'white',face="bold",size = 12),
              panel.background = element_blank(),
              panel.grid.major.x = element_line(size = 0.5, linetype = 'solid',
                                                colour = "lightgrey"),
              panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                                colour = "lightgrey"),
              panel.border=element_rect(colour="black",size=1,fill=NA),
              axis.text.y=element_text(colour="black"),
              legend.box.background = element_blank(),
              legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'), legend.spacing.x = unit(0, "pt"),legend.margin=margin(t=-0.5,l=0.05,b=0.05,r=0.05, unit='cm'))
      
    }, res = 128)
    
    # hide loader
    removeModal()
    
  })
  
  output$downloadDatasewer <- downloadHandler(
    filename = function() {
      if (input$dataFormatsewer == "raw") {
        paste0("sewerrainfall_raw",input$waterCompanyName,"_",Sys.Date(),".xlsx")
      } else {
        paste0("annual_sewerrainfall_aggregated",input$waterCompanyName,"_",Sys.Date(),".xlsx")
      }
    },
    content = function(file) {
      if (input$dataFormatsewer == "raw") {
        write_xlsx(data_values$rainfallsewer, file)  
      } else {
        write_xlsx(data_values$annual_rainfallsewer, file)  
      }
    }
  )
  
}

# run app 
shinyApp(ui = ui, server = server)
