# GLOW Phase 1 Station Comparison Widget
# Github: https://github.com/Chesapeakez/GlowWidget.git
# Design: https://drive.google.com/file/d/1m63OYeS0qDsyOZtQBauhzdCa0CTK_U43/view?usp=sharing
# Created by Gabe Watson for The Commons 
# Created 01.11.2020 
# See last commit on Git 
# TO FIX
 # - #Selected Marker 'should' be able to cover all bases covered by Default Station, and StationOneReactive
 # - #Turn Chart 1 and 2 into a fuction. 
 # - #Functionanize a lot of the common activities like get group, get parameter, get color, get Choices etc.  

library(shiny)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(xts)
library(dygraphs)
library(lubridate)
library(reactlog)
library(sp)
library(rgdal)
options(shiny.reactlog=TRUE) 

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = "styler.css",
  tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
  ),
                
  shinyjs::useShinyjs(),
  
  div(id = "header",
      div(id = "header-title-image"),
      div(id = "header-info-button",
        actionButton("Info","Info")
      )
  ),              
                
  div(class = "mainPanel_A", style='border-bottom:1px solid #cccccc;',
     
  #  tags$head(
     # tags$style(type="text/css", "select { max-width: 160px; }"),
    #  tags$style(type="text/css", ".span4 { max-width: 200px; }"),
    #  tags$style(type="text/css", ".well { max-width: 200px; }")
  #  ),
    div(class = "A_subPanel", style="width:39.5%;",
      uiOutput("GroupFilter"),
      selectInput("DownloadSelect","Download Options", choices = c("Chart One","Chart One Summary", "Chart Two", "Chart Two Summary","All Data", "All Data Summary"), selected = "Chart One", multiple = FALSE),
      downloadButton('DataDownload', 'Download'),
      uiOutput("SampleCountFilter"),
    ),
    div(class = "A_subPanel",style="width:59.5%;",
        uiOutput("GroupText")
    )
   
  ),
  
  
  div(class = "mainPanel_A leafPanel",
            
            #tags$h3(uiOutput("GroupValidate")),
         #   uiOutput("StationOneValidate"),
            leafletOutput("LeafMap", height = 'calc(100vh - 60vh)', width = '100%')
  ),
           
  
  div(class = "mainPanel_A", style='border-top:1px solid #cccccc;',
    div(class = "A_subPanel", style="width: 50%;",
      uiOutput("StationOneFilter"),
      uiOutput("ParameterOneFilter"),
      
      tags$h3(uiOutput("StationOneValidate")),
      tags$h3(uiOutput("ParameterOneValidate")),
      tags$h3(textOutput("ChartOneTitle")),
      tabsetPanel(
        tabPanel("Chart", dygraphOutput("ChartOne", height = 200, width = '100%')),
        tabPanel("Summary Statistics", tableOutput("ChartOneTable"))
      )
    ),
    div(class = "A_subPanel", style="width: 50%;",
      uiOutput("StationTwoFilter"),
      uiOutput("ParameterTwoFilter"),
      
      
      tags$h3(uiOutput("StationTwoValidate")),
      tags$h3(uiOutput("ParameterTwoValidate")),
      tags$h3(textOutput("ChartTwoTitle")),
      tabsetPanel(
        tabPanel("Chart",  dygraphOutput("ChartTwo", height = 200, width = '100%')),
        tabPanel("Summary Statistics", tableOutput("ChartTwoTable"))
      )
    )
  )
 
               

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    
#### DATA IMPORT AND VARIABLE DECLERATION #####

suppressMessages(InputData <- read_csv("Data/Combined_Data11.csv"))

suppressMessages(GroupData <- read_csv("Data/GroupData1.csv"))

#Huc Layer 
suppressMessages(Hucs <- rgdal::readOGR("Data/Huc8s_v3.geojson"))

#Variable Decleration
#Map Data
MapDataReactive <- reactiveValues(df = data.frame(InputData))

#Station One Data 
#Defaults to first station in the input dataset
StationOneReactive <- reactiveValues(S = as.character())

StationTwoReactive <- reactiveValues(S = as.character())
GroupReactive <- reactiveValues(G = as.character())

#Default One station data reactive 
#Used to set the default station after group choice - might turn this into a function !
DefaultStationOne <- reactiveValues(S = as.character(InputData %>% 
                                                       arrange(desc(StationSampleCount))%>% 
                                                       select(station_name)%>%
                                                       unique()%>%
                                                       slice(1L)))
#Default station two data reactive
DefaultStationTwo <- reactiveValues(S = as.character(InputData %>% 
                                                       arrange(desc(StationSampleCount))%>% 
                                                       select(station_name)%>%
                                                       unique()%>%
                                                       slice(12)))
ChartOneRender <- reactiveValues(N = as.numeric())

#Selected Marker for map
#SelectedMarkerOne <- reactiveValues(df = data.frame())

#This needs a default set because it doesn't operate off a reactive like the first station

#SelectedMarkerTwo <- reactiveValues(S = isolate(DefaultStationTwo$S)) 

# Extra Info for Charting
#ParameterList 
ParameterList <- c("Dissolved Oxygen","Nitrate","Phosphate","pH","Turbidity","Temperature")

#UnitList
Units<- c("mg/L","mg/L","mg/L", "pH","NTUs","°C")
ParamUnits <- data.frame(ParameterList,Units)

#Colors for Parameters 
Colors <- c("#0B4F6C","#855A5C","#8A8E91","#B8D4E3","#20BF55","#F2C57C")
ParamColors <- data.frame(ParameterList,Colors)
#### END DATA IMPORT #### 



#### GLOBAL FUNCTIONS 
#Frequent opperations required during program 

#Get Group 
GetGroup <- function(df,Station)
{

GroupName <- df %>%
  filter(station_name == Station)%>%
  select(Group)%>%
  slice(1L)%>%
  as.matrix()%>%
  c()

  return(GroupName)
}

#Get Unit 
GetUnit <- function(Parameter,GroupName)
{
  #print(Parameter)
  #print(GroupName)
  Unit <- ParamUnits %>%
    filter(ParameterList == Parameter)%>%
    select(Units)%>%
    mutate(Units = ifelse(Parameter == "Turbidity" & GroupName == "Euclid" | GroupName == "Rocky","cm",c(as.matrix(Units))))%>%
    as.matrix()%>%
    c()
}


##### MODEL #### 
observeEvent(input$Info,{
showModal(modalDialog(
  title = "Cleveland Water Alliance's GLOW Widget!",
  HTML("This application does things with things! <br> Lorem ipsum dolor sit amet, consectetur adipiscing elit,
       sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, 
       quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
       Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
       Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
   easyClose = TRUE,
   footer = NULL,
))
  
  
  
})
#### END MODEL #### 


#### FILTERS #####
#Group Select
output$GroupFilter <- renderUI({
selectizeInput("GroupSelect","Select a Group", choices = c("All Champions", InputData$Group), selected = "All Champions", multiple = TRUE)
})

#Station One Filter
 output$StationOneFilter <- renderUI({
     #StationSelect
     selectizeInput("StationOneSelect","Select Station One", choices = InputData$station_name, selected = DefaultStationOne$S, multiple = FALSE, options = list(placeholder = 'Select a Station'))
     })
 
 #Paremeter One Filter 
 output$ParameterOneFilter <- renderUI({
    #ParameterSelect 
    selectizeInput("ParameterOneSelect","Select Parameter One", choices = ParameterList, multiple = FALSE)
 })
 
#Station Two Filter
 output$StationTwoFilter <- renderUI({
 #Station Two
 selectizeInput("StationTwoSelect","Select Station Two", choices = InputData$station_name, selected = DefaultStationTwo$S, multiple = FALSE,  options = list(placeholder = 'Select a Station'))
 })
 
 #Parameter One Filter 
 output$ParameterTwoFilter <- renderUI({
   #ParameterSelect 
   selectizeInput("ParameterTwoSelect","Select Parameter Two", choices = ParameterList, multiple = FALSE)
 })
 
 output$SampleCountFilter <- renderUI({
  #3print(MapDataReactive$df)
   
  Min <- InputData %>%
          select(StationSampleCount)%>%
          as.tibble()%>%
          arrange(StationSampleCount)%>%
          slice_head()%>%
          pull()
  
  Max <- InputData %>%
         select(StationSampleCount)%>%
         as.tibble()%>%
         arrange(StationSampleCount)%>%
         slice_tail()%>%
         pull()
  
   sliderInput("SampleCountSelect","Filter by Sample Count", min = Min, max = Max, value = c(Min,Max), dragRange = TRUE)
   
 })
 
 # observeEvent(input$SampleCountSelect,{
 #  req(input$GroupSelect)
 #  print(input$SampleCountSelect[1])
 #  print(input$SampleCountSelect[2])
 #  
 #  
 #  MapDataReactive$df <- MapDataReactive$df %>%
 #                        filter(StationSampleCount >= input$SampleCountSelect[1],
 #                               StationSampleCount <= input$SampleCountSelect[2])%>%
 #                               filter(Group %in% input$GroupSelect)
 # 
 #  #Default Selection for a station is the first and second most sampled stations in the group list
 #  Choices <- MapDataReactive$df %>%
 #    arrange(desc(StationSampleCount))%>%
 #    select(station_name)%>%
 #    unique()
 # 
 #  DefaultStationOne$S <- Choices %>%
 #    slice(1L)%>%
 #    as.character()
 # 
 #  DefaultStationTwo$S <- Choices %>%
 #    slice(2L)%>%
 #    as.character()
 # 
 #  StationOneReactive$S <- DefaultStationOne$S
 #  StationTwoReactive$S <- DefaultStationTwo$S
 # 
 #  #Station One Select
 #  updateSelectizeInput(session, "StationOneSelect", choices = Choices$station_name, selected = DefaultStationOne$S)
 #  updateSelectizeInput(session, "StationTwoSelect", choices = Choices$station_name, selected = DefaultStationTwo$S)
 # })
 # 
 #ObserveEvent altering MapDataReactive$df to display on the map
 #Also updates options for the first station select
 observeEvent(input$GroupSelect, {
   #Map Data filtering
   if("All Champions" %in% input$GroupSelect)
   {
     MapDataReactive$df <- InputData
   }
   else
   {
    # print(input$GroupSelect)
     MapDataReactive$df <- filter(InputData, Group %in% input$GroupSelect)
   }

   #Default Selection for a station is the first and second most sampled stations in the group list
   Choices <- MapDataReactive$df %>%
              arrange(desc(StationSampleCount))%>%
              select(station_name)%>%
              unique()

   DefaultStationOne$S <- Choices %>%
                         slice(1L)%>%
                         as.character()

   DefaultStationTwo$S <- Choices %>%
                         slice(2L)%>%
                         as.character()

   StationOneReactive$S <- DefaultStationOne$S
   StationTwoReactive$S <- DefaultStationTwo$S

   #Station One Select
   updateSelectizeInput(session, "StationOneSelect", choices = Choices$station_name, selected = DefaultStationOne$S)
   updateSelectizeInput(session, "StationTwoSelect", choices = Choices$station_name, selected = DefaultStationTwo$S)
 })
 
 #Turning the input into a reactive value so we can change it based on mapmarker clicks
 observeEvent(input$StationOneSelect,{
   req(MapDataReactive$df)
   StationOneReactive$S <- as.character(input$StationOneSelect)
   
  ParameterOptions <- MapDataReactive$df %>%
            filter(station_name == input$StationOneSelect)%>% 
            select(collection_date:Temperature)%>%
            select(-c(collection_date))%>% 
            replace(is.na(.), 0)%>%
            summarise_all(funs(sum))
  
  ParameterOptions <- as.vector(rownames(data.frame(which(colSums(ParameterOptions) != 0))))
  
   updateSelectizeInput(session, "ParameterOneSelect", choices = ParameterOptions)
   
 })
 
 #Turning Station Two input to a reactive because it helps with rerendering errors 
 observeEvent(input$StationTwoSelect,{
   StationTwoReactive$S <- as.character(input$StationTwoSelect)
   
   ParameterOptions <- MapDataReactive$df %>%
     filter(station_name == input$StationTwoSelect)%>% 
     select(collection_date:Temperature)%>%
     select(-c(collection_date))%>% 
     replace(is.na(.), 0)%>%
     summarise_all(funs(sum))
   
   ParameterOptions <- as.vector(rownames(data.frame(which(colSums(ParameterOptions) != 0))))
   
   updateSelectizeInput(session, "ParameterTwoSelect", choices = ParameterOptions)
 })
#### END FILTERS #####
 
######## VALIDATE SECTION ##########
output$GroupValidate <- renderText({
  #req(input$GroupSelect)
  validate(
    need(input$GroupSelect != "","Select a Group!")
  )
})
 
output$StationOneValidate <- renderText({
  validate(
    need(input$StationOneSelect  != "", "Select a Station!")
   )
}) 
 
output$StationTwoValidate <- renderText({
  #req(input$StationTwoSelect)
  validate(
    need(input$StationTwoSelect != "","Select a Station!")
  )
}) 
 
output$ParameterOneValidate <- renderText({
 # req(input$ParameterOneSelect)
  validate(
    need(input$ParameterOneSelect != "","Select a Parameter!")
  )
})
 
output$ParameterTwoValidate <- renderText({
 # req(input$ParameterTwoSelect)
  validate(
    need(input$ParameterTwoSelect != "","Select a Parameter!")
  )
}) 
 
######## END VALIDATE SECTION ########## 
    
#### GROUP TEXT #####
output$GroupText <- renderUI({
    req(MapDataReactive$df)
   # req(StationOneReactive$S)
  #  req(input$GroupSelect)
  
    GroupName <- GetGroup(MapDataReactive$df,DefaultStationOne$S)
    
    GroupFrame <- filter(GroupData, Group == GroupName)
               
    tagList(
    paste0(GroupFrame$Group),
    HTML("<div class='B_subPanel' style='width:30%;'>"),
      paste0("Watershed(s): ", GroupFrame$HucList),
      HTML("<br/>"),
      paste0("# of Samples: ", GroupFrame$TotalSamples),
      HTML("<br/>"),
      paste0("Years Sampling: ", GroupFrame$YearRange),
      HTML("<br/>"),
      paste0("Website: ", GroupFrame$SiteLink),
    HTML("</div>
         <div class='B_subPanel' style='width:70%;'>"),
      paste0(GroupFrame$Description),
    HTML("</div>")
  )
})
 #### END  GROUP TEXT  #####
 
#### MAP #####
#Map Decleration
output$LeafMap <- renderLeaflet({
     leaflet("LeafMap")%>%
     addProviderTiles("CartoDB.VoyagerLabelsUnder", group = "Streets")%>%
     addPolygons(data = Hucs, color = "#b3b3b3", weight = 3, group = "Watersheds", options = list(zIndex = 1), label = paste(Hucs$NAME, "Watershed", sep = " ")) %>%
     addProviderTiles("Esri.WorldTopoMap", group = "Terrain")%>%
     addProviderTiles("GeoportailFrance.orthos", group = "Satellite")%>%
     addSearchOSM(options = searchOptions(autoCollapse = TRUE, minLength = 2))%>%
     addLayersControl(overlayGroups = c("Watersheds"),
                     baseGroups = c("Streets", "Terrain", "Satellite"),
                     options = layersControlOptions(collapsed = FALSE,  position = 'bottomright'))#%>%
    # setView(lng = -81.7, lat = 42.4, zoom = 6.25)
})
 #Constructs the popup for the leaflet map
 PopupMaker <- function(df)
 {
   req(StationOneReactive$S)
   req(input$StationTwoSelect)
  
 #Gets the last sample date of the station ! needs work
 LastSampled <- df %>%
                  select(collection_date)%>%
                  slice(1L)%>%
                  mutate(collection_date = substring(collection_date,1,10))
 
 #Popup String
 PopupString <- paste("Group:", df$Group, "<br>",
                      "Station:", df$station_name, "<br>",
                      "Last Sampled:",LastSampled, "<br>",
                      "# of Samples:", df$StationSampleCount, "<br>",
                      "Watershed:", df$NAME)

  return(PopupString)
 }
 
 
#Function for drawing selected markers - takes a color parameter 
SelectedMarkerUpdate <- function(df,ColorCode)
{
  suppressWarnings(leafletProxy("LeafMap")%>%
  addCircleMarkers(data = df, lng = ~longitude, lat = ~latitude, stroke = TRUE, color = ColorCode, fillColor = ~Color, fillOpacity = .75, radius = ~ (MarkerSize), options = list(zIndex = 3), label = ~ paste(Group, ": ", station_name, sep = ""),
                   layerId = df$station_name, group = ~Group, popup = PopupMaker(df)))
}

#Function for drawing regular dataframe - only takes the df
NonSelectedMarkerUpdate <- function(df)
{
  suppressWarnings(leafletProxy("LeafMap")%>%
  addCircleMarkers(data = df, lng = ~longitude, lat = ~latitude, stroke = FALSE, fillColor = ~Color, fillOpacity = .75, radius = ~ (MarkerSize), options = list(zIndex = 3), label = ~ paste(Group, ": ", station_name, sep = ""),
                   layerId = df$station_name, group = ~Group))
}

#MapMarker Click Station One Selection
  #Sets the StationOneReactive to equal the  selected station
  #Updates the StationeOneSelect Filter as well
observeEvent(input$LeafMap_marker_click, 
             {
StationOneReactive$S <- MapDataReactive$df %>% 
         filter(latitude == input$LeafMap_marker_click$lat, longitude == input$LeafMap_marker_click$lng) %>%
         select(station_name)%>%
         unique()%>%
         as.character()

updateSelectizeInput(session, "StationOneSelect", label = NULL, choices = unique(MapDataReactive$df$station_name), selected = StationOneReactive$S)
})

#Oberves when any data is changed 
observe({
  #Forgot that the map only needs a universe of stations - not samples. So filtering down to only distinct station_names 
  #Also sorting descending by collection date because this eventually gets fed into the popup and needs the 'last sampled'
  MapingDF <- MapDataReactive$df[ order(MapDataReactive$df$collection_date, decreasing = TRUE),]
  
  MapingDF <- MapingDF %>%
    distinct(station_name, .keep_all = TRUE)
  
  SelectedMarkerOne <- filter(MapingDF, station_name %in% StationOneReactive$S)
  SelectedMarkerTwo <- filter(MapingDF, station_name %in% StationTwoReactive$S)
  leafletProxy("LeafMap")%>%
  clearMarkers()
  NonSelectedMarkerUpdate(MapingDF)
  SelectedMarkerUpdate(SelectedMarkerOne,"#fc090a")
  SelectedMarkerUpdate(SelectedMarkerTwo,"#800080")
})

observeEvent(StationOneReactive$S,{
    req(StationOneReactive$S)
    
    SelectedMarkerOne <- filter(MapDataReactive$df %>% distinct(station_name, .keep_all = TRUE), station_name %in% StationOneReactive$S)
    leafletProxy("LeafMap")%>%
    setView(lng = SelectedMarkerOne$longitude[1], lat = SelectedMarkerOne$latitude[1], zoom = 8.5)

  })


#Zooms in one level on map click
#Shifts to station when station is clicked 
# observeEvent(input$LeafMap_click, {
#   if(input$LeafMap_click$lng %in% as.vector(MapDataReactive$df$longitude))
#   {
#     leafletProxy("LeafMap")%>%
#     setView(lng = input$LeafMap_click$lng, lat = input$LeafMap_click$lat, zoom = input$LeafMap_zoom) 
#   }
#   else
#   {
#   leafletProxy("LeafMap")%>%
#   setView(lng = input$LeafMap_click$lng, lat = input$LeafMap_click$lat, zoom = input$LeafMap_zoom + 1)
#   }
# })

#### END MAP #####
    


## CHART SECTION FUNCTIONS

#Function for making table
ChartDataMaker <- function(df,Station,Parameter)
{
dfOut <- df %>%
  filter(station_name == Station) %>%
  select(station_name,collection_date,Parameter)

#Checks to see if there is insufficent data to plot, turns the dataset into one row, and sets Parameter value to one. This bypasses any dygraph funkiness with NAs and....
#Also takes advantage of dygraphs natural (but unwanted) nature to not chart single data points
if(nrow(dfOut) - sum(is.na(dfOut)) == 0)
{
  dfOut <- dfOut %>%
           slice(1)
  suppressWarnings(dfOut[3] <- 1)
}
#print(dfOut)

dfOut <- xts(dfOut, order.by = as.Date(dfOut$collection_date))
return(dfOut)
}

#Function for making title
ChartTitleMaker <- function(df,Parameter)
{
  #Checks to see if there is unsufficent data
  if(nrow(df) < 2)
  {
    paste(df$station_name[1], ": ", "Insufficient Data", sep = "")
  }
  else
  {
    paste(df$station_name[1], ": ",Parameter, sep = "")
  }
}

#Function for making Chart
ChartMaker <- function(df,Station,Parameter)
{
  GroupName <- GetGroup(MapDataReactive$df,Station)
  
  ColorChoice<- ParamColors %>%
                 filter(ParameterList == Parameter)%>%
                 select(Colors)%>%
                 as.matrix()%>%
                 c()
  
  Unit <- GetUnit(Parameter,GroupName)
 # print(Unit)

  return(dygraph(df, group = "x") %>%
      dyRangeSelector(height = 20, fillColor = "#757575", strokeColor = "#757575")%>%
      dyOptions(drawGrid = FALSE, drawPoints = TRUE, pointSize = 3, connectSeparatedPoints = TRUE, rightGap = 10, colors = ColorChoice) %>%
      dyAxis("x", axisLineColor ="black", axisLineWidth = 2) %>%
      dyAxis("y", axisLineColor ="black", axisLineWidth = 2, rangePad = 5, label = Unit) %>%
      dyLegend(show = "follow", width = 150) %>%
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE))
}

# function for generating data table
TableMaker <- function(df,station)
{
  
  df <- MapDataReactive$df %>%
    filter(station_name == station)%>%
    select(c(`Dissolved Oxygen`, Nitrate, Phosphate, pH, Turbidity, Temperature))
  
  Mean <- sapply(df, mean, na.rm=TRUE)
  Median <- sapply(df, median, na.rm=TRUE)
  suppressWarnings(Minimum <- sapply(df, min, na.rm=TRUE))
  suppressWarnings(Max <- sapply(df, max, na.rm=TRUE))
  `Standard Deviation` <- sapply(df, sd, na.rm = TRUE)

  #Binding and rounding to 3 digits
  df <- round(cbind(Mean,Median,Minimum,Max,`Standard Deviation`),3)
  
  # Turning infintes to NA
  is.na(df) <- sapply(df, is.infinite)
  # Turning NAs to -
  df[is.na(df)] = "-"
  
  return(df)
  
}
#### CHART 1 Calls #####
#Chart One Data
ChartOneData <- reactive({
 req(StationOneReactive$S)
 req(input$ParameterOneSelect) 
  
  df <- ChartDataMaker(MapDataReactive$df,StationOneReactive$S,input$ParameterOneSelect)

  return(df)
})

#Chart 1 Title 
output$ChartOneTitle <- renderText({ 
  req(ChartOneData())
  req(input$ParameterOneSelect)
  req(StationOneReactive$S)

  ChartTitleMaker(ChartOneData(),input$ParameterOneSelect)
})


#Chart One Dygraph
  output$ChartOne <- renderDygraph({
    req(StationOneReactive$S)
    req(input$ParameterOneSelect)
    ChartMaker(ChartOneData(),StationOneReactive$S,input$ParameterOneSelect)
})

  #Chart One Table
  output$ChartOneTable <- renderTable({
    req(StationOneReactive)
    req(MapDataReactive)
    
    head(TableMaker(MapDataReactive$df,StationOneReactive$S))},
    rownames = TRUE,
    width = "100%",
    striped = TRUE,
    align = 'l')
  #### END CHART 1 #####
  
  
  #### CHART TWO CALLS #### 
  #Chart Two Data
  ChartTwoData <- reactive({
    req(StationTwoReactive$S)
    req(input$ParameterTwoSelect) 
 #   req(input$GroupSelect)
    df <- ChartDataMaker(MapDataReactive$df,StationTwoReactive$S,input$ParameterTwoSelect)
    return(df)
  })
  
  #Chart 1 Title 
  output$ChartTwoTitle <- renderText({ 
    req(ChartTwoData())
    req(input$ParameterTwoSelect)
    ChartTitleMaker(ChartTwoData(),input$ParameterTwoSelect)
  })
  
  #Chart Two Dygraph
  output$ChartTwo <- renderDygraph({
    req(StationTwoReactive$S)
    req(input$ParameterTwoSelect)
    ChartMaker(ChartTwoData(),StationTwoReactive$S,input$ParameterTwoSelect)
  })
  
  #Chart Two Table
  output$ChartTwoTable <- renderTable({
    req(StationTwoReactive$S)
    req(MapDataReactive)
    head(TableMaker(MapDataReactive$df,StationTwoReactive$S))},
    rownames = TRUE,
    width = "100%",
    striped = TRUE,
    align = 'l')

#### END CHART 2 #####
  

#### END CHART SECTION


##### DOWNLOADS #### 
## Download Parser ## 

DownloadSelectionReactive <- reactive({

if(input$DownloadSelect == "Chart One")
{
  GroupName <- GetGroup(MapDataReactive$df,StationOneReactive$S)
  
  Unit <- GetUnit(input$ParameterOneSelect,GroupName)
  
  
  Data <- InputData %>%
    filter(station_name == StationOneReactive$S)%>%
    select(station_id,station_name,latitude,longitude,collection_date,input$ParameterOneSelect)%>%
    mutate(Units = Unit)
}
  
if(input$DownloadSelect == "Chart One Summary")
{
 Data <- TableMaker(MapDataReactive$df,StationOneReactive$S)
         Data <- data.frame(ParameterList,Data)
}
  
if(input$DownloadSelect == "Chart Two")
{

  GroupName <- GetGroup(MapDataReactive$df,StationTwoReactive$S)
  
  Unit <- GetUnit(input$ParameterTwoSelect,GroupName)
  
  Data <- InputData %>%
    filter(station_name == StationTwoReactive$S)%>%
    select(station_id,station_name,latitude,longitude,collection_date,input$ParameterTwoSelect)%>%
    mutate(Units = Unit)

}
  
  if(input$DownloadSelect == "Chart Two Summary")
  {
    Data <- TableMaker(MapDataReactive$df,StationTwoReactive$S)
    Data <- data.frame(ParameterList,Data)
  }
  
if(input$DownloadSelect == "All Data")
{
Data <- InputData %>% 
        select(-c(Color,YearRange,MarkerSize))
}
  
  if(input$DownloadSelect == "All Data Summary")
  {
    df <- MapDataReactive$df %>%
      select(c(`Dissolved Oxygen`, Nitrate, Phosphate, pH, Turbidity, Temperature,Group))%>%
      mutate(Turbidity = ifelse(Group == "Euclid" | Group == "Rocky",NA,Turbidity))%>%
      select(-c(Group))

    Mean <- sapply(df, mean, na.rm=TRUE)
    Median <- sapply(df, median, na.rm=TRUE)
    Minimum <- sapply(df, min, na.rm=TRUE)
    Max <- sapply(df, max, na.rm=TRUE)
    `Standard Deviation` <- sapply(df, sd, na.rm = TRUE)
    
    #Binding and rounding to 3 digits
    df <- round(cbind(Mean,Median,Minimum,Max,`Standard Deviation`),3)
    
    # Turning infintes to NA
    is.na(df) <- sapply(df, is.infinite)
    # Turning NAs to -
    df[is.na(df)] = "-"
    
    Data <- data.frame(ParameterList,df)
  }
  
return(Data)
})


## Download Handler ## 
#download handler for project export 
output$DataDownload <- downloadHandler(
  filename = function() { 
    paste(str_remove_all(input$DownloadSelect," "), ".csv", sep="")
  },
  content = function(file) {
    write.csv(DownloadSelectionReactive(), file, row.names=FALSE)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
