# GLOW Phase 1 Station Comparison Widget
# Github: https://github.com/Gabe8850/GlowWidget.git
# Design: https://drive.google.com/file/d/1m63OYeS0qDsyOZtQBauhzdCa0CTK_U43/view?usp=sharing
# Created by Gabe Watson for The Commons 
# Created 01.11.2020 
# See last commit on Git 

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
  mainPanel(
            uiOutput("GroupText"),
            leafletOutput("LeafMap", height = 225, width = 650),
            tags$h3(textOutput("ChartOneTitle")),
            dygraphOutput("ChartOne", height = 200, width = 700),
            tags$h3(textOutput("ChartTwoTitle")),
            dygraphOutput("ChartTwo", height = 200, width = 700)),
  sidebarPanel(
    tags$head(
      tags$style(type="text/css", "select { max-width: 160px; }"),
      tags$style(type="text/css", ".span4 { max-width: 200px; }"),
      tags$style(type="text/css", ".well { max-width: 200px; }")
    ),
               uiOutput("GroupFilter"),
               uiOutput("StationOneFilter"),
               uiOutput("ParameterOneFilter"),
               uiOutput("StationTwoFilter"),
               uiOutput("ParameterTwoFilter"),
               selectInput("DownloadSelect","Download Options", choices = c("Chart One","Chart Two", "All Data"), selected = "Chart One", multiple = FALSE),
               downloadButton('DataDownload', 'Download')),
               

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    
#### DATA IMPORT #####
InputData <- read_csv("Data/Combined_Data7.csv")

GroupData <- read_csv("Data/GroupData1.csv")
print(GroupData)
# Variable Decleration
#Map Data
MapDataReactive <- reactiveValues(df = data.frame(InputData))

#Station One Data 
#Defaults to first station in the input dataset
StationOneReactive <- reactiveValues(S = as.character())

DefaultStationOne <- reactiveValues(S = as.character(InputData %>% 
                                                       arrange(desc(StationSampleCount))%>% 
                                                       select(station_name)%>%
                                                       unique()%>%
                                                       slice(1L)))

DefaultStationTwo <- reactiveValues(S = as.character(InputData %>% 
                                                       arrange(desc(StationSampleCount))%>% 
                                                       select(station_name)%>%
                                                       unique()%>%
                                                       slice(12)))

#Selected Marker for map
SelectedMarkerOne <- reactiveValues(df = data.frame())

#This needs a default set because it doesn't operate off a reactive like the first station
SelectedMarkerTwo <- reactiveValues(df = data.frame(filter(isolate(MapDataReactive$df), station_name %in% isolate(input$StationTwoSelect))))

#ParameterList 
ParameterList <- c("Dissolved Oxygen","Nitrate","Phosphate","pH","Turbidity","Temperature")

#Colors for Parameters 
Colors <- c("#0B4F6C","#855A5C","#8A8E91","#B8D4E3","#20BF55","#F2C57C")
ParamColors <- data.frame(ParameterList,Colors)

#Huc Layer 
Hucs <- rgdal::readOGR("Data/Huc8s_v3.geojson")

#### END DATA IMPORT #### 
    
#### FILTERS #####
#Group Select
output$GroupFilter <- renderUI({
selectizeInput("GroupSelect","Select a Group", choices = c("All Champions", InputData$Group), selected = "All Champions", multiple = TRUE)
})


   
#Station One Filter
 output$StationOneFilter <- renderUI({
     #StationSelect
     selectizeInput("StationOneSelect","Select Station One", choices = InputData$station_name, selected = DefaultStationOne$S, multiple = FALSE)
     })
 
 #Turning the input into a reactive value so we can change it based on mapmarker clicks
 observeEvent(input$StationOneSelect,{
  # print(input$StationOneSelect)
   StationOneReactive$S <- as.character(input$StationOneSelect)
  # print(StationOneReactive$S)
 })
 
 #Paremeter One Filter 
 output$ParameterOneFilter <- renderUI({
    #ParameterSelect 
    selectizeInput("ParameterOneSelect","Select Parameter One", choices = ParameterList, multiple = FALSE)
 })
 
#Station Two Filter
 output$StationTwoFilter <- renderUI({
 #Station Two
 selectizeInput("StationTwoSelect","Select Station Two", choices = InputData$station_name, selected = DefaultStationTwo$S, multiple = FALSE)
 })
 
 #Paremeter One Filter 
 output$ParameterTwoFilter <- renderUI({
   #ParameterSelect 
   selectizeInput("ParameterTwoSelect","Select Parameter Two", choices = ParameterList, multiple = FALSE)
 })
 
 #ObserveEvent altering MapDataReactive$df to display on the map
 #Also updates options for the first station select 
 observeEvent(input$GroupSelect, {
   #req(input$StationOneSelect)
   #req(input$StationTwoSelect)
   #req(input$ParameterOneSelect)
   #req(input$ParameterTwoSelect)
   #Map Data filtering 
   if("All Champions" %in% input$GroupSelect)
   {
     MapDataReactive$df <- InputData
   }
   else
   {
     MapDataReactive$df <- filter(InputData, Group %in% input$GroupSelect)
   }
   
   #Default Selection for a station is the first and second most sampled stations in the group list
   DefaultStationOne$S <- MapDataReactive$df %>% 
     arrange(desc(StationSampleCount))%>% 
     select(station_name)%>%
     unique()%>%
     slice(1L)%>%
     as.character()
   
   DefaultStationTwo$S <- MapDataReactive$df %>% 
     arrange(desc(StationSampleCount))%>% 
     select(station_name)%>%
     unique()%>%
     slice(2L)%>%
     as.character()
   
   #Station One Select 
   updateSelectizeInput(session, "StationOneSelect", choices = unique(MapDataReactive$df$station_name), selected = DefaultStationOne$S)
   updateSelectizeInput(session, "StationTwoSelect", choices = unique(MapDataReactive$df$station_name), selected = DefaultStationTwo$S)
 })
#### END FILTERS #####
# Check section, make sure to remove at some point
# print(isolate(MapDataReactive$df))
# print(isolate(input$StationOneSelect))
# print(isolate(DefaultStationOne$S))
# print(isolate(StationOneReactive$S))
    
#### GROUP TEXT #####
output$GroupText <- renderUI({

GroupName <- MapDataReactive$df %>%
            filter(station_name == StationOneReactive$S)%>%
            select(Group)%>%
            distinct()%>%
            as.character()

GroupFrame <- filter(GroupData, Group == GroupName)
           
tagList(
paste0(GroupFrame$Group),
HTML("<br/>"),
paste0("Watershed(s): ", GroupFrame$HucList),
HTML("<br/>"),
paste0("# of Samples: ", GroupFrame$TotalSamples),
HTML("<br/>"),
paste0("Years Sampling: ", GroupFrame$YearRange),
HTML("<br/>"),
paste0(GroupFrame$Description),
)

#print(input$GroupSelect)
#GroupFrame <- filter()
  

})
 #### END  GROUP TEXT  #####
 
#### MAP #####
#Map Decleration
output$LeafMap <- renderLeaflet({
     leaflet("LeafMap")%>%
     addProviderTiles("CartoDB.VoyagerLabelsUnder", group = "Streets")%>%
     addPolygons(data = Hucs, color = "#b3b3b3", weight = 3, group = "Hucs", options = list(zIndex = 1)) %>%
     addProviderTiles("Esri.WorldTopoMap", group = "Terrain")%>%
     addProviderTiles("GeoportailFrance.orthos", group = "Satellite")%>%
     addLayersControl(overlayGroups = c("Hucs"),
                     baseGroups = c("Streets", "Terrain", "Satellite"),
                     options = layersControlOptions(collapsed = FALSE,  position = 'bottomright'))%>%
     setView(lng = -81.7, lat = 42.4, zoom = 6.25)
})
 
 
 PopupMaker <- function(df)
 {
   req(input$StationOneSelect)
   req(input$StationTwoSelect)
 LastSampled <- df %>%
                arrange(desc(collection_date))%>% 
                select(collection_date)%>%
                slice(1L)
                # mutate(Date = as.Date(substring(collection_date,1,10)))%>% 
                # select(Date)%>%
                # slice(1L)%>%
                # as.character()
 #print(class(LastSampled))
 print(LastSampled)
 PopupString <- paste("Group:", df$Group, "<br>",
                      "Station:", df$station_name, "<br>",
                      "Last Sampled:",LastSampled, "<br>",
                      "# of Samples:", df$StationSampleCount, "<br>")




  return(PopupString)
 }
 
 
#Function for drawing selected markers - takes a color parameter 
SelectedMarkerUpdate <- function(df,ColorCode)
{
  suppressWarnings(leafletProxy("LeafMap")%>%
  addCircleMarkers(data = df, lng = ~longitude, lat = ~latitude, color = ColorCode, radius = ~ (MarkerSize), options = list(zIndex = 3), label = ~ paste(Group, ": ", station_name, sep = ""),
                   layerId = df$station_name, group = ~Group, popup = PopupMaker(df)))
}

#Function for drawing regular dataframe - only takes the df
NonSelectedMarkerUpdate <- function(df)
{
  suppressWarnings(leafletProxy("LeafMap")%>%
  addCircleMarkers(data = df, lng = ~longitude, lat = ~latitude, color = ~Color, radius = ~ (MarkerSize), options = list(zIndex = 3), label = ~ paste(Group, ": ", station_name, sep = ""),
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
  MapingDF <- MapDataReactive$df %>%
              distinct(station_name, .keep_all = TRUE)
  SelectedMarkerOne$df <- filter(MapingDF, station_name %in% StationOneReactive$S)
  SelectedMarkerTwo$df <- filter(MapingDF, station_name %in% input$StationTwoSelect)
  leafletProxy("LeafMap")%>%
  clearMarkers()
  NonSelectedMarkerUpdate(MapingDF)
  SelectedMarkerUpdate(SelectedMarkerOne$df,"#fc090a")
  SelectedMarkerUpdate(SelectedMarkerTwo$df,"#800080")
})




#Zooms in one level on map click
#Shifts to station when station is clicked 
observeEvent(input$LeafMap_click, {
  if(input$LeafMap_click$lng %in% as.vector(MapDataReactive$df$longitude))
  {
    leafletProxy("LeafMap")%>%
    setView(lng = input$LeafMap_click$lng, lat = input$LeafMap_click$lat, zoom = input$LeafMap_zoom) 
  }
  else
  {
  leafletProxy("LeafMap")%>%
  setView(lng = input$LeafMap_click$lng, lat = input$LeafMap_click$lat, zoom = input$LeafMap_zoom + 1)
  }
})

#### END MAP #####
    

  
#### CHART 1 #####
#Chart One Data Reactive 
ChartOneData <- reactive({
req(MapDataReactive$df)
req(StationOneReactive)
req(input$StationOneSelect)
req(input$ParameterOneSelect) 

df <- MapDataReactive$df %>%
      filter(station_name == StationOneReactive$S) %>%
      select(station_name,collection_date,input$ParameterOneSelect)

#Converting to proper format for Dygraphs 
df <- xts(df, order.by = as.Date(df$collection_date))

#Testing Print   
#print(df)
return(df)
})

#Chart 1 Title 
output$ChartOneTitle <- renderText({ 
  req(ChartOneData())
  #Changes chart title if there is insufficient data for the dygraph, 
  #or if the entire param is missing because the group doesn't collect it.
  if(nrow(ChartOneData()) < 2 || (nrow(ChartOneData()) - sum(is.na(ChartOneData()))) == 0)
  {
  paste(StationOneReactive$S, ": ", "Insufficient Data", sep = "")
  }
  else
  {
  paste(StationOneReactive$S, ": ", input$ParameterOneSelect, sep = "")
  }
})

output$ChartOne <- renderDygraph({
req(ChartOneData())
df <- ChartOneData()

ColorOneChoice <- ParamColors %>%
               filter(ParameterList == input$ParameterOneSelect)%>%
               select(Colors)%>%
               as.matrix()%>%
               c()

#Chart One Dygraph
dygraph(df, group = "test") %>%
    dyRangeSelector(height = 20, fillColor = "#757575", strokeColor = "#757575")%>%
    dyOptions(drawGrid = FALSE, drawPoints = TRUE, pointSize = 3, connectSeparatedPoints = FALSE, rightGap = 10, colors = ColorOneChoice) %>% 
    dyAxis("x", axisLineColor ="black", axisLineWidth = 2) %>%
    dyAxis("y", axisLineColor ="black", axisLineWidth = 2, rangePad = 5, label = input$ParmaterOneSelect) %>%
    dyLegend(show = "follow", width = 150) %>%
    dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)

})
#### END CHART 1 #####
    
    
    
#### CHART 2 #####
#Chart Two Data Reactive 
ChartTwoData <- reactive({
  req(MapDataReactive$df)
  req(input$StationTwoSelect)
  req(input$ParameterTwoSelect) 
  
  df <- MapDataReactive$df %>%
    filter(station_name == input$StationTwoSelect) %>%
    select(station_name, collection_date,input$ParameterTwoSelect)
  
  #Converting to proper format for Dygraphs 
  df <- xts(df, order.by = as.Date(df$collection_date))
  
  #Testing Print   
  #print(df)
  return(df)
})

#Chart Two Title
output$ChartTwoTitle <- renderText({ 
  req(ChartTwoData())
  if(nrow(ChartTwoData()) < 2 || (nrow(ChartTwoData()) - sum(is.na(ChartTwoData()))) == 0)
  {
    paste(input$StationTwoSelect, ": ", "Insufficient Data", sep = "")
  }
  else
  {
    paste(input$StationTwoSelect, ": ", input$ParameterTwoSelect, sep = "")
  }
})

#Chart Two Chart Render
output$ChartTwo <- renderDygraph({
  req(ChartTwoData())
  df <- ChartTwoData()
  
  ColorTwoChoice <- ParamColors %>%
    filter(ParameterList == input$ParameterTwoSelect)%>%
    select(Colors)%>%
    as.matrix()%>%
    c()

  #Chart Two Dygraph
  dygraph(df, group = "test") %>%
    dyRangeSelector(height = 20, fillColor = "#757575", strokeColor = "#757575")%>%
    dyOptions(drawGrid = FALSE, drawPoints = TRUE, pointSize = 3, connectSeparatedPoints = FALSE, rightGap = 10, colors = ColorTwoChoice) %>% 
    dyAxis("x", axisLineColor ="black", axisLineWidth = 2) %>%
    dyAxis("y", axisLineColor ="black", axisLineWidth = 2, rangePad = 5, label = input$ParmaterOneSelect) %>%
    dyLegend(show = "follow", width = 150) %>%
    dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE) 
})
#### END CHART 2 #####


##### DOWNLOADS #### 
## Download Parser ## 
DownloadSelectionReactive <- reactive({

  
if(input$DownloadSelect == "Chart One")
{
Data <- ChartOneData()
}
  
if(input$DownloadSelect == "Chart Two")
{
Data <- ChartTwoData()
}
if(input$DownloadSelect == "All Data")
{
Data <- InputData %>% 
        select(-c(Color,YearRange,MarkerSize))
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
