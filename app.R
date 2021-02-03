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

# Define UI for application that draws a histogram
ui <- fluidPage(
  mainPanel(
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
InputData <- read_csv("Data/Combined_Data6.csv")
#### END DATA IMPORT #### 


# Variable Decleration
#Map Data
MapDataReactive <- reactiveValues(df = data.frame())
MapDataReactive$df <- as.data.frame(InputData)
        

#Station One Data 
#Defaults to first station in the input dataset
StationOneReactive <- reactiveValues(S = as.character())

#ParameterList 
ParameterList <- c("Dissolved Oxygen","Nitrate","Phosphate","pH","Turbidity","Temperature")

#Colors for Parameters 
Colors <- c("#0B4F6C","#855A5C","#8A8E91","#B8D4E3","#20BF55","#F2C57C")
ParamColors <- data.frame(ParameterList,Colors)
    
#### FILTERS #####
#Group Select
output$GroupFilter <- renderUI({
selectizeInput("GroupSelect","Select a Group", choices = c("All Champions", InputData$Group), selected = "All Champions", multiple = TRUE)
})

#ObserveEvent altering MapDataReactive$df to display on the map
#Also updates options for the first station select 
   observeEvent(input$GroupSelect, {
    #Map Data filtering 
    x <- data.frame(input$GroupSelect)
    
    if("All Champions" %in% input$GroupSelect)
    {
    MapDataReactive$df <- InputData
    }
    else
    {
    MapDataReactive$df <- filter(InputData, Group %in% input$GroupSelect)
    }
    
    #Defaullt Selection for a station is the first and second most sampled stations in the group list
    StationOneDefault <- MapDataReactive$df %>% 
    arrange(desc(StationSampleCount))%>% 
    select(station_name)%>%
    unique()%>%
    slice(1L)%>%
    as.character()
    
    StationTwoDefault <- MapDataReactive$df %>% 
    arrange(desc(StationSampleCount))%>% 
    select(station_name)%>%
    unique()%>%
    slice(2L)%>%
    as.character()

    #Station One Select 
    updateSelectizeInput(session, "StationOneSelect", choices = unique(MapDataReactive$df$station_name), selected = StationOneDefault)
    updateSelectizeInput(session, "StationTwoSelect", choices = unique(MapDataReactive$df$station_name), selected = StationTwoDefault)
})
   
#Station One Filter
 output$StationOneFilter <- renderUI({
     #StationSelect
     selectizeInput("StationOneSelect","Select Station One", choices = "", multiple = FALSE)
     })
 
 #Turning the input into a reactive value so we can change it based on mapmarker clicks

 observeEvent(input$StationOneSelect,{
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
 selectizeInput("StationTwoSelect","Select Station Two", choices = "", multiple = FALSE)
 })
 
 #Paremeter One Filter 
 output$ParameterTwoFilter <- renderUI({
   #ParameterSelect 
   selectizeInput("ParameterTwoSelect","Select Parameter Two", choices = ParameterList, multiple = FALSE)
 })
 
#### END FILTERS #####
    
    
    
#### MAP #####

#Map Decleration
output$LeafMap <- renderLeaflet({
     leaflet("LeafMap")%>%
     addProviderTiles("CartoDB.VoyagerLabelsUnder", group = "Streets")%>%
     setView(lng = -81.7, lat = 42.4, zoom = 6.25)
})
 
 
#Function for drawing selected markers - takes a color parameter 
SelectedMarkerUpdate <- function(df,ColorCode)
{
  leafletProxy("LeafMap")%>%
  addCircleMarkers(data = df, lng = ~longitude, lat = ~latitude, color = ColorCode, radius = ~ (MarkerSize), label = ~ paste(Group, ": ", station_name, sep = ""),
                   layerId = df$station_name, group = ~Group)
}

#Function for drawing regular dataframe - only takes the df
NonSelectedMarkerUpdate <- function(df)
{
  leafletProxy("LeafMap")%>%
  addCircleMarkers(data = df, lng = ~longitude, lat = ~latitude, color = ~Color, radius = ~ (MarkerSize), label = ~ paste(Group, ": ", station_name, sep = ""),
                   layerId = df$station_name, group = ~Group) 
}

#Original Map Creation
observeEvent(MapDataReactive$df,
    {
    leafletProxy("LeafMap")%>%
    clearMarkers()
    NonSelectedMarkerUpdate(MapDataReactive$df)
    })

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

#Highlights the Selected Station One 
SelectedMarkerOne <- reactiveValues(df = data.frame())
SelectedMarkerTwo <- reactiveValues(df = data.frame(filter(isolate(MapDataReactive$df), station_name %in% isolate(input$StationTwoSelect))))


observeEvent(StationOneReactive$S,
             {
              leafletProxy("LeafMap")%>%
              removeMarker(SelectedMarkerOne$df$station_name )

              SelectedMarkerOne$df <- filter(MapDataReactive$df, station_name %in% StationOneReactive$S)
              
              NonSelectedMarkerUpdate(MapDataReactive$df)
              SelectedMarkerUpdate(SelectedMarkerOne$df,"#fc090a")
              SelectedMarkerUpdate(SelectedMarkerTwo$df,"#800080")
              
              #leafletProxy("LeafMap")%>%
              # addCircleMarkers(data = MapDataReactive$df, lng = ~longitude, lat = ~latitude, color = ~Color, radius = ~ (MarkerSize), label = ~ paste(Group, ": ", station_name, sep = ""),
              #                    layerId = MapDataReactive$df$station_name, group = ~Group)%>%
              # addCircleMarkers(data = SelectedMarkerOne$df, lng = ~longitude, lat = ~latitude, color = "#fc090a", radius = ~ (MarkerSize), label = ~ paste(Group, ": ", station_name, sep = ""),
              #                  layerId = SelectedMarkerOne$df$station_name, group = ~Group)%>%
              # addCircleMarkers(data = SelectedMarkerTwo$df, lng = ~longitude, lat = ~latitude, color = "#800080", radius = ~ (MarkerSize), label = ~ paste(Group, ": ", station_name, sep = ""),
              #                   layerId = SelectedMarkerTwo$df$station_name, group = ~Group)
             })


observeEvent(input$StationTwoSelect,
             {
               leafletProxy("LeafMap")%>%
               removeMarker(SelectedMarkerTwo$df$station_name)
               SelectedMarkerTwo$df <- filter(MapDataReactive$df, station_name %in% input$StationTwoSelect)
               NonSelectedMarkerUpdate(MapDataReactive$df)
               SelectedMarkerUpdate(SelectedMarkerOne$df,"#fc090a")
               SelectedMarkerUpdate(SelectedMarkerTwo$df,"#800080")

               # leafletProxy("LeafMap")%>%
               # addCircleMarkers(data = MapDataReactive$df, lng = ~longitude, lat = ~latitude, color = ~Color, radius = ~ (MarkerSize), label = ~ paste(Group, ": ", station_name, sep = ""),
               #                  layerId =MapDataReactive$df$station_name, group = ~Group)%>%
               # addCircleMarkers(data = SelectedMarkerTwo$df, lng = ~longitude, lat = ~latitude, color = "#800080", radius = ~ (MarkerSize), label = ~ paste(Group, ": ", station_name, sep = ""),
               #                  layerId = SelectedMarkerTwo$df$station_name, group = ~Group)%>%
               # addCircleMarkers(data = SelectedMarkerOne$df, lng = ~longitude, lat = ~latitude, color = "#fc090a", radius = ~ (MarkerSize), label = ~ paste(Group, ": ", station_name, sep = ""),
               #                  layerId = SelectedMarkerOne$df$station_name, group = ~Group)
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

output$ChartTwoTitle <- renderText({ 
  if(nrow(ChartTwoData()) < 2 || (nrow(ChartTwoData()) - sum(is.na(ChartTwoData()))) == 0)
  {
    paste(input$StationTwoSelect, ": ", "Insufficient Data", sep = "")
  }
  else
  {
    paste(input$StationTwoSelect, ": ", input$ParameterTwoSelect, sep = "")
  }
})


output$ChartTwo <- renderDygraph({
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
paste(input$DownloadSelect)
if(input$DownloadSelect == "Chart One")
{
Data <- ChartOneData()
}
if(input$DownloadSelect == "Chart Two")
{
Data <- ChartTwoData()
}
else
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
