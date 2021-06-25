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
library(shinyjs)

options(shiny.reactlog=TRUE) 

# Define UI for application that draws a histogram
ui <- fluidPage(
  shinyjs::useShinyjs(),
  theme = "styler.css",
  tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
  ),
  tags$link(rel = "stylesheet", type="text/css", href="https://fonts.googleapis.com/icon?family=Material+Icons"),
                
  shinyjs::useShinyjs(),
  
#  div(
#    id = "background"
#  ),
  
  #Start Header
  div(id = "header",
      div(id = "header-title-image"),
      div(id = "header-info-button",
        actionButton("Info","Info")
      )
  ),      
  #END Header
  #START Top Menu
                
  div(class = "mainPanel_A", style='border-bottom:1px solid #cccccc;',

    div(class = "A_subPanel", style="width:30%; min-width:360px; margin-right:0px;",
      #  div( id="groupSelect_wrapper",
          uiOutput("GroupFilter"),
          uiOutput("SampleCountFilter")
      #  )
    ),
    div(class = "A_subPanel", id='GroupText_containter', style="position:relative",
        uiOutput("GroupText")
        
        
    ), 
   
      div(class = "A_subPanel downloadPanel",style="position:relative;min-width: 200px; max-width: 200px;",
           #  div(id="download_subpanel", class="B_subPanel",
                 div(id="download_wrapper",
                     selectInput("DownloadSelect","Download Options",choices = c("Chart One Data","Chart One Summary", "Chart Two Data", "Chart Two Summary","All Data", "All Data Summary"), selected = "Chart One", multiple = FALSE),
                     div(id='downloadButton_wrapper',
                         downloadButton('DataDownload','Download')
                         
                     )
                 )
            # )
      
    )
   
  ),
  #END Top Menu
  #START MAP Display
  
  div(class = "mainPanel_A leafPanel",
      style = "",
            
            #tags$h3(uiOutput("GroupValidate")),
         #   uiOutput("StationOneValidate"),
            leafletOutput("LeafMap", height = 'calc(100vh - 59vh)', width = '100%')
  ),
           
  #END Map Panel
  #START Charts
  div(class = "mainPanel_A", style='border-top:1px solid #cccccc;',
      #bottom:0px; position: absolute; width:100%;
    div(class = "A_subPanel chartPanel", style="width: 50%;",
        tags$h3(textOutput("ChartOneTitle")),
    div(style = "width:100%; height:20px; display:block;",
      uiOutput("StationOneFilter"),
      uiOutput("ParameterOneFilter"),
    ),
      #tags$h3(uiOutput("StationOneValidate")),
     # tags$h3(uiOutput("ParameterOneValidate")),
      
      tabsetPanel(
        tabPanel("Chart", dygraphOutput("ChartOne", height = 200, width = '100%')),
        tabPanel("Summary Statistics", tableOutput("ChartOneTable"))
      )
    ),
    div(class = "A_subPanel chartPanel", style="width: 50%;",
        tags$h3(textOutput("ChartTwoTitle")),
        div(style = "width:100%; height:20px; display:block;",
      uiOutput("StationTwoFilter"),
      uiOutput("ParameterTwoFilter"),
        ),
      
     # tags$h3(uiOutput("StationTwoValidate")),
     # tags$h3(uiOutput("ParameterTwoValidate")),
      
      tabsetPanel(
        tabPanel("Chart",  dygraphOutput("ChartTwo", height = 200, width = '100%')),
        tabPanel("Summary Statistics", tableOutput("ChartTwoTable"))
      )
    )
    #END Charts
  )
 
               

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    
#### DATA IMPORT AND VARIABLE DECLERATION #####

suppressMessages(InputData <- read_csv("Data/Combined_Data12.csv"))

suppressMessages(GroupData <- read_csv("Data/GroupData2.csv"))

#Huc Layer 
Hucs <- suppressMessages(rgdal::readOGR("Data/Huc8s_v3.geojson", verbose = FALSE))

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

  Unit <- ParamUnits %>%
    filter(ParameterList == Parameter)%>%
    select(Units)%>%
    mutate(Units = ifelse(Parameter == "Turbidity" & GroupName == "Euclid Creek Watershed Program" | GroupName == "Rocky River Watershed Council","cm",c(as.matrix(Units))))%>%
    as.matrix()%>%
    c()
}


##### MODEL #### 
observeEvent(input$Info,{
  
  url <- "https://clevelandwateralliance.org/smart-citizen-science"
showModal(modalDialog(
  title = HTML("<b> Cleveland Water Alliance's Smart Citizen Science Widget </b>"),
  HTML("<b> About: </b>"),
  tags$a(href="https://clevelandwateralliance.org/smart-citizen-science", "The Smart Citizen Science Initiative "),
  HTML("is a network that connects, supports, and accelerates the Lake Erie Citizen Science Ecosystem
       and serves as a platform for driving regional water quality and ongoing Citizen Science Innovation. 
       The Initiative is led at the regional level by Cleveland Water Alliance and is powered locally by a coalition of 
       Community Foundations and Volunteer Monitoring Programs."),
  HTML("<br>"),
  HTML("<br>"),
  HTML("<b> Widget Tips and Tricks: </b>"),
  HTML("Use the group filter to choose one of the eight Smart Citizen groups. You can also control stations by the number of collected samples. 
  Select your station by clicking on the map or using the station select pulldowns! The charts below will data depending on your station and parameter choices. 
  You can use them to compare water quality data at two different stations, or different parameters from the same station! 
  Download the data using the download button and download options"),
  HTML("<br>"),
  HTML("<br>"),
  HTML("<b> Disclaimer: </b>"),
  HTML("The data in this application is collected by multiple groups, using different protocols, sampling frequencies, and levels of rigor.
  Data is solely for exploratory purposes and is not intended for analysis. 
  Cleveland Water Alliance makes no assurance of the data's quality, or any resulting comparisons made across groups, stations, time, or parameters
"), 
   easyClose = TRUE,
   footer = NULL,
))
  
  
  
})
#### END MODEL #### 


#### FILTERS #####
#Group Select
output$GroupFilter <- renderUI({
selectizeInput("GroupSelect","Select a Group", choices = sort(c("All Champions", InputData$Group), decreasing = FALSE), selected = "All Champions", multiple = TRUE)
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
   req(input$GroupSelect)
   if("All Champions" %in% input$GroupSelect)
   {
     Lim <- InputData
   }
   else
   {
     Lim <- filter(InputData, Group %in% input$GroupSelect) 
   }
   
  Min <-  Lim %>%
          select(StationSampleCount)%>%
          as.tibble()%>%
          arrange(StationSampleCount)%>%
          slice_head()%>%
          pull()
  
  Max <- Lim %>%
         select(StationSampleCount)%>%
         as.tibble()%>%
         arrange(StationSampleCount)%>%
         slice_tail()%>%
         pull()
  
   sliderInput("SampleCountSelect","Filter by Sample Count", min = Min, max = Max, value = c(Min,Max), dragRange = TRUE, step = 10)
   
 })
 
 observeEvent(input$SampleCountSelect,{
  req(input$GroupSelect)

  MapDataReactive$df <- InputData %>%
                        filter(StationSampleCount >= input$SampleCountSelect[1],
                        StationSampleCount <= input$SampleCountSelect[2])

  if("All Champions" %in% input$GroupSelect)
  {
   MapDataReactive$df <- MapDataReactive$df  
  }
  else
  {
    
   MapDataReactive$df <- filter(MapDataReactive$df, Group %in% input$GroupSelect) 
  }

 
 #  #Default Selection for a station is the first and second most sampled stations in the group list
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
   
   if("All Champions" %in% input$GroupSelect || nrow(data.frame(input$GroupSelect)) > 1)
   {
   
     leafletProxy("LeafMap")%>%
       setView(lng = -81.248031, lat = 42.160191, zoom = 7.45)
   }
   else
   {
     leafletProxy("LeafMap")%>%
       setView(lng = MapDataReactive$df %>% filter(station_name == DefaultStationOne$S) %>% slice_head %>% pull(longitude), 
               lat = MapDataReactive$df %>% filter(station_name == DefaultStationOne$S) %>% slice_head %>% pull(latitude), zoom = 8.5)
   }
   ## Changes the zoom extent when group is changed


    
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
# Commenting out this section as I think I fixed it with the correct requires - might put back in if something breaks.
# ######## VALIDATE SECTION ##########
# output$GroupValidate <- renderText({
#   #req(input$GroupSelect)
#   validate(
#     need(input$GroupSelect != "","Select a Group!")
#   )
# })
#  
# output$StationOneValidate <- renderText({
#   validate(
#     need(input$StationOneSelect  != "", "Select a Station!")
#    )
# }) 
#  
# output$StationTwoValidate <- renderText({
#   #req(input$StationTwoSelect)
#   validate(
#     need(input$StationTwoSelect != "","Select a Station!")
#   )
# }) 
#  
# output$ParameterOneValidate <- renderText({
#  # req(input$ParameterOneSelect)
#   validate(
#     need(input$ParameterOneSelect != "","Select a Parameter!")
#   )
# })
#  
# output$ParameterTwoValidate <- renderText({
#  # req(input$ParameterTwoSelect)
#   validate(
#     need(input$ParameterTwoSelect != "","Select a Parameter!")
#   )
# }) 
 
######## END VALIDATE SECTION ########## 

    
#### GROUP TEXT #####
output$GroupText <- renderUI({
    req(MapDataReactive$df)
   # req(StationOneReactive$S)
  #  req(input$GroupSelect)
  
#    tagList(
#      HTML("<div style='display:none;'>"),
      GroupName <- GetGroup(MapDataReactive$df,DefaultStationOne$S)
#     
#    )
    
    GroupFrame <- filter(GroupData, Group == GroupName)

    
    tagList(
  #  HTML("<div style='display:block; background-color: red;'>"),
     
  #  HTML("/div>"),
    HTML("<div class='B_subPanel' style='width:35%;'>"),
        HTML("<div><font style='color:orange;'>Group: </font>"),
        paste0(GroupName),
        HTML("</div>
             <div><font style='color:orange;'>Watershed(s): </font>"),
        paste0( GroupFrame$HucList),
        HTML("</div>
             <div><font style='color:orange;'># of Samples: </font>"),
        paste0(GroupFrame$TotalSamples),
        HTML("</div>
             <div><font style='color:orange;'>Years Sampling: </font>"),
        paste0(GroupFrame$YearRange),
        HTML("</div>
              <div style='display: inline-block; width: 100%;'>
               
                <a style='  
                display: block;
                width: 100%;
                white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;' href='"),
                paste0(GroupFrame$SiteLink),
        HTML("  '> <sub><font style='font-size: 14px; color:orange;' class='material-icons'>public</font></sub>"),paste0(GroupFrame$SiteLink),HTML("</a>
            </div>
      </div>"),
 #   HTML("<div id='website_subpanel' class='B_subPanel'>")
            
  #  HTML("</div>"),
    HTML("<div class='B_subPanel' style='width: 65%; min-height:68px; padding-left: 10px;'>
            <div>"),
              paste0(GroupFrame$Description),
      HTML("</div>
      </div>")
     
  )
})
 #### END  GROUP TEXT  #####
 
#### MAP #####
#Map Decleration
output$LeafMap <- renderLeaflet({
     leaflet("LeafMap")%>%
     setView(lng = -81.248031, lat = 42.160191, zoom = 7.45)%>%
     addMapPane("polygons", zIndex = 210)%>%
     addProviderTiles("CartoDB.VoyagerLabelsUnder", group = "Streets")%>%
     addPolygons(data = Hucs, color = "#b3b3b3", weight = 3, group = "Watersheds", options = pathOptions(pane = "polygons"), label = paste(Hucs$NAME, "Watershed", sep = " ")) %>%
     addProviderTiles("Esri.WorldTopoMap", group = "Terrain")%>%
     addProviderTiles("GeoportailFrance.orthos", group = "Satellite")%>%
     addSearchOSM(options = searchOptions(autoCollapse = TRUE, minLength = 2))%>%
     addLayersControl(overlayGroups = c("Watersheds"),
                     baseGroups = c("Streets", "Terrain", "Satellite"),
                     options = layersControlOptions(collapsed = FALSE,  position = 'bottomright'))#%>%

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

#Oberves when any data is changed and re renders the map
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
  })

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


  return(dygraph(df, group = "x") %>%
     # dyRangeSelector(height = 20, fillColor = "#757575", strokeColor = "#757575")%>%
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

if(input$DownloadSelect == "Chart One Data")
{

  Data <- InputData %>%
    filter(station_name == StationOneReactive$S)%>%
    select(station_id,station_name,latitude,longitude,collection_date,input$ParameterOneSelect)
  
    names(Data)[6] <- paste(as.character(names(Data)[6]),GetUnit(names(Data)[6],GetGroup(MapDataReactive$df,StationTwoReactive$S)))%>%
      str_remove(.,"pH")
}
  
if(input$DownloadSelect == "Chart One Summary")
{
 Data <- TableMaker(MapDataReactive$df,StationOneReactive$S)
         Data <- data.frame(ParameterList,Data) %>%
          mutate(ParameterList = paste(ParameterList,GetUnit(ParameterList,GetGroup(MapDataReactive$df,StationOneReactive$S))))%>%
          mutate(ParameterList = str_remove(ParameterList, "pH"))
         


}
  
if(input$DownloadSelect == "Chart Two Data")
{
  Data <- InputData %>%
    filter(station_name == StationTwoReactive$S)%>%
    select(station_id,station_name,latitude,longitude,collection_date,input$ParameterTwoSelect)

 # names(Data)[6] < 
  names(Data)[6] <- paste(as.character(names(Data)[6]),GetUnit(names(Data)[6],GetGroup(MapDataReactive$df,StationTwoReactive$S)))%>%
                    str_remove(.,"pH")
}
  
  if(input$DownloadSelect == "Chart Two Summary")
  {
    Data <- TableMaker(MapDataReactive$df,StationTwoReactive$S)
    Data <- data.frame(ParameterList,Data)%>%
    mutate(ParameterList = paste(ParameterList,GetUnit(ParameterList,GetGroup(MapDataReactive$df,StationTwoReactive$S))))%>%
    mutate(ParameterList = str_remove("pH"))
  }
  
if(input$DownloadSelect == "All Data")
{
Data <- InputData %>% 
        select(-c(Color,YearRange,MarkerSize))%>%
        rename("Dissolved Oxygen mg/L" = `Dissolved Oxygen`,
           "Nitrate mg/L" = Nitrate,
           "Phosphate mg/L" = Phosphate,
           "Turbidity NTU"  = Turbidity,
           "Temperature C" = Temperature)
}
  
  if(input$DownloadSelect == "All Data Summary")
  {
    df <- InputData %>%
      select(c(`Dissolved Oxygen`, Nitrate, Phosphate, pH, Turbidity, Temperature,Group))%>%
      mutate(Turbidity = ifelse(Group == "Euclid Creek Watershed Program" | Group == "Rocky River Watershed Council",NA,Turbidity))%>%
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
    
    Data <- data.frame(ParameterList,df)%>%
      mutate(ParameterList = paste(ParameterList,GetUnit(ParameterList,"Toledo Metroparks")))%>%
      mutate(ParameterList = str_remove(ParameterList, "pH"))
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
