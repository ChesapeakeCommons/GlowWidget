GLOW Widget
---------------

About
---------------
The GLOW Widget was commisioned by [Cleveland Water Alliance](https://clevelandwateralliance.org/) to highlight the efforts of its Great Lakes One Water initiative.
Built by [The Commons](https://www.ourcommoncode.org/), the GLOW widget combines data from nine small watershed partners across the greater Lake Erie watershed. 
Below you will find general information about the GLOW Widget. Additional documentation can be found inline in **App.R, Styler.css and DataCleaner.R**.


Quick Links
---------------
[Application](https://thecommons.shinyapps.io/CWA_Widget/)

[Git](https://github.com/ChesapeakeCommons/GlowWidget)


Version Control and Access 
---------------
You can download the application from the [Git repository.](https://github.com/ChesapeakeCommons/GlowWidget) After downloading, please fork a new branch, and name it your last name, and date. e.g. Watson_07_08. 
Make any changes you'd like to this branch - it is yours. If there is a bug or problem with the Master branch, please perform a pull request to make changes to the core application and to be subsequently approved by the Commons team.

Application Publishing
---------------
To publish your own application, use the whirlpool icon in Rstudio to publish via [Shinyapps.io](https://www.shinyapps.io/). 
You can see a step by step process of this [here](https://www.r-bloggers.com/2021/05/push-button-publishing-for-shiny-apps/).

Application Structure
---------------
The application is broken into three scripts, **DataCleaner.R, App.R, and Styler.css**. <br> </p>
The **DataCleaner.R** script imports data downloaded from [WaterReporter](www.waterreporter.org).  It creates CombinedDataCleaned.csv, a combination of the nine small watershed groups data. 
It also produces GroupData.csv, a sheet containing summary information about each group. <br> </p>
**App.R** is split into two sections, UI side and the Server side. The Server side ingests the input data sheets and renders the interactive components via the UI side. The majority of the code sits in the Server side. <br>
**App.R Server Side Sections:**<br>
  * **Imports and Variable Delceration** 
     * Imports from **DataCleaner.R** and watershed geojson (Huc8s.geojson).
     * Reactive variable instantiation for controll logic and UI components
  * **Global Functions** 
     * GetGroup()
     * GetUnit()
  * **Model** 
      * Startup information model
  * **Filters** 
      * Group Filter 
      * Station One Filter 
      * Station Two Filter 
      * Parameter One Filter
      * Parameter Two Filter
      * Sample Count Filter
      * Download Filter
      * Group Filter Observe Event
      * Station One Observe Event
      * GroupText, dynamic Group Text display 
   * **Map**
      * Leaflet map original render 
      * Popup Maker function 
      * SelectedMarker Update function for changing colors on map 
      * NonSelectedMarker Update function for changing colors on map
      * Map Marker Click Observe Event 
      * Observe for MapDataReactive changes based on filter choices. 
   * **Charts** 
      * ChartDataMaker, formats and preps data for ChartMaker
      * ChartMaker, generates charts based on station and parameter
      * TableMaker, formats data for table 
      * ChartOneTitle
      * ChartOne, calls ChartMaker, uses ChartOneData reactive
      * ChartOneTable, calls TableMaker 
      * ChartTwoTitle
      * ChartTwo, calls ChartMaker, uses ChartTwoData reactive
      * ChartTwoTable, calls TableMaker
   *  **Download**
      * DownloadSelectionReactive, reads Download filter and produces correct downloadable data.
      * DataDownload, download component.<br> 
      
 The **Styler.css** alters the UI components and formats them correctly for display on mobile and desktop. Structure mirrors that found in the **App.R** UI side. See script for additional documentation.
       
      
      
        



