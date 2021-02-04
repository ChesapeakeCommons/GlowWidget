#Cleaning Data before entry into the GLOW Widget 
# We need these variables 
  # station_id
  # station_name 
  # latitude
  # longitude 
  # collection_date
  # group 
  # Dissolved Oxygen
  # Nitrate 
  # Phosphate 
  # pH
  # Turbidity
  # Temperature
  # Map Color 

library(tidyverse)
library(lubridate)
library(stringr)


#Import 
Euclid <- read_csv("Data/EuclidCreek.csv")
Euclid$PlaceHolder <- "ZZZZZZ"

Rocky <- read_csv("Data/RockyRiver.csv")

BuffaloNiagara <- read_csv("Data/BuffaloNiagaraRiverKeeper.csv")
BuffaloNiagara$PlaceHolder <- "ZZZZZZ"
BuffaloNiagara$PlaceHolder2 <- "ZZZZZZ"

ClevelandMetroParks <- read_csv("Data/ClevelandMetroParks.csv")
ClevelandMetroParks$PlaceHolder <- "ZZZZZZ"
ClevelandMetroParks$PlaceHolder2 <- "ZZZZZZ"
ClevelandMetroParks$PlaceHolder3 <- "ZZZZZZ"

FirelandsCoastalTributaries <- read_csv("Data/FirelandsCoastalTributaries.csv")
FirelandsCoastalTributaries$PlaceHolder <- "ZZZZZZ"

HuronRiver <- read_csv("Data/HuronRiver.csv")
HuronRiver$PlaceHolder <- "ZZZZZZ"

ToledoMetroParks <- read_csv("Data/ToledoMetroParks.csv")
ToledoMetroParks$PlaceHolder <- "ZZZZZZ"
ToledoMetroParks$PlaceHolder2 <- "ZZZZZZ"

SUNY <- read_csv("Data/SUNY.csv")
SUNY$PlaceHolder <- "ZZZZZZ"
SUNY$PlaceHolder2 <- "ZZZZZZ"

ToledoMetroGovernments <- read_csv("Data/ToledoMetroGovernments.csv")


#Reading in Hucs and stations from a join in QGIS 
Hucs <- read_csv("Data/Huc8Stations.csv")

#Descriptive Text about groups made from G sheets 
GroupText <- read_csv("Data/GroupText.csv")


#Function for processing data into clean format - variables described above in the documentation
DataCleaner <- function(inDf,inGroup,inDO, inNitrate, inPhosphate, inpH, inTurbidity, inTemperature, inColor,pNum)
        {
        dfOut <- inDf %>%
                  select(station_id,
                         station_name,
                         latitude,
                         longitude,
                         collection_date,
                         inDO, 
                         inNitrate, 
                         inPhosphate, 
                         inpH, 
                         inTurbidity, 
                         inTemperature)%>%
                   rename("Dissolved Oxygen" = inDO,
                          "Nitrate" = inNitrate,
                          "Phosphate"= inPhosphate,
                          "pH"= inpH,
                          "Turbidity" = inTurbidity,
                          "Temperature"= inTemperature)%>%
                           filter(!is.na(station_id))%>%
                           filter(!is.na(collection_date))%>%
                           filter(!is.na(latitude))%>%
                           mutate(Group = inGroup)%>%
                           mutate(Color = inColor)%>%
                           mutate(Date = as.Date(substring(collection_date,1,10)))%>%
                           mutate(collection_date = as.character(collection_date))%>%
                           mutate(YearRange = (max(year(Date)) - min(year(Date))))%>%
                           select(-c(Date))
                           
                           dfOut$TotalSamples <- (pNum*nrow(dfOut) - sum(is.na(dfOut)))
                           
                            #Getting number of samples per station for parameters
                            #I know there are mnuch cleaner ways to do this but yolo
                           
                           
                            dfOut <- dfOut %>%
                             mutate_if(is.character, list(~na_if(., "ZZZZZZ")))%>%
                             group_by(station_id) %>%
                             mutate(StationSampleCount = sum(!is.na(`Dissolved Oxygen`))
                                                     + sum(!is.na(Nitrate))
                                                     + sum(!is.na(Phosphate))
                                                     + sum(!is.na(pH))
                                                     + sum(!is.na(Turbidity))
                                                     + sum(!is.na(Temperature)))

                          
        return(dfOut)
        }

#Function Calls 
Euclid_Cleaned <- DataCleaner(Euclid,"Euclid","Dissolved Oxygen mg/L [p:1549]","PlaceHolder","Phosphate [p:1543]","pH [p:1547]","Turbidity Tube Trial Two [p:1539]","Temperature YSI [p:1552]","#03aeee",5)
Rocky_Cleaned <- DataCleaner(Rocky, "Rocky", "Dissolved oxygen [p:1561]","Nitrate [p:1560]","Phosphate [p:1559]","pH [p:1555]","Turbidity [p:1554]","Water Temperature [p:1562]","#f69d01",6)
BuffaloNiagara_Cleaned <- DataCleaner(BuffaloNiagara, "Buffalo Niagara", "Dissolved Oxygen (mg/L) [p:779]", "PlaceHolder", "PlaceHolder2","pH [p:777]","Turbidity [p:782]","Temperature [p:776]","#3852a4",4)
ClevelandMetroParks_Cleaned <- DataCleaner(ClevelandMetroParks, "Cleveland MetroParks", "Dissolved oxygen [p:1565]", "PlaceHolder", "PlaceHolder2","Ph [p:1566]","PlaceHolder3","Temperature [p:1564]","#91d100",3)
FirelandsCoastalTributaries_Cleaned <- DataCleaner(FirelandsCoastalTributaries, "Firelands Coastal Tributaries", "Dissolved Oxygen [p:1471]", "Nitrate [p:1475]", "PlaceHolder","pH [p:1469]","Turbidity [p:1472]","Water Temperature [p:1470]","#90353b",5)
HuronRiver_Cleaned <- DataCleaner(HuronRiver, "Huron River Watershed Council", "Dissolved Oxygen [p:1596]", "Nitrate [p:1603]", "Orthophosphate [p:2118]","pH [p:1604]","PlaceHolder","Water temperature [p:1606]","#6e8e84",5)
ToledoMetroParks_Cleaned <- DataCleaner(ToledoMetroParks, "Toledo Metroparks", "PlaceHolder", "Nitrate [p:1578]", "Phosphate [p:1580]","Ph [p:1577]","PlaceHolder2","Water temperature [p:1569]","#172755",4)
SUNY_Cleaned <- DataCleaner(SUNY, "SUNY Fredonia", "Dissolved Oxygen [p:1523]", "PlaceHolder", "PlaceHolder2","pH [p:1522]","Turbidity [p:1946]","Water Temperature [p:1521]","#57634a",4)
ToledoMetroGovernments_Cleaned <- DataCleaner(ToledoMetroGovernments, "Toledo Metropolitan Area Council of Governments", "Dissolved Oxygen [p:1616]","Nitrate [p:1621]","Phosphate [p:1619]","pH [p:1618]","Turbidity [p:1614]","Temperature [p:1612]","#e7ad73",6)
#Merging
CombinedData <- bind_rows(Rocky_Cleaned,
                          Euclid_Cleaned,
                          BuffaloNiagara_Cleaned, 
                          ClevelandMetroParks_Cleaned,
                          FirelandsCoastalTributaries_Cleaned,
                          HuronRiver_Cleaned,
                          ToledoMetroParks_Cleaned,
                          SUNY_Cleaned,
                          ToledoMetroGovernments_Cleaned)

#Creating Marker Size column off of sample station count - custom function dictated by sizing of icons
CombinedData$MarkerSize <- (sqrt(CombinedData$StationSampleCount) * .3) + 5

#Pulling out the stations with below 5 sample count
CombinedData <- CombinedData %>% 
                filter(StationSampleCount > 5)

#Merging in huc names



#Exporting out
#write.csv(CombinedData, "Data/Combined_Data7.csv", row.names = FALSE)

## Ending Input Data ## 
## Group Layer ## 
#Raw Combined data with just groups
CombinedDataHucsWGroups <- CombinedData %>%
                      ungroup()%>%
                      select(Group,station_id)%>%
                      merge(Hucs)%>%
                      distinct()

GroupData <- CombinedData %>%
            select(Group,TotalSamples,YearRange,station_id)%>%
            ungroup()%>%
            select(-c(station_id))%>%
            group_by(Group)%>%
            summarize(TotalSamples = first(TotalSamples), YearRange = first(YearRange))%>%
            mutate(YearRange = ifelse(YearRange == 0, 1, YearRange))

CombinedDataHucsGrouped <- CombinedDataHucsWGroups %>%
                           select(Group,NAME)%>%
                           group_by(Group)%>%
                           distinct()%>%
                           mutate(HucList = paste0(NAME, collapse = ", "))%>%
                           select(-c(NAME))%>%
                           distinct()


### Group Data with total samples, years sampled, and watersheds present #### 
GroupData <- merge(GroupData,CombinedDataHucsGrouped)

GroupDataFinal <- merge(GroupData,GroupText)


write.csv(GroupDataFinal, "Data/GroupData2.csv", row.names = FALSE)

