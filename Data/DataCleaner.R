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
                          ToledoMetroGovernments_Cleaned,
                          SUNY_Cleaned)

#For some reason, SUNY was being dropped. 
CombinedData <- bind_rows(CombinedData, SUNY_Cleaned)

#Creating Marker Size column off of sample station count - custom function dictated by sizing of icons
CombinedData$MarkerSize <- (sqrt(CombinedData$StationSampleCount) * .3) + 4.5

#Pulling out the stations with below 5 sample count
CombinedData <- CombinedData %>% 
                filter(StationSampleCount > 5)


## CLEANED DATA AFTER CHECKS BELOW### 
#ATTEMPTED TO GET NICE DISTRIBUTION FOR ALL VARIABLES## 
CombinedDataCleaned <- CombinedData %>%
  mutate(pH = ifelse(pH > 11 | pH < 2, NA, pH))%>%
  mutate(`Dissolved Oxygen` = ifelse(`Dissolved Oxygen` > 25 | `Dissolved Oxygen` <= 0, NA, `Dissolved Oxygen`))%>%
  mutate(Phosphate = ifelse(Phosphate > 3 ,NA,Phosphate))%>%
  mutate(Turbidity = ifelse(Group == "Euclid",Turbidity*2.54,Turbidity))%>%
  mutate(Turbidity = ifelse(Group == "Rocky",Turbidity*2.54,Turbidity))%>%
  mutate(Temperature = ifelse(Temperature > 80, NA, Temperature))%>%
  mutate(Temperature = ifelse(Temperature > 60,(Temperature -32)/1.8000,Temperature))

### OK WE NEED TO CLEAN SOME VALUES BEFORE SAVING OUT ### 
### Running some stats on the combined data ### 
## Basic stats for the parameters ###

# 
# #pH
# #WILL REMOVE 
# #ALL VALUES OVER 11 and 1 
# ggplot(CombinedData %>% filter(pH < 29)) +
#     geom_histogram(aes(x = pH), bins = 200)
# #Looks good as an overall Distribution 
# 
# 
# #Dissolved Oxygen
# #WILL REMOVE 
# #ALL VALUES > 25 and <= 0

# ggplot(CombinedData) +
#   geom_histogram(aes(x = `Dissolved Oxygen`), bins = 200)

# ggplot(CombinedData %>% filter(`Dissolved Oxygen` < 25)) +
#   geom_histogram(aes(x = `Dissolved Oxygen`), bins = 200)
# 
# 
# 
# #Nitrate
# #So fireland coastal tributaries has a lot of high values in the 20's but they all seem to be downstream of a massive quary/concrete
# #mfg thing, and the distribution just looking at the group aloine is similiar to the full spread minus the group 
# ggplot(CombinedData)+
#   geom_histogram(aes(x = Nitrate), bins = 200)
# 
# ggplot(CombinedData %>% filter(Group == "Firelands Coastal Tributaries"))+
#   geom_histogram(aes(x = Nitrate), bins = 200)
# 
# ggplot(CombinedData %>% filter(Group != "Firelands Coastal Tributaries"))+
#   geom_histogram(aes(x = Nitrate), bins = 200)
# 
# 
# 
# #Phosphate
# # Going to remove values less than 3
# #Its crazy to ge a value more than 1, but most of the sites with high values are in super suburban areas. 
# ggplot(CombinedData)+
#   geom_histogram(aes(x = Phosphate), bins = 200)
# 
# #Phosphate
# ggplot(CombinedData %>% filter(Phosphate < 3))+
#   geom_histogram(aes(x = Phosphate), bins = 200)
# 
# 
# 
# #Turbidity 
# #Ok no really easy way to convert from NTU to cm
# #Euclid needs to be converted from inches to cm 
# ggplot(CombinedData)+
#   geom_histogram(aes(x = Turbidity), bins = 200)
# 
# ggplot(CombinedData %>% filter(Turbidity < .1))+
#   geom_histogram(aes(x = Turbidity), bins = 200)
# 
# 
# 
# 
# 
# #Temperature 
# # Removing values greater than 66
# # Converting values between 30 and 66 to C as we assume these are F from SUNY fredonia 
# #(Note all samples in the higher rante (20-30 fell during Aug and July in highly suburban areas )
# ggplot(CombinedData)+
#   geom_histogram(aes(x = Temperature), bins = 200)
# 
# ggplot(CombinedData %>% filter(Temperature < 30))+
#   geom_histogram(aes(x = Temperature), bins = 200)
# 
# ggplot(CombinedData %>% filter(Temperature > 20))+
#   geom_histogram(aes(x = Temperature), bins = 200)


#Checking out Turbidity again, yes the inches peak moved next to the Cm peak on the hist, 
#will just display with different units and note in disclaimer 
  
# ggplot(CombinedDataCleaned)+
#   geom_histogram(aes(x = Turbidity), bins = 200)
# 
# ggplot(CombinedData)+
#   geom_histogram(aes(x = Turbidity), bins = 200)


#Checking out Temp again 
# ggplot(CombinedDataCleaned)+
#   geom_histogram(aes(x = Temperature), bins = 200)
# 
# ggplot(CombinedData)+
#   geom_histogram(aes(x = Temperature), bins = 200)

#Sweet, looks like an actually histogram now. 

# #Checking out DO and PH,
# #All plots look fairly unifirom 
# ggplot(CombinedDataCleaned) +
#   geom_histogram(aes(x = `Dissolved Oxygen`), bins = 200)
# 
# ggplot(CombinedDataCleaned) +
#   geom_histogram(aes(x = pH), bins = 200)
# 


StationHucs <- Hucs %>%
               select(station_name,NAME)

#Now adding in the Hucs so they can appear in the popups 
CombinedDataCleanedHucs <- inner_join(StationHucs,CombinedDataCleaned) %>% distinct_all()
                           


#Different Numbers (5708 Combined_Data9 vs 5655 Combined Data Cleaned Hucs) after merge despite the row count being identical for the stations above.
#One Station, ADW05 is getting dropped for some weird reason can't figure out why, but it accounts for the 53 row difference. adding here. 

CombinedDataCleaned <-  CombinedDataCleaned %>%
                        mutate(NAME = "Detriot")%>%
                        filter(station_id == "ADW05")%>%
                        bind_rows(CombinedDataCleanedHucs)

                          
  
  ####### FINAL EXPORT LEAVE THIS UNCOMMENTED UNLESS SAVING OUT ####### 
  write.csv(CombinedDataCleaned, "Data/Combined_Data11.csv", row.names = FALSE)
  




### GROUP SECTION 
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

