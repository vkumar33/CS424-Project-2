library(shiny)
library(shinydashboard)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(tidyr)
library(plotly)
library(magrittr)
library(dplyr)
library(pillar)
library(tidyverse)
library(ggplot2)
library(readr)
library(data.table)
library(parallel)
library(raster)
library(leaflet)
library(viridis)
library(zoo)
library(feather)
#library(reshape2)

#---------------------------------- Pre-processing------------------------------------------------------------------

# #Load files from directory
# mapdata1 <- read.csv(file = "aqs_sites.csv",header = TRUE, sep = ",")
# temp = list.files(pattern="^an.*csv")
# allData2 <- lapply(temp, fread, stringsAsFactors = TRUE)
# allData3 <- do.call(rbind, allData2)
# temp <- NULL
# allData2 <- NULL
# 
# allData3 <- allData3%>%
#   arrange(State)
# 
# 
# 
# Hourly data
# temp1 = list.files(pattern="^hourly.*csv")
# hourly <- lapply(temp1, fread, select =c(9,12,13,14,22,23))
# hourlydata <- do.call(rbind, hourly)
# hourlydata$`Date GMT` <- as.Date(fast_strptime(hourlydata$`Date GMT`, "%Y-%m-%d"))
# hourlydata <- data.frame(hourlydata)
# temp1 <- NULL
# hourly <- NULL
# hourlydata$Parameter.Name <- as.factor(hourlydata$Parameter.Name)
# hourlydata$State.Name <- as.factor(hourlydata$State.Name)
# hourlydata$County.Name <- as.factor(hourlydata$County.Name)
# hourlydata$Time.GMT <- as.factor(hourlydata$Time.GMT)

# hourlydata <- hourlydata %>%
#   distinct(Parameter.Name,State.Name,County.Name,Date.GMT,Time.GMT, .keep_all = TRUE)


#makeTime <- function(x) as.POSIXct(x, format = "%H:%M")
#hourlydata$`Time GMT` <- lapply(hourlydata$`Time GMT`, makeTime)

# #hourlydata$`Time GMT` <- lapply(hourlydata$`Time GMT`, paste ,"00", sep = ":")
# 
# 
# #Making a copy of original Dataframe
# allData4 <- allData3
# 
# #Renaming column names
# colnames(allData3)[c(5:10)] <- c("Good","Moderate","Unhealthy(Sensitive)","Unhealthy","Very Unhealthy","Hazardous")
# colnames(allData3)[c(11:13)] <- c("Max AQI", "90th Percentile of AQI","Median AQI")
# colnames(allData3)[c(14:19)] <- c("CO","NO2","Ozone","SO2","PM2_5","PM10")
# 
# #Making a copy of modified Dataframe
# allData5 <- allData3
# 
# #Creating new column AQI Parameters and Days which has the aggregate days of all AQI parameters
# allData3Gather <- gather(allData3, "AQI Parameters", "Days", 5:10)
# #Creating new column Pollutants and Days which has the aggregate days of all Pollutants
# allData3Gather2 <- gather(allData3, "Pollutants", "Days", 14:19)
# #Creating new column Maxq and Values which has the aggregate of Max AQI, median and 90th Percentile of AQI
# allData3Gather3 <- gather(allData3,"Maxq", "Values", 11:13)
# 
# #Creating new columns which has aggregate percentage of all pollutant types
# 
# allData4$CO <- (allData4$`Days CO` / allData4$`Days with AQI`)* 100
# allData4$NO2 <- (allData4$`Days NO2` / allData4$`Days with AQI`) *100
# allData4$Ozone <- (allData4$`Days Ozone` / allData4$`Days with AQI`) *100
# allData4$SO2 <- (allData4$`Days SO2` / allData4$`Days with AQI`) * 100
# allData4$PM2_5 <- (allData4$`Days PM2.5` / allData4$`Days with AQI`) * 100
# allData4$PM_10 <- (allData4$`Days PM10` / allData4$`Days with AQI`) * 100
# 
# 
# allData4 <- allData4 %>% mutate_if(is.numeric, round, 2)
# allData4Gather <- gather(allData4,"PollutantsP","Values",20:25)
# 
# #Creating new columns which has aggregate percentage of all AQI types
# 
# allData5$Good <- (allData5$Good / allData5$`Days with AQI`)* 100
# allData5$Moderate <- (allData5$Moderate / allData5$`Days with AQI`) *100
# allData5$`Unhealthy(Sensitive)` <- (allData5$`Unhealthy(Sensitive)` / allData5$`Days with AQI`) *100
# allData5$Unhealthy <- (allData5$Unhealthy / allData5$`Days with AQI`) * 100
# allData5$`Very Unhealthy` <- (allData5$`Very Unhealthy` / allData5$`Days with AQI`) * 100
# allData5$Hazardous <- (allData5$Hazardous / allData5$`Days with AQI`) * 100
# 
# 
# allData5 <- allData5 %>% mutate_if(is.numeric, round, 2)
# allData5Gather <- gather(allData5,"AQIP","Values",5:10)
# 
# 
# 
# #Load daily files from directory
# temp2 = list.files(pattern="^daily.*csv")
# allData6 <- lapply(temp2, fread, stringsAsFactors = FALSE)
# allData7 <- do.call(rbind, allData6)
# allData7 <- allData7[,c(1,2,5,6,7,8)]
# temp2 <- NULL
# allData6 <- NULL
# 
# #Load annual files from directory
# mapdata <- read.csv(file = "aqs_sites.csv",header = TRUE, sep = ",")
# temp3 = list.files(pattern="^an.*csv")
# allData8 <- lapply(temp3, fread)
# allData9 <- do.call(rbind, allData8)
# temp3 <- NULL
# allData8 <- NULL
# 

# Australian data
# temp3 = list.files(pattern="^openaq.*csv")
# ind <- lapply(temp3, fread, select =c(1,2,4,6,7))
# australiandata <- do.call(rbind, ind)
# temp3 <- NULL
# ind <- NULL
# 
# australiandata$Date <- as.POSIXct(australiandata$utc, format = "%Y-%m-%dT%H:%M:%OSZ")
# australiandata$Date <- format(as.POSIXct(australiandata$utc, format = "%Y-%m-%dT%H:%M:%OSZ"), "%Y-%m-%d" )
# australiandata$Time <- format(as.POSIXct(australiandata$utc, format = "%Y-%m-%dT%H:%M:%OSZ"), "%H:%M" )
# australiandata$Date <- as.Date(fast_strptime(australiandata$Date, "%Y-%m-%d"))
# australiandata$location <- paste0(australiandata$location,"(",australiandata$city,")")
# 
# australiandata$location <- as.factor(australiandata$location)
# australiandata$parameter <- as.factor(australiandata$parameter)
# australiandata <- australiandata[,c(1,4,5,6,7)]
# a <- cast(australiandata, location+Date+Time ~ parameter, sum, value = 'values')
# a$Total <- a$co + a$no2 + a$o3 + a$pm10 + a$pm25 + a$so2
# 
# 
# a$co <- (a$co/a$Total)*100
# a$no2  <- (a$no2/a$Total)*100
# a$o3  <- (a$o3/a$Total)*100
# a$pm10  <- (a$pm10/a$Total)*100
# a$pm25 <- (a$pm25/a$Total)*100
# a$so2   <- (a$so2/a$Total)*100
# 
# 
# a <- a %>% mutate_if(is.numeric, round, 2)
# a <- a[,c(1:9)]
# australianline <- a
# a <- NULL
# australianlineGather <- gather(australianline,"PollutantsP","PValues",4:9)

# #convertYear <- function(x){
# #  return(as.Date(as.character(x)))
# #}
# #no_cores <- detectCores()
# #clust <- makeCluster(no_cores)
# #parLapply(clust,allData7$Date,convertYear)
# #lapply(allData7$Year,format,"%Y")
# #lapply(allData7$Year,as.numeric)
# #lapply(allData7$Date,format,"%B")
# #stopCluster(clust)
# 
# 
# #Converting columns to character and numeric class
# 
# #allData7$Date <- as.Date(as.character(allData7$Date))
# allData7$Date <- as.Date(fast_strptime(as.character(allData7$Date), "%Y-%m-%d"))
# 
# allData7$Year <- format(allData7$Date,"%Y")
# #allData7$Year <- as.Date(fast_strptime(as.character(allData7$Date), "%Y"))
# 
# allData7$Year <- as.numeric(allData7$Year)
# 
# allData7$Month <- format(allData7$Date,"%B")
# #allData7$Month <- as.Date(fast_strptime(as.character(allData7$Date), "%B"))
# 
# colnames(allData7)[1:2] <- c("State","County")
# allData7 <- as.data.frame(allData7)
# 
# 
# #Rectifying map
# mapdata <- mapdata[,c(4,5,23,24)]
# mapdata <- mapdata[complete.cases(mapdata),]
# mapdata <- mapdata %>%
#   filter(Latitude != 0 & Longitude != 0)
# 
# colnames(mapdata)[c(3,4)] <-  c("State","County")
# mapdata$check <- paste(mapdata$State,mapdata$County)
# mapdata <- mapdata[!duplicated(mapdata$check),]
# 
# #Making a copy of original Dataframe
# allData10 <- allData9
# 
# #Renaming column names
# colnames(allData9)[c(5:10)] <- c("Good","Moderate","Unhealthy(Sensitive)","Unhealthy","Very Unhealthy","Hazardous")
# colnames(allData9)[c(11:13)] <- c("Max AQI", "90th Percentile of AQI","Median AQI")
# colnames(allData9)[c(14:19)] <- c("CO","NO2","Ozone","SO2","PM2_5","PM10")
# 
# #Making a copy of modified Dataframe
# allData11 <- allData9
# 
# #Creating new column AQI Parameters and Days which has the aggregate days of all AQI parameters
# allData9Gather <- gather(allData9, "AQI Parameters", "Days", 5:10)
# #Creating new column Pollutants and Days which has the aggregate days of all Pollutants
# allData9Gather2 <- gather(allData9, "Pollutants", "Days", 14:19)
# #Creating new column Maxq and Values which has the aggregate of Max AQI, median and 90th Percentile of AQI
# allData9Gather3 <- gather(allData9,"Maxq", "Values", 11:13)
# 
# #Creating new columns which has aggregate percentage of all pollutant types
# 
# allData10$CO <- (allData10$`Days CO` / allData10$`Days with AQI`)* 100
# allData10$NO2 <- (allData10$`Days NO2` / allData10$`Days with AQI`) *100
# allData10$Ozone <- (allData10$`Days Ozone` / allData10$`Days with AQI`) *100
# allData10$SO2 <- (allData10$`Days SO2` / allData10$`Days with AQI`) * 100
# allData10$PM2_5 <- (allData10$`Days PM2.5` / allData10$`Days with AQI`) * 100
# allData10$PM_10 <- (allData10$`Days PM10` / allData10$`Days with AQI`) * 100
# 
# 
# allData10 <- allData10 %>% mutate_if(is.numeric, round, 2)
# allData10Gather <- gather(allData10,"PollutantsP","PValues",20:25)
# #allData10Gather <- allData10Gather[order(allData10Gather$PollutantsP,-allData10Gather$Values),]
# 
# #Creating new columns which has aggregate percentage of all AQI types
# 
# allData11$Good <- (allData11$Good / allData11$`Days with AQI`)* 100
# allData11$Moderate <- (allData11$Moderate / allData11$`Days with AQI`) *100
# allData11$`Unhealthy(Sensitive)` <- (allData11$`Unhealthy(Sensitive)` / allData11$`Days with AQI`) *100
# allData11$Unhealthy <- (allData11$Unhealthy / allData11$`Days with AQI`) * 100
# allData11$`Very Unhealthy` <- (allData11$`Very Unhealthy` / allData11$`Days with AQI`) * 100
# allData11$Hazardous <- (allData11$Hazardous / allData11$`Days with AQI`) * 100
# 
# 
# allData11 <- allData11 %>% mutate_if(is.numeric, round, 2)
# allData11Gather <- gather(allData11,"AQIP","AValues",5:10)
# #allData11Gather <- allData11Gather[order(allData11Gather$AQIP,-allData11Gather$Values),]
# 
# 
# 
# #Combining AQIP and PollutantP
# combine <- cbind(allData10Gather[,c(1,2,3,20,21)],allData11Gather[,c(14,15)])
# 
# #Combine Map
# combinemap <- merge(combine,mapdata[,c(1:4)], by = c("State","County"))
# 
# #Dataframe for table
# 
# tabledata <- allData7 %>%
#   group_by(State,County, Year, Month) %>%
#   summarize(Good = sum(Category == "Good"),
#             Moderate = sum(Category == "Moderate"),
#             Unhealthy_for_Sensitive_Group = sum(Category == "Unhealthy for Sensitive Groups"),
#             Unhealthy = sum(Category == "Unhealthy"),
#             Very_Unhealthy = sum(Category == "Very Unhealthy"),
#             Hazardous = sum(Category == "Hazardous"))
# 
# USA <-  getData("GADM", country = "usa", level = 2) 


#---------------------------  Trimming Dataframes------------------------------

# allData3Gather <- allData3Gather[,c(1,2,3,14,15)]
# allData3Gather2 <- allData3Gather2[,c(1,2,3,14,15)]
# allData3Gather3 <- allData3Gather3[,c(1,2,3,17,18)]
# allData4 <- allData4[,c(1,2,3,20,21,22,23,24,25)]
# allData4Gather <- allData4Gather[,c(1,2,3,20,21)]
# allData5Gather <- allData5Gather[,c(1,2,3,14,15)]
# mapdata1 <- mapdata1[,c(4,5,23,24)]
# combinemap <- combinemap[,c(1:7)]

# #--------------------------  Saving file--------------------------------------

# save(combinemap, file = "combinemap.Rda")
# save(tabledata, file = "tabledata.Rda")
# save(mapdata, file = "mapdata.Rda")
# save(combine, file = "combine.Rda")
# save(mapdata1, file = "mapdata1.Rda")
# save(hourlydata, file = "hourlydata.Rda")
# save(allData3, file = "allData3.Rda")
# save(allData3Gather, file = "allData3Gather.Rda")
# save(allData3Gather2, file = "allData3Gather2.Rda")
# save(allData3Gather3, file = "allData3Gather3.Rda")
# save(allData4, file = "allData4.Rda")
# save(allData4Gather, file = "allData4Gather.Rda")
# save(allData5, file = "allData5.Rda")
# save(allData5Gather, file = "allData5Gather.Rda")
# save(allData7, file = "allData7.Rda")
# save(allData9, file = "allData9.Rda")
# save(allData9Gather, file = "allData9Gather.Rda")
# save(allData9Gather2, file = "allData9Gather2.Rda")
# save(allData9Gather3, file = "allData9Gather3.Rda")
# save(allData10, file = "allData10.Rda")
# save(allData10Gather, file = "allData10Gather.Rda")
# save(allData11, file = "allData11.Rda")
# save(allData11Gather, file = "allData11Gather.Rda")
# save(counties4, file = "counties4.Rda")
# save(states4, file = "states4.Rda")
# save(USA,file = "USA.Rda")
# save(australiandata,file = "australiandata.Rda")
# save(australianlineGather,file = "australianlineGather.Rda")



#---------------------------------- Saving as feather file-----------------------------------

# write_feather(allData7,"allData7.feather")
# write_feather(combinemap,"combinemap.feather")
# write_feather(allData3,"allData3.feather")
# write_feather(allData3Gather,"allData3Gather.feather")
# write_feather(allData3Gather2,"allData3Gather2.feather")
# write_feather(allData3Gather3,"allData3Gather3.feather")
# write_feather(allData4,"allData4.feather")
# write_feather(allData4Gather,"allData4Gather.feather")
# write_feather(allData5Gather,"allData5Gather.feather")
# write_feather(australianlineGather,"australianlineGather.feather")
# write_feather(hourlydata,"hourlydata.feather")
# write_feather(mapdata1,"mapdata1.feather")
# write_feather(tabledata,"tabledata.feather")


#------------------------------------ Loading as Rda file ---------------------------------------------

# #Load files
# load(file = "allData5.Rda")
# load(file = "allData9.Rda")
# load(file = "allData9Gather.Rda")
# load(file = "allData9Gather2.Rda")
# load(file = "allData9Gather3.Rda")
# load(file = "allData10.Rda")
# load(file = "allData10Gather.Rda")
# load(file = "allData11.Rda")
# load(file = "allData11Gather.Rda")
# load(file = "australiandata.Rda")
# load(file = "mapdata.Rda")
# load(file = "combine.Rda")
# load(file = "combinemap.Rda")
# load(file = "tabledata.Rda")
# load(file = "mapdata1.Rda")
# load(file = "allData5Gather.Rda")
# load(file = "allData7.Rda")
# load(file = "hourlydata.Rda")
# load(file = "allData3.Rda")
# load(file = "allData3Gather.Rda")
# load(file = "allData3Gather2.Rda")
# load(file = "allData3Gather3.Rda")
# load(file = "allData4.Rda")
# load(file = "allData4Gather.Rda")
# load(file = "australianlineGather.Rda")
load(file = "USA.Rda")



#--------------------------------------------- Loading as feather file ------------------

allData7 <- read_feather("allData7.feather")
combinemap <- read_feather("combinemap.feather")
allData3 <- read_feather("allData3.feather")
allData3Gather <- read_feather("allData3Gather.feather")
allData3Gather2 <- read_feather("allData3Gather2.feather")
allData3Gather3 <- read_feather("allData3Gather3.feather")
allData4 <- read_feather("allData4.feather")
allData4Gather <- read_feather("allData4Gather.feather")
allData5Gather <- read_feather("allData5Gather.feather")
australianlineGather <- read_feather("australianlineGather.feather")
hourlydata <- read_feather("hourlydata.feather")
mapdata1 <- read_feather("mapdata1.feather")
tabledata <- read_feather("tabledata.feather")


allData7 <- as.data.frame(allData7)
combinemap <- as.data.frame(combinemap)
allData3 <- as.data.frame(allData3)
allData3Gather <- as.data.frame(allData3Gather)
allData3Gather2 <- as.data.frame(allData3Gather2)
allData3Gather3 <- as.data.frame(allData3Gather3)
allData4 <- as.data.frame(allData4)
allData4Gather <- as.data.frame(allData4Gather)
allData5Gather <- as.data.frame(allData5Gather)
australianlineGather <- as.data.frame(australianlineGather)
hourlydata <- as.data.frame(hourlydata)
mapdata1 <- as.data.frame(mapdata1)
tabledata <- as.data.frame(tabledata)


#----------------------------------------------------------------------------------------------------


# Create the menu items to select the different years, states and counties

years<-c(1980:2018)
counties <- allData3[,2]
states <- unique(allData3[,1])

years1 <-c(1980:2018)
counties1 <- allData3[,2]
states1 <- unique(allData3[,1])

years2 <-c(1980:2018)
counties2 <- allData3[,2]
states2 <- unique(allData3[,1])

years3 <-c(1980:2018)
counties3 <- allData3[,2]
states3 <- unique(allData3[,1])

aqip <- c("Moderate" ,"Good" , "Unhealthy(Sensitive)", "Unhealthy", "Very Unhealthy","Hazardous","Ozone", "SO2", "CO", "NO2", "PM2_5", "PM_10")


counties4 <- allData3[,2]
states4 <- unique(allData3[,1])

aqip2 <- unique(hourlydata$Parameter.Name)
USA2 <- USA
USA1 <- USA


locations <-  c("Auckland Point(Gladstone)", "Memorial Park(Gladstone)", "Newcastle(Lower Hunter)" ,"West Mackay(Mackay)", "Wollongong(Illawarra)")

# Create the shiny dashboard
ui <-fluidPage(
  
  dashboardPage(skin= "red",
    dashboardHeader(title = "Every Breath You Take", titleWidth = 450),
    dashboardSidebar(disable = FALSE, collapsed = FALSE,
                     
                     selectInput("Year", "Select the year to visualize", years, selected = 2018),
                     selectInput("State", "Select the State to visualize", states, selected = "Illinois"),
                     selectInput("County", "Select the County to visualize", counties, selected = "Cook"),
                     
                     
                     
sidebarMenu(
menuItem("About",tabName = "about"),
menuItem("C-AQI Quality",tabName = "aqiquality"),
menuItem("C-Pollutant Types",tabName = "Pollutant"),
menuItem("C- Compare Data of 3 Counties", tabName = "comparedata"),
menuItem("B-AQI Comparision",tabName = "aqicompare"),
menuItem("B-Annual Heat Map",tabName = "comparemap"),
menuItem("A-Compare Hourly Plotly",tabName = "comparehourlyp"),
menuItem("A- Heat Pollutants Map",tabName = "hourlymap"),
menuItem("Grad- Australia Data", tabName= "Australian"))),
    
    dashboardBody(
      tabItems(
        tabItem("about", includeHTML("about.html")),
        tabItem("aqiquality", 
                fluidRow(
                  
                  column(12,
                         fluidRow( 
                           box(
                             width = 18,
                             status = "info",
                             dataTableOutput("AQIQualityTable")
                           ),
                           box(
                             width=15,status = "info", plotlyOutput("AQImaxq")
                           )
                         )
                  ),
                  
                  column(4,
                         fluidRow(
                           box(
                             width = 15,
                             status = "info",
                             plotlyOutput("AQIPiePlot")
                           )
                         )
                  ),
                  
                  column(5,
                         fluidRow(
                           box(
                             width = 18,
                             status = "info",
                             plotlyOutput("AQIBarGraph")
                           )
                         )
                  )
                  
                  
                )
        ),
        tabItem("Pollutant", 
                fluidRow(
                  
                  column(12,
                         fluidRow( 
                           box(
                             width = 14,
                             status = "info",
                             dataTableOutput("PollutantinfoTable")
                           ),
                           box(width = 14, status = "info",dataTableOutput("PollutantTypeTable")),
                           box(width=15,status = "info", plotlyOutput("Pollutantline"))
                         )
                  ),
                  
                  column(4,
                         fluidRow(
                           box(
                             width = 15,
                             status = "info",
                             plotlyOutput("PollutantinfoPiePlot")
                           )
                         )),
                  column(4,fluidRow(
                    box(
                      width = 15,
                      status = "info",
                      plotlyOutput("PollutantinfoBarGraph")
                      
                    ))),
                  column(4,fluidRow(
                    
                    box(title = "Leaflet Map", solidHeader = TRUE, status = "primary", width = 12,
                        leafletOutput("leafmap")
                    )
                    
                  )
                  )
                  
                )
        ),
        
        tabItem("aqicompare", 
                fluidRow(
                  column(12,
                         fluidRow(
                           box(width = 18,status = "info",dataTableOutput("AQIcompareTable")),
                           box(width=15,status = "info", plotlyOutput("AQIcomparemaxq"))
                         )
                  ),
                  column(5,
                         fluidRow(box(width = 18,status = "info",plotlyOutput("AQIcompareBarGraph")))
                  )
                )
        ),
        tabItem("comparemap",
fluidRow(
fluidRow(
box(title = "Heat Map", solidHeader = TRUE, status = "primary", width = 12, leafletOutput("cmap")),
box(selectInput("aqip", "Select the AQI or Pollutant Type to visualize", aqip, selected = "Moderate")),
box(sliderInput("slider1", label = h3("Number of Counties to be displayed"), min = 1, max = 1000, value = 400))
)
)
        ),
        
        tabItem("hourlymap",
fluidRow(
  fluidRow(box(title = "Hourly Map", solidHeader = TRUE, status = "primary", width = 12, leafletOutput("hourlymap"))),
fluidRow(
box(selectInput("aqip2", "Select the AQI or Pollutant Type to visualize", aqip2, selected = "Moderate"),
dateInput("date2","Date: ",min = "2018-01-01", max = "2018-12-31",value=format(as.Date("2018-01-01","%Y-%m-%d",tz="Europe/Stockholm"),"%Y-%m-%d"))),
box(sliderInput("slider2", label = h3("Number of Counties to be displayed"), min = 1, max = 1000, value = 400)))

)
        ),
        
        tabItem("Australian",
                fluidRow(
                  fluidRow(box(width=15,status = "info", plotlyOutput("Australian_daily_line"))),
                  column(3,fluidRow(box(dateInput("date3","From",min = "2018-12-18", max = "2019-03-17",value=format(as.Date("2018-12-18","%Y-%m-%d",tz="Europe/Stockholm"),"%Y-%m-%d"))))),
                  column(3,fluidRow(box(dateInput("date4","To",min = "2018-12-18", max = "2019-03-17",value=format(as.Date("2019-01-18","%Y-%m-%d",tz="Europe/Stockholm"),"%Y-%m-%d"))))),
                  column(3,fluidRow(box(dateInput("date5","On",min = "2018-12-18", max = "2019-03-17",value=format(as.Date("2018-12-18","%Y-%m-%d",tz="Europe/Stockholm"),"%Y-%m-%d"))))),
                  column(3,fluidRow(box(selectInput("Australian_location", "Select the Location", locations, selected = "Memorial Park(Gladstone)")))),
                  column(12,fluidRow(box(width=15,status = "info", plotlyOutput("Australian_hourly_line"))))
                    
                )
        ),
        
        tabItem("comparehourlyp",
fluidRow(
  fluidRow(box(width=15,status = "info", plotlyOutput("Hourlyline1"))),
column(4,fluidRow(box(dateInput("start_date","From",min = "2018-01-01", max = "2018-12-31", value=format(as.Date("2018-01-01","%Y-%m-%d",tz="Europe/Stockholm"),"%Y-%m-%d"))))),
column(4,fluidRow(box(dateInput("end_date","To",min = "2018-01-01", max = "2018-12-31",value=format(as.Date("2018-01-31","%Y-%m-%d",tz="Europe/Stockholm"),"%Y-%m-%d"))))),
column(4,fluidRow(box(dateInput("date1","On",min = "2018-01-01", max = "2018-12-31",value=format(as.Date("2018-01-01","%Y-%m-%d",tz="Europe/Stockholm"),"%Y-%m-%d"))))),
column(12,fluidRow(box(width=15,status = "info", plotlyOutput("Hourlyline2")))))
        ),
        
        
        
        tabItem("comparedata",
                fluidRow(
                  fluidRow(box(width = 4, selectInput("Year1", "Select the year to visualize", years1, selected = 2018, width="120px"),
                               selectInput("State1", "Select the 1st State to visualize", states1, selected = "Illinois", width="120px"),
                               selectInput("County1", "Select the 1st County to visualize", counties1, selected = "Cook", width="120px")),
                           
                           box(width = 4, selectInput("Year2", "Select the year to visualize", years2, selected = 2018, width="120px"),
                               selectInput("State2", "Select the 2nd State to visualize", states2, selected = "Illinois", width="120px"),
                               selectInput("County2", "Select the 2nd County to visualize", counties2, selected = "Cook",width="120px")),
                           
                           box(width = 4, selectInput("Year3", "Select the year to visualize", years3, selected = 2018, width="120px"),
                               selectInput("State3", "Select the 3rd State to visualize", states3, selected = "Illinois", width="120px"),
                               selectInput("County3", "Select the 3rd County to visualize", counties3, selected = "Cook",width="120px"))),
                  
                  column(12,
                         fluidRow(
                           box(
                             width = 15,
                             status = "info",
                             plotlyOutput("Compare_aqiBarGraph")
                           )
                         )),
                  column(12,fluidRow(
                    box(
                      width = 15,
                      status = "info",
                      plotlyOutput("Compare_pollutantBarGraph")
                      
                    ))),
                  
                  column(12,fluidRow(
                    box(
                      width = 15,
                      status = "info",
                      plotlyOutput("Compare_aqiLineGraph")
                      
                    ))),
                  column(12,fluidRow(
                    box(
                      width = 15,
                      status = "info",
                      plotlyOutput("Compare_pollutantLineGraph")
                      
                    )))
                  
                  
                  
                  
                )
        )
      )
    )
    
  )
)

server <- function(input,output,session){
  
  observe({
    
    # Can use character(0) to remove all choices
    if (is.null(input$State))
      input$State <- character(0)
    
    a <- unique(as.character(allData3[allData3$State == input$State,2]))
    
    # Can also set the label and select items
    updateSelectInput(session, "County",
                      choices = sort(a))
  })
  
  observe({
    
    # Can use character(0) to remove all choices
    if (is.null(input$State1))
      input$State1 <- character(0)
    
    b <- unique(as.character(allData3[allData3$State == input$State1,2]))
    
    # Can also set the label and select items
    updateSelectInput(session, "County1",
                      choices = sort(b))
  })
  
  observe({
    
    # Can use character(0) to remove all choices
    if (is.null(input$State2))
      input$State2 <- character(0)
    
    c <- unique(as.character(allData3[allData3$State == input$State2,2]))
    
    # Can also set the label and select items
    updateSelectInput(session, "County2",
                      choices = sort(c))
  })
  
  
  observe({
    
    # Can use character(0) to remove all choices
    if (is.null(input$State3))
      input$State3 <- character(0)
    
    d <- unique(as.character(allData3[allData3$State == input$State3,2]))
    
    # Can also set the label and select items
    updateSelectInput(session, "County3",
                      choices = sort(d))
  })
  
  

  
  # observe({
  #   if (is.null(input$aqip)){
  #     input$aqip <- character(0)
  #   }
  #   if(input$aqip %in% c("Moderate" ,"Good" , "Unhealthy(Sensitive)", "Unhealthy", "Very Unhealthy","Hazardous")){
  #     max_value = nrow(combinemap[combinemap$AQIP == input$aqip & combinemap$Year== input$Year,])
  #   }
  #   else{
  #     max_value = nrow(combinemap[combinemap$PollutantsP == input$aqip &  combinemap$Year== input$Year,])
  #   }
  #   
  #   updateSliderInput(session,"slider1", max = max_value)
  # })
  # 
  # 
  # 
  # observe({
  #   if (is.null(input$aqip2)){
  #     input$aqip2 <- character(0)
  #   }
  #   
  #   max_value = nrow(hourlydata[hourlydata$Parameter.Name == input$aqip2 & hourlydata$Date.GMT == as.Date(input$date2, format = "%Y-%m-%d"),])
  #   
  #   updateSliderInput(session,"slider2", max = max_value)
  # })
  # 
  data1 <- reactive({
    
    df1 <- allData3Gather[(allData3Gather$State == input$State1  & allData3Gather$County == input$County1 & allData3Gather$Year == input$Year1) | 
                            (allData3Gather$State == input$State2  & allData3Gather$County == input$County2 & allData3Gather$Year == input$Year2) | 
                            (allData3Gather$State == input$State3  & allData3Gather$County == input$County3 & allData3Gather$Year == input$Year3) ,c(1,2,4,5) ]
    
    df1 <- df1[order(df1$County),]
    
    df1$'County-State' <- paste0(df1$County,"(",df1$State,")")
    
    df1
  })
  
  
  
  
  data2 <- reactive({
    
    df2 <- allData3Gather2[(allData3Gather2$State == input$State1  & allData3Gather2$County == input$County1 & allData3Gather2$Year == input$Year1) | 
                             (allData3Gather2$State == input$State2  & allData3Gather2$County == input$County2 & allData3Gather2$Year == input$Year2) | 
                             (allData3Gather2$State == input$State3  & allData3Gather2$County == input$County3 & allData3Gather2$Year == input$Year3) ,c(1,2,4,5) ]
    
    df2 <- df2[order(df2$County),]
    
    df2$'County-State' <- paste0(df2$County,"(",df2$State,")")
    
    df2
  })
  
  data3 <- reactive({
    
    df3 <- allData5Gather[(allData5Gather$State == input$State1  & allData5Gather$County == input$County1 ) | 
                            (allData5Gather$State == input$State2  & allData5Gather$County == input$County2) | 
                            (allData5Gather$State == input$State3  & allData5Gather$County == input$County3) ,c(1,2,3,4,5)]
    
    df3 <- df3[order(df3$County),]
    
    df3$'County-State' <- paste0(df3$County,"(",df3$State,")")
    
    df3
  })
  
  data4 <- reactive({
    
    df4 <- allData4Gather[(allData4Gather$State == input$State1  & allData4Gather$County == input$County1) | 
                            (allData4Gather$State == input$State2  & allData4Gather$County == input$County2) | 
                            (allData4Gather$State == input$State3  & allData4Gather$County == input$County3) ,c(1,2,3,4,5)]
    
    df4 <- df4[order(df4$County),]
    
    df4$'County-State' <- paste0(df4$County,"(",df4$State,")")
    
    df4
  })
  
  
  data8 <- reactive({
    df8 <- australianlineGather[australianlineGather$location == input$Australian_location & australianlineGather$Date == as.Date(input$date5, format = "%Y-%m-%d"),]
    
    df8
  })
  
  data9 <- reactive({ 
    
    df9 <- allData3[allData3$County == input$County & allData3$State == input$State & allData3$Year == input$Year ,1:11]
    df9
    
  })
  
  output$AQIQualityTable <-
    renderDataTable(data9(), options = list(columnDefs = list(list(
      targets = 1:11 , className = "right"
    )), pageLength = 5))
  
  
  data13 <- reactive ({
    
    df13 <- allData3Gather[allData3Gather$State == input$State  & allData3Gather$County == input$County & allData3Gather$Year == input$Year, ]
    
    df13 <- df13%>%
      arrange(desc(Days))
    
    df13
    
  })
  
  
  output$AQIBarGraph  <- renderPlotly ({
    
    plot_ly(data = data13(), x = data13()$`AQI Parameters`, y = data13()$Days,
            color = data13()$`AQI Parameters` , text = data13()$Days, textposition = 'outside', 
            type = 'bar', colors = c("#2E4272","#3C8D2F","#E5E540","#AAA238","#C6590F","#C62B0F")) %>%
      layout(title = paste("AQI types for", input$Year))
    
  })
  
  output$AQIPiePlot <- 
    renderPlotly({
      colors <- c("#3C8D2F","#2E4272","#E5E540","#AAA238","#C6590F","#C62B0F")
      plot_ly( data13(), labels = ~`AQI Parameters`, values = ~Days, type = 'pie',
              textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              marker = list(colors = colors,
                            line = list(color = '#FFFFFF', width = 1)),        showlegend = FALSE) %>%
        layout(title = paste('AQI Quality for year ', input$Year),
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    })
  
  
  data14 <- reactive ({
    
    df14 <- allData3Gather3[allData3Gather3$State == input$State & allData3Gather3$County == input$County,]
    
    df14
    
    
  })
  
  
  
  output$AQImaxq <- renderPlotly({ 
    
    plot_ly(data = data14(), 
            x = data14()$Year,
            y = data14()$Values, 
            color = data14()$Maxq,
            colors = c("#970D09","#172457","#077807"),type="scatter",mode="lines+markers") %>%
      layout(title = "AQI over the years")
    
  })
  
  
  
  data15 <- reactive ({
    
    df15 <- allData3[allData3$County == input$County & allData3$State == input$State & allData3$Year == input$Year ,c(1,2,3,14,15,16,17,18,19) ]
    df15
    
  })
  
  
  output$PollutantinfoTable <-
    renderDataTable(data15(), 
                    options = list(columnDefs = list(list(
                      targets = 1:9 , className = "right")), pageLength = 5))
  
  
  
  output$PollutantTypeTable <-
    renderDataTable(allData4[allData4$County == input$County & allData4$State == input$State , ], 
                    options = list(columnDefs = list(list(targets = 1:9 , className = "right")), pageLength = 5))
  
  
  
  
  data16 <- reactive ({
    
    df16 <- allData3Gather2[allData3Gather2$State == input$State  & allData3Gather2$County == input$County & allData3Gather2$Year == input$Year, ]
    
    df16 %>%
      arrange(desc(Days))
    
    df16
    
  })
  
  output$PollutantinfoBarGraph  <- renderPlotly ({
    
    plot_ly(data = data16(), x = data16()$Pollutants, y = data16()$Days,
            color = data16()$Pollutants , textposition = 'outside', text = data16()$Days,
            type = 'bar', colors = c("#801515","#974909","#77074B","#482E74","#2F3F73","#729C34")) %>%
      layout(title = paste("Pollutant types for", input$Year))
    
    
  })
  
  output$PollutantinfoPiePlot <- 
    renderPlotly({
      colors <- c("#801515","#974909","#77074B","#482E74","#2F3F73","#729C34")
      plot_ly(data16(), labels = ~Pollutants , values = ~Days, type = 'pie',
              textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              marker = list(colors = colors,
                            line = list(color = '#FFFFFF', width = 1)),        showlegend = FALSE) %>%
        layout(title = paste('Pollutants for year ', input$Year) ,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    })
  
  data17 <- reactive({
    
    df17 <- allData4Gather[allData4Gather$State == input$State & allData4Gather$County == input$County,]
    
    df17
    
    
  })
  
  output$Pollutantline <- renderPlotly({ 
    
    plot_ly(data = data17(), 
            x = data17()$Year,
            y = data17()$Values, 
            color = data17()$PollutantsP,
            colors = c("#801515","#974909","#77074B","#482E74","#2F3F73","#729C34"),type="scatter",mode="lines+markers") %>%
      layout(title = "Pollutants % over the years")
    
  })
  
  output$leafmap <- renderLeaflet({
    
    map <- leaflet()
    map <- addTiles(map)
    map <- setView(map, lng = mapdata1[mapdata1$County.Name == input$County & mapdata1$State.Name == input$State,2][1], lat = mapdata1[mapdata1$County.Name == input$County & mapdata1$State.Name == input$State,1][1], zoom = 2)
    map <- addCircleMarkers(map, lng = mapdata1[mapdata1$County.Name == input$County & mapdata1$State.Name == input$State,2][1], lat = mapdata1[mapdata1$County.Name == input$County & mapdata1$State.Name == input$State,1][1], popup = input$County)
    
    map
  })
  
  output$Compare_aqiBarGraph <- renderPlotly({
    
    
    ggplot(data1(), aes(y=Days, x=`AQI Parameters`, color=`AQI Parameters`, fill=`AQI Parameters`)) + 
      geom_bar( stat="identity") +  facet_wrap(~`County-State`) + labs( y = "No of Days",title = "AQI types")+
      theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),legend.title =element_blank())
    
  })
  
  output$Compare_pollutantBarGraph <- renderPlotly({
    
    ggplot(data2(), aes(y=Days, x=Pollutants, color=Pollutants, fill=Pollutants)) + 
      geom_bar( stat="identity") +  facet_wrap(~`County-State`) + labs(y = "No of Days",title = "Pollutant types")+
      theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),legend.title =element_blank())
    
    
  })
  
  output$Compare_aqiLineGraph <- renderPlotly({
    
    ggplot(data3(), aes(y=Values, x=Year, color=AQIP, group =AQIP)) + 
      geom_line() +  facet_wrap(~`County-State`) + labs( y = "No of Days",title = paste("AQI types % over the years"))+
      theme(axis.title.x=element_blank(),legend.title =element_blank())
  })
  
  output$Compare_pollutantLineGraph <- renderPlotly({
    
    
    ggplot(data4(), aes(y=Values, x=Year, color=PollutantsP, group =PollutantsP )) + 
      geom_line() +  facet_wrap(~`County-State`) + labs(y = "No of Days",title = paste("Pollutant types % over the years"))+
      theme(axis.title.x=element_blank(),legend.title =element_blank())
    
    
  })
  output$AQIcompareTable <-
    renderDataTable(tabledata[tabledata$County == input$County & tabledata$State == input$State & tabledata$Year == input$Year ,4:10])
  
  
  
  
  data12 <- reactive({
    df12 <- allData7[allData7$State == input$State & allData7$County == input$County & allData7$Year == input$Year,]
    
    df12
  })
  
  
  output$AQIcompareBarGraph  <- renderPlotly ({
    
    xlab = list(categoryorder = "array", categoryarray = c("January", "February", "March" ,"April","May" ,"June" ,"July" , "August" ,"September" ,"October" ,"November", "December" ))
    ylab = list(title = "", showticklabels = FALSE)
    
    plot_ly(data = data12(), 
            x = data12()$Month,
            y = data12()$Category, 
            color = data12()$Category,
            type="bar") %>%
      layout(title = paste("AQI types for", input$Year), barmode = "stack", xaxis = xlab, yaxis = ylab) 
  })
  
  
  output$AQIcomparemaxq <- renderPlotly({ 
    
    plot_ly(data = data12(), 
            x = data12()$Date,
            y = data12()$AQI, 
            color = data12()$`Defining Parameter` ,
            type="scatter",mode="lines+markers") %>%
      layout(title = paste("Pollutants over the year" , input$Year))
    
  })
  
  
  data6 <- reactive({
    
    df6 <- combinemap %>%
    filter(combinemap$Year == input$Year & (combinemap$AQIP == input$aqip | combinemap$PollutantsP == input$aqip))
    
    if(input$aqip %in% c("Moderate" ,"Good" , "Unhealthy(Sensitive)", "Unhealthy", "Very Unhealthy","Hazardous")){
      df6$value <- df6$AValues
      df6 <- df6%>%
        arrange(desc(AValues))
    }else{
      df6$value <- df6$PValues
      df6 <- df6%>%
        arrange(desc(PValues))
    }
    
    df6 <- as.data.frame(df6)
    
    # Get USA polygon data
    
    temp <- merge(USA1, df6,
                  by.x = c("NAME_1", "NAME_2"), by.y = c("State", "County"), all.x = TRUE)
    
    temp <- head(temp,input$slider1)
    temp
    
    
  })
  
  
  
  output$cmap <- renderLeaflet({
    
    
    leaflet(data = data6()) %>% 
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      setView(lat = 39.8283, lng = -98.5795, zoom = 4) })
  
  observe({
      
    mypal <- colorNumeric(palette = "viridis", domain = data6()$value, na.color = "white", reverse = TRUE)
    
      leafletProxy("cmap", data = data6()) %>%
      clearShapes() %>%
      addPolygons(data = USA1, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.3,
                  fillColor = ~mypal(data6()$value),
                  popup = paste("State: ", data6()$NAME_1, "<br>","County: ", data6()$NAME_2, "<br>",
                                "Value: ", data6()$value, "<br>")) %>%
      addLegend(position = "bottomleft", pal = mypal, values = data6()$value,title = "Value",opacity = 1)
    
    
  })
  
  data11 <- reactive({
    
    df11 <- hourlydata[hourlydata$State.Name  == input$State & hourlydata$County.Name == input$County & hourlydata$Date.GMT >= as.Date(input$start_date, format = "%Y-%m-%d") & hourlydata$Date.GMT <= as.Date(input$end_date, format = "%Y-%m-%d"),]
    df11
      
  })
  
  output$Hourlyline1 <- renderPlotly({ 
    
    plot_ly(data = data11(), 
            x = data11()$Date.GMT,
            y = data11()$Sample.Measurement, 
            color = data11()$Parameter.Name,
            type="scatter",mode="lines+markers") %>%
      layout(title = paste("Hourly data between",as.Date(input$start_date, format = "%Y-%m-%d"), "and",as.Date(input$end_date, format = "%Y-%m-%d")))   
    
    
  })
  
  data10 <- reactive({
    
    hdata = hourlydata[hourlydata$State.Name  == input$State & hourlydata$County.Name == input$County & hourlydata$Date.GMT == as.Date(input$date1, format = "%Y-%m-%d"),]
    hdata
    
    
  })
  
  # output$Hourlyline2 <- renderPlotly({ 
  #   
  # 
  #   ggplot(data10(), aes(x=Time.GMT,y = Sample.Measurement, color=Parameter.Name, group =Parameter.Name), show.legend = FALSE) + 
  #     geom_line(show.legend = FALSE) +  facet_wrap(~`Parameter.Name`, ncol = 3 ) + labs(y = "Values",title = "Line Chart for Hourly Data")+
  #     theme(axis.title.x=element_blank(),legend.title =element_blank(), legend.position = "none")
  #   
  # })
  
  
  output$Hourlyline2 <- renderPlotly({ 
    
    
    plot_ly(data = data10(), 
            x = data10()$Time.GMT,
            y = data10()$Sample.Measurement, 
            color = data10()$Parameter.Name,
            type="scatter",mode="lines+markers") %>%
      layout(title = paste("Hourly data on", as.Date(input$date1, format = "%Y-%m-%d")))
    
    
  })
  
  output$Australian_hourly_line <- renderPlotly({ 
    
    plot_ly(data = data8(), 
            x = data8()$Time,
            y = data8()$PValues, 
            color = data8()$PollutantsP,type="scatter",mode="lines+markers") %>%
      layout(title = paste("Pollutants % on ", as.Date(input$date5, format = "%Y-%m-%d"), " for ", input$Australian_location))
  })
  
  
  
  data20 <- reactive({
    df20 <- australianlineGather[australianlineGather$location == input$Australian_location & australianlineGather$Date >= as.Date(input$date3, format = "%Y-%m-%d")  & australianlineGather$Date <= as.Date(input$date4, format = "%Y-%m-%d") ,]
    
    
    df20
  })
  
  output$Australian_daily_line <- renderPlotly({ 
    plot_ly(data = data20(), 
            x = data20()$Date,
            y = data20()$PValues, 
            color = data20()$PollutantsP,type="scatter",mode="lines+markers") %>%
      layout(title = paste("Pollutants % between ", as.Date(input$date3, format = "%Y-%m-%d"), " and ", as.Date(input$date4, format = "%Y-%m-%d")," for ", input$Australian_location ))
    
    
  })
  
  
  data7 <- reactive({
    
    df7 <- hourlydata %>%
    filter(hourlydata$Date.GMT == as.Date(input$date2, format = "%Y-%m-%d") & hourlydata$Parameter.Name== input$aqip2)
    
    df7 <- df7%>%
      arrange(desc(Sample.Measurement))
    
    df7 <- as.data.frame(df7)
    
    # Get USA polygon data
    
    temp2 <- merge(USA2, df7,by.x = c("NAME_1", "NAME_2"), by.y = c("State.Name", "County.Name"),all.x = TRUE,  duplicateGeoms = TRUE)
    
    temp2 <- head(temp2,input$slider2)
    temp2
    
  })
  
  

  output$hourlymap <- renderLeaflet({
  
  
  
    leaflet(data = data7()) %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      setView(lat = 39.8283, lng = -98.5795, zoom = 4)
  
  
  })
  
  observe({
    mypal <- colorNumeric(palette = "viridis", domain = data7()$Sample.Measurement, na.color = "white", reverse = TRUE)
  
    leafletProxy("hourlymap", data = data7()) %>%
      clearShapes() %>%
      addPolygons(data = USA2, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.3,fillColor = ~mypal(data7()$Sample.Measurement),
                  popup = paste("State: ", data7()$NAME_1, "<br>","County: ", data7()$NAME_2, "<br>","Value: ", data7()$Sample.Measurement, "<br>")) %>%
      addLegend(position = "bottomleft", pal = mypal, values = data7()$Sample.Measurement,title = "Value",opacity = 1)


  
  })

  

  
  session$allowReconnect(TRUE)
  
}
shinyApp(server = server, ui = ui)
