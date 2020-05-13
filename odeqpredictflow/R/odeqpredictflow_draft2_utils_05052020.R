



# Umpqua Test sites:
#       Little River at Peel, OR,        14318000     43.252618,-123.026172
#       South Umpqua Near Brockway, OR,  14312000     43.133172,-123.398405
#       North Umpqua at Winchester, OR,  14319500     43.271,   -123.412299
#       Umpqua River Near Elkton, OR,    14321000     43.58595, -123.555373


#       West fork cow creek nr Glendale  14309500    	42.804004	-123.610907










Source.Station.From.List <- function(station_in){
  station_detail <-odeqpredict_stations[odeqpredict_stations$MLocID==as.character(station_in),]
  return(list(station_detail$snapLat[1], station_detail$snapLong[1], station_detail$near_USGS[1]))
  #[[1]] is snapped target lat, [[2]] is snapped target lon, [[3]] is nearest USGS station
}


Source.Station.From.Coords <- function(lat_in,lon_in){

  point_df <- data.frame("Lat" = lat_in, "Lon" = lon_in)
  target_Point <- sf::st_as_sf(point_df, coords = c("Lon", "Lat"), crs = 4269, agr = "constant")
  join_point <- sf::st_join(target_Point, odeqpredict_polygons)

  return(list(lat_in, lon_in, join_point$site_no[1]))
  #[[1]] is target lat, [[2]] is target lon, [[3]] is nearest USGS station
}

Stats.Series <- function(target_Lat,target_Lon,Source_Station,Start_Date,End_Date){
  print("Retrieving target site stream statistics...")
  # Watershed is delineated and workspace created
  Step1_Watershed_Part1 <- httr::GET(paste0("https://streamstats.usgs.gov/streamstatsservices/watershed.geojson?rcode=OR&xlocation=",
                                     target_Lon,
                                     "&ylocation=",
                                     target_Lat,
                                     "&crs=4326&includeparameters=true&includeflowtypes=false&includefeatures=false&simplify=true"))

  try(Step1_Watershed_Part2 <- jsonlite::fromJSON(httr::content(Step1_Watershed_Part1, "text", encoding = "UTF-8")), silent = TRUE)
  attempts<-1
  if(!exists("Step1_Watershed_Part2")){
    while (!exists("Step1_Watershed_Part2")) {
      Sys.sleep(10)
      try(Step1_Watershed_Part1 <- httr::GET(paste0("https://streamstats.usgs.gov/streamstatsservices/watershed.geojson?rcode=OR&xlocation=",
                                              target_Lon,
                                              "&ylocation=",
                                              target_Lat,
                                              "&crs=4326&includeparameters=true&includeflowtypes=false&includefeatures=false&simplify=true")), silent = TRUE)
      try(Step1_Watershed_Part2 <- jsonlite::fromJSON(httr::content(Step1_Watershed_Part1, "text", encoding = "UTF-8")), silent = TRUE)
      attempts<-attempts+1
      if(attempts>3){
        stop("Failed to initialize watershed, check StreamStats and try again.")
      }
    }


  }

  # Get WorkspaceID to access flow statistics for target location
  workspace <- Step1_Watershed_Part2$workspaceID

  # Retreive flow statistics
  try(Step2_FlowStatistics <- httr::GET(paste0("https://streamstats.usgs.gov/streamstatsservices/flowstatistics.json?rcode=OR&workspaceID=",
                                          workspace,
                                          "&includeflowtypes=FDS")), silent = TRUE
  )
  # Check if it failed, retry up to three times before giving up
  attempts<-1

  if(!exists("Step2_FlowStatistics")){
    # There are two production servers, try to make statistics request directly to server
    prodweb <- Step1_Watershed_Part1$headers$`usgswim-hostname`
    while(!exists("Step2_FlowStatistics")){
      Sys.sleep(10)
      try(Step2_FlowStatistics <- httr::GET(paste0("https://",
                                              prodweb,
                                              ".streamstats.usgs.gov/streamstatsservices/flowstatistics.json?rcode=OR&workspaceID=",
                                              workspace,
                                              "&includeflowtypes=FDS")), silent = TRUE
      )
      attempts <- attempts+1
      if(attempts>3){
        stop("Failed to initialize watershed, check StreamStats and try again.")
        }
    }
  }

  # Extract flow duration series from retreived data
  try(Full_FlowStatistics <- jsonlite::fromJSON(httr::content(Step2_FlowStatistics, "text", encoding = "UTF-8"))[[3]][[1]][[6]][[1]], silent = TRUE)

  attempts<-1

  if(!exists("Full_FlowStatistics")){
    # There are two production servers, try to make statistics request directly to server
    prodweb <- Step1_Watershed_Part1$headers$`usgswim-hostname`
    while(!exists("Full_FlowStatistics")){
      Sys.sleep(10)
      try(Step2_FlowStatistics <- httr::GET(paste0("https://",
                                             prodweb,
                                             ".streamstats.usgs.gov/streamstatsservices/flowstatistics.json?rcode=OR&workspaceID=",
                                             workspace,
                                             "&includeflowtypes=FDS")), silent = TRUE
      )
      try(Full_FlowStatistics <- jsonlite::fromJSON(httr::content(Step2_FlowStatistics, "text", encoding = "UTF-8"))[[3]][[1]][[6]][[1]], silent = TRUE)
      attempts <- attempts+1
      if(attempts>3){stop("Failed to retrieve flow statistics after delineating watershed, check StreamStats and try again.")}
    }
  }

  # Check if any errors were reported by the StreamStats API
  Number_of_Errors <- sum(lengths(Full_FlowStatistics$Errors))
  if(Number_of_Errors>0){
    warning("Errors reported for target station statistics by StreamStats API")
  }

  # Get just the flow and recurrence Duration values
  Simple_FlowStatistics <- Full_FlowStatistics[c("code","Value")]
  Simple_FlowStatistics$Duration <- sapply(Simple_FlowStatistics$code, function(x) as.numeric(substr(x,2,nchar(x))))



  print("Retrieving source site stream flows...")
  # Get mean daily flow timeseries from source station via USGS NWIS dataRetrieval package
  Source_Station_Mean_Daily <- dataRetrieval::readNWISdv(Source_Station, parameterCd = "00060", startDate = Start_Date, endDate = End_Date)

  if(nrow(Source_Station_Mean_Daily)==0){
    stop("Failed to retrieve mean daily flow record from source USGS gage. Check data availability.")
  }

  print("Retrieving source site stream statistics...")
  # Get flow duration series for station from StreamStats gage page (not accessible through API, scraped from html)
  # First, read gage page
  Gage_Page <- xml2::read_html(paste0("https://streamstatsags.cr.usgs.gov/gagepages/html/",
                                Source_Station,
                                ".htm"))

  # Then isolate tables from page
  Gage_Page_tables <- rvest::html_table(rvest::html_nodes(Gage_Page, "table"),fill = TRUE)

  # Find the table with flow statistics
  for (i in 1:length(Gage_Page_tables)){
    if(Gage_Page_tables[[i]][3,1] == "Statistic Name"){
      stats_table <- i
      break
    }
  }

  # In the flow statistics table, find the rows with the flow duration series
  # First, find the flow duration series header
  for (i in 1:nrow(Gage_Page_tables[[stats_table]])){
    if(Gage_Page_tables[[stats_table]][[1]][i] == "Flow-Duration Statistics"){
      FDS_row <- i
      break
    }
  }

  # Next, find the end of the flow duration series by locating the next header
  for (i in FDS_row+1:nrow(Gage_Page_tables[[stats_table]])){
    if(Gage_Page_tables[[stats_table]][[1]][i] == Gage_Page_tables[[stats_table]][[2]][i]){
      next_header <- i
      break
    }
  }

  # Finally, extract the flow duration series to a dataframe
  Source_Station_Flow_Statistics <- data.frame(
    "DurationTXT"=Gage_Page_tables[[stats_table]][[1]][(FDS_row+1):(next_header-1)],
    "Flow"=as.numeric(Gage_Page_tables[[stats_table]][[2]][(FDS_row+1):(next_header-1)]))

  # Convert duration column to numeric, txt to character
  Source_Station_Flow_Statistics$Duration <- as.numeric(sub("_Percent_Duration","", Source_Station_Flow_Statistics$DurationTXT))
  Source_Station_Flow_Statistics$DurationTXT <- as.character(Source_Station_Flow_Statistics$DurationTXT)


  # Use the relative slopes to extrapolate 0 and 100% flows
  sd15 <- (Source_Station_Flow_Statistics$Flow[1]-Source_Station_Flow_Statistics$Flow[4])/4
  sd510 <- (Source_Station_Flow_Statistics$Flow[4]-Source_Station_Flow_Statistics$Flow[5])/5
  ratiosd1 <- sd15/sd510

  td510 <- (Simple_FlowStatistics$Value[1]-Simple_FlowStatistics$Value[2])/5
  D0x <- Simple_FlowStatistics$Value[1]+td510*ratiosd1*5

  sd5095 <- (Source_Station_Flow_Statistics$Flow[13]-Source_Station_Flow_Statistics$Flow[22])/45
  sd9599 <- (Source_Station_Flow_Statistics$Flow[22]-Source_Station_Flow_Statistics$Flow[25])/4
  ratiosd100 <- sd9599/sd5095

  td5095 <- (Simple_FlowStatistics$Value[4]-Simple_FlowStatistics$Value[5])/45
  D100x <- Simple_FlowStatistics$Value[5]-td5095*ratiosd100*5

  Simple_FlowStatistics[6,] <- list("D0x",D0x,0)
  Simple_FlowStatistics[7,] <- list("D100x",D100x,100)

  # Establish interpolation function for target
  TargetDurationToFlow <- approxfun(Simple_FlowStatistics$Duration,Simple_FlowStatistics$Value)


  #### also correct max out at 1/99 for source - just do extension of slope on last step last percent
  # # Extrapolate 0 and 100 % duration flows
  sD0x <- 2*Source_Station_Flow_Statistics$Flow[1]-Source_Station_Flow_Statistics$Flow[2]
  sD100x <- 2*Source_Station_Flow_Statistics$Flow[25]-Source_Station_Flow_Statistics$Flow[24]
  Source_Station_Flow_Statistics[26,] <- list("0_percent_extrapolated",sD0x,0)
  Source_Station_Flow_Statistics[27,] <- list("100_percent_extrapolated",sD100x,100)


  # Use this function to convert source flows to durations
  SourceFlowToDuration <- approxfun(Source_Station_Flow_Statistics$Flow,Source_Station_Flow_Statistics$Duration)


  print("Building target site stream flows...")
  # Start building target timeseries, first by filling in dates and initializing flow column
  Target_Station_Mean_Daily <- data.frame("Date" = Source_Station_Mean_Daily$Date)
  Target_Station_Mean_Daily$Flow <- 0
  Target_Station_Mean_Daily$Duration <- 0

  # Iterate through dates to build target timeseries
  for (i in 1:nrow(Target_Station_Mean_Daily)){
    #if flow is less than 100% exceedance flow, use 100% value
    if(Source_Station_Mean_Daily$X_00060_00003[i]<min(Source_Station_Flow_Statistics$Flow)){
      translate_duration <- 100
    } else {
      #if flow is greater than 0% exceedance flow, use 0%
      if(Source_Station_Mean_Daily$X_00060_00003[i]>max(Source_Station_Flow_Statistics$Flow)){
        translate_duration <- 0
      } else {
        #flow is in range, just use linear interpolation
        translate_duration <- SourceFlowToDuration(Source_Station_Mean_Daily$X_00060_00003[i])
      }
    }
    Target_Station_Mean_Daily$Duration[i] <- translate_duration
    Target_Station_Mean_Daily$Flow[i] <- max(c(0,TargetDurationToFlow(translate_duration)))
  }

  colnames(Target_Station_Mean_Daily) <- c("Date","PredictedFlow(cfs)","Duration(%)")

  details <- data.frame("Detail" = "", "Value" = "", stringsAsFactors = FALSE)
  details[1,] <- list("Target Latitude",target_Lat)
  details[2,] <- list("Target Longitude",target_Lon)
  details[3,] <- list("Nearest Reference Gage",Source_Station)
  details[4,] <- list("Drainage Area (sq. mi.)",Step1_Watershed_Part2[["parameters"]][["value"]][3])



  print("Complete!")

  return(list(Target_Station_Mean_Daily,details))
}

Stats.Vector <- function(target_Lat,target_Lon,Source_Station,Date_Vector){
  print("Retrieving target site stream statistics...")
  # Watershed is delineated and workspace created
  Step1_Watershed_Part1 <- httr::GET(paste0("https://streamstats.usgs.gov/streamstatsservices/watershed.geojson?rcode=OR&xlocation=",
                                            target_Lon,
                                            "&ylocation=",
                                            target_Lat,
                                            "&crs=4326&includeparameters=true&includeflowtypes=false&includefeatures=false&simplify=true"))

  try(Step1_Watershed_Part2 <- jsonlite::fromJSON(httr::content(Step1_Watershed_Part1, "text", encoding = "UTF-8")), silent = TRUE)
  attempts<-1
  if(!exists("Step1_Watershed_Part2")){
    while (!exists("Step1_Watershed_Part2")) {
      Sys.sleep(10)
      try(Step1_Watershed_Part1 <- httr::GET(paste0("https://streamstats.usgs.gov/streamstatsservices/watershed.geojson?rcode=OR&xlocation=",
                                                    target_Lon,
                                                    "&ylocation=",
                                                    target_Lat,
                                                    "&crs=4326&includeparameters=true&includeflowtypes=false&includefeatures=false&simplify=true")), silent = TRUE)
      try(Step1_Watershed_Part2 <- jsonlite::fromJSON(httr::content(Step1_Watershed_Part1, "text", encoding = "UTF-8")), silent = TRUE)
      attempts<-attempts+1
      if(attempts>3){
        stop("Failed to initialize watershed, check StreamStats and try again.")
      }
    }


  }

  # Get WorkspaceID to access flow statistics for target location
  workspace <- Step1_Watershed_Part2$workspaceID

  # Retreive flow statistics
  try(Step2_FlowStatistics <- httr::GET(paste0("https://streamstats.usgs.gov/streamstatsservices/flowstatistics.json?rcode=OR&workspaceID=",
                                               workspace,
                                               "&includeflowtypes=FDS")), silent = TRUE
  )
  # Check if it failed, retry up to three times before giving up
  attempts<-1

  if(!exists("Step2_FlowStatistics")){
    # There are two production servers, try to make statistics request directly to server
    prodweb <- Step1_Watershed_Part1$headers$`usgswim-hostname`
    while(!exists("Step2_FlowStatistics")){
      Sys.sleep(10)
      try(Step2_FlowStatistics <- httr::GET(paste0("https://",
                                                   prodweb,
                                                   ".streamstats.usgs.gov/streamstatsservices/flowstatistics.json?rcode=OR&workspaceID=",
                                                   workspace,
                                                   "&includeflowtypes=FDS")), silent = TRUE
      )
      attempts <- attempts+1
      if(attempts>3){stop("Failed to retrieve flow statistics after delineating watershed, check StreamStats and try again.")}
    }
  }

  # Extract flow duration series from retreived data
  try(Full_FlowStatistics <- jsonlite::fromJSON(httr::content(Step2_FlowStatistics, "text", encoding = "UTF-8"))[[3]][[1]][[6]][[1]], silent = TRUE)

  attempts<-1

  if(!exists("Full_FlowStatistics")){
    # There are two production servers, try to make statistics request directly to server
    prodweb <- Step1_Watershed_Part1$headers$`usgswim-hostname`
    while(!exists("Full_FlowStatistics")){
      Sys.sleep(10)
      try(Step2_FlowStatistics <- httr::GET(paste0("https://",
                                                   prodweb,
                                                   ".streamstats.usgs.gov/streamstatsservices/flowstatistics.json?rcode=OR&workspaceID=",
                                                   workspace,
                                                   "&includeflowtypes=FDS")), silent = TRUE
      )
      try(Full_FlowStatistics <- jsonlite::fromJSON(httr::content(Step2_FlowStatistics, "text", encoding = "UTF-8"))[[3]][[1]][[6]][[1]])
      attempts <- attempts+1
      if(attempts>3){stop("Failed to retrieve flow statistics after delineating watershed, check StreamStats and try again.")}
    }
  }

  # Check if any errors were reported by the StreamStats API
  Number_of_Errors <- sum(lengths(Full_FlowStatistics$Errors))
  if(Number_of_Errors>0){
    warning("Errors reported for target station statistics by StreamStats API")
  }

  # Get just the flow and recurrence Duration values
  Simple_FlowStatistics <- Full_FlowStatistics[c("code","Value")]
  Simple_FlowStatistics$Duration <- sapply(Simple_FlowStatistics$code, function(x) as.numeric(substr(x,2,nchar(x))))



  print("Retrieving source site stream flows...")
  # Get mean daily flow timeseries from source station via USGS NWIS dataRetrieval package
  Date_Vector <- as.Date(Date_Vector, format = "%Y-%m-%d")

  Source_Station_Mean_Daily <- dataRetrieval::readNWISdv(Source_Station, parameterCd = "00060", startDate = min(Date_Vector), endDate = max(Date_Vector))

  if(nrow(Source_Station_Mean_Daily)==0){
    stop("Failed to retrieve mean daily flow record from source USGS gage. Check data availability.")
  }
  Source_Station_Mean_Daily$Date <- as.Date(Source_Station_Mean_Daily$Date, format = "%Y-%m-%d")

  print("Retrieving source site stream statistics...")
  # Get flow duration series for station from StreamStats gage page (not accessible through API, scraped from html)
  # First, read gage page
  Gage_Page <- xml2::read_html(paste0("https://streamstatsags.cr.usgs.gov/gagepages/html/",
                                      Source_Station,
                                      ".htm"))

  # Then isolate tables from page
  Gage_Page_tables <- rvest::html_table(rvest::html_nodes(Gage_Page, "table"),fill = TRUE)

  # Find the table with flow statistics
  for (i in 1:length(Gage_Page_tables)){
    if(Gage_Page_tables[[i]][3,1] == "Statistic Name"){
      stats_table <- i
      break
    }
  }

  # In the flow statistics table, find the rows with the flow duration series
  # First, find the flow duration series header
  for (i in 1:nrow(Gage_Page_tables[[stats_table]])){
    if(Gage_Page_tables[[stats_table]][[1]][i] == "Flow-Duration Statistics"){
      FDS_row <- i
      break
    }
  }

  # Next, find the end of the flow duration series by locating the next header
  for (i in FDS_row+1:nrow(Gage_Page_tables[[stats_table]])){
    if(Gage_Page_tables[[stats_table]][[1]][i] == Gage_Page_tables[[stats_table]][[2]][i]){
      next_header <- i
      break
    }
  }

  # Finally, extract the flow duration series to a dataframe
  Source_Station_Flow_Statistics <- data.frame(
    "DurationTXT"=Gage_Page_tables[[stats_table]][[1]][(FDS_row+1):(next_header-1)],
    "Flow"=as.numeric(Gage_Page_tables[[stats_table]][[2]][(FDS_row+1):(next_header-1)]))

  # Convert duration column to numeric, txt to character
  Source_Station_Flow_Statistics$Duration <- as.numeric(sub("_Percent_Duration","", Source_Station_Flow_Statistics$DurationTXT))
  Source_Station_Flow_Statistics$DurationTXT <- as.character(Source_Station_Flow_Statistics$DurationTXT)


  # Use the relative slopes to extrapolate 0 and 100% flows
  sd15 <- (Source_Station_Flow_Statistics$Flow[1]-Source_Station_Flow_Statistics$Flow[4])/4
  sd510 <- (Source_Station_Flow_Statistics$Flow[4]-Source_Station_Flow_Statistics$Flow[5])/5
  ratiosd1 <- sd15/sd510

  td510 <- (Simple_FlowStatistics$Value[1]-Simple_FlowStatistics$Value[2])/5
  D0x <- Simple_FlowStatistics$Value[1]+td510*ratiosd1*5

  sd5095 <- (Source_Station_Flow_Statistics$Flow[13]-Source_Station_Flow_Statistics$Flow[22])/45
  sd9599 <- (Source_Station_Flow_Statistics$Flow[22]-Source_Station_Flow_Statistics$Flow[25])/4
  ratiosd100 <- sd9599/sd5095

  td5095 <- (Simple_FlowStatistics$Value[4]-Simple_FlowStatistics$Value[5])/45
  D100x <- Simple_FlowStatistics$Value[5]-td5095*ratiosd100*5

  Simple_FlowStatistics[6,] <- list("D0x",D0x,0)
  Simple_FlowStatistics[7,] <- list("D100x",D100x,100)

  # Establish interpolation function for target
  TargetDurationToFlow <- approxfun(Simple_FlowStatistics$Duration,Simple_FlowStatistics$Value)


  #### also correct max out at 1/99 for source - just do extension of slope on last step last percent
  # # Extrapolate 0 and 100 % duration flows
  sD0x <- 2*Source_Station_Flow_Statistics$Flow[1]-Source_Station_Flow_Statistics$Flow[2]
  sD100x <- 2*Source_Station_Flow_Statistics$Flow[25]-Source_Station_Flow_Statistics$Flow[24]
  Source_Station_Flow_Statistics[26,] <- list("0_percent_extrapolated",sD0x,0)
  Source_Station_Flow_Statistics[27,] <- list("100_percent_extrapolated",sD100x,100)


  # Use this function to convert source flows to durations
  SourceFlowToDuration <- approxfun(Source_Station_Flow_Statistics$Flow,Source_Station_Flow_Statistics$Duration)


  print("Building target site stream flows...")
  # Start building target timeseries, first by filling in dates and initializing flow column
  Target_Station_Mean_Daily <- data.frame("Date" = Date_Vector)
  Target_Station_Mean_Daily$Flow <- 0
  Target_Station_Mean_Daily$Duration <- 0

  # Iterate through dates to build target timeseries
  for (i in 1:nrow(Target_Station_Mean_Daily)){
    #if flow is less than 100% exceedance flow, use 100% value
    if(Source_Station_Mean_Daily[Source_Station_Mean_Daily$Date==Target_Station_Mean_Daily$Date[i],]$X_00060_00003[1] < min(Source_Station_Flow_Statistics$Flow)){
      translate_duration <- 100
    } else {
      #if flow is greater than 0% exceedance flow, use 0%
      if(Source_Station_Mean_Daily[Source_Station_Mean_Daily$Date==Target_Station_Mean_Daily$Date[i],]$X_00060_00003[1] > max(Source_Station_Flow_Statistics$Flow)){
        translate_duration <- 0
      } else {
        #flow is in range, just use linear interpolation
        translate_duration <- SourceFlowToDuration(Source_Station_Mean_Daily[Source_Station_Mean_Daily$Date==Target_Station_Mean_Daily$Date[i],]$X_00060_00003[1])
      }
    }
    Target_Station_Mean_Daily$Duration[i] <- translate_duration
    Target_Station_Mean_Daily$Flow[i] <- max(c(0,TargetDurationToFlow(translate_duration)))
  }
  colnames(Target_Station_Mean_Daily) <- c("Date","PredictedFlow(cfs)","Duration(%)")

  details <- data.frame("Detail" = "", "Value" = "", stringsAsFactors = FALSE)
  details[1,] <- list("Target Latitude",target_Lat)
  details[2,] <- list("Target Longitude",target_Lon)
  details[3,] <- list("Nearest Reference Gage",Source_Station)
  details[4,] <- list("Drainage Area (sq. mi.)",Step1_Watershed_Part2[["parameters"]][["value"]][3])



  print("Complete!")

  return(list(Target_Station_Mean_Daily,details))
}
