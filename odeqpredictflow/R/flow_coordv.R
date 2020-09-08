#' Predict Mean Daily Flow
#'
#' Estimates mean daily flow series for ungaged stream site in Oregon,using USGS StreamStats and NWIS data. Based on the methods described in Lorenz & Ziegeweid (2016) and other USGS publications.
#'
#' Returns a list with two dataframes. The first dataframe (1) is the generated mean daily flow timeseries. The second dataframe (2) contains additional details about the request, such as:
#' - The target site longitude and latitude
#' - The nearest USGS reference gage identified
#' - The drainage area of the watershed delineated by StreamStats for the target site
#'
#' Note: If the returned drainage area is zero or much smaller than expected, check the StreamStats website to ensure that the target coordinates align with the stream grid. (https://streamstats.usgs.gov/ss/)
#'
#' @param latitude The latitude of a target stream site, in decimal degrees
#' @param longitude The longitude of a target stream site, in decimal degrees
#' @param date_vector A vector of dates in “YYYY-MM-DD” format, not necessarily continuous
#' @return A list object with: 1 = a dataframe with the mean daily flow series and 2 = additional details
#' @export
#' @examples
#' result <- Predict.Mean.Daily.Flow.Coord_Vector(43.252618,-123.026172,c("2010-01-01","2011-03-04","2012-12-31"))
#' timeseries <- result[[1]]
#' flows <- timeseries$`PredictedFlow(cfs)`
#' details <- result[[2]]
#'
Predict.Mean.Daily.Flow.Coord_Vector <- function(latitude,longitude,date_vector){
  stations <- Source.Station.From.Coords(latitude,longitude)
  return(Stats.Vector(stations[[1]],stations[[2]],stations[[3]],date_vector))
}