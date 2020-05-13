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
#' @param station A single MLocID for an ODEQ monitoring station (example: “UmpNF-078”, "31878-ORDEQ", or "14138900")
#' @param start_date A single date in “YYYY-MM-DD” format for the start of a continuous daily series
#' @param end_date A single date in “YYYY-MM-DD” format for the end of a continuous daily series
#' @return A list object with: 1 = a dataframe with the mean daily flow series and 2 = additional details
#' @export
#' @examples
#' result <- Predict.Mean.Daily.Flow.Station_Series("UmpNF-078","2010-01-01","2012-12-31")
#' timeseries <- result[[1]]
#' flows <- timeseries$`PredictedFlow(cfs)`
#' details <- result[[2]]
#'

Predict.Mean.Daily.Flow.Station_Series <- function(station,start_date,end_date){
  stations <- Source.Station.From.List(station)
  return(Stats.Series(stations[[1]],stations[[2]],stations[[3]],start_date,end_date))
}
