
library(odeqpredictflow)

result <- flow_station("UmpNF-078","2010-01-01","2012-12-31")

timeseries <- result[[1]]
flows <- timeseries$`PredictedFlow(cfs)`
details <- result[[2]]
