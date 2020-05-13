# odeqpredictflow
 R pacakge to predict stream flow at ungaged sites in Oregon (DRAFT)

 Developed for Oregon DEQ by Tetra Tech with funds provided by U.S. EPA.

The strategy implemented to predict flow timeseries is based on the QPPQ (flow-probability-probability-flow) method described in in Lorenz & Ziegeweid (2016) and utilizes online USGS resources to retrieve stream information. The data are processed to return predicted mean daily streamflows for an ungaged stream location. The method is implemented through an R package, with four functions to handle four different styles of inputs. 

To identify the target location, two different types of input are available:
  1.	Latitude and longitude (decimal degrees, NAD83) for the stream site 
  2.	The Oregon DEQ station ID 

Additionally, two different types of input are available for the time range input: 
  1.	A start and end date, for which a series of mean daily flows will be returned 
  2.	A vector of (not necessarily continuous) dates, for which the individual mean daily flows will be returned 
  
To use the QPPQ method of predicting streamflow, three datasets are required to predict the mean daily stream flow at the target site: the mean daily flow for the same date range at a nearby reference site, flow-duration statistics for the reference site, and flow-duration statistics for the target site.

Figure 1 outlines the QPPQ method process. From the reference site flow on a given date, the exceedance probability is determined from the flow-duration statistics for that site. The flow-duration statistics for the target site are then used to identify the flow at the target site that would have equal exceedance probability to the reference site’s flow. The resulting flow is applied to the corresponding date for the target site. To produce a series of mean daily flows, the process is repeated for each day in the series.

![QPPQ method](https://github.com/rmichie/odeqpredictflow/blob/master/Lorenz_and_Ziegewed_2016_QPPQ_diagram.png) 
Figure 1. Diagram of the QPPQ method for flow prediction, from Lorenz & Ziegeweid (2016).

From the user-supplied inputs, this script executes the following processes:
  1.	Identify the reference station 
  2.	Retrieve target site flow-duration statistics  
  3.	Retrieve reference site mean daily flow series 
  4.	Retrieve reference site flow-duration statistics 
  5.	Interpolate and extrapolate flow-duration statistics 
  6.	Calculate target site mean daily flow series 

Each of these six processes are discussed in the following sections. A flowchart of how the inputs are passed through the script to carry out these steps is presented in Figure 2.

![QPPQ method](https://github.com/rmichie/odeqpredictflow/blob/master/function_steps_flowchart.png) 
Figure 2. Flowchart overview of script process to generate a predicted flow series from user inputs.

References

Lorenz, David L., and Jeffrey R. Ziegeweid. Methods to estimate historical daily streamflow for ungaged stream locations in Minnesota. No. 2015-5181. US Geological Survey, 2016.

Nash, J. E.; Sutcliffe, J. V. (1970). "River flow forecasting through conceptual models part I — A discussion of principles". Journal of Hydrology. 10 (3): 282–290.

Ries, K.G., III, Guthrie, J.D., Rea, A.H., Steeves, P.A., Stewart, D.W., 2008, StreamStats: A water resources web application: U.S. Geological Survey Fact Sheet 2008-3067, 6 p.

Risley, John, Adam Stonewall, and Tana Haluska. Estimating flow-duration and low-flow frequency statistics for unregulated streams in Oregon. No. FHWA-OR-RD-09-03. Geological Survey (US), 2008.

