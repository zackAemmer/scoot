library(ggmap)
library(gmapsdistance)
library(Hmisc)
library(tmaptools)


setwd('/Users/zack/Desktop/stl/scoot')

# Read survey responses

# Corrections to mislabeled cross streets
# mturk_data_2 = read.csv('./data/corCheck.csv')
# mturk_data_2$olat = mturk_data_2$corrOrigin_lat
# mturk_data_2$olon = mturk_data_2$corrOrigin_long
# mturk_data_2$dlat = mturk_data_2$corrDest_lat
# mturk_data_2$dlon = mturk_data_2$corrDest_long
# mturk_data = mturk_data_2

# First and second waves of data collection
mturk_data_1 = read.csv('./data/tripinfo1218.csv')
mturk_data = read.csv('./data/scoot_mturk_2r_tripinfo.csv')

# # Get geocoded locations for mturk cross streets
# register_google(key=Sys.getenv(MAPS_API_KEY))
# mturk_data['origin'] = paste0(mturk_data$ox1, " and ", mturk_data$ox2, ", ", mturk_data$ocity, ", ", mturk_data$ostate)
# mturk_data['destination'] = paste0(mturk_data$dx1, " and ", mturk_data$dx2, ", ", mturk_data$dcity, ", ", mturk_data$dstate)
# mturk_origins_geocoded = geocode(mturk_data$origin)
# mturk_destinations_geocoded = geocode(mturk_data$destination)
# mturk_data['olat'] = mturk_origins_geocoded$lat
# mturk_data['olon'] = mturk_origins_geocoded$lon
# mturk_data['dlat'] = mturk_destinations_geocoded$lat
# mturk_data['dlon'] = mturk_destinations_geocoded$lon
# write.csv(mturk_data, file='./data/mturk_responses_geocoded.csv')

# Match Mturk modes to Google API
mturk_data = read.csv('./data/mturk_responses_geocoded.csv')
mturk_data[mturk_data$mode %in% c("drive","taxi"),]$mode = "driving"
mturk_data[mturk_data$mode %in% c("walk"),]$mode = "walking"
mturk_data[mturk_data$mode %in% c("micromobility","bike"),]$mode = "bicycling"
mturk_data[mturk_data$mode %in% c("bus","rail"),]$mode = "transit"

# Get distance between origin and destination for MTurk
# TRPMILES is calculated as distance on Google Maps by NHTS
set.api.key("AIzaSyC7yJXuITtdnXgy-9CypNsXZqWCmmzfZtQ")
all_times = c()
all_dists = c()
all_statuses = c()
for (i in 1:nrow(mturk_data)) {
  od = mturk_data[i,]
  if (is.na(od$olat)) {
    all_times = append(all_times, NA)
    all_dists = append(all_dists, NA)
    all_statuses = append(all_statuses, NA)
  } else {
    dist_results = tryCatch({
      gmapsdistance(origin=paste0(od$olat,"+",od$olon),
                    destination=paste0(od$dlat,"+",od$dlon),
                    mode=od$mode,
                    combinations="pairwise")
    }, error = function(e){
      list(Time=NA,Distance=NA,Status="ERROR")
    })
    all_times = append(all_times,dist_results$Time)
    all_dists = append(all_dists,dist_results$Distance)
    all_statuses = append(all_statuses, dist_results$Status)
  }
}
# Units are meters for distance, seconds for time
# mturk_data$gtime = all_times
# mturk_data$gdistance = all_dists
# mturk_data$gstatus = all_statuses
mturk_data$corr_gtime = all_times
mturk_data$corr_gdistance = all_dists
mturk_data$corr_gstatus = all_statuses
write.csv(mturk_data, file='./data/mturk_responses_dists_corr.csv')



# Read NHTS survey data
trip_data = read.csv('/Volumes/GoogleDrive-100069302124626889369/My Drive/STL/Datasets/NHTS/trippub.csv')
# hh_data = read.csv('/Volumes/GoogleDrive-100069302124626889369/My Drive/STL/Datasets/NHTS/hhpub.csv')
# veh_data = read.csv('/Volumes/GoogleDrive-100069302124626889369/My Drive/STL/Datasets/NHTS/vehpub.csv')

# Read mturk data
mturk_data = read.csv('./data/mturk_responses_dists.csv')

# Limit NHTS to columns of interest and rename to be more descriptive
trip_data = data.frame(trip_data$TRPMILES,trip_data$TRPTRANS,trip_data$TRVLCMIN,trip_data$WTTRDFIN)
colnames(trip_data) = c('distance','mode','travel_time','weight')

# Drop NA values, and survey non-answers
trip_data = trip_data[complete.cases(trip_data),]
trip_data = trip_data[trip_data$mode %in% c(1,2,3,4,5,6,8,11,12,14,15,16,17,18),]
trip_data = trip_data[trip_data$travel_time > 0,]
trip_data = trip_data[trip_data$travel_time < 1200,]
trip_data = trip_data[trip_data$distance > 0,]
trip_data = trip_data[trip_data$distance < 9621.053,]

# Re-code mode choice variable to join similar modes
trip_data[trip_data$mode %in% c(1),]$mode = "Walk"
trip_data[trip_data$mode %in% c(2),]$mode = "Bike"
trip_data[trip_data$mode %in% c(3,4,5,6,8,18),]$mode = "Car"
trip_data[trip_data$mode %in% c(11,14,15,16),]$mode = "Transit"
trip_data[trip_data$mode %in% c(12,17),]$mode = "Ridehail"

# Fit linear model to each mode, plot and summarize results
probs_list = c(.05,.95)

# Try log-log transform
trip_data$distance = log(trip_data$distance)
trip_data$travel_time = log(trip_data$travel_time)
mturk_data$distance = log(mturk_data$gdistance*0.000621371)
mturk_data$travel_time = log(mturk_data$gtime/60)
mturk_data$travel_time = log(as.integer(mturk_data$triptime))

makeTimeDistPlot = function(data, probs_list) {
  time_pctiles = wtd.quantile(data$travel_time, weights=data$weight, probs=probs_list, na.rm=TRUE, normwt=TRUE)
  dist_pctiles = wtd.quantile(data$distance, weights=data$weight, probs=probs_list, na.rm=TRUE, normwt=TRUE)
  data = data[data$travel_time>time_pctiles[1],]
  data = data[data$travel_time<time_pctiles[2],]
  data = data[data$distance>dist_pctiles[1],]
  data = data[data$distance<dist_pctiles[2],]
  model_lm=lm(distance~travel_time, data, weights=data$weight)
  # walk_model_loess=loess(distance~travel_time, data, weights=data$weight)
  print(summary(model_lm))
  plot(data$travel_time, data$distance)
  j = order(data$travel_time)
  abline(model_lm, col="blue")
  # lines(data$travel_time[j],walk_model_loess$fitted[j], col="red")
  return(model_lm)
}

# First NHTS, then MTurk Plots
data = trip_data[trip_data$mode=="Walk",]
walk_model_lm = makeTimeDistPlot(data=data, probs_list=probs_list)
data = mturk_data[mturk_data$mode=="walking",]
makeTimeDistPlot(data=data, probs_list=probs_list)

data = trip_data[trip_data$mode=="Bike",]
bike_model_lm = makeTimeDistPlot(data=data, probs_list=probs_list)
data = mturk_data[mturk_data$mode=="bicycling",]
makeTimeDistPlot(data=data, probs_list=probs_list)

data = trip_data[trip_data$mode=="Car",]
car_model_lm = makeTimeDistPlot(data=data, probs_list=probs_list)
data = mturk_data[mturk_data$mode=="driving",]
makeTimeDistPlot(data=data, probs_list=probs_list)

data = trip_data[trip_data$mode=="Transit",]
transit_model_lm = makeTimeDistPlot(data=data, probs_list=probs_list)
data = mturk_data[mturk_data$mode=="transit",]
makeTimeDistPlot(data=data, probs_list=probs_list)

data = trip_data[trip_data$mode=="Ridehail",]
ridehail_model_lm = makeTimeDistPlot(data=data, probs_list=probs_list)
makeTimeDistPlot(data=data, probs_list=probs_list)

# Save the models for sharing on the group drive
saveRDS(walk_model_lm, file="./data/walk_model_lm.rds")
saveRDS(bike_model_lm, file="./data/bike_model_lm.rds")
saveRDS(car_model_lm, file="./data/car_model_lm.rds")
saveRDS(transit_model_lm, file="./data/transit_model_lm.rds")
saveRDS(ridehail_model_lm, file="./data/ridehail_model_lm.rds")

# Read in models generated from NHTS data, calculate distance for each mturk trip based on mode/time
walk_model_lm = readRDS(file="./data/walk_model_lm.rds")
bike_model_lm = readRDS(file="./data/bike_model_lm.rds")
car_model_lm = readRDS(file="./data/car_model_lm.rds")
transit_model_lm = readRDS(file="./data/transit_model_lm.rds")
ridehail_model_lm = readRDS(file="./data/ridehail_model_lm.rds")

mturk_data$dist_mode = NA
mturk_data[mturk_data$mode=="walking",]$dist_mode = walk_model_lm$coefficients[1] + (walk_model_lm$coefficients[2]*log(as.integer(mturk_data[mturk_data$mode=="walking",]$triptime)))
mturk_data[mturk_data$mode=="bicycling",]$dist_mode = bike_model_lm$coefficients[1] + (bike_model_lm$coefficients[2]*log(as.integer(mturk_data[mturk_data$mode=="bicycling",]$triptime)))
mturk_data[mturk_data$mode=="driving",]$dist_mode = car_model_lm$coefficients[1] + (car_model_lm$coefficients[2]*log(as.integer(mturk_data[mturk_data$mode=="driving",]$triptime)))
mturk_data[mturk_data$mode=="transit",]$dist_mode = transit_model_lm$coefficients[1] + (transit_model_lm$coefficients[2]*log(as.integer(mturk_data[mturk_data$mode=="transit",]$triptime)))

# Get rid of logs
mturk_data$dist_mode = exp(mturk_data$dist_mode)
mturk_data$dist_xy = exp(mturk_data$distance)

# Take a look
data = mturk_data[mturk_data$mode=='walking',]
data = data[complete.cases(data$dist_mode),]
data = data[complete.cases(data$dist_xy),]
plot(data$dist_mode, data$dist_xy)
data = mturk_data[mturk_data$mode=='bicycling',]
data = data[complete.cases(data$dist_mode),]
data = data[complete.cases(data$dist_xy),]
plot(data$dist_mode, data$dist_xy)
data = mturk_data[mturk_data$mode=='driving',]
data = data[complete.cases(data$dist_mode),]
data = data[complete.cases(data$dist_xy),]
plot(data$dist_mode, data$dist_xy)
data = mturk_data[mturk_data$mode=='transit',]
data = data[complete.cases(data$dist_mode),]
data = data[complete.cases(data$dist_xy),]
plot(data$dist_mode, data$dist_xy)

# Write to file to share on group drive
write.csv(mturk_data, file='./data/mturk_data_processed.csv')
