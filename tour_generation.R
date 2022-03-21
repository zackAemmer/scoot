library(Hmisc)
library(dplyr)
library(tidyr)
library(factoextra)
library(stringr)
library(mltools)
library(nnet)
library(mlogit)
library(markovchain)
library(DBI)
library(RPostgres)
library(profvis)
library(data.table)
library(foreach)
library(doParallel)
library(doSNOW)

# setwd('/Users/zack/Desktop/stl/scoot')


# Read NHTS
# trip_data = read.csv('/Volumes/GoogleDrive-100069302124626889369/My Drive/STL/Datasets/NHTS/trippub.csv')
trip_data = read.csv('G:/My Drive/STL/Datasets/NHTS/trippub.csv')
# hh_data = read.csv('/Volumes/GoogleDrive-100069302124626889369/My Drive/STL/Datasets/NHTS/hhpub.csv')
# veh_data = read.csv('/Volumes/GoogleDrive-100069302124626889369/My Drive/STL/Datasets/NHTS/vehpub.csv')

# Clean data
trip_data = trip_data[trip_data$TRIPPURP != -9,]
trip_data = trip_data[trip_data$TDWKND != 1,]

#### Bottom Up Approach ####
# Can look at WHYTRP1S for categorizing trips inspiration
data = data.frame(paste0(trip_data$HOUSEID,trip_data$PERSONID),trip_data$WHYFROM,trip_data$WHYTO)
colnames(data) = c('ID','WHYFROM','WHYTO')
data$ID_LEAD = lead(data$ID, 1, default = 'END')

# A simple tour is only associated with one type of activity,
# while a complex tour includes two or more types of activities.
# A tour that includes at least one optional and at least one flexible activity, we term a complex non- mandatory tour.
# A tour that includes at least one mandatory activity, along with at least one optional or flexible activity, we term a complex mandatory tour.
# Simple Optional H-O-H
# Simple Flexible H-F-H
# Simple Mandatory H-M-H
# Complex Non-Mandatory H-F&O-H
# Complex Mandatory H-(F/O)&M-H

# NHTS seems to treat this as home
home_codes = c(1)
# Mandatory; work, childcare, medical
mandatory_codes = c(2,3,4,6,8,9,10,18)
# Flexible; banking, grocery, shopping
flexible_codes = c(11,12,13,14)
# Optional; recreation, visiting
optional_codes = c(5,15,16,17,19,97,-9,-8,-7)
# Mode Transfer "change type of transportation"
transfer_codes = c(7)

# Group possible states
data[data$WHYFROM %in% home_codes,]$WHYFROM = 'H'
data[data$WHYFROM %in% mandatory_codes,]$WHYFROM = 'M'
data[data$WHYFROM %in% flexible_codes,]$WHYFROM = 'F'
data[data$WHYFROM %in% optional_codes,]$WHYFROM = 'O'
data[data$WHYFROM %in% transfer_codes,]$WHYFROM = 'T'
data[data$WHYTO %in% home_codes,]$WHYTO = 'H'
data[data$WHYTO %in% mandatory_codes,]$WHYTO = 'M'
data[data$WHYTO %in% flexible_codes,]$WHYTO = 'F'
data[data$WHYTO %in% optional_codes,]$WHYTO = 'O'
data[data$WHYTO %in% transfer_codes,]$WHYTO = 'T'

# Terminal state; assmue everyone's tour ends after their last recorded trip
data[data$ID != data$ID_LEAD,]$WHYTO = 'END'

# Start state; assume everyone's tour starts from first recorded trip
data$WHYTO_LAG = lag(data$WHYTO, 1, default='END')
data[data$WHYTO_LAG == 'END',]$WHYFROM = 'START'

# Remove IDs and make factors for generating transitions
data = data.frame(data$WHYFROM,data$WHYTO)
colnames(data) = c('WHYFROM','WHYTO')
triptype_levels = c('START','END','F','H','M','O','T')
data$WHYFROM = factor(data$WHYFROM, levels=triptype_levels)
data$WHYTO = factor(data$WHYTO, levels=triptype_levels)

# Construct transition matrix of probabilities and Markov model
transition_matrix = table(data)
elem = c()
for (i in seq(1:nrow(transition_matrix))) {
  for (j in seq(1:ncol(transition_matrix))) {
    elem = c(elem,transition_matrix[j,i])
  }
}
normalize = function(x) {
    return (x/sum(x))
}
transition_matrix = matrix(elem, nrow=7, ncol=7)
transition_matrix = transition_matrix / apply(transition_matrix, MARGIN=1, FUN=sum)
transition_matrix[is.na(transition_matrix)] = 0
transition_matrix[2,2] = 1.0 # END should be an absorbing state; only goes to self
transition_matrix
rm(i,j,elem)
markov_model = new('markovchain',
                   transitionMatrix=transition_matrix,
                   states=triptype_levels,
                   byrow=TRUE,
                   name='Trip Chain Probabilities')
plot(markov_model)
print(markov_model)
print(transientStates(markov_model))
print(recurrentStates(markov_model))
print(meanAbsorptionTime(markov_model))

# Draw a sequence from markov model, then sample trip distance
simulated_tours = c()
for (i in seq(1,10000)) {
  new_tour = rmarkovchain(n=10, object=markov_model, t0='START', include.t0=TRUE)
  new_tour = new_tour[new_tour != 'END']
  simulated_tours = c(simulated_tours, new_tour)
}
rm(i)
simulated_tours





#### Top Down Approach ####
# Get count of each trip purpose in each tour
all_data = c()
trip_purposes = unique(trip_data$TRIPPURP)
for (purpose in trip_purposes) {
  data = trip_data %>%
    group_by(HOUSEID,PERSONID) %>%
    tally(TRIPPURP==purpose, name=paste0("n_",purpose))
  all_data = c(all_data,data)
}
tour_data = data.frame(paste0(all_data$HOUSEID,all_data$PERSONID),
                       all_data$n_HBO,
                       all_data$n_HBSHOP,
                       all_data$n_HBSOCREC,
                       all_data$n_HBW,
                       all_data$n_NHB)

# Get total trips in each tour
# tour_data$n = rowSums(tour_data[,2:ncol(tour_data)])

# Get total distance and time in each tour
tour_stats = trip_data %>%
  group_by(HOUSEID,PERSONID) %>%
  summarise(
    dist = sum(TRPMILES),
    # time = sum(TRVLCMIN)
  )
tour_data$dist = tour_stats$dist
# tour_data$time = tour_stats$time

# # Get weekend or not
# tour_weekend = trip_data %>%
#   group_by(HOUSEID,PERSONID) %>%
#   arrange(TDWKND) %>%
#   filter(row_number()==1) %>%
#   arrange(HOUSEID,PERSONID)
# #1y 2n
# tour_weekend$TDWKND[tour_weekend$TDWKND==2] = 0
# tour_data$weekend = tour_weekend$TDWKND

# Get start time of tour
tour_span = trip_data %>%
  group_by(HOUSEID,PERSONID) %>%
  arrange(STRTTIME) %>%
  filter(row_number()==1) %>%
  arrange(HOUSEID,PERSONID)
# Hours to minutes into the day
tour_span$STRTTIME = str_pad(tour_span$STRTTIME, 4, pad="0")
tour_span$STRTTIME = as.numeric(substr(tour_span$STRTTIME, 1, 2)) * 60 + as.numeric(substr(tour_span$STRTTIME, 3, 4))
# Linear to cyclical variable
tour_data$start_time_cos = cos(2 * pi * tour_span$STRTTIME / 1440)
tour_data$start_time_sin = sin(2 * pi * tour_span$STRTTIME / 1440)

# Label columns
colnames(tour_data) = c("TOURID","n_HBO","n_HBSHOP","n_HBSOCREC","n_HBW","n_NHB","dist_mi","start_time_cos","start_time_sin")
rm(data, all_data, tour_stats, purpose, trip_purposes, tour_span)

# Features
X = tour_data[,2:ncol(tour_data)]

# Cluster tours
# Standardize or normalize columns
standardize = function(x) {
  z = (x-mean(x)) / sd(x)
  return (z)
}
X = apply(X, 2, standardize)
y = kmeans(X, centers=3, nstart=100)
fviz_cluster(y,
             data = X,
             geom="point",
             ggtheme=theme_bw())
table(y$cluster)

# Model cluster as a factor of socioeconomic characteristics
# Count of categorical vars
X = trip_data %>%
  group_by(HOUSEID,PERSONID) %>%
  summarise(
    age = R_AGE_IMP,
    edu = EDUC,
    gaspr = GASPRICE,
    size = HHSIZE,
    veh = HHVEHCNT,
    inc = HHFAMINC,
    sex = R_SEX_IMP,
    urb = URBRUR
  )
X = unique(X)

# OHE categorical variables
X = X %>% mutate(value=1) %>%
  spread(urb, value, fill=0, sep="")
X = X %>% mutate(value=1) %>%
  spread(sex, value, fill=0, sep="")

# Given person; generate a tour: First model their cluster, then sample a tour from cluster
# Should make the cluster selection slightly random; people don't always make same type of trip
num_numeric = 6 # first n columns to be standardized
X = X[,3:ncol(X)] # Drop ID variables
X[,1:num_numeric] = apply(X[,1:num_numeric], 2, standardize)

# MNL Regression
Xy = data.frame(X,y$cluster)
split = as.integer(0.8 * nrow(Xy))
Xy_train = Xy[1:split,]
Xy_test = Xy[split:nrow(Xy),]
mnl_cluster_model = multinom(y.cluster~age+edu+gaspr+size+veh+inc, data=Xy_train)
y_pred = as.integer(predict(mnl_cluster_model, Xy_test))
accuracy = sum(y_pred == Xy_test[,ncol(Xy_test)]) / nrow(Xy_test)
summary(mnl_cluster_model)

rm(X,Xy,Xy_train,Xy_test,y, transition_matrix,accuracy,
   flexible_codes,home_codes,mandatory_codes,new_tour,num_numeric,
   optional_codes,simulated_tours,split,transfer_codes,triptype_levels,y_pred)



#### Tour Destination Tracts Generation ####

host = Sys.getenv("MAIN_HOST")
dbname = Sys.getenv("MAIN_DB")
user = Sys.getenv("MAIN_USER")
password = Sys.getenv("MAIN_PWD")
port = Sys.getenv("MAIN_PORT")

# Need to reduce by magnitude of 3
# 150000000/100*7/60/24
# Likes to get stuck sometimes (check number of trips)

# Sample from NHTS for testing
trip_data = as.data.table(trip_data)
generated_tours = tour_data[sample(nrow(tour_data), 100),]

# Function to filter to relevant NHTS trips and choose destination tracts for each
assignTracts = function(i, generated_tours, trip_data, host, dbname, user, password, port) {
  main_con = dbConnect(
    Postgres(),
    host = host,
    dbname = dbname,
    user = user,
    password = password,
    port = port
  )
  # Get the set of NHTS trips corresponding to generated tour
  tour_id = generated_tours[i,]$TOURID
  house_id = substr(tour_id, 0, 8)
  person_id = substr(tour_id, 9, nchar(tour_id))
  nhts_tour = trip_data[(trip_data$HOUSEID==house_id & trip_data$PERSONID==person_id),]
  nhts_tour = nhts_tour[order(nhts_tour$TDTRPNUM),]
  home_tract = "53009001400"
  tour_tracts = c()
  current_tract = home_tract
  
  # Choose a tract for each trip in the tour
  for (j in 1:nrow(nhts_tour)) {
    if (j==nrow(nhts_tour)) {
      tour_tracts = c(tour_tracts, home_tract) # Always end at home
      break
    } else {
      tour_tracts = c(tour_tracts, current_tract)
    }
    # Get buffered tract intersects to choose from
    buffer_dist = nhts_tour[j,]$TRPMILES * 1609/111*.001 # Miles to meters to degrees
    geoid = current_tract
    query = paste0("
      WITH otract AS (
        SELECT ST_BUFFER(geom, ",buffer_dist,") AS geom_buffered
        FROM msa_puma_tract_join
        WHERE geoid = '",geoid,"')
      SELECT dtract.geoid
      FROM otract
      JOIN msa_puma_tract_join AS dtract
      ON ST_INTERSECTS(otract.geom_buffered, dtract.geom)")
    
    dtracts = dbGetQuery(main_con, query)
    # Select destination tract at random
    current_tract = dtracts[sample(nrow(dtracts), 1),]
  }
  dbDisconnect(main_con)
  return (tour_tracts)
}

# Sequential Implementation
# 54 sec/10
# 528 sec/100
start_time = Sys.time()
generated_tracts = vector(mode='list', length=100)
for (i in 1:nrow(generated_tours)) {
  print(i)
  generated_tracts[[i]] = assignTracts(i,
                                       generated_tours,
                                       trip_data,
                                       host=host,
                                       dbname=dbname,
                                       user=user,
                                       password=password,
                                       port=port)
}
print("Sequential")
seq_time = Sys.time() - start_time
print(seq_time)


# Parallel Implementation
# 37 sec/10
# 645 sec/100
start_time = Sys.time()
numCores = detectCores()
cl = makeCluster(numCores-1)
registerDoSNOW(cl)
clusterEvalQ(cl, {
  library(DBI)
  library(RPostgres)
  # main_con = dbConnect(
  #   Postgres(),
  #   host = Sys.getenv("MAIN_HOST"),
  #   dbname = Sys.getenv("MAIN_DB"),
  #   user = Sys.getenv("MAIN_USER"),
  #   password = Sys.getenv("MAIN_PWD"),
  #   port = Sys.getenv("MAIN_PORT")
  # )
  NULL
})
generated_tracts = foreach (i=1:nrow(generated_tours)) %dopar% {
  assignTracts(i,
               generated_tours,
               trip_data,
               host=host,
               dbname=dbname,
               user=user,
               password=password,
               port=port)
}
stopCluster(cl)
print("Parallel")
par_time = Sys.time() - start_time
print(par_time)
