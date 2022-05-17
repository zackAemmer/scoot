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
library(SWKM)
library(mefa)


setwd('/Users/zack/Desktop/stl/scoot')


# Read NHTS
# trip_data = read.csv('/Volumes/GoogleDrive-100069302124626889369/My Drive/STL/Datasets/NHTS/trippub.csv')
trip_data = read.csv('/Users/zack/Library/CloudStorage/GoogleDrive-zae5op@uw.edu/My Drive/STL/Datasets/NHTS/trippub.csv')
# hh_data = read.csv('/Volumes/GoogleDrive-100069302124626889369/My Drive/STL/Datasets/NHTS/hhpub.csv')
# veh_data = read.csv('/Volumes/GoogleDrive-100069302124626889369/My Drive/STL/Datasets/NHTS/vehpub.csv')

# Read Synthetic pop
popsim_data = read.csv('./populationsim-master/example_msa_survey/combined_stats/popsim_sample.csv')
popsim_data$TRACT = str_pad(popsim_data$TRACT, 11, pad='0')

# Read shapes
# msa_puma_tract_join = read.csv('./data/msa_puma_tract_join.csv')
# popsim_data_T = merge(popsim_data, msa_puma_tract_join, by.x="TRACT", by.y="geoid")

# Clean data
trip_data = trip_data[trip_data$TRIPPURP != -9,]
trip_data = trip_data[trip_data$TDWKND != 1,]


# #### Bottom Up Approach ####
# # Can look at WHYTRP1S for categorizing trips inspiration
# data = data.frame(paste0(trip_data$HOUSEID,trip_data$PERSONID),trip_data$WHYFROM,trip_data$WHYTO)
# colnames(data) = c('ID','WHYFROM','WHYTO')
# data$ID_LEAD = lead(data$ID, 1, default = 'END')
# 
# # A simple tour is only associated with one type of activity,
# # while a complex tour includes two or more types of activities.
# # A tour that includes at least one optional and at least one flexible activity, we term a complex non- mandatory tour.
# # A tour that includes at least one mandatory activity, along with at least one optional or flexible activity, we term a complex mandatory tour.
# # Simple Optional H-O-H
# # Simple Flexible H-F-H
# # Simple Mandatory H-M-H
# # Complex Non-Mandatory H-F&O-H
# # Complex Mandatory H-(F/O)&M-H
# 
# # NHTS seems to treat this as home
# home_codes = c(1)
# # Mandatory; work, childcare, medical
# mandatory_codes = c(2,3,4,6,8,9,10,18)
# # Flexible; banking, grocery, shopping
# flexible_codes = c(11,12,13,14)
# # Optional; recreation, visiting
# optional_codes = c(5,15,16,17,19,97,-9,-8,-7)
# # Mode Transfer "change type of transportation"
# transfer_codes = c(7)
# 
# # Group possible states
# data[data$WHYFROM %in% home_codes,]$WHYFROM = 'H'
# data[data$WHYFROM %in% mandatory_codes,]$WHYFROM = 'M'
# data[data$WHYFROM %in% flexible_codes,]$WHYFROM = 'F'
# data[data$WHYFROM %in% optional_codes,]$WHYFROM = 'O'
# data[data$WHYFROM %in% transfer_codes,]$WHYFROM = 'T'
# data[data$WHYTO %in% home_codes,]$WHYTO = 'H'
# data[data$WHYTO %in% mandatory_codes,]$WHYTO = 'M'
# data[data$WHYTO %in% flexible_codes,]$WHYTO = 'F'
# data[data$WHYTO %in% optional_codes,]$WHYTO = 'O'
# data[data$WHYTO %in% transfer_codes,]$WHYTO = 'T'
# 
# # Terminal state; assmue everyone's tour ends after their last recorded trip
# data[data$ID != data$ID_LEAD,]$WHYTO = 'END'
# 
# # Start state; assume everyone's tour starts from first recorded trip
# data$WHYTO_LAG = lag(data$WHYTO, 1, default='END')
# data[data$WHYTO_LAG == 'END',]$WHYFROM = 'START'
# 
# # Remove IDs and make factors for generating transitions
# data = data.frame(data$WHYFROM,data$WHYTO)
# colnames(data) = c('WHYFROM','WHYTO')
# triptype_levels = c('START','END','F','H','M','O','T')
# data$WHYFROM = factor(data$WHYFROM, levels=triptype_levels)
# data$WHYTO = factor(data$WHYTO, levels=triptype_levels)
# 
# # Construct transition matrix of probabilities and Markov model
# transition_matrix = table(data)
# elem = c()
# for (i in seq(1:nrow(transition_matrix))) {
#   for (j in seq(1:ncol(transition_matrix))) {
#     elem = c(elem,transition_matrix[j,i])
#   }
# }
# normalize = function(x) {
#     return (x/sum(x))
# }
# transition_matrix = matrix(elem, nrow=7, ncol=7)
# transition_matrix = transition_matrix / apply(transition_matrix, MARGIN=1, FUN=sum)
# transition_matrix[is.na(transition_matrix)] = 0
# transition_matrix[2,2] = 1.0 # END should be an absorbing state; only goes to self
# transition_matrix
# rm(i,j,elem)
# markov_model = new('markovchain',
#                    transitionMatrix=transition_matrix,
#                    states=triptype_levels,
#                    byrow=TRUE,
#                    name='Trip Chain Probabilities')
# plot(markov_model)
# print(markov_model)
# print(transientStates(markov_model))
# print(recurrentStates(markov_model))
# print(meanAbsorptionTime(markov_model))
# 
# # Draw a sequence from markov model, then sample trip distance
# simulated_tours = c()
# for (i in seq(1,10000)) {
#   new_tour = rmarkovchain(n=10, object=markov_model, t0='START', include.t0=TRUE)
#   new_tour = new_tour[new_tour != 'END']
#   simulated_tours = c(simulated_tours, new_tour)
# }
# rm(i)
# simulated_tours


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
# Get total distance in each tour
data = trip_data %>%
  group_by(HOUSEID,PERSONID) %>%
  summarise(
    dist = sum(TRPMILES),
  )
tour_data$dist = data$dist

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

# Get tour weights
data = trip_data %>%
  group_by(HOUSEID,PERSONID) %>%
  arrange(WTTRDFIN) %>%
  filter(row_number()==1) %>%
  select(HOUSEID,PERSONID,WTTRDFIN)
tour_data$weight = data$WTTRDFIN

# Label columns
colnames(tour_data) = c("TOURID","n_HBO","n_HBSHOP","n_HBSOCREC","n_HBW","n_NHB","dist_mi","start_time_cos","start_time_sin","weight")
rm(data, all_data, purpose, trip_purposes, tour_span)

# Features
X = tour_data[,2:(ncol(tour_data)-1)]

# Cluster tours
# Standardize or normalize columns
standardize = function(x) {
  z = (x-mean(x)) / sd(x)
  return (z)
}
X = apply(X, 2, standardize)

# The weights in NHTS are too large to load in RAM; scale by lowest
min_weight = min(tour_data$weight)
tour_data$weight = tour_data$weight / min_weight
X = data.frame(X)
X_cluster = rep(X, tour_data$weight)

# Sampling essential to run the number of clusters analysis
set.seed(42)
wcss = vector()
# Check within cluster sum of squares to get number of clusters
X_cluster = X_cluster[sample(nrow(X_cluster), 200000),]
for (i in 1:10) wcss[i] = sum(kmeans(X_cluster, i, nstart=20)$withinss)
plot(1:10,
     wcss,
     type = 'b', # for lines and points
     main = paste('Optimal Number of Clusters'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

# Run clustering (7 clusters)
y_cluster = kmeans(X_cluster, centers=7, nstart=1)
fviz_cluster(y_cluster,
             data = X_cluster,
             geom="point",
             ggtheme=theme_bw())
table(y_cluster$cluster)

# Assign a cluster to every NHTS tour
predict_cluster <- function(x, centers) {
  # compute squared euclidean distance from each sample to each cluster center
  tmp <- sapply(seq_len(nrow(x)),
                function(i) apply(centers, 1,
                                  function(v) sum((x[i, ]-v)^2)))
  max.col(-t(tmp))  # find index of min distance
}
tour_data$cluster = predict_cluster(X, y_cluster[['centers']])

# Model cluster as a factor of socioeconomic characteristics
# Count of categorical vars
X = trip_data %>%
  group_by(HOUSEID,PERSONID) %>%
  summarise(
    age = R_AGE_IMP,
    edu = EDUC,
    size = HHSIZE,
    veh = HHVEHCNT,
    inc = HHFAMINC,
    sex = R_SEX_IMP,
  )
X = unique(X)

# OHE categorical variables
X = X %>% mutate(value=1) %>%
  spread(sex, value, fill=0, sep="")

# Given person; generate a tour: First model their cluster, then sample a tour from cluster
# Should make the cluster selection slightly random; people don't always make same type of trip
num_numeric = 7 # first n columns to be standardized
X = X[,3:ncol(X)] # Drop ID variables
X[,1:num_numeric] = apply(X[,1:num_numeric], 2, standardize)

# MNL Regression with 10-fold validation
k_fold_accuracies = c()
data_len = nrow(X)
for (i in seq(0,.9,.1)) {
  print(i)
  start_idx = i*data_len
  end_idx = (i+.1)*data_len
  Xy = data.frame(X,tour_data$cluster)
  Xy_train = Xy[-(start_idx:end_idx),]
  Xy_test = Xy[(start_idx:end_idx),]
  mnl_cluster_model = multinom(tour_data.cluster~age+edu+size+veh+inc, data=Xy_train)
  y_pred = as.integer(predict(mnl_cluster_model, Xy_test))
  accuracy = sum(y_pred == Xy_test[,ncol(Xy_test)]) / nrow(Xy_test)
  k_fold_accuracies = c(k_fold_accuracies, accuracy)
}
mean(k_fold_accuracies)
summary(mnl_cluster_model)

rm(i,data_len,end_idx,num_numeric,min_weight,X,Xy,Xy_test,y,y_pred,Xy_train,X_cluster,y_cluster,wcss)


# Predict tour cluster for popsim data
# Recode survey variables to match NHTS (on which MNL model must be trained)
popsim_data$edu_nhts = popsim_data$edu
popsim_data$edu_nhts[popsim_data$edu_nhts==4] = 3
popsim_data$edu_nhts[popsim_data$edu_nhts==5] = 4
popsim_data$edu_nhts[popsim_data$edu_nhts %in% c(6,7,8)] = 5

popsim_data$hhincome_nhts = popsim_data$hhincome
popsim_data$hhincome_nhts[popsim_data$hhincome_nhts %in% c(3,4)] = 3
popsim_data$hhincome_nhts[popsim_data$hhincome_nhts==5] = 4
popsim_data$hhincome_nhts[popsim_data$hhincome_nhts==6] = 5
popsim_data$hhincome_nhts[popsim_data$hhincome_nhts==7] = 6
popsim_data$hhincome_nhts[popsim_data$hhincome_nhts==8] = 7
popsim_data$hhincome_nhts[popsim_data$hhincome_nhts==9] = 8 # No way to do
popsim_data$hhincome_nhts[popsim_data$hhincome_nhts==9] = 8 # No way to do
popsim_data$hhincome_nhts[popsim_data$hhincome_nhts==12] = 11


# How many tours to generate? (Samples from synthetic population)
n_tours = 1000
popsim_data_sample = popsim_data[sample(nrow(popsim_data), n_tours),]

# Standardize and make cluster predictions
X = data.frame(popsim_data_sample$age, popsim_data_sample$edu_nhts, popsim_data_sample$hhsize, popsim_data_sample$veh, popsim_data_sample$hhincome_nhts)
colnames(X) = c("age","edu","size","veh","inc")
X = apply(X, 2, standardize)
y_pred_prob = predict(mnl_cluster_model, X, type="probs")
y_pred_class = c()
for (i in 1:nrow(y_pred_prob)) {
  y_pred_class = c(y_pred_class, sample(seq(1,7,1), size=1, prob=y_pred_prob[i,]))
}
popsim_data_sample$cluster_id = y_pred_class

# Sample matching cluster tours
sample_tour = function(cluster_id) {
  possible_tours = tour_data[tour_data$cluster==cluster_id,]
  tour = possible_tours[sample(nrow(possible_tours), 1),]
  tour_id = tour$TOURID
  return (tour_id)
}
popsim_data_sample$tour_id = sapply(popsim_data_sample$cluster_id, sample_tour)
popsim_data_sample = merge(popsim_data_sample, tour_data, by.x="tour_id", by.y="TOURID")

rm(X,y_pred_prob,i,y_pred_class)


#### Tour Destination Tracts Generation ####
host = Sys.getenv("MAIN_HOST")
dbname = Sys.getenv("MAIN_DB")
user = Sys.getenv("MAIN_USER")
password = Sys.getenv("MAIN_PWD")
port = Sys.getenv("MAIN_PORT")

# Function to filter to relevant NHTS trips and choose destination tracts for each
assignTracts = function(i, popsim_data_sample, trip_data, host, dbname, user, password, port) {
  main_con = dbConnect(
    Postgres(),
    host = host,
    dbname = dbname,
    user = user,
    password = password,
    port = port
  )
  # Get the set of NHTS trips corresponding to generated tour
  tour_id = popsim_data_sample[i,]$tour_id
  house_id = substr(tour_id, 0, 8)
  person_id = substr(tour_id, 9, nchar(tour_id))
  nhts_tour = trip_data[(trip_data$HOUSEID==house_id & trip_data$PERSONID==person_id),]
  nhts_tour = nhts_tour[order(nhts_tour$TDTRPNUM),]
  home_tract = popsim_data_sample[i,]$TRACT
  tour_tracts = c()
  current_tract = home_tract
  
  # Choose a tract for each trip in the tour
  for (j in 1:nrow(nhts_tour)) {
    # Always end at home
    if (j==nrow(nhts_tour)) {
      tour_tracts = c(tour_tracts, home_tract)
      break
    } else {
      # Get buffered tract intersects to choose from
      buffer_dist = nhts_tour[j,]$TRPMILES/69 # Miles to meters to degrees
      geoid = current_tract
      query = paste0("
      WITH otract AS (
        SELECT ST_Buffer(ST_Centroid(geom), ",buffer_dist,") AS geom_buffered
        FROM msa_puma_tract_join
        WHERE geoid = '",geoid,"')
      SELECT dtract.geoid
      FROM otract
      JOIN msa_puma_tract_join AS dtract
      ON ST_Intersects(otract.geom_buffered, dtract.geom)")
      dtracts = dbGetQuery(main_con, query)

      # Select destination tract at random
      current_tract = dtracts[sample(nrow(dtracts), 1),]
      tour_tracts = c(tour_tracts, current_tract)
    }
  }
  dbDisconnect(main_con)
  return (tour_tracts)
}

# Sequential Implementation
# 1hr/1000
start_time = Sys.time()
generated_tracts = vector(mode='list', length=nrow(popsim_data_sample))
for (i in 1:nrow(popsim_data_sample)) {
  print(i)
  generated_tracts[[i]] = assignTracts(i,
                                       popsim_data_sample,
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


# # Parallel Implementation
# # 37 sec/10
# # 645 sec/100
# start_time = Sys.time()
# numCores = detectCores()
# cl = makeCluster(numCores-1)
# registerDoSNOW(cl)
# clusterEvalQ(cl, {
#   library(DBI)
#   library(RPostgres)
#   # main_con = dbConnect(
#   #   Postgres(),
#   #   host = Sys.getenv("MAIN_HOST"),
#   #   dbname = Sys.getenv("MAIN_DB"),
#   #   user = Sys.getenv("MAIN_USER"),
#   #   password = Sys.getenv("MAIN_PWD"),
#   #   port = Sys.getenv("MAIN_PORT")
#   # )
#   NULL
# })
# generated_tracts = foreach (i=1:nrow(generated_tours)) %dopar% {
#   assignTracts(i,
#                generated_tours,
#                trip_data,
#                host=host,
#                dbname=dbname,
#                user=user,
#                password=password,
#                port=port)
# }
# stopCluster(cl)
# print("Parallel")
# par_time = Sys.time() - start_time
# print(par_time)


# Clean up and save
drops <- c("X","index","household_id","start_time_cos","start_time_sin","cluster")
popsim_data_sample = popsim_data_sample[,!(names(popsim_data_sample) %in% drops)]

saveRDS(trip_data, './data/trip_data.rds')
saveRDS(generated_tracts, './data/generated_tracts.rds')
saveRDS(popsim_data_sample, './data/synthetic_persons.rds')
