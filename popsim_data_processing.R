library(tidycensus)

# First and second waves of data collection
geo_data = read.csv('./data/TIGER/msa_puma_tract_join.csv')

# Write to file to share on group drive
write.csv(mturk_data, file='./data/mturk_data_processed.csv')
