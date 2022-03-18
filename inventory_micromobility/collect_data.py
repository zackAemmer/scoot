#!/usr/bin/env python3
import requests
from datetime import datetime, timedelta

import psycopg2
import config as cfg
import boto3
import pandas as pd


def connect_to_rds():
    conn = psycopg2.connect(
        host=cfg.HOST,
        database=cfg.DATABASE,
        user=cfg.UID,
        password=cfg.PWD)
    return conn

def get_epoch_and_pst_24hr():
    utc = datetime.utcnow()
    pst = timedelta(hours=7)
    current_hour = (utc - pst).hour
    epoch = round(utc.timestamp())
    return current_hour, epoch

def call_endpoint(key = "", endpoint = ""):
    call_text = endpoint + key
    response = requests.get(call_text)
    response = response.json()
    return response

def upload_to_rds(to_upload, conn, database, col_names, additional_vals):
    # Transform DataFrame rows to tuples with variable for each stated column
    to_upload_list = []
    for i, vehicle_status in to_upload.iterrows():
        # Some feeds don't have all the desired columns
        row = []
        for x in col_names:
            if x in vehicle_status:
                row.append(str(vehicle_status[x]))
            else:
                row.append(None)
        # Include operator, city, etc.
        for val in additional_vals.values():
            row.append(str(val))
        to_upload_list.append(tuple(row))
    with conn.cursor() as curs:
        try:
            z = '(' + ','.join('%s' for x in range(0,len(to_upload_list[0]))) + ')' # String with %s's same length as cols
            args_str = ','.join(curs.mogrify(z, x).decode('utf-8') for x in to_upload_list) # Replace %s's with values
            query_str = f'INSERT INTO {database} ({",".join(col_names+list(additional_vals.keys()))}) VALUES ' + args_str
            curs.execute(query_str)
            conn.commit()
            print(f"Success uploading {len(to_upload_list)} rows {additional_vals} to {database}")
        except Exception as e:
            # Catch all errors and continue to keep server up and running
            if len(to_upload_list) == 0:
                print(f"Error uploading {additional_vals} to {database}: No values in feed")
            else:
                print(f"Error uploading {additional_vals}")
            conn.rollback()
            print(str(e))
    return 1


def main_function():
    # Connect to RDS and get time of collection
    conn = connect_to_rds()
    current_hour, current_epoch = get_epoch_and_pst_24hr()
    print(f"Current epoch: {current_epoch}")

    # # Connect S3 and get list of micromobility API endpoints to inventory
    # session = boto3.Session(
    #     aws_access_key_id = cfg.ACCESS_ID,
    #     aws_secret_access_key = cfg.ACCESS_KEY
    # )
    # s3 = session.resource('s3')
    # obj = s3.Object(bucket_name='micromobility-sources', key='endpoints.csv')
    # response = obj.get()
    # endpoint_list = pd.read_csv(response['Body'])
    endpoint_list = pd.read_csv('./endpoints.csv')
    endpoint_list = endpoint_list[endpoint_list['Note']=='GBFS']
    endpoint_list = endpoint_list[endpoint_list['try']!='N']
    print(f"Querying endpoints: \n{endpoint_list}")

    # Iterate through all data sources in the list
    for i, row in endpoint_list.iterrows():
        api_response = call_endpoint(row['Link'])
        operator = row['Operator']
        city = row['City']
        gbfs_feeds = pd.DataFrame(api_response['data']['en']['feeds'])

        # Iterate through different available GBFS feeds for a given source
        for j, feed in gbfs_feeds.iterrows():
            feed_name = feed['url'].rsplit('/')[-1]
            # Handle freefloat-based feeds
            if feed_name == 'free_bike_status.json':
                data = call_endpoint(feed['url'])['data']['bikes']
                data = pd.DataFrame(data)
                # Some use t/f instead of 1/0
                if data.shape[0] == 0:
                    print(f"{city}, {operator} has feed but no free float bikes")
                    continue
                data[data['is_disabled']==True] = 1
                data[data['is_reserved']==False] = 0
                upload_to_rds(data,
                    conn,
                    'free_bike_status',
                    ['bike_id','lat','lon','type','is_disabled','is_reserved'],
                    {'city':city,'operator':operator,'collected_time':current_epoch})
            # Handle station-based feeds
            elif feed_name == 'station_status.json':
                data = call_endpoint(feed['url'])['data']['stations']
                data = pd.DataFrame(data)
                if data.shape[0] == 0:
                    print(f"{city}, {operator} has feed but no station bikes")
                    continue
                upload_to_rds(data,
                    conn,
                    'station_status',
                    ['station_id','num_bikes_available','num_ebikes_available','num_docks_available','station_status','is_returning','is_renting'],
                    {'city':city,'operator':operator,'collected_time':current_epoch})
    conn.close()

if __name__ == "__main__":
    main_function()