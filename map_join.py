#!/usr/bin/python

### Mapper to join input, pickup, and dropoff for taxi data
### Jacqueline Gutman
### Nasser Zalmout

import sys
import csv
import StringIO
import os
from datetime import datetime
import fileinput

#input comes from STDIN (stream data that goes to the program)
infile = sys.stdin
#infile = fileinput.input()
input_filename = os.environ['mapreduce_map_input_file']
source_key = input_filename.partition(".csv")[0]
date_format = "%Y-%m-%d %H:%M:%S"

source_index = 0 # original input file
if (input_filename.find("dropoff") != -1):
    source_index = 1 # contains dropoff neighborhood + distance
elif (input_filename.find("pickup") != -1):
    source_index = 2 # contains pickup neighborhood + distance

source_type = 0 # uber data
if (input_filename.find("yellow") != -1):
    source_type = 1 # yellow cab data
elif (input_filename.find("green") != -1):
    source_type = 2 # green cab data

if (source_index == 0): # original input data has headers
    next(infile)

for line in infile:
    csv_file = StringIO.StringIO(line)
    csv_reader = csv.reader(csv_file)

    for record in csv_reader:
        if (len(record) == 0): # skip blank rows in the input file
            continue
        # key gives yellow/green/uber + month ^ original/dropoff/pickup
        key = "%s^%d" % (source_key, source_index)
        if (source_index == 0): # original input data
            if (source_type == 1): # yellow cab data
                # trip_key gives pickup_datetime, dropoff_datetime
                # pickup_latitude, pickup_longitude,
                # dropoff_latitude, dropoff_longitude
                p_datetime = record[1]
                d_datetime = record[2]
                trip_key = "%s,%s,%0.5f,%0.5f,%0.5f,%0.5f" % (
                                p_datetime, d_datetime,
                                float(record[6]), float(record[5]),
                                float(record[10]), float(record[9]))

                pickup_date = datetime.strptime(p_datetime, date_format)
                dropoff_date = datetime.strptime(d_datetime, date_format)

                p_hour = int(pickup_date.strftime("%H"))
                p_weekday = int(pickup_date.strftime("%w"))
                p_weeknum = int(pickup_date.strftime("%U"))
                d_hour = int(dropoff_date.strftime("%H"))
                d_weekday = int(dropoff_date.strftime("%w"))
                d_weeknum = int(dropoff_date.strftime("%U"))

                distance = "%0.2f" % float(record[4])
                fare = "%0.2f" % float(record[12])
                total = "%0.2f" % float(record[17])
                passengers = int(record[3])
                values = "%d,%d,%d,%d,%d,%d,%s,%s,%s,%d" % (
                            p_hour, p_weekday,
                            p_weeknum, d_hour, d_weekday, d_weeknum, distance,
                            fare, total, passengers)

                print "%s^%s\t%s" % (trip_key, key, values)

            elif (source_type == 2): # green cab data
                # trip_key gives pickup_datetime, dropoff_datetime
                # pickup_latitude, pickup_longitude,
                # dropoff_latitude, dropoff_longitude
                p_datetime = record[1]
                d_datetime = record[2]
                trip_key = "%s,%s,%0.5f,%0.5f,%0.5f,%0.5f" % (
                                p_datetime, d_datetime,
                                float(record[6]), float(record[5]),
                                float(record[8]), float(record[7]))

                pickup_date = datetime.strptime(p_datetime, date_format)
                dropoff_date = datetime.strptime(d_datetime, date_format)

                p_hour = int(pickup_date.strftime("%H"))
                p_weekday = int(pickup_date.strftime("%w"))
                p_weeknum = int(pickup_date.strftime("%U"))
                d_hour = int(dropoff_date.strftime("%H"))
                d_weekday = int(dropoff_date.strftime("%w"))
                d_weeknum = int(dropoff_date.strftime("%U"))

                distance = "%0.2f" % float(record[10])
                fare = "%0.2f" % float(record[11])
                total = "%0.2f" % float(record[17])
                passengers = int(record[9])
                values = "%d,%d,%d,%d,%d,%d,%s,%s,%s,%d" % (
                            p_hour, p_weekday,
                            p_weeknum, d_hour, d_weekday, d_weeknum, distance,
                            fare, total, passengers)

                print "%s^%s\t%s" % (trip_key, key, values)

            else: # uber data, no pickup + dropoff to be joined
                continue # we don't need original input, just pickup file
        else: # pickup or dropoff data
            if (source_type == 1): # yellow cab data - dropoffs / pickups
                p_datetime = record[1]
                d_datetime = record[2]
                trip_key = "%s,%s,%0.5f,%0.5f,%0.5f,%0.5f" % (
                                p_datetime, d_datetime,
                                float(record[6]), float(record[5]),
                                float(record[10]), float(record[9]))
                values = "%s,%0.3f" % (record[18], float(record[19]))

                print "%s^%s\t%s" % (trip_key, key, values)

            elif (source_type == 2): # green cab data - dropoffs / pickups
                p_datetime = record[1]
                d_datetime = record[2]
                trip_key = "%s,%s,%0.5f,%0.5f,%0.5f,%0.5f" % (
                                p_datetime, d_datetime,
                                float(record[6]), float(record[5]),
                                float(record[8]), float(record[7]))
                values = "%s,%0.3f" % (record[22], float(record[23]))

                print "%s^%s\t%s" % (trip_key, key, values)

            elif (source_type == 0 and source_index == 2):
                # uber dropoff data does not exist, ignore
                # uber pickup data just needs to be reformatted
                p_datetime = record[0]
                pickup_date = datetime.strptime(p_datetime, "%m/%d/%Y %H:%M:%S")
                p_hour = int(pickup_date.strftime("%H"))
                p_weekday = int(pickup_date.strftime("%w"))
                p_weeknum = int(pickup_date.strftime("%U"))

                # pickup neighborhood + distance to subway
                values = "%d,%d,%d,%s,%0.3f" % (p_hour, p_weekday,
                            p_weeknum, record[4], float(record[5]))

                print "%s\t%s" % (key, values)
