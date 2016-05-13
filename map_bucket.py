#!/usr/bin/python

### Mapper to join input, pickup, and dropoff for taxi data
### Jacqueline Gutman
### Nasser Zalmout

import sys
import csv
import StringIO
import os
import fileinput

#input comes from STDIN (stream data that goes to the program)
infile = sys.stdin
#infile = fileinput.input()
input_filename = os.environ['mapreduce_map_input_file']
#input_filename = "/user/jg3862/greentrips/april/part-00000"

source_type = 0 # uber data
if (input_filename.find("yellow") != -1):
    source_type = 1 # yellow cab data
elif (input_filename.find("green") != -1):
    source_type = 2 # green cab data

for line in infile:
    csv_file = StringIO.StringIO(line)
    csv_reader = csv.reader(csv_file)

    for record in csv_reader:
        if (len(record) == 0): # skip blank rows in the input file
            continue
        if (source_type == 1 or source_type == 2):
            color = "yellow" if source_type == 1 else "green"
            if (record[0] == "None"):
                continue
            pickup_hour = record[0]
            pickup_weekday = record[1]
            pickup_neighborhood = record[10]
            if not pickup_neighborhood:
                continue
            join_key = "%s^%s^%s^%s" % (color, pickup_hour, pickup_weekday,
                                        pickup_neighborhood)
            # values = count, distance_traveled, fare_amount, total_amount,
                # num_passengers, pickup_dist_subway,dropoff_dist_subway

            values = "%d,%s,%s,%s,%s,%s,%s" % (1, record[6], record[7],
                        record[8], record[9], record[11], record[13])
            # secondary_values = pickup_weeknum, dropoff_hour, dropoff_weekday,
    		    # dropoff_weeknum, dropoff_neighborhood
            secondary_values = "%s,%s,%s,%s,%s" % (record[2], record[3],
                        record[4], record[5], record[12])
            print "%s\t%s" % (join_key, values)

        else: # uber data
            if (record[0] == "None"):
                continue
            pickup_hour = record[0]
            pickup_weekday = record[1]
            pickup_neighborhood = record[3]
            if not pickup_neighborhood:
                continue
            join_key = "%s^%s^%s^%s" % ("uber", pickup_hour, pickup_weekday,
                                        pickup_neighborhood)
            # values = count, avg_pickup_distance_subway, pickup_weeknum
            values = "%d,%s,%s" % (1, record[4], record[2])
            print "%s\t%s" % (join_key, values)
