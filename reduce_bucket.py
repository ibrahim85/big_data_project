#!/usr/bin/python

### Mapper to join input, pickup, and dropoff for taxi data
### Jacqueline Gutman
### Nasser Zalmout

import sys
import fileinput

current_color = None
current_bucket = None
count, distance_traveled, fare_amount, total_amount = 0, 0.0, 0.0, 0.0
num_passengers, pickup_dist_subway, dropoff_dist_subway = 0.0, 0.0, 0.0

#input comes from STDIN (stream data that goes to the program)
infile = sys.stdin
#infile = fileinput.input()

for line in infile:
    join_key, values = line.strip().split("\t", 1)
    color, bucket = join_key.split("^", 1)

    if (color == "uber"):
        value_list = values.split(",", 2)
        if (bucket != current_bucket):
            if current_bucket:
                pickup_dist_subway = pickup_dist_subway / count
                print "%s\t%d,%0.2f" % (current_bucket, count,
                    pickup_dist_subway)

            current_bucket = bucket
            current_color = color
            count = int(value_list[0])
            try:
                pickup_dist_subway = float(value_list[1])
            except ValueError:
                pickup_dist_subway = 0.0

        else:
            count = count + int(value_list[0])
            try:
                pickup_dist_subway = pickup_dist_subway + float(value_list[1])
            except ValueError:
                pass

    else: # if color == "yellow" or "green"
        value_list = values.split(",", 6)
        if (bucket != current_bucket):
            if current_bucket:
                distance_traveled = distance_traveled / count
                fare_amount = fare_amount / count
                total_amount = total_amount / count
                num_passengers = num_passengers / count
                pickup_dist_subway = pickup_dist_subway / count
                dropoff_dist_subway = dropoff_dist_subway / count

                print "%s\t%d,%0.2f,%0.2f,%0.2f,%0.2f,%0.2f,%0.2f" % (
                    current_bucket, count, distance_traveled, fare_amount,
                    total_amount, num_passengers, pickup_dist_subway,
                    dropoff_dist_subway)

            current_bucket = bucket
            current_color = color

            count = int(value_list[0])
            try:
                distance_traveled = float(value_list[1])
            except ValueError:
                distance_traveled = 0.0
            try:
                fare_amount = float(value_list[2])
            except ValueError:
                fare_amount = 0.0
            try:
                total_amount = float(value_list[3])
            except ValueError:
                total_amount = 0.0
            try:
                num_passengers = float(value_list[4])
            except ValueError:
                num_passengers = 0.0
            try:
                pickup_dist_subway = float(value_list[5])
            except ValueError:
                pickup_dist_subway = 0.0
            try:
                dropoff_dist_subway = float(value_list[6])
            except ValueError:
                dropoff_dist_subway = 0.0

        else:
            count = count + int(value_list[0])
            try:
                distance_traveled = distance_traveled + float(value_list[1])
            except ValueError:
                pass
            try:
                fare_amount = fare_amount + float(value_list[2])
            except ValueError:
                pass
            try:
                total_amount = total_amount + float(value_list[3])
            except ValueError:
                pass
            try:
                num_passengers = num_passengers + float(value_list[4])
            except ValueError:
                pass
            try:
                pickup_dist_subway = pickup_dist_subway + float(value_list[5])
            except ValueError:
                pass
            try:
                dropoff_dist_subway = dropoff_dist_subway + float(value_list[6])
            except ValueError:
                pass

if current_bucket:
    if (current_color == "uber"):
        pickup_dist_subway = pickup_dist_subway / count
        print "%s\t%d,%0.2f" % (current_bucket, count,
            pickup_dist_subway)
    else:
        distance_traveled = distance_traveled / count
        fare_amount = fare_amount / count
        total_amount = total_amount / count
        num_passengers = num_passengers / count
        pickup_dist_subway = pickup_dist_subway / count
        dropoff_dist_subway = dropoff_dist_subway / count

        print "%s\t%d,%0.2f,%0.2f,%0.2f,%0.2f,%0.2f,%0.2f" % (
            current_bucket, count, distance_traveled, fare_amount, total_amount,
            num_passengers, pickup_dist_subway, dropoff_dist_subway)
