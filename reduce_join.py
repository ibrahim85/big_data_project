#!/usr/bin/python

### Mapper to join input, pickup, and dropoff for taxi data
### Jacqueline Gutman
### Nasser Zalmout

import sys
import fileinput

current_trip_key = None
current_source_key = None
buffered_original = None
buffered_pickup = ","
buffered_dropoff = ","

#input comes from STDIN (stream data that goes to the program)
infile = sys.stdin
#infile = fileinput.input()

for line in infile:
    join_key, values = line.strip().split("\t",1)
    if (join_key.find("uber") != -1):
        print values
        continue
    else:
        trip_key, source_key, source_index = join_key.split("^",2)
        if (trip_key != current_trip_key):
            if current_trip_key: # first entry for given trip key
                if not buffered_pickup:
                    buffered_pickup = ","
                if not buffered_dropoff:
                    buffered_dropoff = ","
                print "%s,%s,%s" % (buffered_original,
                    buffered_pickup, buffered_dropoff)
            current_trip_key = trip_key
            current_source_key = source_key
            buffered_original = None
            buffered_pickup = ","
            buffered_dropoff = ","

        # add current values to buffer
        if (int(source_index) == 0):
            buffered_original = values
        elif (int(source_index) == 1):
            buffered_dropoff = values
        elif (int(source_index) == 2):
            buffered_pickup = values

if current_trip_key:
    if not buffered_pickup:
        buffered_pickup = ","
    if not buffered_dropoff:
        buffered_dropoff = ","
    print "%s,%s,%s" % (buffered_original,
        buffered_pickup, buffered_dropoff)
