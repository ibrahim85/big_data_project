# Run Nasser's location tool
cd geocoder_files/

# Uber Data: April to September 2014
# head -1 ../data/AprilSept2014/uber-raw-data-apr14.csv
for month in "apr" "may" "jun" "jul" "aug" "sep"
do
  python location_geocoder.py ../data/AprilSept2014/uber-raw-data-${month}14.csv 1 2
done

# Green Cab Data: April to September 2014
# head -1 ../data/AprilSept2014/green_tripdata_2014-04.csv
for month in "04" "05" "06" "07" "08" "09"
do
  python location_geocoder.py ../data/AprilSept2014/green_tripdata_2014-${month}.csv 6 5
done

# Yellow Cab Data: April to September 2014
# head -1 ../data/AprilSept2014/yellow_tripdata_2014-04.csv
for month in "04" "05" "06" "07" "08" "09"
do
  python location_geocoder.py ../data/AprilSept2014/yellow_tripdata_2014-${month}.csv 6 5
done

for f in ../data/AprilSept2014/*location; do mv "$f" "${f}_pickup.csv"; done

# Green Cab Data: April to September 2014
# head -1 ../data/AprilSept2014/green_tripdata_2014-04.csv
for month in "04" "05" "06" "07" "08" "09"
do
  python location_geocoder.py ../data/AprilSept2014/green_tripdata_2014-${month}.csv 8 7
done

# Yellow Cab Data: April to September 2014
# head -1 ../data/AprilSept2014/yellow_tripdata_2014-04.csv
for month in "04" "05" "06" "07" "08" "09"
do
  python location_geocoder.py ../data/AprilSept2014/yellow_tripdata_2014-${month}.csv 10 9
done

for f in ../data/AprilSept2014/*location; do mv "$f" "${f}_dropoff.csv"; done
