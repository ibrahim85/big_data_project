for color in "yellow" "green"
do
  for month in "04" "05" "06" "07" "08" "09"
  do
    hadoop fs -copyFromLocal data/AprilSept2014/${color}_tripdata_2014-${month}.csv ${color}_tripdata_2014-${month}.csv

    hadoop fs -copyFromLocal data/AprilSept2014/${color}_tripdata_2014-${month}.csv_location_dropoff.csv ${color}_tripdata_2014-${month}.csv_location_dropoff.csv

    hadoop fs -copyFromLocal data/AprilSept2014/${color}_tripdata_2014-${month}.csv_location_pickup.csv ${color}_tripdata_2014-${month}.csv_location_pickup.csv
  done
done

for month in "apr" "may" "jun" "jul" "aug" "sep"
do
  hadoop fs -copyFromLocal data/AprilSept2014/uber-raw-data-${month}14.csv_location_pickup.csv uber-raw-data-${month}14.csv_location_pickup.csv
done

username="jg3862"

declare -A months=( ["04"]="april" ["05"]="may" ["06"]="june" ["07"]="july" ["08"]="august" ["09"]="september")
declare -A ubermonths=( ["apr"]="april" ["may"]="may" ["jun"]="june" ["jul"]="july" ["aug"]="august" ["sep"]="september")

for color in "yellow" "green"
do
  for month in "04" "05" "06" "07" "08" "09"
  do
    hjs -D mapreduce.job.reduces=1 -files map_join.py,reduce_join.py -mapper map_join.py -reducer reduce_join.py -input /user/${username}/${color}_tripdata_2014-${month}.csv -input /user/${username}/${color}_tripdata_2014-${month}.csv_location_dropoff.csv -input /user/${username}/${color}_tripdata_2014-${month}.csv_location_pickup.csv -output ${color}trips/${months[$month]} -partitioner org.apache.hadoop.mapred.lib.KeyFieldBasedPartitioner -jobconf stream.map.output.field.separator=^ -jobconf stream.num.map.output.key.fields=3 -jobconf mapreduce.map.output.key.field.separator=^ -jobconf num.key.fields.for.partition=2
  done
done

for month in "apr" "may" "jun" "jul" "aug" "sep"
do
  hjs -D mapreduce.job.reduces=1 -files map_join.py,reduce_join.py -mapper map_join.py -reducer reduce_join.py -input /user/${username}/uber-raw-data-${month}14.csv_location_pickup.csv -output ubertrips/${ubermonths[$month]}
done

for color in "yellow" "green" "uber"
do
  for month in "april" "may" "june" "july" "august" "september"
  do
    hadoop fs -get ${color}trips/${month} data/AprilSept2014/${color}_${month}
  done
done

for color in "yellow" "green" "uber"
do
    hjs -D mapreduce.job.reduces=1 -files map_bucket.py,combine_bucket.py,reduce_bucket.py -mapper map_bucket.py -combiner combine_bucket.py -reducer reduce_bucket.py -input /user/${username}/${color}trips/april/part-00000 -input /user/${username}/${color}trips/may/part-00000 -input /user/${username}/${color}trips/june/part-00000 -input /user/${username}/${color}trips/july/part-00000 -input /user/${username}/${color}trips/august/part-00000 -input /user/${username}/${color}trips/september/part-00000 -output ${color}trips/buckets
  done
done

hadoop fs -get yellowtrips/buckets data/AprilSept2014/yellow_buckets
hadoop fs -get greentrips/buckets data/AprilSept2014/green_buckets
hadoop fs -get ubertrips/buckets data/AprilSept2014/uber_buckets
