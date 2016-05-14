mkdir -p ./data/AprilSept2014

cat data_sources.txt | while read line
do
	# do something with $line here
	echo $line
	wget -P ./data/AprilSept2014 $line
done
