import shapegeocode, csv, math, numpy, sys
from scipy.spatial import cKDTree
 
def coordinates_distance(lat1, long1, lat2, long2):
    degrees_to_radians = math.pi/180.0
         
    phi1 = (90.0 - lat1)*degrees_to_radians
    phi2 = (90.0 - lat2)*degrees_to_radians
         
    theta1 = long1*degrees_to_radians
    theta2 = long2*degrees_to_radians
     
    cos = (math.sin(phi1)*math.sin(phi2)*math.cos(theta1 - theta2) + math.cos(phi1)*math.cos(phi2))
    arc = math.acos(cos)

    return arc

def get_subway_points(subway_csv):
	subway_points = []
	with open(subway_csv) as f:
		reader = csv.reader(f)
		for row in reader:
			try:
				geom_splits = row[1].split(" ")
				lon = float(geom_splits[1].strip().lstrip("("))
				lat = float(geom_splits[2].strip().rstrip(")"))

				subway_points.append((lat,lon))
			except:
				continue

	return subway_points


def get_zip_to_hood_mapping(zip_file):
	zip_to_hood = {}
	with open(zip_file) as f:
		reader = csv.reader(f)
		for row in reader:
			for item in row[1:]:
				zip_to_hood[item] = row[0].strip()
	return zip_to_hood


if __name__ == "__main__":

	zip_file = "nyc_zip_codes.csv"
	subway_csv = "DOITT_SUBWAY_ENTRANCE_01_13SEPT2010.csv"
	nyc_shapefile = "nyc/nyc"

	file_name = sys.argv[1]
	lat_index = int(sys.argv[2])
	lon_index = int(sys.argv[3])

	subway_points = get_subway_points(subway_csv)
	tree = cKDTree(subway_points)

	zip_to_hood = get_zip_to_hood_mapping(zip_file)

	gc = shapegeocode.geocoder(nyc_shapefile)
	out_file = open(file_name+"_location", 'w')
	writer = csv.writer(out_file, delimiter=',')

	removed_count = 0
	total_count = 0

	with open(file_name, 'rb') as f:
		print "Running..."
		reader = csv.reader(f)
		for row in reader:
			try:
				lat = float(row[lat_index])
				lon = float(row[lon_index])

				subway_distance, index = tree.query(numpy.array([lat, lon]), k=1)

				#To get the distance in kilometers, multiply by 6373
				subway_distance = 3960*coordinates_distance(subway_points[index][0], subway_points[index][1], lat, lon)

				zip = gc.geocode(lat,lon)['postalCode']

				row.append(zip_to_hood[zip])
				row.append(subway_distance)
				writer.writerow(row)

				total_count += 1
			except:
				removed_count += 1
				continue

	print "Removed "+str(removed_count)+" trips, that is "+str((removed_count/float(total_count))*100)+"% of the total trips in this file."

