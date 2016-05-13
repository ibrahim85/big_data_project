from mpl_toolkits.basemap import Basemap
import matplotlib.pyplot as plt
import numpy as np
import csv, sys
from matplotlib.colors import LinearSegmentedColormap

def read_data(file_name, lat_index, lon_index):
	long = []
	lat = []

	with open(file_name, 'rb') as f:
		reader = csv.reader(f)
		for row in reader:
			try:
				longitude = float(row[lon_index])
				latitude = float(row[lat_index])

				long.append(longitude)
				lat.append(latitude)
			except:
				continue

	return long, lat

def draw(long, lat):

	max_lat = -999
	min_lat = 999
	max_lon = -999
	min_lon = 999

	for longitude, latitude in zip(long, lat):
		if longitude > max_lon:
				max_lon = longitude 
		if longitude < min_lon:
				min_lon = longitude 
		if latitude > max_lat:
				max_lat = latitude 
		if latitude < min_lat:
				min_lat = latitude 

	plt.figure(figsize=(10,8))

	map = Basemap(projection='merc',
			resolution='l',
			llcrnrlat=min_lat-0.01,
			llcrnrlon=min_lon-0.01,
			urcrnrlat=max_lat+0.01,
			urcrnrlon=max_lon+0.01)

	map.drawmapboundary()

	map.readshapefile('nyc/nyc', 'comarques')
	lats = lat
	lons = long
	m = map

	db = 0.002 # bin padding
	lon_bins = np.linspace(min(lons)-db, max(lons)+db, 20+1) # 10 bins
	lat_bins = np.linspace(min(lats)-db, max(lats)+db, 23+1) # 13 bins
					
	density, _, _ = np.histogram2d(lats, lons, [lat_bins, lon_bins])

	# Turn the lon/lat of the bins into 2 dimensional arrays ready
	# for conversion into projected coordinates
	lon_bins_2d, lat_bins_2d = np.meshgrid(lon_bins, lat_bins)

	# convert the bin mesh to map coordinates:
	xs, ys = m(lon_bins_2d, lat_bins_2d) # will be plotted using pcolormesh

	# define custom colormap, white -> nicered, #E6072A = RGB(0.9,0.03,0.16)
	cdict = {'red':((0.0,  1.0,  1.0),
					(1.0,  0.9,  1.0)),
			'green':((0.0,  1.0,  1.0),
					(1.0,  0.03, 0.0)),
			'blue': ((0.0,  1.0,  1.0),
					(1.0,  0.16, 0.0))}
	custom_map = LinearSegmentedColormap('custom_map', cdict)
	plt.register_cmap(cmap=custom_map)


	# add histogram squares and a corresponding colorbar to the map:
	plt.pcolormesh(xs, ys, density, cmap="custom_map")

	cbar = plt.colorbar(orientation='horizontal', shrink=0.925, aspect=30, fraction=0.2,pad=0.02)
	cbar.set_label('Number of trips',size=10)

	# translucent blue scatter plot of epicenters above histogram:    
	x,y = m(lons, lats)
	m.plot(x, y, 'o', markersize=2,zorder=2, markerfacecolor='#424FA4',markeredgecolor="none", alpha=0.05)
		
					
	# http://matplotlib.org/basemap/api/basemap_api.html#mpl_toolkits.basemap.Basemap.drawmapscale
	m.drawmapscale(-119, 80, -119, 80, 2000, barstyle='fancy', yoffset=200)
					
	# make image bigger:
	plt.gcf().set_size_inches(11,9)
	plt.show()

if __name__ == "__main__":
	file_name = sys.argv[1]
	lat_index = int(sys.argv[2])
	lon_index = int(sys.argv[3])

	longs, lats = read_data(file_name, lat_index, lon_index)
	draw(longs, lats)



