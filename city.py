import pandas as pd
import numpy as np
import math
import csv
import geopy.distance

#data = pd.read_csv("luas_cor.csv",delimiter=',')
data = pd.read_csv("city.csv",delimiter=',')
house = (data[['latitude', 'longitude']]).dropna()
bus = (data[['Bus_lat', 'Bus_lon']]).dropna()


final_list = []

for y in range(len(house.index)):
    all_distaces = []
    for x in range(len(bus.index)):
        cor1 = (bus['Bus_lon'][x],bus['Bus_lat'][x])
        cor2 = (house['longitude'][y],house['latitude'][y])
        distance = geopy.distance.vincenty(cor1, cor2).km
        all_distaces.append(distance)
    final_list.append(min(all_distaces))


with open('dist_to_city.csv','a+') as file:
	writer = csv.writer(file, delimiter = ',')
	writer.writerow(final_list)
