from mpl_toolkits.basemap import Basemap
import matplotlib.pyplot as plt

m = Basemap(projection = 'mill',llcrnrlat = 25,llcrnrlon=-130,urcrnrlat=50,urcrnrlon = -60,resolution='1')
m.drawcostlines()
m.drawcountries(linewidth=2)
m.drawstates(color='b')
m.bluemarble()
plt.title('Basemap')
plt.show()
