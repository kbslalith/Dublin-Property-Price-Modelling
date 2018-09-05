from daftlistings import Daft, SaleType
import csv


offset = 0
pages = True
daft_data = []

#Check the fastest way to write a list into csv format!
# while pages:

#for pages in range(1):
while pages:
    daft = Daft()
    daft.set_county('Dublin')
    #daft.set_area([])
    daft.set_listing_type(SaleType.PROPERTIES)
    daft.set_offset(offset)

    listings = daft.get_listings()

    if not listings:
        pages = False

    for listing in listings:
        link = listing.get_daft_link()
        county = listing.get_county()
        town = listing.get_town()
        address = listing.get_formalised_address()
        price = listing.get_price()
        dwelling_type = listing.get_dwelling_type()
        bedrooms = listing.get_num_bedrooms()
        bathrooms = listing.get_num_bathrooms()
        agent = listing.get_agent()
        facilities = listing.get_facilities()
        features = listing.get_features()
        description = listing.get_description()
        views = listing.get_views()
        date = listing.get_date()
        overview = listing.get_overview()
        area = listing.get_floor_area()
        ber = listing.get_ber()
        first_date = listing.get_first_listeddate()
        first_price = listing.get_first_price()
        get_location = listing.get_location()


        data1 = [link, county, town, address, price, dwelling_type, bedrooms, bathrooms, agent,
                facilities, features, date, views, description, overview, area, ber, first_date, first_price, get_location]

        with open ('houseprice_data.csv','a+') as file:
            writer=csv.writer(file, delimiter = ',')
            writer.writerow(data1)


        #daft_data.append(data1)

    offset += 20

   # writer=csv.writer(file, delimiter = ',')
   # for row in daft_data:
   #    writer.writerow(row)
