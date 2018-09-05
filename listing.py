from .request import Request

class Listing(object):
    def __init__(self, data, verbose=False):
        self._data = data
        self._verbose = verbose

    def get_price(self):
        """
       This method returns the price.
       :return:
       """
        try:
            return self._data.find('strong', {'class': 'price'}).text
        except Exception as e:
            if self._verbose:
                print(e.message)
            return

    def get_price_change(self):
        """
       This method returns any price change.
       :return:
       """
        try:
            return self._data.find('div', {'class': 'price-changes-sr'}).text
        except Exception as e:
            if self._verbose:
                print(e.message)
            return

    def get_upcoming_viewings(self):
        """
       Returns an array of upcoming viewings for a property.
       :return:
       """
        upcoming_viewings = []
        try:
            viewings = self._data.find_all('div', {'class': 'smi-onview-text'})
            for viewing in viewings:
                upcoming_viewings.append(viewing.text.strip())
            return upcoming_viewings
        except Exception as e:
            if self._verbose:
                print(e.message)
            return
        # for viewing in viewings:
            # upcoming_viewings.append(viewing.text.strip())
        # return upcoming_viewings

    def get_facilities(self):
        """
       This method returns the properties facilities.
       :return:
       """
        facilities = []
        link = self.get_daft_link()
        req = Request(verbose=self._verbose)
        soup = req.get(link)
        try:
            facility_table = soup.find('table', {'id': 'facilities'})
            list_items = facility_table.find_all(['li'])
            for li in list_items:
                facilities.append(li.text)
            return facilities
        except Exception as e:
            if self._verbose:
                print(e.message)
            return

        #for li in list_items:
            # facilities.append(li.text)
        # return facilities

    def get_features(self):
        """
       This method returns the properties features.
       :return:
       """
        features = []
        link = self.get_daft_link()
        req = Request(verbose=self._verbose)
        soup = req.get(link)
        try:
            feats = soup.find('div', {'id': 'features'})
            list_items = feats.find_all('li')
            for li in list_items:
                features.append(li.text)
            return features

        except Exception as e:
            if self._verbose:
                print(e.message)
            return

        #for li in list_items:
        #    features.append(li.text)
        # return features

    def get_formalised_address(self):
        """
       This method returns the formalised address.
       :return:
       """
        try:
            t = self._data.find('a').contents[0]
            s = t.split('-')
            a = s[0].strip()
            if 'SALE AGREED' in a:
                a = a.split()
                a = a[3:]
                a = ' '.join([str(x) for x in a])
            return a.lower().title().strip()
            
        except Exception as e:
            if self._verbose:
                print(e.message)
            return
        #s = t.split('-')
        #a = s[0].strip()
        # if 'SALE AGREED' in a:
        #     a = a.split()
        #     a = a[3:]
        #     a = ' '.join([str(x) for x in a])
        # return a.lower().title().strip()

    def get_address_line_1(self):
        """
       This method returns the first line of the address.
       :return:
       """
        formalised_address = self.get_formalised_address()
        if formalised_address is None:
            return
        try:
            address = formalised_address.split(',')
            return address[0].strip()
        except Exception as e:
            if self._verbose:
                print(e.message)
            return

        # return address[0].strip()

    def get_address_line_2(self):
        """
       This method returns the second line of the address.
       :return:
       """
        formalised_address = self.get_formalised_address()
        if formalised_address is None:
            return

        try:
            address = formalised_address.split(',')
        except Exception as e:
            if self._verbose:
                print(e.message)
            return

        if len(address) == 4:
            return address[1].strip()
        else:
            return

    def get_town(self):
        """
       This method returns the town name.
       :return:
       """
        formalised_address = self.get_formalised_address()
        if formalised_address is None:
            return
        try:
            address = formalised_address.split(',')
            return address[-2].strip()
        except Exception as e:
            if self._verbose:
                print(e.message)
            return

    def get_county(self):
        """
       This method returns the county name.
       :return:
       """
        formalised_address = self.get_formalised_address()

        if formalised_address is None:
            return

        try:
            address = formalised_address.split(',')
            return address[-1].strip()
        except Exception as e:
            if self._verbose:
                print(e.message)
            return

    def get_listing_image(self):
        """
       This method returns the listing image.
       :return:
       """

        req = Request(verbose=self._verbose)
        link = self.get_daft_link()
        soup = req.get(link)

        try:
            span = soup.find("span", {"class": "p1"})
            return span.find('img')['src']
        except Exception as e:
            if self._verbose:
                print(e.message)
            return

        # return span.find('img')['src']

    def get_agent(self):
        """
       This method returns the agent name.
       :return:
       """
        try:
            agent = self._data.find('ul', {'class': 'links'}).text
            return agent.split(':')[1].strip()
        except Exception as e:
            if self._verbose:
                print(e.message)
            return

    def get_agent_url(self):
        """
       This method returns the agent's url.
       :return:
       """
        try:
            agent = self._data.find('ul', {'class': 'links'})
            links = agent.find_all('a')
            return links[1]['href']
        except Exception as e:
            if self._verbose:
                print(e.message)
            return

    def get_contact_number(self):
        """
       This method returns the contact phone number.
       :return:
       """
        req = Request(verbose=self._verbose)
        link = self.get_daft_link()
        soup = req.get(link)
        try:
            number = soup.find('div', {'class': 'phone-number'}).text
            return number.strip()
        except Exception as e:
            if self._verbose:
                print(e.message)
            return

        # return number.strip()

    def get_daft_link(self):
        """
       This method returns the url of the listing.
       :return:
       """
        link = self._data.find('a', href=True)
        try:
            return 'http://www.daft.ie' + link['href']
        except Exception as e:
            if self._verbose:
                print(e.message)
            return

    def get_dwelling_type(self):
        """
       This method returns the dwelling type.
       :return:
       """
        try:
            info = self._data.find('ul', {"class": "info"}).text
            s = info.split('|')
            return s[0].strip()
        except Exception as e:
            if self._verbose:
                print(e.message)
            return

        # s = info.split('|')
        # return s[0].strip()

    def get_posted_since(self):
        """
       This method returns the date the listing was entered.
       :return:
       """
        try:
            info = self._data.find('div', {"class": "date_entered"}).text
            s = info.split(':')
            return s[-1].strip()
        except Exception as e:
            if self._verbose:
                print(e.message)
            return

        # s = info.split(':')
        # return s[-1].strip()

    def get_num_bedrooms(self):
        """
       This method gets the number of bedrooms.
       :return:
       """
        try:
            info = self._data.find('ul', {"class": "info"}).text
            s = info.split('|')
            nb = s[1].strip()
            return int(nb.split()[0])
        except Exception as e:
            if self._verbose:
                print(e.message)
            return

        # s = info.split('|')
        # nb = s[1].strip()
        # return int(nb.split()[0])

    def get_num_bathrooms(self):
        """
       This method gets the number of bathrooms.
       :return:
       """
        try:
            info = self._data.find('ul', {"class": "info"}).text
            s = info.split('|')
            nb = s[2].strip()
            return int(nb.split()[0])
        except Exception as e:
            if self._verbose:
                print(e.message)
            return
        #NOTE_TO_SELF : Shifting the code below into the try statement solved the problem
        # s = info.split('|')
        # nb = s[2].strip()
        # return int(nb.split()[0])

    def get_description(self):
        """
       This method gets the property description.
       :return:
       """
        description = []
        link = self.get_daft_link()
        req = Request(verbose=self._verbose)
        soup = req.get(link)
        try:
            descript = soup.find('div', {'id':'description'})
            loc =  descript.select('h3')
            for text in (loc[0].find_next_siblings(text = True)):
                description.append(text.strip())
            return description

        except Exception as e:
            if self._verbose:
                print(e.message)
            return
        #loc[0] has property Description Information
        # for text in (loc[0].find_next_siblings(text = True)):
        #     description.append(text.strip())
        # return description

    def get_views(self):
        """
       This method gets the number of views.
       :return:
       """
        link = self.get_daft_link()
        req = Request(verbose=self._verbose)
        soup = req.get(link)
        try:
            descript = soup.find('div', {'id':'description'})
            loc =  descript.select('h3')
            for stuff in loc:
                data = (stuff.get_text())
                if data == "Date Entered/Renewed:":
                    info = (stuff.find_next_siblings(text = True))
                    # info[0] has date entered information
                    return int(info[1].strip().replace(",", ""))

        except Exception as e:
            if self._verbose:
                print(e.message)
            return


    def get_date(self):
        """
       This method gets the date entered/renewed.
       :return:
       """
        link = self.get_daft_link()
        req = Request(verbose=self._verbose)
        soup = req.get(link)
        try:
            descript = soup.find('div', {'id':'description'})
            loc =  descript.select('h3')
            for stuff in loc:
                data = (stuff.get_text())
                if data == "Date Entered/Renewed:":
                    info = (stuff.find_next_siblings(text = True))
                    info2 = (info[0].strip())
                    if " (" in info2:
                        info3 = info2.split(" (", 1)
                        return info3[0]
                    else:
                        return info2
                            
        except Exception as e:
            if self._verbose:
                print(e.message)
            return
        # for stuff in loc:
        #     data = (stuff.get_text())
        #     if data == "Date Entered/Renewed:":
        #         info = (stuff.find_next_siblings(text = True))
        #         info2 = (info[0].strip())
        #         if " (" in info2:
        #             info3 = info2.split(" (", 1)
        #             return info3[0]
        #         else:
        #             return info2


    def get_overview(self):
        """
       This method returns the property overview.
       :return:
       """
        overview = []
        link = self.get_daft_link()
        req = Request(verbose=self._verbose)
        soup = req.get(link)
        try:
            overv = soup.find('div', {'id': 'overview'})
            list_items = overv.find_all('li')
            for li in list_items:
                overview.append(li.text)
            return overview

        except Exception as e:
            if self._verbose:
                print(e.message)
            return

        # for li in list_items:
        #     overview.append(li.text)
        # return overview

    def get_floor_area(self):
        """
       This method gets the overall floor area in metres squared.
       :return:
       """

        link = self.get_daft_link()
        req = Request(verbose=self._verbose)
        soup = req.get(link)
        try:
            area = soup.find('div', {'class': 'smi-tab-content'})
            list_items = area.find_all()
            for stuff in list_items:
                data = (stuff.get_text())
                if data == "Overall Floor Area:":
                    info = (stuff.find_next_siblings(text = True))
                    info2 = str(info[0].strip())
                    if " Sq. Metres" in info2:
                        info3 = info2.split(" Sq. Metres", 1)
                        return info3[0]
        except Exception as e:
            if self._verbose:
                print(e.message)
            return

        # for stuff in list_items:
        #     data = (stuff.get_text())
        #     if data == "Overall Floor Area:":
        #         info = (stuff.find_next_siblings(text = True))
        #         info2 = str(info[0].strip())
        #         if " Sq. Metres" in info2:
        #             info3 = info2.split(" Sq. Metres", 1)
        #             return info3[0]
        #             # return (info[0].strip())

    def get_ber(self):
        """
       This method gets the BER.
       :return:
       """
        overview = []
        link = self.get_daft_link()
        req = Request(verbose=self._verbose)
        soup = req.get(link)
        try:
            area = soup.find('span', {'class': 'ber-hover'})
            list_items = area.find_all('span')

        except Exception as e:
            if self._verbose:
                print(e.message)
            return

        for li in list_items:
            overview.append(li)

#        return overview[0]
        data = str(overview[0])
        if data[21] == "G" or data[21] == "F":
            return data[21]
        else:
            return data[21] + data[22]

    def get_area_size(self):
        """
       This method returns the area size. This method should only be called when retrieving commercial type listings.
       :return:
       """
        try:
            info = self._data.find('ul', {"class": "info"}).text
        except Exception as e:
            if self._verbose:
                print(e.message)
            return
        s = info.split('|')
        print (s)
        return s[1].strip()

    def get_first_listeddate(self):
        """
       This method gets the first listed date.
       :return:
       """

        link = self.get_daft_link()
        req = Request(verbose=self._verbose)
        soup = req.get(link)
        try:

            date = soup.find('div', {'id': 'pch-view'})
            frst_list = date.find_all('span',{'class' : 'price-changes-sr first-listed'})
            x = frst_list[0].find_next_siblings()
            #print (x.text.strip()) #This wont work
            for date in x:
                return (date.text.strip())

        except Exception as e:
            if self._verbose:
                print(e.message)
            return

    def get_first_price(self):
        """
       This method gets the first listed price.
       :return:
       """

        link = self.get_daft_link()
        req = Request(verbose=self._verbose)
        soup = req.get(link)
        try:

            date = soup.find('div', {'id': 'pch-view'})
            tp = date.find_all('div',{'class' : 'pch-smi-searchresult'})
            price = tp[-1].find('span',{'class':'pch-smi-price'})
            return (price.text.strip())

        except Exception as e:
            if self._verbose:
                print(e.message)
            return


    def contact_advertiser(self, name, email, contact_number, message):
        """
       This method allows you to contact the advertiser of a listing.
       :param name: Your name
       :param email: Your email address.
       :param contact_number: Your contact number.
       :param message: Your message.
       :return:
       """
        req = Request(verbose=self._verbose)
        link = self.get_daft_link()
        soup = req.get(link)

        ad_search_type = soup.find('input', {'id': 'ad_search_type'})
        agent_id = soup.find('input', {'id': 'agent_id'})
        ad_id = soup.find('input', {'id': 'ad_id'})

        response = req.post('https://www.daft.ie/ajax_endpoint.php?', params={
            'action': 'daft_contact_advertiser',
            'from': name,
            'email': email,
            'message': message,
            'contact_number': contact_number,
            'type': ad_search_type['value'],
            'agent_id': agent_id['value'],
            'id': ad_id['value']
        })

        if self._verbose:
            print("Status code: %d" % response.status_code)
        return response.status_code == 200


    def get_location(self):
        """
       This method gets the first listed price.
       :return:
       """

        link = self.get_daft_link()
        req = Request(verbose=self._verbose)
        soup = req.get(link)
        try:

            print(soup)
        except Exception as e:
            pass
            return


    def as_dict(self):
        """
       Return a Listing object as Dictionary
       :return: dict
       """
        return {
            'price': self.get_price(),
            'price_change': self.get_price_change(),
            'viewings': self.get_upcoming_viewings(),
            'facilities': self.get_features(),
            'features': self.get_features(),
            'description': self.get_description(),
            'views':self.get_views(),
            'date': self.get_date(),
            'formalised_address': self.get_formalised_address(),
            'address_line_1': self.get_address_line_1(),
            'address_line_2': self.get_address_line_2(),
            'town': self.get_town(),
            'county': self.get_county(),
            'listing_image': self.get_listing_image(),
            'agent': self.get_agent(),
            'agent_url': self.get_agent_url(),
            'contact_number': self.get_contact_number(),
            'daft_link': self.get_daft_link(),
            'dwelling_type': self.get_dwelling_type(),
            'posted_since': self.get_posted_since(),
            'num_bedrooms': self.get_num_bedrooms(),
            'num_bathrooms': self.get_num_bathrooms(),
            'area_size': self.get_area_size(),
            'first_date':self.get_first_listeddate(),
            'first_price':self.get_first_price()
            'get-location':self.get_location()
        }
