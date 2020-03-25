'''
Scrap all text from covid-19 wiki page for every country/statue i could find
'''


import requests
from bs4 import BeautifulSoup
import sys
import pandas as pd


URL = 'https://en.wikipedia.org/w/index.php?title=Special:Search&limit=500&offset=0&profile=default&search=2020+coronavirus+pandemic+in+&advancedSearch-current={}&ns0=1'
page = requests.get(URL)

soup = BeautifulSoup(page.content, 'html.parser')
results = soup.find(id='mw-content-text')


job_elems = results.find_all('div', class_='mw-search-result-heading')

countries = []
countries_text = []

for job_elem in job_elems:
    if(job_elem.text.find("2020 coronavirus pandemic in") == 0):
        print(job_elem.text)
        countries.append(job_elem.text.replace("2020 coronavirus pandemic in ", ''))
        link = job_elem.find('a', href=True)
        link_for_this_country = ('https://en.wikipedia.org/' + link['href'])
        country_page = requests.get(link_for_this_country)

        country_page.raise_for_status()
        #Just to raise the status code
        wiki = BeautifulSoup(country_page.text, 'html.parser')
        elems = wiki.select('p')
        country_text = ''
        for i in range(len(elems)):
            country_text = country_text + (elems[i].getText())
        countries_text.append(country_text)


df = pd.DataFrame(list(zip(countries, countries_text)),
               columns =['Country', 'Wiki_text'])


df.to_csv('wiki_text_by_country.csv', index = False, header=True)
