#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Scrape child asthma prevalence rates per state from pdf

"""

from tabula import read_pdf
import os
import pandas
os.chdir('/Users/kaitlincornwell/Documents/BIOSTATS 696 - Spatial Data Analysis/biostat-696-project')

filename = "AsthmaRates2016.pdf"
left, top, width, height = 76.71, 74.12, 456.63, 584.9
pg1 = read_pdf(filename, pages = 1, lattice = False, 
               area = (top, left, top+height, left + width))
pg1 = pg1.drop(columns = ['Unnamed: 4', '|| Weighted', '||', 'Unnamed: 8', 'Unnamed: 10'])
pg1.columns = ['state', 'sampled_count', 'prevalence', 'prev_error', 'prev_int', 'total_count', 'total_int']
pg1 = pg1.drop([0])

height = 123.57
pg2 = read_pdf(filename, pages = 2, lattice = False, 
               area = (top, left, top+height, left + width))
pg2 = pg2.drop(columns = ['Unnamed: 4', '||', '||.1', 'Unnamed: 8', 'Unnamed: 9', 'Unnamed: 11'])
pg2.columns = ['state', 'sampled_count', 'prevalence', 'prev_error', 'prev_int', 'total_count', 'total_int']
pg2 = pg2.drop([4, 5])

prev = pandas.concat([pg1, pg2])
prev.to_csv("2016prevalence", index = False)