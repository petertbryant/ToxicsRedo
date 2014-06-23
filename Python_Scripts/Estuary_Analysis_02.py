# -*- coding: utf-8 -*-
"""
Created on Mon Jun 23 10:46:38 2014

@author: MPsaris

Script using salinity and conductivity to classify estuary stations
"""

import arcpy
import numpy
import pandas as pd
from arcpy import env
import os.path

arcpy.env.overwriteOutput = True
workspace = "E:/GitHub/ToxicsRedo/Estuary_Analysis"
arcpy.env.workspace = workspace

#Import the two csv files with conductivity and salinity data
in_wqp_consal = 'E:/GitHub/ToxicsRedo/Estuary_Analysis/WQP_consal.csv'
in_lasar_consal = 'E:/GitHub/ToxicsRedo/Estuary_Analysis/LASAR_consal.csv'
wqp_consal = pd.read_csv(in_wqp_consal)[['CharacteristicName', 'MonitoringLocationIdentifier', 'ResultMeasureValue']]
lasar_consal = pd.read_csv(in_lasar_consal)[['NAME', 'STATION_KEY', 'Result_clean']]
wqp_consal.columns = ['Name', 'Station', 'Value']
lasar_consal.columns = ['Name', 'Station', 'Value']
consal = wqp_consal.append(lasar_consal, ignore_index = True)
consal_max = consal.pivot_table('Value', 'Station', 'Name', 'max')

#Join data up to station feature class
out_file = 'E:/GitHub/ToxicsRedo/Estuary_Analysis/temp_csv.csv'
consal_max.to_csv(out_file)
in_table = "Estuaries.gdb/consal"
arcpy.TableToTable_conversion(out_file, workspace, in_table)
os.remove(out_file)
in_fc = 'Estuaries.gdb/stations_subset_est2010'
out_fc = 'Estuaries.gdb/stations_subset_est2010_consal'
arcpy.CopyFeatures_management(in_fc, out_fc)
arcpy.JoinField_management(out_fc, 'STATION', in_table, 'Station', ['Conductivi', 'Salinity'])
