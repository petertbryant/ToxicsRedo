# -*- coding: utf-8 -*-
"""
Created on Mon Jun 23 10:46:38 2014

@author: MPsaris

Script using salinity and conductivity to classify estuary stations
"""

import arcpy
#import numpy
import pandas as pd
from arcpy import env
import os.path
import subprocess

arcpy.env.overwriteOutput = True
workspace = "E:/GitHub/ToxicsRedo/Estuary_Analysis"
arcpy.env.workspace = workspace

#Run ConSal_Data_Cleanup.R 
#******This doesn't work correctly. Run through R interface*************
rscript = 'ConSal_Data_Cleanup.R'
rcode = subprocess.call(['C:/Program Files/R/R-3.1.0/bin/RScript', (workspace + '/' + rscript)])

#Join up temporary .csv file created in ConSal_Data_Cleanup.R 
in_table = "Estuaries.gdb/consal"
out_file = 'E:/GitHub/ToxicsRedo/Estuary_Analysis/temp_csv.csv'
arcpy.TableToTable_conversion(out_file, workspace, in_table)
#os.remove(out_file)
in_fc = 'Estuaries.gdb/stations_subset_est2010'
out_fc = 'Estuaries.gdb/stations_subset_est2010_consal'
arcpy.CopyFeatures_management(in_fc, out_fc)
arcpy.JoinField_management(out_fc, 'STATION', in_table, 'Station', ['Sal_ppth'])
arcpy.AddField_management(out_fc, 'Est_Sal', 'STRING')
code = """def isEstuary(salinity):
    if salinity is None:
        return 'Needs Further Review'
    elif salinity > 0.09717282:
        return 'Estuary'
    else:
        return 'Freshwater'"""
expression = "isEstuary(!Sal_ppth!)"
arcpy.CalculateField_management(out_fc, 'Est_Sal', expression, "PYTHON_9.3", code)

#This last feature class is copied and classifications are made manually based on all the information now available.
#Once the manual review is over, the new fields are joined to the main feature class.

in_fc = 'E:/GitHub/ToxicsRedo/StationsToLocate/FinalList/All_Final.gdb/All_stations_final'
out_fc = 'Estuaries.gdb/All_stations_final_est'
in_table = 'Estuaries.gdb/stations_subset_est2010_consal_Manual'
arcpy.CopyFeatures_management(in_fc, out_fc)
arcpy.JoinField_management(out_fc, 'STATION', in_table, 'Station', ['Estuary_2010', 'Sal_ppth', 
                                                                    'Est_Sal', 'Est_Final', 'Est_Comments'])

#Select all stations that were not sub-setted for estuary analysis and label them freshwater.
#This only applies to stations that have been approved for use (ie those not labeled "Potential Digitization', or 
#'Remove')                                                                   
huc4 = "F:/Base_Data/Hydrography/NHD/NHDH_OR_931v210/NHDH_OR.gdb/WBD/WBD_HU8"
huc4_lyr = "huc4lyr"
huc4_lyr = "huc4lyr2"
deq_streams = 'E:/GitHub/ToxicsRedo/StationsToLocate/Assessment2010_copy.gdb/DEQ_Streams_25APR2013'
streams_lyr = "deq_streams_lyr"
stations_2010 = "E:/GitHub/ToxicsRedo/Estuary_Analysis/Estuaries.gdb/Stations_2010"
stations_lyr = "stations_2010_lyr"
final_station_lyr = "stations_final"
streams_query = """"NAME" = 'Pacific Ocean'"""
st_query = '"ESTUARY" = 1'

arcpy.MakeFeatureLayer_management(huc4, huc4_lyr)
arcpy.MakeFeatureLayer_management(deq_streams, streams_lyr, streams_query)
arcpy.MakeFeatureLayer_management(stations_2010, stations_lyr, st_query)
arcpy.MakeFeatureLayer_management(out_fc, final_station_lyr)
arcpy.SelectLayerByLocation_management(huc4_lyr, 'INTERSECT', streams_lyr)
arcpy.SelectLayerByLocation_management(huc4_lyr, 'INTERSECT', stations_lyr, 0,'ADD_TO_SELECTION')
arcpy.SelectLayerByLocation_management(final_station_lyr, 'INTERSECT', huc4_lyr)
print(int(arcpy.GetCount_management(final_station_lyr).getOutput(0)))
arcpy.SelectLayerByAttribute_management(final_station_lyr, 'SWITCH_SELECTION')
arcpy.SelectLayerByAttribute_management(final_station_lyr, 'SUBSET_SELECTION', """"QAQC2" not in ('Remove', 'Potential Digitization')""")
print(int(arcpy.GetCount_management(final_station_lyr).getOutput(0)))
arcpy.CalculateField_management(final_station_lyr, 'Est_Final', '"Freshwater"')