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
from IR2012_Functions import renameField

arcpy.env.overwriteOutput = True
workspace = "E:/GitHub/ToxicsRedo/Estuary_Analysis"
arcpy.env.workspace = workspace

#Run ConSal_Data_Cleanup.R 
rscript = 'ConSal_Data_Cleanup.R'
rcode = subprocess.call(['C:/Program Files/R/R-3.1.0/bin/RScript', (workspace + '/' + rscript)])

#Join up temporary .csv file created in ConSal_Data_Cleanup.R 
in_table = "Estuaries.gdb/consal"
out_file = 'E:/GitHub/ToxicsRedo/Estuary_Analysis/temp_csv.csv'
#The head of the table has stations which are numeric only. Since Arc only uses the first seven or so rows
#to determine field type, Arc classifies the Station column as Double, rather than text.To override this
# the Schema.ini file must be edited to specify the type for each field. Check to make sure the schema.ini file
#located here: 
#has the following contents: //Deqhq1/mpsaris/GitHub/ToxicsRedo/Estuary_Analysis
#==============================================================================
# [temp_csv.csv]
# Col1=Station Text
# Col2=Sal_ppth Double
#==============================================================================

arcpy.TableToTable_conversion(out_file, workspace, in_table)
os.remove(out_file)
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

#Fill in final 'Matrix' field
out_fc = 'E:/GitHub/ToxicsRedo/Estuary_Analysis/Estuaries.gdb/All_stations_final_est'
code_block = """def Reclass(matrix):
    if matrix == 'Estuary':
        return 'ES'
    elif matrix == 'Marine':
        return 'SW'
    elif matrix is None:
        return None
    else:
        return 'FW'"""
expression = 'Reclass(!Est_Final!)'
arcpy.AddField_management(out_fc, 'MATRIX', 'TEXT')
arcpy.CalculateField_management(out_fc, 'MATRIX', expression, 'PYTHON_9.3', code_block)
#arcpy.DeleteField_management(out_fc, 'MATRIX')

#Do the same for stations2010
out_fc = 'E:/GitHub/ToxicsRedo/Estuary_Analysis/Estuaries.gdb/Stations_2010'
out_fc_lyr = 'stations2010_lyr'
query_es = '"ESTUARY" = 1'
query_sw = '"MARINE_WATERS" = 1'
query_fw = '"ESTUARY" <> 1 and "MARINE_WATERS" <> 1'
arcpy.AddField_management(out_fc, 'MATRIX', 'TEXT')
arcpy.MakeFeatureLayer_management(out_fc, out_fc_lyr, query_es)
arcpy.CalculateField_management(out_fc_lyr, 'MATRIX', '"ES"')
arcpy.MakeFeatureLayer_management(out_fc, out_fc_lyr, query_sw)
arcpy.CalculateField_management(out_fc_lyr, 'MATRIX', '"SW"')
arcpy.MakeFeatureLayer_management(out_fc, out_fc_lyr, query_fw)
arcpy.CalculateField_management(out_fc_lyr, 'MATRIX', '"FW"')
#arcpy.DeleteField_management(out_fc, 'MATRIX')

#Merge this output with stations_2010 for final station use list for 2012, and remove unnecessary fields

in_fc1 = 'F:/Base_Data/DEQ_Data/WQ_2010_IntegratedReport_V3/WQ_2010_IntegratedReport_V3/Assessment.gdb/SurfaceWaterMonitoringStations/Stations_2010'
in_fc1_join = 'E:/GitHub/ToxicsRedo/Estuary_Analysis/Estuaries.gdb/Stations_2010'
in_fc1_temp = "E:/GitHub/ToxicsRedo/Estuary_Analysis/Estuaries.gdb/Stations_2010_temp"
in_fc2 = 'E:/GitHub/ToxicsRedo/Estuary_Analysis/Estuaries.gdb/All_stations_final_est'
in_fc2_lyr = 'only_finalized_stations'
query = """"QAQC2" not in ('Potential Digitization', 'Remove', 'Further Review Needed')"""
out_fc = "E:/GitHub/ToxicsRedo/Estuary_Analysis/Estuaries.gdb/Stations_2012_Analysis"
arcpy.CopyFeatures_management(in_fc1, in_fc1_temp)
arcpy.AddField_management(in_fc1_temp, 'AGENCY', 'TEXT')
arcpy.AddField_management(in_fc1_temp, 'AGENCY_ID', 'TEXT')
arcpy.CalculateField_management(in_fc1_temp, 'AGENCY', '"Oregon Department of Environmental Quality"')
arcpy.CalculateField_management(in_fc1_temp, 'AGENCY_ID', '"ODEQ"')
arcpy.JoinField_management(in_fc1_temp, 'STATION', in_fc1_join, 'STATION', 'MATRIX')
renameField(in_fc1_temp, 'STATION', 'STATION_TEMP', 'TEXT')
renameField(in_fc1_temp, 'STATION_TEMP', 'STATION')
arcpy.MakeFeatureLayer_management(in_fc2, in_fc2_lyr, query)
arcpy.Merge_management([in_fc1_temp, in_fc2_lyr], out_fc)
arcpy.Delete_management(in_fc1_temp)
fieldList = arcpy.ListFields(out_fc)
fields_to_drop = []

for field in fieldList:
    if field.name not in ['Shape','OBJECTID', 'STATION', 'DESCRIPTION', 'DEC_LAT', 'DEC_LONG', 'LLID', 'RIVER_MILE', 
                          'LAKE_NAME', 'LAKE_LLID', 'GIS_Source_LAKE', 'GIS_STREAMNAME', 'GIS_Source', 
                          'EPA_BEACH_ID', 'BEACH_NAME', 'AGENCY', 'AGENCY_ID', 'MATRIX']:
        fields_to_drop.append(field.name)

arcpy.DeleteField_management(out_fc, fields_to_drop)
renameField(out_fc, 'GIS_STREAMNAME', 'STREAM_NAME')
renameField(out_fc, 'GIS_Source_LAKE', 'LAKE_SOURCE')
renameField(out_fc, 'GIS_Source', 'STREAM_SOURCE')

#Remove Matrix field
out_fc_final = "E:/GitHub/ToxicsRedo/Estuary_Analysis/Estuaries.gdb/Stations_2012"
arcpy.CopyFeatures_management(out_fc, out_fc_final)
arcpy.DeleteField_management(out_fc_final, 'MATRIX')