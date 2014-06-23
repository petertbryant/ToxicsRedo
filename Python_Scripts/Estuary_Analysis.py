# -*- coding: utf-8 -*-
"""
Created on Mon Jun 16 14:00:34 2014

@author: MPsaris
"""

import arcpy
import numpy
import pandas as pd
import re
from arcpy import env
import os.path
from __builtin__ import any

arcpy.env.overwriteOutput = True
workspace = "E:/GitHub/ToxicsRedo/Estuary_Analysis"
arcpy.env.workspace = workspace

#Join salinity and other attribute data associated with 2010 stations to the stations2010 fc
in_feature = "E:/GitHub/ToxicsRedo/StationsToLocate/Assessment2010_copy.gdb/Stations_2010"
out_feature = "E:/GitHub/ToxicsRedo/Estuary_Analysis/Estuaries.gdb/Stations_2010"
#the join table was pulled from //Deqhq1/wqassessment/2010_WQAssessment/Databases/WorkingTables_2010.mdb/StationUseList_2010
join_table = "E:/GitHub/ToxicsRedo/Estuary_Analysis/station_2010.csv"
out_table = "E:/GitHub/ToxicsRedo/Estuary_Analysis/Estuaries.gdb/station_2010"

arcpy.CopyFeatures_management(in_feature, out_feature)
arcpy.TableToTable_conversion(join_table, out_table[:-13], "station_2010")
arcpy.JoinField_management(out_feature, 'STATION', out_table, 'STATION')
arcpy.Delete_management(out_table)

#Make initial subset of the new stations using two 'select by location' queries
#1) HUC_8 watersheds which intersect the Pacific Ocean linear feature in DEQ streams
#2) HUC_8 watersheds which have 2010 stations classified as estuary in them

huc4 = "F:/Base_Data/Hydrography/NHD/NHDH_OR_931v210/NHDH_OR.gdb/WBD/WBD_HU8"
huc4_lyr = "huc4lyr"
huc4_lyr = "huc4lyr2"
deq_streams = 'E:/GitHub/ToxicsRedo/StationsToLocate/Assessment2010_copy.gdb/DEQ_Streams_25APR2013'
streams_lyr = "deq_streams_lyr"
stations_2010 = "E:/GitHub/ToxicsRedo/Estuary_Analysis/Estuaries.gdb/Stations_2010"
stations_lyr = "stations_2010_lyr"
streams_query = """"NAME" = 'Pacific Ocean'"""
st_query = '"ESTUARY" = 1'

arcpy.MakeFeatureLayer_management(huc4, huc4_lyr)
arcpy.MakeFeatureLayer_management(deq_streams, streams_lyr, streams_query)
arcpy.MakeFeatureLayer_management(stations_2010, stations_lyr, st_query)
arcpy.SelectLayerByLocation_management(huc4_lyr, 'INTERSECT', streams_lyr)
arcpy.SelectLayerByLocation_management(huc4_lyr, 'INTERSECT', stations_lyr, 0,'ADD_TO_SELECTION')

hucs = []
with arcpy.da.SearchCursor(huc4_lyr, 'HU_8_Name') as cursor:
    for row in cursor:
        hucs.append(row[0])

print(hucs)

result = int(arcpy.GetCount_management(huc4_lyr).getOutput(0)) 
print result

#Clip out new stations which are inside the remaining hucs
stations_new = 'E:/GitHub/ToxicsRedo/StationsToLocate/FinalList/All_Final.gdb/All_stations_final'
out_fc = 'E:/GitHub/ToxicsRedo/Estuary_Analysis/Estuaries.gdb/stations_subset'
arcpy.Clip_analysis(stations_new, huc4_lyr, out_fc)

#Use this function to check if one station has an upstream station classified as an estuary
def upstreamEstuary(llid, rm, estuary_stations):
    stations_lyr = "stations_2010_lyr"
    st_query = '"ESTUARY" = 1'
    arcpy.MakeFeatureLayer_management(estuary_stations, stations_lyr, st_query)
    est = pd.DataFrame({'llid': [], 'rm':[], 'estuary':[]})
    with arcpy.da.SearchCursor(stations_lyr, ['LLID', 'RIVER_MILE', 'ESTUARY']) as cursor:
        for row in cursor:
            if row[0] == llid:
                est = est.append({'llid': str(row[0]), 'rm':row[1], 'estuary':row[2]}, ignore_index=True)
    est = est[est['rm']>= rm]
    if any(x==1 for x in est['estuary']):
        return('Estuary')
    else:
        return('Needs Further Review')

upstreamEstuary('1244292424210', 0.485352, stations_2010)

#Create new estuary field and populate it using upstreamEstuary function
in_fc = "E:/GitHub/ToxicsRedo/Estuary_Analysis/Estuaries.gdb/stations_subset"
out_fc = "E:/GitHub/ToxicsRedo/Estuary_Analysis/Estuaries.gdb/stations_subset_est2010"
arcpy.CopyFeatures_management(in_fc, out_fc)
arcpy.AddField_management(out_fc, 'Estuary_2010', 'TEXT')
with arcpy.da.UpdateCursor(out_fc, ['LLID', 'RIVER_MILE', 'Estuary_2010']) as cursor:
    for row in cursor:
        print(type(row[0]))
        if row[0] == 0 or row[0] is None:
            row[2] = 'No stream LLID'
        else:
            row[2] = upstreamEstuary(str("{:13.0f}".format(row[0])), row[1], stations_2010)
        cursor.updateRow(row)

