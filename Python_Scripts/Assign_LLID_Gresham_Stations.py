# -*- coding: utf-8 -*-

# Assign LLID's to sampling stations

# Import necessary modules 
import arcpy
import numpy
import pandas as pd
import re
from arcpy import env
import os.path

arcpy.env.overwriteOutput = True

temp_location = r"E:\GitHub\ToxicsRedo\StationsToLocate"
workspace = r"E:\GitHub\ToxicsRedo\StationsToLocate\assign_llid_temp.gdb"
temp_gdb = "assign_llid_temp.gdb"
final_gdb = "Gresham_Stations.gdb"
original_sampling_stations = r"E:\GitHub\ToxicsRedo\StationsToLocate\FinalList\Gresham.gdb\Gresham"
sampling_stations = "stations_copy"
stream_network = "F:/Base_Data/DEQ_Data/WQ_2010_IntegratedReport_V3/WQ_2010_IntegratedReport_V3/Assessment.gdb/DEQ_Streams_25APR2013"
station_river_name_field = "DESCRIPTIO"
streams_river_name_field = "NAME"
rid = "LLID"
search_radius = 12000
output_table = "C:/assign_llid_temp.gdb/out1"
output_success = "out_success"
output_fail = "out_fail"
qc_lyr = "qc_lyr"
qc_success = "qc_success"
qc_review = "qc_needs_review"
outside_threshold = "outside_threshold"
properties = "RID POINT MEAS"
qc2_stations = "qc2_stations"
qc2_streams = "qc2_streams"
temp = "C:/assign_llid_temp.gdb/individual_runs"
reruns = "C:/assign_llid_temp.gdb/reruns"

# Check to see if a temp geodatabase exists. If not, create it.
if os.path.exists(temp_location + temp_gdb):
    print "It exist!"
else:
    arcpy.CreateFileGDB_management(temp_location, temp_gdb)

if os.path.exists((temp_location + final_gdb)):
    print "It exist!"
else:
    arcpy.CreateFileGDB_management(temp_location, final_gdb)

arcpy.env.workspace = workspace

arcpy.CopyFeatures_management(original_sampling_stations, sampling_stations)
arcpy.AddField_management(sampling_stations, "Unique_ID", "DOUBLE")
arcpy.CalculateField_management(sampling_stations, "Unique_ID", "!OBJECTID!", "PYTHON")
nrow = arcpy.GetCount_management(sampling_stations)

# Execute LocateFeaturesAlongRoutes
arcpy.LocateFeaturesAlongRoutes_lr(sampling_stations, stream_network, rid, search_radius, output_table, 
                                   properties)
successful_features = arcpy.da.TableToNumPyArray(output_table, 'Unique_ID')['Unique_ID']

#Add QC fields to table
arcpy.AddField_management(output_table, "QAQC1", "STRING")
arcpy.AddField_management(output_table, "QAQC2", "STRING")

#Now, begin primary qc by using character matching to verify that successful rows have matching stream names.

stream_names_from_deq_streams = arcpy.da.TableToNumPyArray(stream_network, ['LLID', streams_river_name_field])[['LLID', streams_river_name_field]]

with arcpy.da.UpdateCursor(output_table, [station_river_name_field,'RID','QAQC1', 'QAQC2']) as cursor:
    for row in cursor:
        deq_streams = stream_names_from_deq_streams[streams_river_name_field][nonzero(stream_names_from_deq_streams['LLID'] == row[1])][0]
        if row[0].replace(" ", "").lower() == deq_streams.replace(" ", "").lower():
            row[2] = 'Reviewed'
            row[3] = 'Not Required'
        else:
            row[2] = 'Needs Secondary Review'
        cursor.updateRow(row)

#Create a 'success' fc and a 'fail' fc

#First, copy the original station fc to new fcs. One for success, one for failure.
arcpy.CopyFeatures_management(sampling_stations, output_success)
arcpy.CopyFeatures_management(sampling_stations, output_fail)

#Then, use cursors to remove failed rows from the success fc
with arcpy.da.UpdateCursor(output_success, "Unique_ID") as cursor:
    for row in cursor:
        if row not in successful_features:
            cursor.deleteRow()

#And remove successful rows from the fail fc
with arcpy.da.UpdateCursor(output_fail, "Unique_ID") as cursor:
    for row in cursor:
        if row in successful_features:
            cursor.deleteRow()

#Note: With a large enough search radius the fail fc will be empty. 

#Remove all fields from the success fc except the Unique_ID so it can be merged with the output table
#Note: I'm not sure what would happen here if the success fc is empty. I suspect it would throw an exception.
#      If this happens, increase the search radius.

fieldList = arcpy.ListFields(output_success)
fields_to_drop = []

for field in fieldList:
    if field.name not in ['Unique_ID', 'Shape','OBJECTID']:
        fields_to_drop.append(field.name)

arcpy.DeleteField_management(output_success, fields_to_drop)

#Merge with output table
arcpy.JoinField_management(output_success, 'Unique_ID', output_table, 'Unique_ID')

#Now split success fc into one fc with successful qc and one with stations needing review
arcpy.MakeFeatureLayer_management(output_success, qc_lyr)
arcpy.SelectLayerByAttribute_management(qc_lyr, "NEW_SELECTION", """ "QAQC1" = 'Reviewed' """)

if int(arcpy.GetCount_management(qc_lyr).getOutput(0)) == len(successful_features):
    arcpy.CopyFeatures_management(qc_lyr, (temp_location + final_gdb + "/" + qc_success))
elif int(arcpy.GetCount_management(qc_lyr).getOutput(0)) == 0:
    arcpy.CopyFeatures_management(output_success, (temp_location + final_gdb + "/" + qc_review))
elif int(arcpy.GetCount_management(qc_lyr).getOutput(0)) < len(successful_features) and int(arcpy.GetCount_management(qc_lyr).getOutput(0)) > 0:
    arcpy.CopyFeatures_management(qc_lyr, (temp_location + final_gdb + "/" + qc_success))
    arcpy.SelectLayerByAttribute_management(qc_lyr, "NEW_SELECTION", """ "QAQC1" = 'Needs Secondary Review' """)
    arcpy.CopyFeatures_management(qc_lyr, (temp_location + final_gdb + "/" + qc_review))

arcpy.CopyFeatures_management(output_fail, (temp_location + final_gdb + '/' + outside_threshold))
arcpy.SelectLayerByAttribute_management(qc_lyr, "CLEAR_SELECTION")

#The following attempted to use spatial location and character matching to force the station be properly
#addressed. It was never completed due to time constraints.

#Begin secondary review

#List of Unique_IDs needing review
#arcpy.AddField_management((temp_location + temp_gdb + "/" + qc_review), "names_tmp", "STRING")
#with arcpy.da.UpdateCursor((temp_location + temp_gdb + "/" + qc_review), [station_river_name_field,'names_tmp']) as cursor:
#    for row in cursor:
#        row[1] = row[0].replace(" ", "").lower()
#        cursor.updateRow(row)

unique_ids_needing_review = arcpy.da.TableToNumPyArray((temp_location + temp_gdb + "/" + qc_review), ['Unique_ID'])['Unique_ID']
unique_ids_needing_review2 = unique_ids_needing_review[660:670]

#n=[]
n2=[]
for uid in unique_ids_needing_review:
    #uid = unique_ids_needing_review[0]
    arcpy.MakeFeatureLayer_management((temp_location + temp_gdb + "/" + qc_review), qc2_stations)
    arcpy.MakeFeatureLayer_management(stream_network, qc2_streams)
    arcpy.SelectLayerByAttribute_management(qc2_stations, "NEW_SELECTION", """ "UNIQUE_ID" = {} """.format(uid))
    station_stream_name = arcpy.da.TableToNumPyArray(qc2_stations, [station_river_name_field])[station_river_name_field][0].lower()
    arcpy.SelectLayerByLocation_management(qc2_streams, 'WITHIN_A_DISTANCE', qc2_stations, search_radius)
    #n.append(int(arcpy.GetCount_management(qc2_streams).getOutput(0)))
    #print(n)
    #The REPLACE statement is not included in SQL-92, which is the standard used by file geodatabases, so I use TRIM instead
    arcpy.SelectLayerByAttribute_management(qc2_streams, "SUBSET_SELECTION", """ TRIM(BOTH ' ' FROM LOWER("{0}")) = '{1}' """.format(streams_river_name_field, station_stream_name))
    if int(arcpy.GetCount_management(qc2_streams).getOutput(0)) == 1:
        # Execute LocateFeaturesAlongRoutes tool for selected station and stream
        arcpy.LocateFeaturesAlongRoutes_lr(qc2_stations, qc2_streams, rid, search_radius, temp, 
                                           properties)
        if arcpy.Exists(rerun):
            arcpy.Merge_management([temp, reruns])
        else:
            arcpy.TableToTable_conversion(temp, reruns[:-7], reruns[-6:])
    n2.append(int(arcpy.GetCount_management(qc2_streams).getOutput(0)))
    #print(n2)

n2s = pd.Series(n2)
index = n2s[n2s == 1].index
#Unique IDs that match and can be rerun through selection
qced = unique_ids_needing_review[index]

#Create two new copies of the reviewed shapefile

