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

temp_location = "E:/GitHub/ToxicsRedo/StationsToLocate/FinalList/"
workspace = "E:/GitHub/ToxicsRedo/StationsToLocate/FinalList/assign_llid_temp.gdb"
temp_gdb = "assign_llid_temp.gdb"
final_gdb = "Additional_LASAR_Stations.gdb"
original_sampling_stations = "E:/GitHub/ToxicsRedo/StationsToLocate/FinalList/ShapeFiles/Additional_LASAR_Stations.shp"
sampling_stations = "stations_copy"
stream_network = "F:/Base_Data/DEQ_Data/WQ_2010_IntegratedReport_V3/WQ_2010_IntegratedReport_V3/Assessment.gdb/DEQ_Streams_25APR2013"
station_river_name_field = "LOCATION_D"
streams_river_name_field = "NAME"
rid = "LLID"
search_radius = 12000
output_table = "E:/GitHub/ToxicsRedo/StationsToLocate/FinalList/assign_llid_temp.gdb/out1"
output_success = "out_success"
output_fail = "out_fail"
qc_lyr = "qc_lyr"
qc_success = "qc_success"
qc_review = "qc_needs_review"
outside_threshold = "outside_threshold"
properties = "RID POINT MEAS"
qc2_stations = "qc2_stations"
qc2_streams = "qc2_streams"
temp = "E:/GitHub/ToxicsRedo/StationsToLocate/FinalList/assign_llid_temp.gdb/individual_runs"
reruns = "E:/GitHub/ToxicsRedo/StationsToLocate/FinalList/assign_llid_temp.gdb/reruns"

#
##Subset the 44 new lasar stations from master lasar station shapefile
#ls_df = pd.read_csv("E:/GitHub/ToxicsRedo/StationsToLocate/FinalList/Additional_LASAR_Stations_to_locate_06112014.csv", header=0)
#ls_keys = ls_df['STATION_KEY'].values
#
#in_feature = "//Deqlead03/gis_wa/Project_Working_Folders/LASAR_Stations/LASAR_Stations/LASAR_Stations_26sept13.shp"
#out_feature = "E:/GitHub/ToxicsRedo/StationsToLocate/FinalList/ShapeFiles/Additional_LASAR_Stations_test.shp"
#lstations = "lstations"
#query = """ "STATION_KE" in """ + "(" + ', '.join([str(i) for i in ls_keys]) +")"
#
#arcpy.MakeFeatureLayer_management(in_feature, lstations)
#arcpy.SelectLayerByAttribute_management(lstations, "NEW_SELECTION", query)
##arcpy.GetCount_management(lstations).getOutput(0)
#arcpy.CopyFeatures_management(lstations, out_feature)
#


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
