# -*- coding: utf-8 -*-
"""
Created on Thu Jun 12 11:49:12 2014

@author: MPsaris

This script is used to merge the station lists that resulted from two rounds of assigning LLIDs and River Miles.
After running the 'Assign_LLID.py' script, the attribute tables of the resulting feature classes were exported to 
excel so a manual review could be conducted. Once manual changes were finalized, the modified spreadsheets were 
saved as csv files. This script takes those csv files, joins them back up with the original shapefiles, merges 
them, and finally formats the field names so the stations are ready to add to the 2010 station list. This list
will be the master station list for the 2012 IR toxics "redo".
"""

import arcpy
import numpy
import pandas as pd
import re
from arcpy import env
import os.path

arcpy.env.overwriteOutput = True
workspace = "E:/GitHub/ToxicsRedo/StationsToLocate/FinalList"
arcpy.env.workspace = workspace

#Define three functions to simplify code

#The first function replaces old attributes with the manually edited attribute tables. 
def replace_attributes(out_path, in_feat, merge_file):
    merge_table_path = out_path
    merge_table_name = merge_file[:-4]
    fieldList = arcpy.ListFields(in_feat)
    fields_to_drop = []
    
    for field in fieldList:
        if field.name not in ['Unique_ID', 'Shape','OBJECTID']:
            fields_to_drop.append(field.name)
            
    arcpy.DeleteField_management(in_feat, fields_to_drop)
    
    arcpy.TableToTable_conversion(merge_file, merge_table_path, merge_table_name)
    arcpy.JoinField_management(in_feat, 'Unique_ID', (merge_table_path + "/" + merge_table_name), 'Unique_ID')
    arcpy.Delete_management((merge_table_path + "/" + merge_table_name))

#This one renames attribute field names. Make sure the new field name is not a duplicate of another field. New field will not be created if it is.
def renameField(fc, old_name, new_name):
    fields = arcpy.ListFields(fc)
    dtype = [x.type for x in fields if x.name == old_name]
    arcpy.AddField_management(fc, new_name, dtype[0])
    arcpy.CalculateField_management(fc, new_name, "!%s!" % old_name, "PYTHON_9.3")
    arcpy.DeleteField_management(in_file, old_name)

#This one removes duplicates based off of 'STATION' field
def removeDuplicates(in_fc):
    in_file = in_fc
    expression = 'isDuplicate( !STATION! )'
    codeblock = """uniqueList = []
def isDuplicate(inValue):
    if inValue in uniqueList:
        return 1
    else:
        uniqueList.append(inValue)
        return 0"""
    
    arcpy.AddField_management(in_file, "Duplicate", "SHORT")
    
    arcpy.CalculateField_management(in_file, "Duplicate", expression, "PYTHON_9.3", 
                                    codeblock)
    with arcpy.da.UpdateCursor(in_file, "Duplicate") as cursor:
        for row in cursor:
            if row[0] == 1:
                cursor.deleteRow()


#Link manually edited results back up with their shapefiles using replace_attributes function

#First for Additional Lasar Stations. No known duplicates so no need to run the removeDuplicate function.
out_path = "Additional_LASAR_Stations_Edits.gdb"
in_fc = "Additional_LASAR_Stations_Edits.gdb/All_stations"
out_fc = "Additional_LASAR_Stations_Edits.gdb/All_stations_update"
in_table = "Additional_LASAR_Stations_merge.csv"
arcpy.CopyFeatures_management(in_fc, out_fc)
replace_attributes(out_path, out_fc, in_table)


#Then for Master_List_of_Stations_Results_Tol12000_II_Edits qc_success
out_path = "Master_List_of_Stations_Results_Tol12000_II_Edits.gdb"
in_fc = "Master_List_of_Stations_Results_Tol12000_II_Edits.gdb/qc_success"
out_fc = "Master_List_of_Stations_Results_Tol12000_II_Edits.gdb/qc_success_update"
in_table = "qc_success_table.csv"
arcpy.CopyFeatures_management(in_fc, out_fc)
replace_attributes(out_path, out_fc, in_table)
removeDuplicates(out_fc)

#Link new results up for Master_List_of_Stations_Results_Tol12000_II_Edits outside_threshold
out_path = "Master_List_of_Stations_Results_Tol12000_II_Edits.gdb"
in_fc = "Master_List_of_Stations_Results_Tol12000_II_Edits.gdb/outside_threshold"
out_fc = "Master_List_of_Stations_Results_Tol12000_II_Edits.gdb/outside_threshold_update"
in_table = "outside_threshold_table.csv"
arcpy.CopyFeatures_management(in_fc, out_fc)
replace_attributes(out_path, out_fc, in_table)
removeDuplicates(out_fc)

#Then for Master_List_of_Stations_Results_Tol12000_II_Edits qc_needs_review
out_path = "Master_List_of_Stations_Results_Tol12000_II_Edits.gdb"
in_fc = "Master_List_of_Stations_Results_Tol12000_II_Edits.gdb/qc_needs_review"
out_fc = "Master_List_of_Stations_Results_Tol12000_II_Edits.gdb/qc_needs_review_update"
in_table = "needs_review_table.csv"
arcpy.CopyFeatures_management(in_fc, out_fc)
replace_attributes(out_path, in_fc, in_table)
removeDuplicates(out_fc)


#Merge all datasets together and convert river feet (RF) to river miles (RM)

f1 = "Master_List_of_Stations_Results_Tol12000_II_Edits.gdb/qc_needs_review_update"
f2 = "Additional_LASAR_Stations_Edits.gdb/All_stations"
f3 = "Master_List_of_Stations_Results_Tol12000_II_Edits.gdb/qc_success_update"
f4 = "Master_List_of_Stations_Results_Tol12000_II_Edits.gdb/outside_threshold_update"
out_file = "All_Final.gdb/All_stations"
arcpy.Merge_management([f1, f2, f3, f4], out_file)
arcpy.AddField_management(out_file, 'RIVER_MILE', 'DOUBLE')
arcpy.CalculateField_management(out_file, 'RIVER_MILE', '!RF!/5280', "PYTHON_9.3")

#Spatially join HUC 3 and 4 field
huc3 = "F:/Base_Data/Hydrography/NHD/NHDH_OR_931v210/NHDH_OR.gdb/WBD/WBD_HU6"
huc4 = "F:/Base_Data/Hydrography/NHD/NHDH_OR_931v210/NHDH_OR.gdb/WBD/WBD_HU8"
in_file = "All_Final.gdb/All_stations"
out_file = "All_Final.gdb/All_stations_huc3"
arcpy.SpatialJoin_analysis(in_file, huc3, out_file)
in_file = "All_Final.gdb/All_stations_huc3"
out_file = "All_Final.gdb/All_stations_huc4"
arcpy.SpatialJoin_analysis(in_file, huc4, out_file)

#Copy fc and remove Unwanted fields so fc is ready to merge with 2010 stations
stations2010_formatting = "All_Final.gdb/All_stations_final"
arcpy.CopyFeatures_management(out_file, stations2010_formatting)
fieldList = arcpy.ListFields(stations2010_formatting)
fields_to_drop = []

for field in fieldList:
    if field.name not in ['Shape','OBJECTID', 'LLID', 'LAKE_LLID', 'RIVER_MILE', 'AGENCY', 'AGENCY_ID', 'STATION', 'DEC_LAT', 'DEC_LONG', 
                          'DESCRIPTION', 'QAQC1', 'QAQC2', 'Comments', 'HUC_6', 'HU_6_Name', 'HUC_8', 'HU_8_Name']:
        fields_to_drop.append(field.name)

arcpy.DeleteField_management(stations2010_formatting, fields_to_drop)

#Join the following fields using LLID: GIS_STREAMNAME, LAKE_NAME, GIS_Source_LAKE, GIS_Source
in_file = stations2010_formatting
stream_names = "F:/Base_Data/DEQ_Data/WQ_2010_IntegratedReport_V3/WQ_2010_IntegratedReport_V3/Assessment.gdb/DEQ_Streams_25APR2013"
lake_names = "F:/Base_Data/DEQ_Data/WQ_2010_IntegratedReport_V3/WQ_2010_IntegratedReport_V3/Assessment.gdb/DEQLakes_14JUN2013"

arcpy.JoinField_management(in_file, 'LLID', stream_names, 'LLID', ['NAME', 'SOURCE'])
arcpy.JoinField_management(in_file, 'LAKE_LLID', lake_names, 'WATERBODYI', ['NAME', 'SOURCE'])

#Change these new field names to meaningful ones.
renameField(in_file, "NAME", "GIS_STREAMNAME")
renameField(in_file, "NAME_1", "LAKE_NAME")
renameField(in_file, "Source", "GIS_Source")
renameField(in_file, "SOURCE_1", "GIS_Source_LAKE")


#****The following fields have not been included: EPA_BEACH_ID, and BEACH_NAME, RIVER_MIKE_LAKE****


#This section of code is outdated, but may be useful for subsetting the data later on.

#
##Select stations that are fully qc'd from the two reviewed fcs
#workspace = "E:/GitHub/ToxicsRedo/StationsToLocate/FinalList"
#arcpy.env.workspace = workspace
#lay1 = "additional_lasar_stations"
#lay2 = "master_list_of_statiosn"
#
##List unique values in the QAQC2 field of needs_review_update and All_stations
#values = [row[0] for row in arcpy.da.SearchCursor(f1, "QAQC2")]
#uniqueValues = set(values)
#uniqueList1 = list(uniqueValues)
#print uniqueList1
#
#values = [row[0] for row in arcpy.da.SearchCursor(f2, "QAQC2")]
#uniqueValues = set(values)
#uniqueList2 = list(uniqueValues)
#print uniqueList2
#
##Remove "remove", "Further review", and "Potential digitization" from this list
#items_to_remove = ["Potential Digitization", "Further Review Needed", "Remove"]
#uniqueList1 = [x for x in uniqueList1 if x  not in items_to_remove]
#uniqueList2 = [x for x in uniqueList2 if x not in items_to_remove]
#
##Setup Queries based on the lists of unique fields with only stations we want to keep
#query1 = """ "QAQC2" in """ + "('" + "', '".join(uniqueList1) +"')"
#query2 = """ "QAQC1" = 'Reviewed' or "QAQC2" in """ + "('" + "', '".join(uniqueList2) +"')"
#
#arcpy.MakeFeatureLayer_management(f1, lay1)
#arcpy.MakeFeatureLayer_management(f2, lay2)
#arcpy.SelectLayerByAttribute_management(lay1, "NEW_SELECTION", query1)
#arcpy.SelectLayerByAttribute_management(lay2, "NEW_SELECTION", query2)

#Create output geodatabase
#out_geo = "All_Final.gdb"
#arcpy.CreateFileGDB_management(workspace, out_geo, 'CURRENT')
