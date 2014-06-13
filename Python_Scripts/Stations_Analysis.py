# -*- coding: utf-8 -*-
"""
Created on Thu Jun 12 11:49:12 2014

@author: MPsaris
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

#Define two functions to simplify code

#The first function replaces old attributes with the manually edited attribute tables
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

#This one renames attribute field names.
def renameField(fc, old_name, new_name):
    fields = arcpy.ListFields(fc)
    dtype = [x.type for x in fields if x.name == old_name]
    arcpy.AddField_management(fc, new_name, dtype[0])
    arcpy.CalculateField_management(fc, new_name, "!%s!" % old_name, "PYTHON_9.3")
    arcpy.DeleteField_management(in_file, old_name)


#Subset the 44 new lasar stations from master lasar station shapefile
ls_df = pd.read_csv("E:/GitHub/ToxicsRedo/StationsToLocate/FinalList/Additional_LASAR_Stations_to_locate_06112014.csv", header=0)
ls_keys = ls_df['STATION_KEY'].values

in_feature = "//Deqlead03/gis_wa/Project_Working_Folders/LASAR_Stations/LASAR_Stations/LASAR_Stations_26sept13.shp"
out_feature = "E:/GitHub/ToxicsRedo/StationsToLocate/FinalList/ShapeFiles/Additional_LASAR_Stations_test.shp"
lstations = "lstations"
query = """ "STATION_KE" in """ + "(" + ', '.join([str(i) for i in ls_keys]) +")"

arcpy.MakeFeatureLayer_management(in_feature, lstations)
arcpy.SelectLayerByAttribute_management(lstations, "NEW_SELECTION", query)
#arcpy.GetCount_management(lstations).getOutput(0)
arcpy.CopyFeatures_management(lstations, out_feature)


#Link manually edited results back up with their shapefiles using replace_attributes function

#First for Additional Lasar Stations
out_path = "Additional_LASAR_Stations_Edits_test.gdb"
in_fc = "Additional_LASAR_Stations_Edits_test.gdb/All_stations"
in_table = "Additional_LASAR_Stations_merge.csv"
replace_attributes(out_path, in_fc, in_table)


#Then for Master_List_of_Stations_Results_Tol12000_II_Edits qc_success
out_path = "Master_List_of_Stations_Results_Tol12000_II_Edits.gdb"
in_fc = "Master_List_of_Stations_Results_Tol12000_II_Edits.gdb/qc_success_update"
in_table = "qc_success.csv"
replace_attributes(out_path, in_fc, in_table)

#Remove duplicates from qc_success
in_file = "E:/GitHub/ToxicsRedo/StationsToLocate/FinalList/Master_List_of_Stations_Results_Tol12000_II_Edits.gdb/qc_success"
expression = 'isDuplicate( !STATION_ID! )'
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
#Duplicates were removed manually in ArcGIS. This code can be used to automate it
with arcpy.da.UpdateCursor(in_file, "Duplicate") as cursor:
    for row in cursor:
        if row[0] == 1:
            cursor.deleteRow()


#Link new results up for Master_List_of_Stations_Results_Tol12000_II_Edits outside_threshold
out_path = "Master_List_of_Stations_Results_Tol12000_II_Edits.gdb"
in_fc = "Master_List_of_Stations_Results_Tol12000_II_Edits.gdb/outside_threshold_update"
in_table = "outside_threshold.csv"
replace_attributes(out_path, in_fc, in_table)

#Then for Master_List_of_Stations_Results_Tol12000_II_Edits qc_needs_review
out_path = "Master_List_of_Stations_Results_Tol12000_II_Edits.gdb"
in_fc = "Master_List_of_Stations_Results_Tol12000_II_Edits.gdb/qc_needs_review_update"
in_table = "needs_review.csv"
replace_attributes(out_path, in_fc, in_table)

#Remove duplicates from qc_needs_review_update
in_file = "E:/GitHub/ToxicsRedo/StationsToLocate/FinalList/Master_List_of_Stations_Results_Tol12000_II_Edits.gdb/qc_needs_review_update"
expression = 'isDuplicate( !STATION_ID! )'
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


#Select stations that are fully qc'd from the two reviewed fcs
workspace = "E:/GitHub/ToxicsRedo/StationsToLocate/FinalList"
arcpy.env.workspace = workspace
f1 = "Master_List_of_Stations_Results_Tol12000_II_Edits.gdb/qc_needs_review_update"
f2 = "Additional_LASAR_Stations_Edits.gdb/All_stations"
lay1 = "additional_lasar_stations"
lay2 = "master_list_of_statiosn"

#List unique values in the QAQC2 field of needs_review_update and All_stations
values = [row[0] for row in arcpy.da.SearchCursor(f1, "QAQC2")]
uniqueValues = set(values)
uniqueList1 = list(uniqueValues)
print uniqueList1

values = [row[0] for row in arcpy.da.SearchCursor(f2, "QAQC2")]
uniqueValues = set(values)
uniqueList2 = list(uniqueValues)
print uniqueList2

#Remove "remove", "Further review", and "Potential digitization" from this list
items_to_remove = ["Potential Digitization", "Further Review Needed", "Remove"]
uniqueList1 = [x for x in uniqueList1 if x  not in items_to_remove]
uniqueList2 = [x for x in uniqueList2 if x not in items_to_remove]

#Setup Queries based on the lists of unique fields with only stations we want to keep
query1 = """ "QAQC2" in """ + "('" + "', '".join(uniqueList1) +"')"
query2 = """ "QAQC1" = 'Reviewed' or "QAQC2" in """ + "('" + "', '".join(uniqueList2) +"')"

arcpy.MakeFeatureLayer_management(f1, lay1)
arcpy.MakeFeatureLayer_management(f2, lay2)
arcpy.SelectLayerByAttribute_management(lay1, "NEW_SELECTION", query1)
arcpy.SelectLayerByAttribute_management(lay2, "NEW_SELECTION", query2)

#Merge all datasets together

f3 = "Master_List_of_Stations_Results_Tol12000_II_Edits.gdb/qc_success_update"
out_file = "All.gdb/All_non_review_stations"
arcpy.Merge_management([lay1, lay2, f3], out_file)

#Add DEQ stream and lake names using LLID
in_file = out_file
stream_names = "F:/Base_Data/DEQ_Data/WQ_2010_IntegratedReport_V3/WQ_2010_IntegratedReport_V3/Assessment.gdb/DEQ_Streams_25APR2013"
lake_names = "F:/Base_Data/DEQ_Data/WQ_2010_IntegratedReport_V3/WQ_2010_IntegratedReport_V3/Assessment.gdb/DEQLakes_14JUN2013"

arcpy.JoinField_management(in_file, 'LLID', stream_names, 'LLID', 'NAME')
arcpy.JoinField_management(in_file, 'LAKE_LLID', lake_names, 'WATERBODYI', 'NAME')


#Change these new field names to meaningful ones.
renameField(in_file, "NAME", "Stream_Name")
renameField(in_file, "NAME_1", "Lake_Name")

