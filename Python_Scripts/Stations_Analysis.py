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


#Link manually edited results back up with their shapefiles. I have not figured out how to use functions yet,
#so for the time being I'm just going to rewrite the scripts for each one.

#First for Additional Lasar Stations
in_feature = "E:/GitHub/ToxicsRedo/StationsToLocate/FinalList/Additional_LASAR_Stations_Edits.gdb/All_stations"
merge_file = "E:/GitHub/ToxicsRedo/StationsToLocate/FinalList/Additional_LASAR_Stations_merge.csv"
merge_table_path = "E:/GitHub/ToxicsRedo/StationsToLocate/FinalList/Additional_LASAR_Stations_Edits.gdb"
merge_table_name = "Additional_LASAR_Stations_merge"
fieldList = arcpy.ListFields(in_feature)
fields_to_drop = []

for field in fieldList:
    if field.name not in ['Unique_ID', 'Shape','OBJECTID']:
        fields_to_drop.append(field.name)

arcpy.DeleteField_management(in_feature, fields_to_drop)

arcpy.TableToTable_conversion(merge_file, merge_table_path, merge_table_name)
arcpy.JoinField_management(in_feature, 'Unique_ID', (merge_table_path + "/" + merge_table_name), 'Unique_ID')
arcpy.Delete_management((merge_table_path + "/" + merge_table_name))
pd.read_csv(merge_file, header=0)

#Then for Master_List_of_Stations_Results_Tol12000_II_Edits qc_success
in_feature = "E:/GitHub/ToxicsRedo/StationsToLocate/FinalList/Master_List_of_Stations_Results_Tol12000_II_Edits.gdb/qc_success_update"
merge_file = "E:/GitHub/ToxicsRedo/StationsToLocate/FinalList/qc_success.csv"
merge_table_path = "E:/GitHub/ToxicsRedo/StationsToLocate/FinalList/Master_List_of_Stations_Results_Tol12000_II_Edits.gdb"
merge_table_name = "qc_success"
fieldList = arcpy.ListFields(in_feature)
fields_to_drop = []

for field in fieldList:
    if field.name not in ['Unique_ID', 'Shape','OBJECTID']:
        fields_to_drop.append(field.name)

arcpy.DeleteField_management(in_feature, fields_to_drop)

arcpy.TableToTable_conversion(merge_file, merge_table_path, merge_table_name)
arcpy.JoinField_management(in_feature, 'Unique_ID', (merge_table_path + "/" + merge_table_name), 'Unique_ID')
arcpy.Delete_management((merge_table_path + "/" + merge_table_name))

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


#Then for Master_List_of_Stations_Results_Tol12000_II_Edits outside_threshold
in_feature = "E:/GitHub/ToxicsRedo/StationsToLocate/FinalList/Master_List_of_Stations_Results_Tol12000_II_Edits.gdb/outside_threshold_update"
merge_file = "E:/GitHub/ToxicsRedo/StationsToLocate/FinalList/outside_threshold.csv"
merge_table_path = "E:/GitHub/ToxicsRedo/StationsToLocate/FinalList/Master_List_of_Stations_Results_Tol12000_II_Edits.gdb"
merge_table_name = "outside_threshold"
fieldList = arcpy.ListFields(in_feature)
fields_to_drop = []

for field in fieldList:
    if field.name not in ['Unique_ID', 'Shape','OBJECTID']:
        fields_to_drop.append(field.name)

arcpy.DeleteField_management(in_feature, fields_to_drop)

arcpy.TableToTable_conversion(merge_file, merge_table_path, merge_table_name)
arcpy.JoinField_management(in_feature, 'Unique_ID', (merge_table_path + "/" + merge_table_name), 'Unique_ID')
arcpy.Delete_management((merge_table_path + "/" + merge_table_name))

#Then for Master_List_of_Stations_Results_Tol12000_II_Edits qc_needs_review
in_feature = "E:/GitHub/ToxicsRedo/StationsToLocate/FinalList/Master_List_of_Stations_Results_Tol12000_II_Edits.gdb/qc_needs_review_update"
merge_file = "E:/GitHub/ToxicsRedo/StationsToLocate/FinalList/needs_review.csv"
merge_table_path = "E:/GitHub/ToxicsRedo/StationsToLocate/FinalList/Master_List_of_Stations_Results_Tol12000_II_Edits.gdb"
merge_table_name = "qc_needs_review"
fieldList = arcpy.ListFields(in_feature)
fields_to_drop = []

for field in fieldList:
    if field.name not in ['Unique_ID', 'Shape','OBJECTID']:
        fields_to_drop.append(field.name)

arcpy.DeleteField_management(in_feature, fields_to_drop)

arcpy.TableToTable_conversion(merge_file, merge_table_path, merge_table_name)
arcpy.JoinField_management(in_feature, 'Unique_ID', (merge_table_path + "/" + merge_table_name), 'Unique_ID')
arcpy.Delete_management((merge_table_path + "/" + merge_table_name))

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


#Select stations that are fully qc'd
workspace = "E:/GitHub/ToxicsRedo/StationsToLocate/FinalList"
arcpy.env.workspace = workspace
f1 = "Master_List_of_Stations_Results_Tol12000_II_Edits.gdb/qc_needs_review_update"
f2 = "Additional_LASAR_Stations_Edits.gdb/All_stations"
lay1 = "additional_lasar_stations"
lay2 = "master_list_of_statiosn"

#Define unique values in QAQC2 field of needs_review_update
valueList = []
rows = arcpy.SearchCursor(f1)
for row in rows:
    valueList.append(row.getValue("QAQC2"))

uniqueSet = set(valueList)
uniqueList1 = list(uniqueSet)
uniqueList1.sort()
del rows
del row
print uniqueList1

valueList = []
rows = arcpy.SearchCursor(f2)
for row in rows:
    valueList.append(row.getValue("QAQC2"))

uniqueSet = set(valueList)
uniqueList2 = list(uniqueSet)
uniqueList2.sort()
del rows
del row
print uniqueList2


#Remove "remove", "Further review", and "Potential digitization"
items_to_remove = ["Potential Digitization", "Further Review Needed", "Remove"]
uniqueList1 = [x for x in uniqueList1 if x  not in items_to_remove]
uniqueList2 = [x for x in uniqueList2 if x not in items_to_remove]

#Setup Queries

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

#Join DEQ stream and lake names
in_file = out_file
stream_names = "F:/Base_Data/DEQ_Data/WQ_2010_IntegratedReport_V3/WQ_2010_IntegratedReport_V3/Assessment.gdb/DEQ_Streams_25APR2013"
lake_names = "F:/Base_Data/DEQ_Data/WQ_2010_IntegratedReport_V3/WQ_2010_IntegratedReport_V3/Assessment.gdb/DEQLakes_14JUN2013"

arcpy.JoinField_management(in_file, 'LLID', stream_names, 'LLID', 'NAME')
arcpy.JoinField_management(in_file, 'LAKE_LLID', lake_names, 'WATERBODYI', 'NAME')

#Create a function that renames fields.
def renameField(fc, old_name, new_name):
    fields = arcpy.ListFields(fc)
    dtype = [x.type for x in fields if x.name == old_name]
    arcpy.AddField_management(fc, new_name, dtype[0])
    arcpy.CalculateField_management(fc, new_name, "!%s!" % old_name, "PYTHON_9.3")
    arcpy.DeleteField_management(in_file, old_name)

#Update field names to meaningful ones.
renameField(in_file, "NAME", "Stream_Name")
renameField(in_file, "NAME_1", "Lake_Name")

