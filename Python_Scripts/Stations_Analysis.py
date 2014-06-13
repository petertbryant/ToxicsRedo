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
