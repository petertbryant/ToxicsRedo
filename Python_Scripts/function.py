# -*- coding: utf-8 -*-
"""
Created on Thu Jun 12 16:28:40 2014

@author: MPsaris
"""

import arcpy
import numpy
import pandas as pd
import re
from arcpy import env
import os.path

workspace = "E:/GitHub/ToxicsRedo/StationsToLocate/FinalList"
arcpy.env.workspace = workspace

def merge_data(gdb, in_feat, merge_file):
    print gdb
    print in_feat
    print merge_file
    merge_table_path = gdb
    merge_table_name = merge_file[:-4]
    fieldList = arcpy.ListFields((gdb + "/" + in_feat))
    fields_to_drop = []
    
    for field in fieldList:
        if field.name not in ['Unique_ID', 'Shape','OBJECTID']:
            fields_to_drop.append(field.name)
            
    arcpy.DeleteField_management(gdb + "/" + in_feat, fields_to_drop)
    
    arcpy.TableToTable_conversion(merge_file, merge_table_path, merge_table_name)
    arcpy.JoinField_management(in_feat, 'Unique_ID', (merge_table_path + "/" + merge_table_name), 'Unique_ID')
    arcpy.Delete_management((merge_table_path + "/" + merge_table_name))


merge_data("Additional_LASAR_Stations_Edits_function_test.gdb", "All_stations", "Additional_LASAR_Stations_merge.csv")