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

arcpy.env.overwriteOutput = True
workspace = "E:/GitHub/ToxicsRedo/Estuary_Analysis"
arcpy.env.workspace = workspace

in_feature = "E:/GitHub/ToxicsRedo/StationsToLocate/Assessment2010_copy.gdb/Stations_2010"
out_feature = "E:/GitHub/ToxicsRedo/Estuary_Analysis/Estuaries.gdb/Stations_2010"
join_table = "E:/GitHub/ToxicsRedo/Estuary_Analysis/station_2010.csv"
out_table = "E:/GitHub/ToxicsRedo/Estuary_Analysis/Estuaries.gdb/station_2010"
lstations = "lstations"

arcpy.CopyFeatures_management(in_feature, out_feature)
arcpy.TableToTable_conversion(join_table, out_table[:-13], "station_2010")
arcpy.JoinField_management(out_feature, 'STATION', out_table, 'STATION')
arcpy.Delete_management(out_table)