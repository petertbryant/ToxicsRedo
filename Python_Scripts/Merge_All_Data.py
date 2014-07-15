# -*- coding: utf-8 -*-
"""
Created on Mon Jul 14 10:16:25 2014

@author: MPsaris
"""

import arcpy
from arcpy import env
import os.path

arcpy.env.overwriteOutput = True
workspace = r'E:\GitHub\ToxicsRedo'
arcpy.env.workspace = workspace

#This script does final analysis and preps data for inclusion in the 2012 IR Access table

#First, stations inside tribal lands need to be labeled as such
in_fc = r'C:\Users\MPsaris\DEQ_Stream_Lake_Additions\Additions.gdb\All_stations_final_est_pd'
out_fc = r'E:\GitHub\ToxicsRedo\Shapefiles_for_Access\All_stations_final_est_pd.shp'
arcpy.CopyFeatures_management(in_fc, out_fc)
arcpy.AddField_management(out_fc, 'TRIBAL', 'FLOAT')

all_st_lyr = 'allStations'
arcpy.MakeFeatureLayer_management(out_fc, all_st_lyr)

tribal_fc = r'F:\Base_Data\Cultural\Indian_Res\indianlands\polygon'
tribal_lyr = 'tribal'
expression = """"AIANA-DESC" = 'American Indian Reservation' AND "STATE" = 'OR'"""
arcpy.MakeFeatureLayer_management(tribal_fc, tribal_lyr, expression)
arcpy.SelectLayerByLocation_management(all_st_lyr, 'INTERSECT', tribal_lyr)
print arcpy.GetCount_management(all_st_lyr)[0]
arcpy.CalculateField_management(all_st_lyr, 'TRIBAL', 1)

#Then, rejoin DATUM
join_fc = r'E:\GitHub\ToxicsRedo\StationsToLocate\FinalList\All_Final.gdb\All_stations'
arcpy.JoinField_management(out_fc, 'STATION', join_fc, 'STATION', 'DATUM')
