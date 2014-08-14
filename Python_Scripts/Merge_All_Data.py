# -*- coding: utf-8 -*-
"""
Created on Mon Jul 14 10:16:25 2014

@author: MPsaris
"""

import arcpy
from arcpy import env
import os.path
custom_script_location = r'E:\GitHub\ToxicsRedo\Python_Scripts'
if custom_script_location not in sys.path:
    sys.path.append(custom_script_location)

from IR2012_Functions import *

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
expression = """ "AIANA-DESC" = 'American Indian Reservation' AND "STATE" = 'OR' """
arcpy.MakeFeatureLayer_management(tribal_fc, tribal_lyr, expression)
arcpy.SelectLayerByLocation_management(all_st_lyr, 'INTERSECT', tribal_lyr)
print arcpy.GetCount_management(all_st_lyr)[0]
arcpy.CalculateField_management(all_st_lyr, 'TRIBAL', 1)

#Then, rejoin DATUM
join_fc = r'E:\GitHub\ToxicsRedo\StationsToLocate\FinalList\All_Final.gdb\All_stations'
arcpy.JoinField_management(out_fc, 'STATION', join_fc, 'STATION', 'DATUM')

#Export Gresham stations to shapefile
in_fc = r'E:\GitHub\ToxicsRedo\StationsToLocate\Gresham_Stations_Edits.gdb\qc_needs_review'
out_fc = r'E:\GitHub\ToxicsRedo\StationsToLocate\Gresham_Stations_Edits.gdb\qc_needs_review_huc3'
huc3 = "F:/Base_Data/Hydrography/NHD/NHDH_OR_931v210/NHDH_OR.gdb/WBD/WBD_HU6"
huc4 = "F:/Base_Data/Hydrography/NHD/NHDH_OR_931v210/NHDH_OR.gdb/WBD/WBD_HU8"
arcpy.SpatialJoin_analysis(in_fc, huc3, out_fc)
in_fc = r'E:\GitHub\ToxicsRedo\StationsToLocate\Gresham_Stations_Edits.gdb\qc_needs_review_huc3'
out_fc = r'E:\GitHub\ToxicsRedo\StationsToLocate\Gresham_Stations_Edits.gdb\qc_needs_review_huc4'
out_fc_ac = r'E:\GitHub\ToxicsRedo\Shapefiles_for_Access\Gresham_Stations.shp'
arcpy.SpatialJoin_analysis(in_fc, huc4, out_fc)
arcpy.AddField_management(out_fc, 'LAKE_LLID', 'TEXT')
in_file = out_fc
stream_names = "F:/Base_Data/DEQ_Data/WQ_2010_IntegratedReport_V3/WQ_2010_IntegratedReport_V3/Assessment.gdb/DEQ_Streams_25APR2013"
lake_names = "F:/Base_Data/DEQ_Data/WQ_2010_IntegratedReport_V3/WQ_2010_IntegratedReport_V3/Assessment.gdb/DEQLakes_14JUN2013"
arcpy.JoinField_management(in_file, 'RID', stream_names, 'LLID', ['NAME', 'SOURCE'])
arcpy.JoinField_management(in_file, 'LAKE_LLID', lake_names, 'WATERBODYI', ['NAME', 'SOURCE'])
#Change these new field names to meaningful ones.
renameField(in_file, "NAME", "GIS_STREAMNAME")
renameField(in_file, "NAME_1", "LAKE_NAME")
renameField(in_file, "Source", "GIS_Source")
renameField(in_file, "SOURCE_1", "GIS_Source_LAKE")
toKeep =['STATION_ID', 'RID', 'MEAS', 'SITE_DESCRIPTION_LOCATION', 'LATITUDE_DECIMAL_DEGREES', 'LONGITUDE_DECIMAL_DEGREES', 
         'LAT_LONG_DATUM', 'QAQC1', 'QAQC2', 'Matrix', 'HUC_6', 'HU_6_Name', 'HUC_8', 'HU_8_Name', 'LAKE_LLID', 
         'GIS_STREAMNAME', 'LAKE_NAME', 'GIS_Source', 'GIS_Source_LAKE']
delAllExcept(in_file, toKeep)
arcpy.CopyFeatures_management(in_file, out_fc_ac)

#Merge and export Mercury stations to shapefile
merge_fc1 = r'E:\GitHub\ToxicsRedo\StationsToLocate\Mercury_Stations_Edits.gdb\outside_threshold_Manual'
merge_fc2 = r'E:\GitHub\ToxicsRedo\StationsToLocate\Mercury_Stations_Edits.gdb\qc_needs_review_Manual'
#renameField(merge_fc2, 'Lake_LLLID', 'LAKE_LLID') #Fixed an error in the field name
out_fc = r'E:\GitHub\ToxicsRedo\StationsToLocate\Mercury_Stations_Edits.gdb\final'
arcpy.Merge_management([merge_fc1, merge_fc2], out_fc)
in_fc = r'E:\GitHub\ToxicsRedo\StationsToLocate\Mercury_Stations_Edits.gdb\final'
out_fc = r'E:\GitHub\ToxicsRedo\StationsToLocate\Mercury_Stations_Edits.gdb\final_huc3'
huc3 = "F:/Base_Data/Hydrography/NHD/NHDH_OR_931v210/NHDH_OR.gdb/WBD/WBD_HU6"
huc4 = "F:/Base_Data/Hydrography/NHD/NHDH_OR_931v210/NHDH_OR.gdb/WBD/WBD_HU8"
arcpy.SpatialJoin_analysis(in_fc, huc3, out_fc)
in_fc = r'E:\GitHub\ToxicsRedo\StationsToLocate\Mercury_Stations_Edits.gdb\final_huc3'
out_fc = r'E:\GitHub\ToxicsRedo\StationsToLocate\Mercury_Stations_Edits.gdb\final_huc4'
out_fc_ac = r'E:\GitHub\ToxicsRedo\Shapefiles_for_Access\Mercury_Stations.shp'
arcpy.SpatialJoin_analysis(in_fc, huc4, out_fc)
in_file = out_fc
stream_names = "F:/Base_Data/DEQ_Data/WQ_2010_IntegratedReport_V3/WQ_2010_IntegratedReport_V3/Assessment.gdb/DEQ_Streams_25APR2013"
lake_names = "F:/Base_Data/DEQ_Data/WQ_2010_IntegratedReport_V3/WQ_2010_IntegratedReport_V3/Assessment.gdb/DEQLakes_14JUN2013"
arcpy.JoinField_management(in_file, 'RID', stream_names, 'LLID', ['NAME', 'SOURCE'])
arcpy.JoinField_management(in_file, 'LAKE_LLID', lake_names, 'WATERBODYI', ['NAME', 'SOURCE'])
#Change these new field names to meaningful ones.
renameField(in_file, "NAME", "GIS_STREAMNAME")
renameField(in_file, "NAME_1", "LAKE_NAME")
renameField(in_file, "Source", "GIS_Source")
renameField(in_file, "SOURCE_1", "GIS_Source_LAKE")
toKeep =['STATION_ID', 'RID', 'MEAS', 'LOCATION_D', 'Latitude', 'Longitude', 'DATUM',
         'QAQC1', 'QAQC2', 'Matrix', 'HUC_6', 'HU_6_Name', 'HUC_8', 'HU_8_Name', 'LAKE_LLID', 'GIS_STREAMNAME',
         'LAKE_NAME', 'GIS_Source', 'GIS_Source_LAKE']
delAllExcept(in_file, toKeep)
arcpy.CopyFeatures_management(in_file, out_fc_ac)


#Export MORE stations to shapefile
in_fc = r'E:\GitHub\ToxicsRedo\StationsToLocate\MORE_Stations_Edits.gdb\qc_needs_review'
out_fc = r'E:\GitHub\ToxicsRedo\StationsToLocate\MORE_Stations_Edits.gdb\qc_needs_review_huc3'
huc3 = "F:/Base_Data/Hydrography/NHD/NHDH_OR_931v210/NHDH_OR.gdb/WBD/WBD_HU6"
huc4 = "F:/Base_Data/Hydrography/NHD/NHDH_OR_931v210/NHDH_OR.gdb/WBD/WBD_HU8"
arcpy.SpatialJoin_analysis(in_fc, huc3, out_fc)
in_fc = r'E:\GitHub\ToxicsRedo\StationsToLocate\MORE_Stations_Edits.gdb\qc_needs_review_huc3'
out_fc = r'E:\GitHub\ToxicsRedo\StationsToLocate\MORE_Stations_Edits.gdb\qc_needs_review_huc4'
out_fc_ac = r'E:\GitHub\ToxicsRedo\Shapefiles_for_Access\MORE_Stations.shp'
arcpy.SpatialJoin_analysis(in_fc, huc4, out_fc)
arcpy.AddField_management(out_fc, 'LAKE_LLID', 'TEXT')
in_file = out_fc
stream_names = "F:/Base_Data/DEQ_Data/WQ_2010_IntegratedReport_V3/WQ_2010_IntegratedReport_V3/Assessment.gdb/DEQ_Streams_25APR2013"
lake_names = "F:/Base_Data/DEQ_Data/WQ_2010_IntegratedReport_V3/WQ_2010_IntegratedReport_V3/Assessment.gdb/DEQLakes_14JUN2013"
arcpy.JoinField_management(in_file, 'RID', stream_names, 'LLID', ['NAME', 'SOURCE'])
arcpy.JoinField_management(in_file, 'LAKE_LLID', lake_names, 'WATERBODYI', ['NAME', 'SOURCE'])
#Change these new field names to meaningful ones.
renameField(in_file, "NAME", "GIS_STREAMNAME")
renameField(in_file, "NAME_1", "LAKE_NAME")
renameField(in_file, "Source", "GIS_Source")
renameField(in_file, "SOURCE_1", "GIS_Source_LAKE")
toKeep =['RID', 'MEAS', 'Organizat_1', 'site_only', 'Monitori_1', 'LatitudeMe', 'LongitudeM', 'Horizont_3',
         'QAQC1', 'QAQC2', 'Matrix', 'HUC_6', 'HU_6_Name', 'HUC_8', 'HU_8_Name', 'LAKE_LLID', 'GIS_STREAMNAME',
         'LAKE_NAME', 'GIS_Source', 'GIS_Source_LAKE']
delAllExcept(in_file, toKeep)
arcpy.CopyFeatures_management(in_file, out_fc_ac)

#Fix errors - export the stations needing updating
in_fc = r'E:\GitHub\ToxicsRedo\StationsToLocate\FinalList\All_Final.gdb\All_stations_final'
out_fc = r'E:\GitHub\ToxicsRedo\StationsToLocate\FinalList\All_Final.gdb\Station_error_fix'
final_fc = r'E:\GitHub\ToxicsRedo\Shapefiles_for_Access\Station_error_fix.shp'
in_lyr = 'asf'
expression = """ "STATION" in ('35331', '14201', '36720', '36228') """
arcpy.MakeFeatureLayer_management(in_fc,in_lyr,expression)
arcpy.GetCount_management(in_lyr)[0]
arcpy.CopyFeatures_management(in_lyr, out_fc)
map_out_fc = [out_fc]*2
in_fields = ['LLID', 'LAKE_LLID']
temp_fields = ['LLID_temp', 'LAKE_LLID_temp']
dtyp = ['TEXT']*2
map(renameField, map_out_fc, in_fields, temp_fields, dtyp)
map(renameField, map_out_fc, temp_fields, in_fields)
arcpy.CopyFeatures_management(out_fc, final_fc)

