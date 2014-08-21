# -*- coding: utf-8 -*-

# Assign LLID's to sampling stations
# I'm including code in here beyond simply the station location. It'll add huc, stream names, and other important info.

# Import necessary modules
custom_script_location = r'E:\GitHub\ToxicsRedo\Python_Scripts'
if custom_script_location not in sys.path:
    sys.path.append(custom_script_location)

from IR2012_Functions import *
import arcpy
import numpy
import os.path


arcpy.env.overwriteOutput = True

temp_location = "E:/GitHub/ToxicsRedo/StationsToLocate/FinalList/"
workspace = "E:/GitHub/ToxicsRedo/StationsToLocate/FinalList/assign_llid_temp.gdb"
temp_gdb = "assign_llid_temp.gdb"
final_gdb = "Toxics_do_post_toxicsRedo_Stations.gdb"
original_sampling_stations = "E:/GitHub/ToxicsRedo/StationsToLocate/Post_ToxicsRedo_Stations/toxics_do_unverified_all.shp"
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


# # Subset the 57 new lasar stations from master lasar station shapefile.
# # Four of these were not in the shapefile, and had to be converted manually.
# ls_df = pd.read_csv(r'E:\GitHub\ToxicsRedo\StationsToLocate\Post_ToxicsRedo_Stations\toxics_do_unverified.csv', header=0)
# ls_keys = ls_df['STATION'].values
#
# in_feature = "//Deqlead03/gis_wa/Project_Working_Folders/LASAR_Stations/LASAR_Stations/LASAR_Stations_26sept13.shp"
# out_feature = "E:/GitHub/ToxicsRedo/StationsToLocate/Post_ToxicsRedo_Stations/toxics_do_unverified.shp"
# lstations = "lstations"
# query = """ "STATION_KE" in """ + "(" + ', '.join([str(i) for i in ls_keys]) +")"
#
# arcpy.MakeFeatureLayer_management(in_feature, lstations)
# arcpy.SelectLayerByAttribute_management(lstations, "NEW_SELECTION", query)
# arcpy.GetCount_management(lstations).getOutput(0)
# arcpy.CopyFeatures_management(lstations, out_feature)



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
        deq_streams = stream_names_from_deq_streams[streams_river_name_field][numpy.nonzero(stream_names_from_deq_streams['LLID'] == row[1])][0]
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


#Once this process is complete, add attribute information.

fc_original = r'E:\GitHub\ToxicsRedo\StationsToLocate\FinalList\Toxics_do_post_toxicsRedo_Stations_Edits.gdb\qc_needs_review'
fc_copy = r'E:\GitHub\ToxicsRedo\StationsToLocate\FinalList\Toxics_do_post_toxicsRedo_Stations_Edits.gdb\qc_needs_review_copy'
arcpy.CopyFeatures_management(fc_original, fc_copy)
arcpy.AddField_management(fc_copy, 'RIVER_MILE', 'DOUBLE')
arcpy.CalculateField_management(fc_copy, 'RIVER_MILE', '!MEAS!/5280', "PYTHON_9.3")

#Spatially join HUC 3 and 4 field
huc3 = 'F:/Base_Data/Hydrography/NHD/NHDH_OR_931v210/NHDH_OR.gdb/WBD/WBD_HU6'
huc4 = 'F:/Base_Data/Hydrography/NHD/NHDH_OR_931v210/NHDH_OR.gdb/WBD/WBD_HU8'
in_file = r'E:\GitHub\ToxicsRedo\StationsToLocate\FinalList\Toxics_do_post_toxicsRedo_Stations_Edits.gdb\qc_needs_review_copy'
out_file = r'E:\GitHub\ToxicsRedo\StationsToLocate\FinalList\Toxics_do_post_toxicsRedo_Stations_Edits.gdb\post_redo_stations_huc3'
arcpy.SpatialJoin_analysis(in_file, huc3, out_file)
in_file = r'E:\GitHub\ToxicsRedo\StationsToLocate\FinalList\Toxics_do_post_toxicsRedo_Stations_Edits.gdb\post_redo_stations_huc3'
out_file = r'E:\GitHub\ToxicsRedo\StationsToLocate\FinalList\Toxics_do_post_toxicsRedo_Stations_Edits.gdb\post_redo_stations_huc4'
arcpy.SpatialJoin_analysis(in_file, huc4, out_file)

#Copy fc and remove Unwanted fields so fc is ready to merge with 2010 stations
stations2010_formatting = r'E:\GitHub\ToxicsRedo\StationsToLocate\FinalList\Toxics_do_post_toxicsRedo_Stations_Edits.gdb\post_redo_stations_final'
arcpy.CopyFeatures_management(out_file, stations2010_formatting)


#Join the following fields using LLID: GIS_STREAMNAME, LAKE_NAME, GIS_Source_LAKE, GIS_Source
renameField(stations2010_formatting, 'RID', 'LLID')
arcpy.AddField_management(stations2010_formatting, 'LAKE_LLID', 'TEXT')
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
renameField(in_file, "STATION_KE", "STATION")
renameField(in_file, "LOCATION_D", "DESCRIPTION")
renameField(in_file, "Latitude", "DEC_LAT")
renameField(in_file, "Longitude", "DEC_LONG")

fieldList = arcpy.ListFields(stations2010_formatting)
fields_to_drop = []

for field in fieldList:
    if field.name not in ['Shape','OBJECTID', 'LLID', 'LAKE_LLID', 'RIVER_MILE', 'AGENCY', 'AGENCY_ID', 'STATION', 'DEC_LAT', 'DEC_LONG',
                          'DESCRIPTION', 'QAQC1', 'QAQC2', 'Comments', 'HUC_6', 'HU_6_Name', 'HUC_8', 'HU_8_Name', 'GIS_STREAMNAME', 'LAKE_NAME',
                          'GIS_Source', 'GIS_Source_LAKE']:
        fields_to_drop.append(field.name)

arcpy.DeleteField_management(stations2010_formatting, fields_to_drop)

out_fc = r'E:\GitHub\ToxicsRedo\StationsToLocate\Post_ToxicsRedo_Stations\post_toxicsRedo_stations_final.shp'
arcpy.CopyFeatures_management(stations2010_formatting, out_fc)
