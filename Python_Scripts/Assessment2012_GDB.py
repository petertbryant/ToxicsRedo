import arcpy
from arcpy import env
import os.path
import pyodbc
import pandas
import numpy
custom_script_location = r'E:\GitHub\ToxicsRedo\Python_Scripts'
if custom_script_location not in sys.path:
    sys.path.append(custom_script_location)

from IR2012_Functions import *

# Set up workspace and write privileges
arcpy.env.overwriteOutput = True
workspace = r'E:\GitHub\ToxicsRedo'
arcpy.env.workspace = workspace

# If it doesn't already exist, create the Assessment 2012 GDB
assessment_gdb = 'Assessment2012.gdb'
if not arcpy.Exists(workspace + '/' + assessment_gdb):
    arcpy.CreateFileGDB_management(workspace, assessment_gdb)

# Create master station feature class w/o attributes. This will only have a 'STATION' field which I will use to join
# up attributes
lasar = r'F:\Base_Data\DEQ_Data\Ambient Monitoring Stations\2009 LASAR Monitoring Stations\Shapefile\Stations_OregonLambert.shp'
out_lasar = workspace + '/' + assessment_gdb + '/' + 'lasar'
st2010 = r'F:\Base_Data\DEQ_Data\WQ_2010_IntegratedReport_V3\WQ_2010_IntegratedReport_V3\Assessment.gdb\SurfaceWaterMonitoringStations\Stations_2010'
out_st2010 = workspace + '/' + assessment_gdb + '/' + 'st2010'
stations = r'E:/GitHub/ToxicsRedo/Shapefiles_for_Access/All_stations_final_est_pd.shp'
out_stations = workspace + '/' + assessment_gdb + '/' + 'stations'
gresh = r'E:/GitHub/ToxicsRedo/Shapefiles_for_Access/Gresham_Stations.shp'
out_gresh = workspace + '/' + assessment_gdb + '/' + 'gresh'
merc1 = r'E:/GitHub/ToxicsRedo/Shapefiles_for_Access/Mercury_Stations.shp'
out_merc1 = workspace + '/' + assessment_gdb + '/' + 'merc1'
merc2 = r'E:/GitHub/ToxicsRedo/Shapefiles_for_Access/merlist_to_map.shp'
out_merc2 = workspace + '/' + assessment_gdb + '/' + 'merc2'
more = r'E:/GitHub/ToxicsRedo/Shapefiles_for_Access/MORE_Stations.shp'
out_more = workspace + '/' + assessment_gdb + '/' + 'more'
post_toxics = r'E:/GitHub/ToxicsRedo/StationsToLocate\Post_ToxicsRedo_Stations\post_toxicsRedo_stations_final.shp'
out_post_toxics = workspace + '/' + assessment_gdb + '/' + 'post_toxics'
cotd = r'E:/GitHub/ToxicsRedo/StationsToLocate\Post_ToxicsRedo_Stations\CityOfTheDalles.shp'
out_cotd = workspace + '/' + assessment_gdb + '/' + 'cotd'

in_st = [lasar, st2010, stations, gresh, merc1, merc2, more, post_toxics, cotd]
out_st = [out_lasar, out_st2010, out_stations, out_gresh, out_merc1, out_merc2, out_more, out_post_toxics, out_cotd]

map(arcpy.CopyFeatures_management, in_st, out_st)
fields_needing_updating = [out_st[0]] + out_st[3:7]
old_field_names = ['STATION_KE', 'STATION_ID', 'STATION_ID', 'STATI', 'site_only']
new_field_names = ['STATION']*5
dtype = ['TEXT']*5
renameField(out_post_toxics, 'STATION', 'ST_temp', 'TEXT')
renameField(out_post_toxics, 'ST_temp', 'STATION')
#Convert all STATION fields to type TEXT
map(renameField, fields_needing_updating, old_field_names, new_field_names, dtype)
#Remove all fields but the STATION field
map(delAllExcept, out_st, [['STATION']]*9)
master_stations = workspace + '/' + assessment_gdb + '/' + 'master_stations'
out_st.reverse()
#Merge all fcs and create one master STATION list
arcpy.Merge_management(out_st, master_stations)

print arcpy.GetCount_management(master_stations)[0]
removeDuplicates(master_stations, 'STATION')
print arcpy.GetCount_management(master_stations)[0]
st2012 = workspace + '/' + assessment_gdb + '/' + 'stations2012_temp'

#Start here when recreating station use list gdb
arcpy.Copy_management(master_stations, st2012)

#Pull StationUseList from sql server and join to the STATION list
db = 'WQAssessment'
access_con_string = r"Driver={SQL Server};Server=DEQSQL2\DEV;database=%s" % db
cnxn = pyodbc.connect(access_con_string)
cursor = cnxn.cursor()
cursor.execute("select * from StationUseList where USE_Final = 1 and Year_Added IN (2010, 2012)")
rows = cursor.fetchall()
#numpy.rec.fromrecords()
st2012data = numpy.rec.fromrecords(rows)
col_names = [str(x.column_name) for x in cursor.columns(table='StationUseList')]
st2012data.dtype.names = col_names
st2012data = pandas.DataFrame(st2012data)
assessment_fields = ['AGENCY', 'AGENCY_ID', 'STATION', 'DESCRIPTION', 'Water_Type', 'RIVER_MILE', 'Stream_Name',
                     'STREAM_LLID', 'LAKE_NAME', 'LAKE_LLID', 'GIS_Source', 'GIS_Source_LAKE', 'HUC_3rd_Field',
                     'HU_8_Name', 'DEC_LAT', 'DEC_LONG', 'DATUM']
gdb_fields = ['AGENCY', 'AGENCY_ID', 'STATION', 'DESCRIPTION', 'WATER_TYPE', 'RIVER_MILE', 'STREAM_NAME',
                     'STREAM_LLID', 'LAKE_NAME', 'LAKE_LLID', 'GIS_SOURCE_STREAM', 'GIS_SOURCE_LAKE', 'HUC3_NAME',
                     'HUC4_NAME', 'DEC_LAT', 'DEC_LONG', 'DATUM']
st2012data = st2012data[assessment_fields]
st2012data.columns = gdb_fields
st2012data = st2012data.sort(['STATION'], ascending=0)
temp = 'C:/sul2012_temp.csv'
st2012data.to_csv(temp)

#Remove stations that will not be used
sf = st2012data['STATION'].tolist()
with arcpy.da.UpdateCursor(st2012, 'STATION') as cursor:
    for row in cursor:
        if row[0] not in sf:
            cursor.deleteRow()

out_table = 'dataTable2012'
arcpy.TableToTable_conversion(temp, (workspace + '/' + assessment_gdb), out_table)
arcpy.JoinField_management(st2012, 'STATION', (workspace + '/' + assessment_gdb + '/' + out_table), 'STATION')

delAllExcept(st2012, gdb_fields)
st2012final = workspace + '/' + assessment_gdb + '/' + 'stations2012'
arcpy.Sort_management(st2012, st2012final, [['AGENCY', 'ASCENDING'], ['STATION', 'ASCENDING']])
reorder_temp = [x+'temp' for x in gdb_fields]
reorder_final = gdb_fields
map(renameField, [st2012final]*len(reorder_final), reorder_final, reorder_temp)
map(renameField, [st2012final]*len(reorder_final), reorder_temp, reorder_final)

#Cleanup
os.remove(temp)
arcpy.Delete_management((workspace + '/' + assessment_gdb + '/' + out_table))
arcpy.Delete_management(st2012)
map(arcpy.Delete_management, out_st)

#Copy gdb to public drive
arcpy.Copy_management((workspace + '/' + assessment_gdb), r'I:\2012_WQAssessment\ToxicsRedo\Assessment2012.gdb')
