__author__ = 'MPsaris'
custom_script_location = r'E:\GitHub\ToxicsRedo\Python_Scripts'
import arcpy
from arcpy import env
import os.path
import pandas
if custom_script_location not in sys.path:
    sys.path.append(custom_script_location)

from IR2012_Functions import *

arcpy.env.overwriteOutput = True
workspace = r'E:\GitHub\ToxicsRedo'
arcpy.env.workspace = workspace

assessment_gdb = 'Assessment2012.gdb'
if not arcpy.Exists(workspace + '/' + assessment_gdb):
    arcpy.CreateFileGDB_management(workspace, assessment_gdb)

#Create master station feature class w/o attributes. This will only have a 'STATION' field which I will use to join
#up attributes
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
in_st = [st2010, stations, gresh, merc1, merc2, more]
out_st = [out_st2010, out_stations, out_gresh, out_merc1, out_merc2, out_more]

map(arcpy.CopyFeatures_management, in_st, out_st)
fields_needing_updating = out_st[2:]
old_field_names = ['STATION_ID', 'STATION_ID', 'STATI', 'site_only']
new_field_names = ['STATION']*4
dtype = ['TEXT']*4
#Convert all STATION fields to type TEXT
map(renameField, fields_needing_updating, old_field_names, new_field_names, dtype)
#Remove all fields but the STATION field
map(delAllExcept, out_st, [['STATION']]*6)
st2012 = workspace + '/' + assessment_gdb + '/' + 'stations2012'
out_st.reverse()
#Merge all fcs and create one master STATION list
arcpy.Merge_management(out_st, st2012)

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

    arcpy.DeleteField_management(in_fc, 'Duplicate')

arcpy.GetCount_management(st2012)[0]
removeDuplicates(st2012)
arcpy.GetCount_management(st2012)[0]

#Create table which can be joined to the STATION list
in_csv = r'I:\2012_WQAssessment\ToxicsRedo\StationsToLocate\stUseList2012_Final.csv'

#Remove stations that will not be used
st2012data = pandas.read_csv(in_csv)
sf = st2012data['STATION'].tolist()
with arcpy.da.UpdateCursor(st2012, 'STATION') as cursor:
    for row in cursor:
        if row[0] not in sf:
            cursor.deleteRow()

delAllExcept(st2012, ['STATION'])
st_only = workspace + '/' + assessment_gdb + '/' + 'stations'
arcpy.CopyFeatures_management(st2012, st_only)

out_table = 'dataTable2012'
arcpy.TableToTable_conversion(in_csv, (workspace + '/' + assessment_gdb), out_table)
arcpy.JoinField_management(st2012, 'STATION', (workspace + '/' + assessment_gdb + '/' + out_table), 'STATION')

#Cleanup
arcpy.Delete_management((workspace + '/' + assessment_gdb + '/' + out_table))
#map(arcpy.Delete_management, out_st)

#Copy gdb to public drive
arcpy.Copy_management((workspace + '/' + assessment_gdb), r'I:\2012_WQAssessment\ToxicsRedo\Assessment2012.gdb')
