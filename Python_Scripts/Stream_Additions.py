# -*- coding: utf-8 -*-
"""
Created on Tue Jul 01 16:43:05 2014

@author: MPsaris
"""

import arcpy
from arcpy import env
import os.path

arcpy.env.overwriteOutput = True
workspace = "C:/Users/MPsaris/DEQ_Stream_Lake_Additions"
arcpy.env.workspace = workspace

#Creates a project gdb and inserts an fc with the stations that need to have streams and waterbodies added

#If there isn't a project geodatabase, create one and add the shapefile with the stations needing streams and waterbodies
if not arcpy.Exists(workspace + '/Additions.gdb'):
    arcpy.CreateFileGDB_management(**workspace**, 'Additions')
    in_fc = '//deqhq1/mpsaris/GitHub/ToxicsRedo/Estuary_Analysis/Estuaries.gdb/All_stations_final_est'
    in_fc_lyr = 'new_wb_needed'
    out_fc = "C:/Users/MPsaris/DEQ_Stream_Lake_Additions/Additions.gdb/All_stations_final_est_unfinalized"
    query = """"QAQC2" in ( 'Further Review Needed' , 'Potential Digitization' )"""
    arcpy.MakeFeatureLayer_management(in_fc, in_fc_lyr, query)
    arcpy.CopyFeatures_management(in_fc_lyr, out_fc)

#Create NHDM dataset from the 10 downloaded regions of NHD
nhd_reg = [1604,1705,1706,1707,1708,1709,1710,1712,1801,1802]
in_nhd_st = [('C:/Users/MPsaris/Downloads/NHDM' + str(i) +'.gdb/Hydrography/NHDFlowline') for i in nhd_reg]
out_nhd_st = "C:/Users/MPsaris/DEQ_Stream_Lake_Additions/Additions.gdb/NHDMFlowline_unclipped"
out_nhd_wb = "C:/Users/MPsaris/DEQ_Stream_Lake_Additions/Additions.gdb/NHDMWaterbody_unclipped"
or_clip = 'F:/Base_Data/Admin_Boundaries/OR_StateBnd_BLM/or_state_boundary.shp'
or_clip_buf5000 = "C:/Users/MPsaris/DEQ_Stream_Lake_Additions/Additions.gdb/OR_clip_buf5000"
out_nhd_st_unproj = "C:/Users/MPsaris/DEQ_Stream_Lake_Additions/Additions.gdb/NHDMFlowline_OR_unproj"
out_nhd_wb_unproj = "C:/Users/MPsaris/DEQ_Stream_Lake_Additions/Additions.gdb/NHDMWaterbody_OR_unproj"
out_nhd_st_final = "C:/Users/MPsaris/DEQ_Stream_Lake_Additions/Additions.gdb/NHDMFlowline_OR"
out_nhd_wb_final = "C:/Users/MPsaris/DEQ_Stream_Lake_Additions/Additions.gdb/NHDMWaterbody_OR"
in_nhd_wb = [('C:/Users/MPsaris/Downloads/NHDM' + str(i) +'.gdb/Hydrography/NHDWaterbody') for i in nhd_reg]
#arcpy.Merge_management(in_nhd_st, out_nhd_st)
#arcpy.Merge_management(in_nhd_wb, out_nhd_wb)
#arcpy.Buffer_analysis(or_clip, or_clip_buf5000, 5000)
#arcpy.Clip_analysis(out_nhd_st, or_clip_buf5000, out_nhd_st_unproj)
#arcpy.Clip_analysis(out_nhd_wb, or_clip_buf5000, out_nhd_wb_unproj)
#arcpy.Project_management(out_nhd_st_unproj, out_nhd_st_final, in_pnw_st)
arcpy.Project_management(out_nhd_wb_unproj, out_nhd_wb_final, in_pnw_st)
#arcpy.Delete_management(out_nhd_st)
#arcpy.Delete_management(out_nhd_wb)

#Copy pnw streams and lakes shapefiles
in_pnw_st = 'F:/Base_Data/Hydrography/PNW/PNW_Hydrography/water_courses/wc_oregon.shp'
out_pnw_st = "C:/Users/MPsaris/DEQ_Stream_Lake_Additions/Additions.gdb/pnw_wc_oregon"
in_pnw_wb = 'F:/Base_Data/Hydrography/PNW/PNW_Hydrography/water_bodies/wb_oregon.shp'
out_pnw_wb = "C:/Users/MPsaris/DEQ_Stream_Lake_Additions/Additions.gdb/pnw_wb_oregon"
#arcpy.CopyFeatures_management(in_pnw_st, out_pnw_st)
#arcpy.CopyFeatures_management(in_pnw_wb, out_pnw_wb)

#Copy NHDH streams and lakes
in_nhdh_st = 'F:/Base_Data/Hydrography/NHD/NHDH_OR_931v210/NHDH_OR.gdb/Hydrography/NHDFlowline'
out_nhdh_st_unproj = "C:/Users/MPsaris/DEQ_Stream_Lake_Additions/Additions.gdb/NHDHFlowline_OR_unprojected"
out_nhdh_st = "C:/Users/MPsaris/DEQ_Stream_Lake_Additions/Additions.gdb/NHDHFlowline_OR"
in_nhdh_wb = 'F:/Base_Data/Hydrography/NHD/NHDH_OR_931v210/NHDH_OR.gdb/Hydrography/NHDWaterbody'
out_nhdh_wb = "C:/Users/MPsaris/DEQ_Stream_Lake_Additions/Additions.gdb/NHDHWaterbody_OR"
#arcpy.CopyFeatures_management(in_nhdh_st, out_nhdh_st_unproj)
#arcpy.Project_management(out_nhdh_st_unproj, out_nhdh_st, in_pnw_st)
#arcpy.Delete_management(out_nhdh_st_unproj)
arcpy.Project_management(in_nhdh_wb, out_nhdh_wb, in_pnw_st)

#Copy Current DEQ streams and lakes
in_deq_st = 'F:/Base_Data/DEQ_Data/WQ_2010_IntegratedReport_V3/WQ_2010_IntegratedReport_V3/Assessment.gdb/DEQ_Streams_25APR2013'
out_deq_st = "C:/Users/MPsaris/DEQ_Stream_Lake_Additions/Additions.gdb/DEQ_Streams_25APR2013"
in_deq_wb = 'F:/Base_Data/DEQ_Data/WQ_2010_IntegratedReport_V3/WQ_2010_IntegratedReport_V3/Assessment.gdb/DEQLakes_14JUN2013'
out_deq_wb = "C:/Users/MPsaris/DEQ_Stream_Lake_Additions/Additions.gdb/DEQLakes_14JUN2013"
#arcpy.CopyFeatures_management(in_deq_st, out_deq_st)
#arcpy.CopyFeatures_management(in_deq_wb, out_deq_wb)

#Create empty feature classes
out_path = 'C:/Users/MPsaris/DEQ_Stream_Lake_Additions/Additions.gdb'

#First for watercourses
fc_pnw = 'additions_pnw_wc'
fc_nhdh = 'additions_nhdh_wc'
fc_nhdm = 'additions_nhdm_wc'
fc_digitized = 'additions_digitized'
llid_points = 'additions_llid_points'

#arcpy.CreateFeatureclass_management(out_path, fc_pnw, 'POLYLINE', spatial_reference = out_pnw_st)
#arcpy.CreateFeatureclass_management(out_path, fc_nhdh, 'POLYLINE', spatial_reference = out_pnw_st)
#arcpy.CreateFeatureclass_management(out_path, fc_nhdm, 'POLYLINE', spatial_reference = out_pnw_st)
#arcpy.CreateFeatureclass_management(out_path, fc_digitized, 'POLYLINE', spatial_reference = out_pnw_st)
#arcpy.CreateFeatureclass_management(out_path, llid_points, 'POINT', spatial_reference = out_pnw_st)

#Then for waterbodies
fc_pnw_wb = 'additions_pnw_wb'
fc_nhdh_wb = 'additions_nhdh_wb'
fc_nhdm_wb = 'additions_nhdm_wb'

arcpy.CreateFeatureclass_management(out_path, fc_pnw_wb, 'POLYGON', spatial_reference = out_pnw_st)
arcpy.CreateFeatureclass_management(out_path, fc_nhdh_wb, 'POLYGON', spatial_reference = out_pnw_st)
arcpy.CreateFeatureclass_management(out_path, fc_nhdm_wb, 'POLYGON', spatial_reference = out_pnw_st)


#At this point, the fc is copied, and manual review is conducted using the copy: 
#All_stations_final_est_unfinalized_manual
