# -*- coding: utf-8 -*-
"""
Created on Thu Jul 10 16:28:23 2014

@author: MPsaris
"""

import arcpy
from arcpy import env
import os.path
import pyodbc
import numpy

arcpy.env.overwriteOutput = True
location = r"E:\GitHub\ToxicsRedo\StationsToLocate\FinalList"
gdb = 'Gresham.gdb'
workspace = location + '/' + gdb
if not arcpy.Exists(workspace):
    arcpy.CreateFileGDB_management(location, gdb)

arcpy.env.workspace = workspace

accb = r"\\deqhq1\wqassessment\2012_WqAssessment\WQ_2012_Assessment_Working\2012_303d_Toxics.accdb"
access_con_string = r"Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=%s" % accb
cnxn   = pyodbc.connect(access_con_string)
cursor = cnxn.cursor()
cursor.execute("select * from Gresham_STATIONS")
rows = cursor.fetchall()
array = numpy.rec.fromrecords(rows)
array.dtype.names = attribute_array

table = workspace + '/' + 'xydata'
out_fc = workspace + '/Gresham'
layer = 'gresham'
spatial_ref = arcpy.SpatialReference(4269)
arcpy.da.NumPyArrayToTable(array, table)
arcpy.MakeXYEventLayer_management(table, 'LONGITUDE_DECIMAL_DEGREES', 'LATITUDE_DECIMAL_DEGREES', layer, spatial_ref)
arcpy.CopyFeatures_management(layer, out_fc)
arcpy.Delete_management(table)