# -*- coding: utf-8 -*-
"""
Created on Mon Jun 23 10:46:38 2014

@author: MPsaris

Script using salinity and conductivity to classify estuary stations
"""

import arcpy
import numpy
import pandas as pd
from arcpy import env
import os.path
import subprocess

arcpy.env.overwriteOutput = True
workspace = "E:/GitHub/ToxicsRedo/Estuary_Analysis"
arcpy.env.workspace = workspace

#Run ConSal_Data_Cleanup.R 
#******This doesn't work correctly. Run through R interface*************
rscript = 'ConSal_Data_Cleanup.R'
rcode = subprocess.call(['C:/Program Files/R/R-3.1.0/bin/RScript', (workspace + '/' + rscript)])

#Join up temporary .csv file created in ConSal_Data_Cleanup.R 
in_table = "Estuaries.gdb/consal"
out_file = 'E:/GitHub/ToxicsRedo/Estuary_Analysis/temp_csv.csv'
arcpy.TableToTable_conversion(out_file, workspace, in_table)
#os.remove(out_file)
in_fc = 'Estuaries.gdb/stations_subset_est2010'
out_fc = 'Estuaries.gdb/stations_subset_est2010_consal'
arcpy.CopyFeatures_management(in_fc, out_fc)
arcpy.JoinField_management(out_fc, 'STATION', in_table, 'Station', ['Sal_ppth'])
arcpy.AddField_management(out_fc, 'Est_Sal', 'STRING')
code = """def isEstuary(salinity):
    if salinity is None:
        return 'Needs Further Review'
    elif salinity > 0.09717282:
        return 'Estuary'
    else:
        return 'Freshwater'"""
expression = "isEstuary(!Sal_ppth!)"
arcpy.CalculateField_management(out_fc, 'Est_Sal', expression, "PYTHON_9.3", code)