# -*- coding: utf-8 -*-
"""
Created on Tue Jul 01 10:00:07 2014

@author: MPsaris
"""

import arcpy

def renameField(fc, old_name, new_name, dtype = []):
    fields = arcpy.ListFields(fc)
    if len(dtype) == 0:
        dtype = [x.type for x in fields if x.name == old_name]
    else:
        dtype = [dtype]
    arcpy.AddField_management(fc, new_name, dtype[0])
    arcpy.CalculateField_management(fc, new_name, "!%s!" % old_name, "PYTHON_9.3")
    arcpy.DeleteField_management(fc, old_name)

def delAllExcept(fc, fields_to_keep):
    fieldList = arcpy.ListFields(fc)
    fields_to_keep.append(fieldList[0].name)
    fields_to_keep.append(fieldList[1].name)
    fields_to_drop = []
    for field in fieldList:
        if field.name not in fields_to_keep:
            fields_to_drop.append(field.name)
    arcpy.DeleteField_management(fc, fields_to_drop)

def removeDuplicates(in_fc, field):
    in_file = in_fc
    expression = 'isDuplicate( !%s! )' % field
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
