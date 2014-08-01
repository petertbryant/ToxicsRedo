
ref.con <- odbcConnect('WQAssessment')
access.con <- odbcConnectAccess('//deqhq1/wqassessment/2012_WQAssessment/2012IR_ToxicsRedo_Staging.mdb')

sqlVarTypes <- c('Record_ID'='int',
                 "Stream_Lake_Name"='nvarchar(255)',
                 "Stream_Name"='nvarchar(255)',
                 "Lake_Name"='nvarchar(255)',
                 "Beach_Name"='nvarchar(255)',
                 "LLID_Stream_Lake"='nvarchar(100)',
                 "LLID_Stream"='nvarchar(255)',
                 "LLID_Lake"='nvarchar(15)',
                 "EPA_Beach_ID"='nvarchar(255)',
                 "Segment_ID"='int',
                 "HUC_4th_Code"='nvarchar(255)',
                 "HUC_4th_Name"='nvarchar(255)',
                 "HUC_3rd_Name"='nvarchar(255)',
                 "Miles"='varchar(24)',
                 "RM1"='real',
                 "RM2"='real',
                 "SegmentMiles"='float',
                 "Pollutant"='nvarchar(50)',
                 "Pollutant_ID"='int',
                 "Season"='nvarchar(50)',
                 "Season_ID"='int',
                 "Summary"='varchar(6000)',
                 "Assessment_ID"='int',
                 "AssessmentYear"='int',
                 "AssessmentDate"='datetime',
                 "Action"='nvarchar(100)',
                 "Action_ID"='int',
                 "Criteria"='nvarchar(255)',
                 "NumericCriteria_ID"='int',
                 "AffectedUses"='nvarchar(255)',
                 "Listing_Status"='nvarchar(100)',
                 "Status_ID"='int',
                 "PreviousStatus"='varchar(100)',
                 "PreviousAction"='varchar(100)',
                 "ListingYear"='varchar(100)',
                 "TMDLInfo"='varchar(210)',
                 "Comments"='varchar(500)')

AccessVarTypes <- c('Record_ID'='INTEGER',
                    "Stream_Lake_Name"='VARCHAR(255)',
                    "Stream_Name"='VARCHAR(255)',
                    "Lake_Name"='VARCHAR(255)',
                    "Beach_Name"='VARCHAR(255)',
                    "LLID_Stream_Lake"='VARCHAR(100)',
                    "LLID_Stream"='VARCHAR(255)',
                    "LLID_Lake"='VARCHAR(15)',
                    "EPA_Beach_ID"='VARCHAR(255)',
                    "Segment_ID"='INTEGER',
                    "HUC_4th_Code"='VARCHAR(255)',
                    "HUC_4th_Name"='VARCHAR(255)',
                    "HUC_3rd_Name"='VARCHAR(255)',
                    "Miles"='VARCHAR(24)',
                    "RM1"='DOUBLE',
                    "RM2"='DOUBLE',
                    "SegmentMiles"='DOUBLE',
                    "Pollutant"='VARCHAR(50)',
                    "Pollutant_ID"='INTEGER',
                    "Season"='VARCHAR(50)',
                    "Season_ID"='INTEGER',
                    "Summary"='memo',
                    "Assessment_ID"='INTEGER',
                    "AssessmentYear"='INTEGER',
                    "AssessmentDate"='VARCHAR(50)',
                    "Action"='VARCHAR(100)',
                    "Action_ID"='INTEGER',
                    "Criteria"='VARCHAR(255)',
                    "NumericCriteria_ID"='INTEGER',
                    "AffectedUses"='VARCHAR(255)',
                    "Listing_Status"='VARCHAR(100)',
                    "Status_ID"='INTEGER',
                    "PreviousStatus"='VARCHAR(100)',
                    "PreviousAction"='VARCHAR(100)',
                    "ListingYear"='VARCHAR(100)',
                    "TMDLInfo"='VARCHAR(210)',
                    "Comments"='memo')
ars.2012$AssessmentDate <- strftime(ars.2012$AssessmentDate, '%m/%d/%Y')

ars.2012.summary <- ars.2012[,c('Assessment_ID','Summary')]
ars.2012.comments <- ars.2012[,c('Assessment_ID', 'Comments')]
ars.2012.remaining <- ars.2012[,setdiff(names(ars.2012), c('Summary', 'Comments'))]
#rm(list = setdiff(ls(), c('ars.2012', 'ars.2012.remaining', 'ars.2012.comments', 'ars.2012.summary', 'access.con', 'AccessVarTypes')))
#These SQL saves take A LONG TIME (about an hour each) so be prepared to wait a while for them to run.
#ars.2012.summary[nchar(ars.2012.summary$Summary) > 8000,'Summary'] <- 'x'
#sqlDrop(access.con, 'ars_summary')
write.csv(ars.2012, '//deqhq1/wqassessment/2012_WQassessment/2012IR_ToxicsRedo_DRAFT.csv')
odbcCloseAll()
#Import to Access from .csv

# system.time(sqlSave(access.con, ars.2012.summary, tablename = 'ars_summary', varTypes = AccessVarTypes))
# system.time(sqlSave(access.con, ars.2012.comments, tablename = 'ars_comments', varTypes = AccessVarTypes))
# system.time(sqlSave(access.con, ars.2012.remaining, tablename = 'ars_remaining', varTypes = AccessVarTypes))
# 
# sqlSave(ref.con, unique(allrecs[,c('Pollutant','AffectedUses')]), tablename = '2012IR_Toxics_AffectedUse_Lookup')
# odbcCloseAll()


