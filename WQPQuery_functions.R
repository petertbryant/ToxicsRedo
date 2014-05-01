#### Using the water quality portal REST service for Characteristic names ####
library(RCurl)
library(XML)

#### Define domain value retrieval function ####
#This function takes a string as the value argument and retrieves domains in XML format and converts them to dataframes in R
#for the domains for the available search terms go to http://www.waterqualitydata.us/webservices_documentation.jsp#Domain
#As of 4/8/2014 the available parameter names include: countrycode, statecode, countycode, Sitetype, Organization, Samplemedia,
#Characteristictype, Characteristicname, providers
WQP.domain.get <- function(value) {
  theURL <- paste('http://www.waterqualitydata.us/Codes/', value, '?mimeType=xml', sep = '')
  tmp <- getURL(theURL)
  tmp.top <- xmlRoot(xmlTreeParse(tmp))
  tmp.df <- data.frame(value = rep("",length(tmp.top)),desc = rep("",length(tmp.top)),providers = rep("",length(tmp.top)), stringsAsFactors = FALSE)
  for (i in 1:length(tmp.top)) {
    tmp.df$value[i] <- xmlGetAttr(tmp.top[[i]],'value')
    tmp.df$desc[i] <- xmlGetAttr(tmp.top[[i]],'desc')
    tmp.df$providers[i] <- xmlGetAttr(tmp.top[[i]],'providers')  
  }
  return(tmp.df)
}

#### URL encoding function ####
#NOTE: This function has been modified to be consistent with WQP REST service encoding
URLencode.PTB <- function (URL, reserved = FALSE) 
{
  OK <- "[^-ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvywxyz0-9']"
  x <- strsplit(URL, "")[[1L]]
  z <- grep(OK, x)
  if (length(z)) {
    y <- sapply(x[z], function(x) {if(x == ' ') {
      '+'
    } else if (x == '('){
      '('
    } else if (x == ')'){
      ')'
    } else {
      paste0("%", as.character(charToRaw(x)), collapse = "")
    }
    })
    x[z] <- y
  }
  paste(x, collapse = "")
}

#### Water Quality Portal Station Query ####
#All arguments are required to be from the WQP Domain values accessible using the WQP.domain.get function
#This function will return a dataframe of stations whose data match the criteria specified.
wqp.station.query <- function(stateCode, siteType, sampleMedia, characteristicName, startDate, endDate) {
  h <- basicHeaderGatherer()  
  
  theStationURL <- paste('http://www.waterqualitydata.us/Station/search?',
                         'statecode=', Oregon,
                         '&siteType=', siteType, 
                         '&sampleMedia=', sampleMedia,
                         '&characteristicName=', characteristicName,
                         '&startDateLo=', startDate,
                         '&startDateHi=', endDate,
                         '&mimeType=csv', sep ='')
  
  getURI(theStationURL, headerfunction = h$update)
  
  if(as.numeric(h$value()["Total-Site-Count"]) == 0){
    print("There is a problem with your query. No results are returned")
    ifelse(names(h$value()["STORET-Warning"]) == "STORET-Warning",
           print(h$value()["STORET-Warning"]),
           print("You'll have to figure out the problem."))
  } else {
    tmp.stations <- getURL(theStationURL)
    wqp.stations <- read.csv(textConnection(tmp.stations), stringsAsFactors = FALSE) 
  }
}

#### Water Quality Portal Data Query ####
#All arguments are required to be from the WQP Domain values accessible using the WQP.domain.get function
#This function will return a dataframe of data which match the criteria specified.
wqp.data.query <- function(stateCode, siteType, sampleMedia, characteristicName, startDate, endDate) {
  h <- basicHeaderGatherer() 
  
  theDataURL <- paste('http://www.waterqualitydata.us/Result/search?',
                      'statecode=', Oregon,
                      '&siteType=', siteType, 
                      '&sampleMedia=', sampleMedia,
                      '&characteristicName=', characteristicName,
                      '&startDateLo=', startDate,
                      '&startDateHi=', endDate,
                      '&mimeType=csv', sep ='')
  
  getURI(theDataURL, headerfunction = h$update)
  
  
  if(as.numeric(h$value()["Total-Site-Count"]) == 0){
    print("There is a problem with your query. No results are returned")
    ifelse(names(h$value()["STORET-Warning"]) == "STORET-Warning",
           print(h$value()["STORET-Warning"]),
           print("You'll have to figure out the problem."))
  } else {
    tmp.data <- getURL(theDataURL)
    wqp.data <- read.csv(textConnection(tmp.data), stringsAsFactors = FALSE)
  }
}
