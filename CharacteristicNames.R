#### Testing water quality portal REST service ####
library(RCurl)
library(XML)
theURL <- 'http://www.waterqualitydata.us/Codes/Characteristicname?mimeType=xml'
test <- getURL(theURL)
tmp.XML.top <- xmlRoot(xmlTreeParse(test))
xml.df <- data.frame(value = rep("",length(tmp.XML.top)),desc = rep("",length(tmp.XML.top)),providers = rep("",length(tmp.XML.top)), stringsAsFactors = FALSE)
for (i in 1:length(tmp.XML.top)) {
  xml.df$value[i] <- xmlGetAttr(tmp.XML.top[[i]],'value')
  xml.df$desc[i] <- xmlGetAttr(tmp.XML.top[[i]],'desc')
  xml.df$providers[i] <- xmlGetAttr(tmp.XML.top[[i]],'providers')  
}

