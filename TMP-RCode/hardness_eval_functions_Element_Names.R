library(plyr)
library(wq)

#establish the constants to use in the criteria calculations and associate them with
constants <- data.frame('Name.alone' = c('Cadmium', 'Copper', 'Cadmium', 'Chromium', 'Lead', 'Nickel', 'Silver', 'Zinc'),
                        'Analyte' = c('Cadmium, Total recoverable', 'Copper, Total recoverable', 'Cadmium, Dissolved', 'Chromium, Dissolved', 
                                   'Lead, Dissolved', 'Nickel, Dissolved', 'Silver, Dissolved', 'Zinc, Dissolved'),
                        'mA' = c(1.128, 0.9422, NA, 0.8190, 1.273, 0.8460, 1.72, 0.8473),
                        'bA' = c(-3.828, -1.464, NA, 3.7256, -1.460, 2.255, -6.59, 0.884),
                        'mC' = c(NA, 0.8545, 0.7409, 0.8190, 1.273, 0.8460, NA, 0.8473),
                        'bC' = c(NA, -1.465, -4.719, 0.6848, -4.705, 0.0584, NA, 0.884),
                        'CFA' = c(1, 1, 1, 0.316, 1, 0.998, 0.85, 0.978),
                        'CFC' = c(1, 1, 1, 0.860, 1, 0.997, 0.85, 0.986),
                        'CFA_SW' = c(1,1,0.994,0.993,0.951,0.990,0.85,0.946),
                        'CFC_SW' = c(1,1,0.994,0.993,0.951,0.990,1,0.946))

hardness.crit.calc <- function(df, remove.chromium = TRUE) {
  if(remove.chromium == FALSE) {
    constants <- constants[constants$Name.alone != 'Chromium',]
  }
  
  df$relate <- paste(df$SampleRegID, df$Sampled, substr(df$Name.full, nchar(df$Name.full), nchar(df$Name.full)))
  
  metals <- df[df$criterianame %in% constants$Name.alone,]
  
  hardness <- df[grep('Hardness',df$Name.full),c('relate','Name.full','tResult')]
  
  mh <- merge(metals, hardness, by = 'relate', suffixes = c('.metal','.hardness'),all.x = TRUE)
  
  mh$tResult.hardness <- as.numeric(mh$tResult.hardness)
  mh[is.na(mh$tResult.hardness),'tResult.hardness'] <- 25
  mh$tResult.hardness <- ifelse(mh$tResult.hardness < 25, 
                                25, 
                                ifelse(mh$tResult.hardness > 400, 
                                       400, 
                                       mh$tResult.hardness))
  
  mhc <- merge(mh, constants, by.x = 'criterianame', by.y = 'Name.alone', all.x = TRUE)
  
  for (i in 1:nrow(mhc)) {
    if(mhc$Analyte[i] == c('Cadmium, Dissolved')) {
      mhc$CFC[i] <- 1.101672-(log(mhc$tResult.hardness[i])*(0.041838))
    } else if (mhc$Analyte[i] == c('Lead, Dissolved')) {
      mhc$CFA[i] <- 1.46203-(log(mhc$tResult.hardness[i])*(0.145712))
      mhc$CFC[i] <- 1.46203-(log(mhc$tResult.hardness[i])*(0.145712))
    }
  }
  
  mhc$'Table 30 Toxic Substances - Freshwater Acute' <- exp(mhc$mA*(log(mhc$tResult.hardness)) + mhc$bA) * mhc$CFA
  mhc$'Table 30 Toxic Substances - Freshwater Chronic' <- exp(mhc$mC*(log(mhc$tResult.hardness)) + mhc$bC) * mhc$CFC

  mhc$'Table 30 Toxic Substances - Saltwater Acute' <- exp(mhc$mA*(log(mhc$tResult.hardness)) + mhc$bA) * mhc$CFA_SW
  mhc$'Table 30 Toxic Substances - Saltwater Chronic' <- exp(mhc$mC*(log(mhc$tResult.hardness)) + mhc$bC) * mhc$CFC_SW
  
  mhc$'Table 30 Toxic Substances - Freshwater Acute' <- ifelse(mhc$Matrix == 'SW',NA,mhc$'Table 30 Toxic Substances - Freshwater Acute')
  mhc$'Table 30 Toxic Substances - Freshwater Chronic' <- ifelse(mhc$Matrix == 'SW',NA,mhc$'Table 30 Toxic Substances - Freshwater Chronic')
  mhc$'Table 30 Toxic Substances - Saltwater Acute' <- ifelse(mhc$Matrix %in% c('FW','ES'),NA,mhc$'Table 30 Toxic Substances - Saltwater Acute')
  mhc$'Table 30 Toxic Substances - Saltwater Chronic' <- ifelse(mhc$Matrix %in% c('FW','ES'),NA,mhc$'Table 30 Toxic Substances - Saltwater Chronic')
  
  mhc.melted <- melt(mhc, measure.vars = c('Table 30 Toxic Substances - Freshwater Acute', 'Table 30 Toxic Substances - Freshwater Chronic',
                                           'Table 30 Toxic Substances - Saltwater Acute', 'Table 30 Toxic Substances - Saltwater Chronic'))
  
  mhcm <- mhc.melted[!is.na(mhc.melted$value),]
  
  mhcm <- rename(mhcm, c('Name.full.metal' = 'Name.full', 'tResult.metal' = 'tResult', 'Matrix' = 'Matrix.x', 'Analyte' = 'Criteria.Name.full'))
  
  mhcm$Matrix.y <- mhcm$Matrix.x
  
  #mhcm$Pollutant <- mhcm$Pollutant
  
  mhcm$criterianame.x <- mhcm$Name
  
  mhcm <- within(mhcm, rm(relate))
  
  return(mhcm)
}


#the general case -- This requires two input dataframes, a detect dataframe and a constants dataframe. the detect dataframe must have
#the detect columns     Name.full: A concatentation of Total Recoverable or Dissolved and the metal name with no space in between
#                            Pollutant: The metal name alone
#                             tResult: A clean numeric Result field in micrograms/L
#                              ID: A concatenation of the STATION and SAMPLE_DATE fields
#the constants columns Name.alone: The metal name alone (equivalent to Pollutant in the detect dataframe)
#                       Name.full: The metal name preceded by Total Recoverable or Dissolved
#                          mA, bA: The coefficients for acute criteria calculation
#                          mC, bC: The coefficients for chronic criteria calculation
#                        CFA, CFC: The conversion factor for converting between total and dissolved criteria
#The option remove.chromium removes the Chromium III hardness dependent criteria. If you want to include it, set it to FALSE.
hardnessEval <- function(metal, df, remove.chromium = TRUE){
  if(remove.chromium == FALSE) {
    constants <- constants[constants$Name.alone != 'Chromium',]
  }
  
  #name.split <- strsplit(df$Analyte, split = ', ')
  
  #name.split <- data.frame(matrix(unlist(name.split), nrow=length(name.split), byrow=T))
  
  #name.split <- rename(name.split, c('X1' = 'Name.alone', 'X2' = 'PARAMETER_MODIFIER_ABBREVIATION'))
  
  #df <- cbind(df, name.split)
  
  total.string <- paste(metal,'Total recoverable',sep=', ')
  dissolved.string <- paste(metal,'Dissolved',sep=', ')
  
  #metal <- detect[detect$Name.full %in% c(total.string, dissolved.string),]
  metal.df <- df[df$Analyte %in% c(total.string, dissolved.string),]
  
  if (nrow(metal.df) == 0) {
    return('')
  } else {
    metal.df <- merge(metal.df, constants, by = 'Analyte', all.x = TRUE)
    
    #metal.h <- detect[detect$ID %in% metal$ID & detect$Name.full == "DissolvedHardness as CaCO3",c('ID','tResult')]
    metal.h <- df[df$ID %in% metal.df$ID & df$Name.alone == "Hardness as CaCO3",c('ID','tResult','PARAMETER_MODIFIER_ABBREVIATION')]
    metal.h$tResult <- as.numeric(metal.h$tResult)
    metal.h$tResult <- ifelse(metal.h$tResult < 25, 25, metal.h$tResult)
    metal.h$tResult <- ifelse(metal.h$tResult > 400, 400, metal.h$tResult)
        
    metal.df.h <- merge(metal.df, metal.h, by = 'ID', suffixes = c('metal','hardness'), all.x = TRUE)
    
    metal.df.h$PARAMETER_MODIFIER_ABBREVIATION <- metal.df.h$PARAMETER_MODIFIER_ABBREVIATIONmetal
    
    metal.df.h$tResulthardness <- as.numeric(metal.df.h$tResulthardness)
    
    for (i in 1:nrow(metal.df.h)) {
      if (metal.df.h$PARAMETER_MODIFIER_ABBREVIATION[i] == 'Dissolved') {
        if (metal.df.h$Name.alone.x[i] == 'Cadmium') {
          metal.df.h$CFC[i] <- 1.101672-(log(metal.df.h$tResulthardness[i])*(0.041838))
        } else if (metal.df.h$Name.alone.x[i] == 'Lead') {
          metal.df.h$CFA[i] <- 1.46203-(log(metal.df.h$tResulthardness[i])*(0.145712))
          metal.df.h$CFC[i] <- 1.46203-(log(metal.df.h$tResulthardness[i])*(0.145712))
        }
      }
    }
    
    metal.df.h$Table30Acute <- exp(metal.df.h$mA*(log(metal.df.h$tResulthardness)) + metal.df.h$bA) * metal.df.h$CFA
    metal.df.h$Table30Chronic <- exp(metal.df.h$mC*(log(metal.df.h$tResulthardness)) + metal.df.h$bC) * metal.df.h$CFC
        
    metal.df.h$tResultmetal <- as.numeric(metal.df.h$tResultmetal)
    
    metal.df.h$Table30Acute.Exceed <- ifelse(metal.df.h$tResultmetal > metal.df.h$Table30Acute, 1, 0)
    metal.df.h$Table30Acute.Magnitude <- metal.df.h$tResultmetal/metal.df.h$Table30Acute
    
    metal.df.h$Table30Chronic.Exceed <- ifelse(metal.df.h$tResultmetal > metal.df.h$Table30Chronic, 1, 0)
    metal.df.h$Table30Chronic.Magnitude <- metal.df.h$tResultmetal/metal.df.h$Table30Chronic
    
    return(metal.df.h)
  }
  
}

pentachlorophenol.crit.calc <- function(df) {

  df$ID <- paste(df$SampleRegID, df$Sampled)
  
  penta <- df[df$criterianame == 'Pentachlorophenol',]
  
  ph <- df[df$Pollutant == 'pH',c('ID','Pollutant','tResult')]
  
  pp <- merge(penta, ph, by = 'ID', suffixes = c('.penta','.ph'),all.x = TRUE)
  
  pp$tResult.ph <- as.numeric(pp$tResult.ph)
  
  pp$'Table 30 Toxic Substances - Freshwater Acute' <- exp(1.005*(pp$tResult.ph)-4.869)
  pp$'Table 30 Toxic Substances - Freshwater Chronic' <- exp(1.005*(pp$tResult.ph)-5.134)
  
  pp.melted <- melt(pp, measure.vars = c('Table 30 Toxic Substances - Freshwater Acute', 'Table 30 Toxic Substances - Freshwater Chronic'))
  
  ppm <- pp.melted[!is.na(pp.melted$value),]
  
  ppm <- rename(ppm, c('Pollutant.penta' = 'Analyte', 'tResult.penta' = 'tResult', 'Matrix' = 'Matrix.x'))
  
  ppm$Matrix.y <- 'FW'
  
  ppm$Pollutant <- ppm$Analyte
  
  return(ppm)
}

ammonia.crit.calc <- function(df, salmonids = 'all') {
  if (salmonids == 'all') {
    df$salmonids <- TRUE
  } else if (salmonids == 'none') {
    df$salmonids <- FALSE
  } 
  
  df$relate <- paste(df$SampleRegID, df$day)
  
  amm <- df[df$criterianame == 'Ammonia as N',]
  
  ph <- df[df$criterianame == 'pH',c('relate','Name','tResult')]
  
  temp <- df[df$criterianame == 'Temperature',c('relate','Name','tResult','Unit')]
  temp$tResult <- ifelse(temp$Unit == '°F', (temp$tResult - 32)*(5/9), temp$tResult)
  temp <- within(temp, rm(Unit))
  
  sal <- df[df$criterianame == 'Salinity',c('relate','Name','tResult')]
  
  ap <- merge(amm, ph, by = 'relate', suffixes = c('.amm','.ph'),all.x = TRUE)
  
  apt <- merge(ap, temp, by = 'relate', suffixes = c('.ap','.temp'),all.x = TRUE)

  apt$tResult.ph <- as.numeric(apt$tResult.ph)
  
  apt$tResult <- as.numeric(apt$tResult)
  
  #The freshwater criteria
  apt.fw <- apt[apt$Matrix %in% c('FW','ES'),]
  
  apt.fw$tResult.ph <- ifelse(apt.fw$tResult.ph < 6.5, 
                                6.5, 
                                ifelse(apt.fw$tResult.ph > 9, 
                                       9, 
                                       apt.fw$tResult.ph))
  
  apt.fw$tResult <- ifelse(apt.fw$tResult > 30, 30, apt.fw$tResult)
  
  for (i in 1:nrow(apt.fw)) {
      apt.fw$TCAP.CMC[i] <- ifelse(apt.fw$salmonids[i] == TRUE,20,25)
      apt.fw$TCAP.CCC[i] <- ifelse(apt.fw$salmonids[i] == TRUE,15,20)
      
      apt.fw$FT.CMC[i] <- ifelse(apt.fw$tResult[i] <= apt.fw$TCAP.CMC[i],10^(0.03*(20-apt.fw$tResult[i])),10^(0.03*(20-apt.fw$TCAP.CMC)))
      apt.fw$FT.CCC[i] <- ifelse(apt.fw$tResult[i] <= apt.fw$TCAP.CCC[i],10^(0.03*(20-apt.fw$tResult[i])),10^(0.03*(20-apt.fw$TCAP.CCC)))
      
      apt.fw$FPH[i] <- ifelse(apt.fw$tResult.ph[i] <= 8,((1 + 10^(7.4-apt.fw$tResult.ph[i]))/1.25),1)
      apt.fw$RATIO[i] <- ifelse(apt.fw$tResult.ph[i] <= 7.7,24*((10^(7.7-apt.fw$tResult.ph[i]))/(1 + 10^(7.4-apt.fw$tResult.ph[i]))),16)
      
      apt.fw$pka[i] <- 0.09018+(2729.92/(273.15+apt.fw$tResult[i]))
      apt.fw$fraction[i] <- 1/(10^(apt.fw$pka[i]-apt.fw$tResult.ph[i])+1)
    
  }
    
  apt.fw$'Table 30 Toxic Substances - Freshwater Acute' <- ((0.52/apt.fw$FT.CMC/apt.fw$FPH/2)/apt.fw$fraction)*0.822
  apt.fw$'Table 30 Toxic Substances - Freshwater Chronic' <- ((0.80/apt.fw$FT.CCC/apt.fw$FPH/apt.fw$RATIO)/apt.fw$fraction)*0.822
  
  apt.fw.melted <- melt(apt.fw, measure.vars = c('Table 30 Toxic Substances - Freshwater Acute', 'Table 30 Toxic Substances - Freshwater Chronic'))
  
  aptm <- apt.fw.melted[!is.na(apt.fw.melted$value),]
  
  aptm$Matrix.y <- aptm$Matrix
  
  aptm <- within(aptm, rm(Name))
  
  #The saltwater criteria
  aptc <- merge(apt, sal, by = 'relate', suffixes = c('.temp','.sal'),all.x = TRUE)
  
  aptc <- aptc[aptc$Matrix %in% c('SW'),]
  
  aptc$tResult.sal <- as.numeric(aptc$tResult.sal)
  
  aptc$tResult.sal <- ifelse(is.na(aptc$tResult.sal),10,aptc$tResult.sal)
  
  #for testing let's actually make these conductivities more saline
#   set.seed(3000)
#   aptc$tResult.sal <- rnorm(14115, 20, sd = 1)
#   
  #Since we already had some salinity it made more sense to convert all to salinity and take the max so this should already be done
#     #to use the Salinity function in the wq package we have to convert to mS
#   aptc$tResult.cond.mS <- aptc$tResult.cond/1000
#   
#   #Then we can actually do the conversion - THIS equation appears to only work for actual conductance
#   #aptc$Salinity <- ec2pss(aptc$tResult.cond.mS, aptc$tResult.temp)
#   
#   #This equation comes from http://pubs.usgs.gov/tm/2006/tm1D3/pdf/TM1D3.pdf page 36 
#   #The report says this equation is used to convert specific conductance to salinity
#   aptc$R <- aptc$tResult.cond.mS/53.087
#   aptc$Salinity <- (0.0120 + (-0.2174*(aptc$R^(1/2))) + (25.3283*aptc$R) + (13.7714*(aptc$R^(3/2))) + 
#                     (-6.4788*(aptc$R^2)) + (2.5842*(aptc$R^(5/2))))
  
  
  #Now that we have Salinity we can use it in the calculation
  mis <- (19.9273*aptc$tResult.sal/(1000-1.005109*aptc$tResult.sal))
  pk <- ifelse(mis>0.85,NA,9.245+0.116*mis)
  pu <- (1/(1+10^(pk+0.0324*(298-aptc$tResult.temp-273)+0.0415*1/(aptc$tResult.temp+273)-aptc$tResult.ph)))
  unionized.acute <- 0.233
  unionized.chronic <- 0.035
  totalNH3.acute <- unionized.acute/pu
  totalNH3.chronic <- unionized.chronic/pu
  aptc$'Table 30 Toxic Substances - Saltwater Acute' <- totalNH3.acute*0.822
  aptc$'Table 30 Toxic Substances - Saltwater Chronic' <- totalNH3.chronic*0.822
  
  aptc.melted <- melt(aptc, measure.vars = c('Table 30 Toxic Substances - Saltwater Acute', 'Table 30 Toxic Substances - Saltwater Chronic'))
  
  aptcm <- aptc.melted[!is.na(aptc.melted$value),]
  
  aptcm <- rename(aptcm, c('tResult.temp' = 'tResult'))
  
  aptcm$Matrix.y <- aptcm$Matrix
  
  aptcm <- within(aptcm, rm(Name.temp,Name.sal,tResult.sal))
  
  aptm.sub <- (aptm[,names(aptm)[names(aptm) %in% names(aptcm)]])
  
  amm.fw.sw <- rbind(aptcm,aptm.sub)
  
  amm.fw.sw$Name <- amm.fw.sw$Name.amm
  
  amm.fw.sw <- rename(amm.fw.sw, c('Matrix' = 'Matrix.x', 'tResult' = 'tResult.temp', 'tResult.amm' = 'tResult'))
  
  #Convert to micrograms per liter for consistency
  amm.fw.sw$value <- amm.fw.sw$value* 1000

  amm.fw.sw <- within(amm.fw.sw, rm(relate))

  amm.fw.sw$criterianame.x <- amm.fw.sw$criterianame

  amm.fw.sw$Criteria.Name.full <- amm.fw.sw$criterianame

  return(amm.fw.sw)
  
  
}

resolveMRLs <- function(ids, dnd, results){
  
  dnd.sum <- ave(dnd, ids, FUN = sum)
  cases   <- findInterval(dnd.sum, c(0, 1, 2))
  
  id.max <- ave(results, ids, FUN = max)
  id.min <- ave(results, ids, FUN = min)
  
  i0 <- cases == 1 & id.min == results
  i1 <- cases == 2 & dnd == 1
  i2 <- cases == 3 & id.max == results
  
  return(i0 | i1 | i2)
}

remove.dups <- function(tname) {
  no.dups <- aggregate(tResult ~ code, data = tname, FUN = max)
  tname <- tname[!duplicated(tname$code),]
  tname <- merge(no.dups, tname, by = 'code')
  #tname$tResult <- round(tname$tResult.x, 2)
  tname$tResult <- tname$tResult.x
  tname <- within(tname, rm(tResult.x, tResult.y))
}

text.summary <- function(x) {
  x <- arrange(x, RIVER_MILE)
  y <- x[x$cat == 3,]
  z <- x[x$cat != 3,]
  cat3 <- ifelse(nrow(z) > 0,
                 ifelse(nrow(y) > 1, 
                        paste('Additionally, there are ', nrow(y), ' stations with insufficient data', sep = ''), 
                        ifelse(nrow(y) == 1,
                               paste('Additionally, there is 1 station with insufficient data', sep = ''),
                               '')),
                 ifelse(nrow(y) > 1, 
                        paste('There are ', nrow(y), ' stations with insufficient data', sep = ''), 
                        ifelse(nrow(y) == 1,
                               paste('There is 1 station with insufficient data', sep = ''),
                                     '')))
  text <- ifelse(nrow(z) > 0, 
                 paste('[', z$Agency, '] STATION ', z$SampleRegID, ' at RM ', z$RIVER_MILE, ' for ', z$total_n, ' samples from ',
                       strftime(z$min_date, '%m/%d/%Y'), ' to ', strftime(z$max_date, '%m/%d/%Y'), ', ', z$exceed, ' of ', 
                       z$valid_n, ' valid samples exceed the ', z$value, ' criteria',
                       sep = '', collapse = ';\r\n'), 
                 "")
  ifelse(text == '',
         cat3,
         ifelse(cat3 == '',
                text,
                paste(text, cat3, sep = ';\r\n')))
}

process <- function(df, source.df) {
  df.us <- source.df[source.df$code %in% rownames(df),]
  df.summary <- ddply(df.us, .(LLID_Stream_Lake, Stream_Name, STREAM_LLID, LAKE_LLID, LAKE_NAME, Pollutant, Pollutant_ID, RM_MIN, RM_MAX, value), text.summary)
}
