library(readxl)
library(foreign)
library(stringr)
library(dplyr)
library(data.table)
library(readr)
library(writexl)
library(tidyr)
library(ggplot2)
library(ggtext)
library(car)
library(texreg)
library(lme4)

#############################
#LOAD DATA

#FP (Focal Papers)
dataFP_report <- read_excel("Marketing_FP_CitationReport.xlsx")
dataFP_core <- read.dbf("Marketing_FP_core.dbf", as.is = F)
dataFP_refs <- read.dbf("Marketing_FP_refs.dbf", as.is = F)
# dataFP_report <- read_excel("AI_FP_CitationReport.xlsx")
# dataFP_core <- read.dbf("AI_FP_core.dbf", as.is = F)
# dataFP_refs <- read.dbf("AI_FP_refs.dbf", as.is = F)

#CP (Citing Papers) (outputted from 'ReadWoSFiles.R')
load("Marketing_CP.RData")
# load("AI_CP.RData")

#############################
#WRANGLE DATA

###FP###
#clean up DOI column (for merging)
dataFP_report$DOI <- str_trim(tolower(dataFP_report$DOI))
dataFP_core$DOI <- str_trim(tolower(dataFP_core$DOI))
#merge
dataFP <- merge(dataFP_report, dataFP_core, by = "DOI")
rm(dataFP_report, dataFP_core)
#set Publication year column to int
dataFP$PY <- as.integer(as.character(dataFP$PY))
#set factor columns to char
dataFP <- dataFP %>% mutate(across(where(is.factor), as.character))
#remove missing DOIs
dataFP <- dataFP[!is.na(dataFP$DOI), ]
#remove duplicate DOIs
dataFP <- dataFP[!duplicated(dataFP$DOI), ]
#clean up DOI column
dataFP$DOI <- str_trim(tolower(dataFP$DOI))
#remove FPs published >= 2020 & < 1975 (minimal Citation Window of 3 years)
dataFP <- dataFP[dataFP$PY >= 1975 & dataFP$PY < 2020, ]
#add citation count for different windows
for (i in 1:nrow(dataFP)){
  if(dataFP$PY[i] <= 2019){
    dataFP$TC_3[i] <- rowSums(dataFP[i, as.character(seq(dataFP$PY[i], (dataFP$PY[i] + 3)))])
  } else {
    dataFP$TC_3[i] <- NA
  }
  if(dataFP$PY[i] <= 2017){
    dataFP$TC_5[i] <- rowSums(dataFP[i, as.character(seq(dataFP$PY[i], (dataFP$PY[i] + 5)))])
  } else {
    dataFP$TC_5[i] <- NA
  } 
  if(dataFP$PY[i] <= 2012){
    dataFP$TC_10[i] <- rowSums(dataFP[i, as.character(seq(dataFP$PY[i], (dataFP$PY[i] + 10)))])
  } else {
    dataFP$TC_10[i] <- NA
  }
  if(dataFP$PY[i] <= 2019){
    dataFP$TC_2022[i] <- rowSums(dataFP[i, as.character(seq(dataFP$PY[i], 2022))])
  } else {
    dataFP$TC_2022[i] <- NA
  }
}
# #add page count
# dataFP_report$`Ending Page` <- as.integer(dataFP_report$`Ending Page`)
# dataFP_report$`Beginning Page` <- as.integer(dataFP_report$`Beginning Page`)
# dataFP_report$ArticleLength <- dataFP_report$`Ending Page` - dataFP_report$`Beginning Page`
#keep relevant columns only
dataFP <- dataFP[ , c("DOI", "Title", "Authors", "NR", "SO", "TC_3", "TC_5", "TC_10", "TC_2022", "TC", "PY", "PD", "NREF")]

###FP_refs###
#keep relevant columns only
dataFP_refs <- dataFP_refs[ , c("NR", "DOI")]
#set factor columns to char
dataFP_refs <- dataFP_refs %>% mutate(across(where(is.factor), as.character))
#remove extra chars from DOI column
dataFP_refs$DOI <- str_extract(dataFP_refs$DOI, pattern = "(10\\.[^\\s,\\]\\n\\;]+)(?=[\\s,\\]\\n]*)")
#clean up DOI column
dataFP_refs$DOI <- str_trim(tolower(dataFP_refs$DOI))
#remove missing DOIs
dataFP_refs <- dataFP_refs[!is.na(dataFP_refs$DOI), ]

###CP###
#create individual row for each CR
CP_long <- Marketing_CP[, .(CR_DOI = unlist(CR)), by = .(CP_DOI,CP_PY)] #Marketing
#CP_long <- AI_CP[, .(CR_DOI = unlist(CR)), by = .(CP_DOI,CP_PY)] #AI

#for easier filtering
setkey(CP_long, CR_DOI)

#############################
#CALCULATIONS CD index COMPONENTS (per citation window)

#LOOP over FPs
for (j in 1:nrow(dataFP)){
  #get CR DOIs
  CR_DOIs <- dataFP_refs$DOI[dataFP_refs$NR == dataFP$NR[j]]
  
  #get CPs
  CP <- CP_long[CR_DOI %in% c(CR_DOIs, dataFP$DOI[j]), .(CR = list(CR_DOI)), keyby = .(CP_DOI, CP_PY)]
  #CP_test <- CP_long[CR_DOI %in% c(CR_DOIs, dataFP$DOI[j])] #all individual citations
  
  #filter CP
  if (paste(str_escape(CR_DOIs), collapse = "|") == ""){
    #NO REFS
    CP_f <- CP[(CR %like% str_escape(dataFP$DOI[j]))] #CP cites FP
    CP_b <- CP[0]
    CP_bf <- CP[0]
  } else{
    #REFS
    CP_f <- CP[(CR %like% str_escape(dataFP$DOI[j])) & !(CR %like% paste(str_escape(CR_DOIs), collapse = "|"))] #CP cites FP and NOT any of its refs
    CP_b <- CP[!(CR %like% str_escape(dataFP$DOI[j])) & (CR %like% paste(str_escape(CR_DOIs), collapse = "|"))] #CP does NOT cite FP and AT LEAST 1 of its refs
    CP_bf <- CP[(CR %like% str_escape(dataFP$DOI[j])) & (CR %like% paste(str_escape(CR_DOIs), collapse = "|"))] #CP cites BOTH FP and AT LEAST 1 of its refs
  }
  #test <- CP[!(CR %like% str_escape(dataFP$DOI[j])) & !(CR %like% paste(str_escape(CR_DOIs), collapse = "|"))] #should be 0 rows


  #ADD VALUES TO dataFP df
  
  #CW <= 3
  dataFP$N_f_CW_3[j]       <- nrow(CP_f[CP_PY <= (dataFP$PY[j] + 3)]) #CP cites FP and NOT any of its refs
  dataFP$N_b_CW_3[j]       <- nrow(CP_b[CP_PY <= (dataFP$PY[j] + 3)]) #CP does NOT cite FP and AT LEAST 1 of its refs
  dataFP$N_bf_BC_1_CW_3[j] <- nrow(CP_bf[CP_PY <= (dataFP$PY[j] + 3)]) #CP cites BOTH FP and AT LEAST 1 of its refs
  dataFP$N_bf_BC_5_CW_3[j] <- sum(lengths(CP_bf$CR[CP_bf$CP_PY <= (dataFP$PY[j] + 3)]) >= 6) #CP cites BOTH FP and AT LEAST 5 of its refs
  
  #CW <= 5
  dataFP$N_f_CW_5[j]       <- nrow(CP_f[CP_PY <= (dataFP$PY[j] + 5)]) 
  dataFP$N_b_CW_5[j]       <- nrow(CP_b[CP_PY <= (dataFP$PY[j] + 5)])
  dataFP$N_bf_BC_1_CW_5[j] <- nrow(CP_bf[CP_PY <= (dataFP$PY[j] + 5)])
  dataFP$N_bf_BC_5_CW_5[j] <- sum(lengths(CP_bf$CR[CP_bf$CP_PY <= (dataFP$PY[j] + 5)]) >= 6)
  
  #CW <= 10
  dataFP$N_f_CW_10[j]       <- nrow(CP_f[CP_PY <= (dataFP$PY[j] + 10)])
  dataFP$N_b_CW_10[j]       <- nrow(CP_b[CP_PY <= (dataFP$PY[j] + 10)])
  dataFP$N_bf_BC_1_CW_10[j] <- nrow(CP_bf[CP_PY <= (dataFP$PY[j] + 10)])
  dataFP$N_bf_BC_5_CW_10[j] <- sum(lengths(CP_bf$CR[CP_bf$CP_PY <= (dataFP$PY[j] + 10)]) >= 6) 
  
  #CW <= 2022 (year)
  dataFP$N_f_CW_2022[j]       <- nrow(CP_f[CP_PY <= 2022]) 
  dataFP$N_b_CW_2022[j]       <- nrow(CP_b[CP_PY <= 2022])
  dataFP$N_bf_BC_1_CW_2022[j] <- nrow(CP_bf[CP_PY <= 2022])
  dataFP$N_bf_BC_5_CW_2022[j] <- sum(lengths(CP_bf$CR[CP_bf$CP_PY <= 2022]) >= 6)
  
  #logging
  print(j)
}

#calculate refs for each FP (included in the data)
for (a in 1:nrow(dataFP)){
  dataFP$NREF_ownCalc[a] <- length(dataFP_refs$DOI[dataFP_refs$NR == dataFP$NR[a]])
}

#calculate average refs per publication year
refCount <- round(aggregate(x = dataFP$NREF_ownCalc, by = list(dataFP$PY), FUN = mean), 0)
names(refCount) <- c("PY", "NREF")

for (b in 1:nrow(dataFP)){
  dataFP$NREF_fieldAvgYear[b] <- refCount$NREF[refCount$PY ==  dataFP$PY[b]]
}


#############################
#CALCULATIONS CD index

#custom function for index calc
CD <- function(N_f, N_bf_nom, N_bf_denom, N_b){
  if((N_f + N_bf_denom + N_b) == 0){
    return(NA)
  } else{
    return((N_f - N_bf_nom)/(N_f + N_bf_denom + N_b))
  }
}

#run calculations
for (k in 1:nrow(dataFP)){
  ###Original CD###
  #BC >= 1
  dataFP$CD_BC_1_CW_3[k]    <- CD(N_f = dataFP$N_f_CW_3[k],    N_bf_nom = dataFP$N_bf_BC_1_CW_3[k],    N_bf_denom = dataFP$N_bf_BC_1_CW_3[k],    N_b = dataFP$N_b_CW_3[k])
  dataFP$CD_BC_1_CW_5[k]    <- CD(N_f = dataFP$N_f_CW_5[k],    N_bf_nom = dataFP$N_bf_BC_1_CW_5[k],    N_bf_denom = dataFP$N_bf_BC_1_CW_5[k],    N_b = dataFP$N_b_CW_5[k])
  dataFP$CD_BC_1_CW_10[k]   <- CD(N_f = dataFP$N_f_CW_10[k],   N_bf_nom = dataFP$N_bf_BC_1_CW_10[k],   N_bf_denom = dataFP$N_bf_BC_1_CW_10[k],   N_b = dataFP$N_b_CW_10[k])
  dataFP$CD_BC_1_CW_2022[k] <- CD(N_f = dataFP$N_f_CW_2022[k], N_bf_nom = dataFP$N_bf_BC_1_CW_2022[k], N_bf_denom = dataFP$N_bf_BC_1_CW_2022[k], N_b = dataFP$N_b_CW_2022[k])
  #BC >= 5
  dataFP$CD_BC_5_CW_3[k]    <- CD(N_f = dataFP$N_f_CW_3[k],    N_bf_nom = dataFP$N_bf_BC_5_CW_3[k],     N_bf_denom = dataFP$N_bf_BC_5_CW_3[k],    N_b = dataFP$N_b_CW_3[k])
  dataFP$CD_BC_5_CW_5[k]    <- CD(N_f = dataFP$N_f_CW_5[k],    N_bf_nom = dataFP$N_bf_BC_5_CW_5[k],     N_bf_denom = dataFP$N_bf_BC_5_CW_5[k],    N_b = dataFP$N_b_CW_5[k])
  dataFP$CD_BC_5_CW_10[k]   <- CD(N_f = dataFP$N_f_CW_10[k],   N_bf_nom = dataFP$N_bf_BC_5_CW_10[k],    N_bf_denom = dataFP$N_bf_BC_5_CW_10[k],   N_b = dataFP$N_b_CW_10[k])
  dataFP$CD_BC_5_CW_2022[k] <- CD(N_f = dataFP$N_f_CW_2022[k], N_bf_nom = dataFP$N_bf_BC_5_CW_2022[k],  N_bf_denom = dataFP$N_bf_BC_5_CW_2022[k], N_b = dataFP$N_b_CW_2022[k])
  
  ###CD_D (Exclude N_bf in nominator)###
  dataFP$CD_D_BC_1_CW_3[k]    <- CD(N_f = dataFP$N_f_CW_3[k],    N_bf_nom = 0, N_bf_denom = dataFP$N_bf_BC_1_CW_3[k],    N_b = dataFP$N_b_CW_3[k])
  dataFP$CD_D_BC_1_CW_5[k]    <- CD(N_f = dataFP$N_f_CW_5[k],    N_bf_nom = 0, N_bf_denom = dataFP$N_bf_BC_1_CW_5[k],    N_b = dataFP$N_b_CW_5[k])
  dataFP$CD_D_BC_1_CW_10[k]   <- CD(N_f = dataFP$N_f_CW_10[k],   N_bf_nom = 0, N_bf_denom = dataFP$N_bf_BC_1_CW_10[k],   N_b = dataFP$N_b_CW_10[k])
  dataFP$CD_D_BC_1_CW_2022[k] <- CD(N_f = dataFP$N_f_CW_2022[k], N_bf_nom = 0, N_bf_denom = dataFP$N_bf_BC_1_CW_2022[k], N_b = dataFP$N_b_CW_2022[k])
  
  ###CD_NoB (Exclude N_b)###
  #BC >= 1
  dataFP$CD_NoB_BC_1_CW_3[k]    <- CD(N_f = dataFP$N_f_CW_3[k],    N_bf_nom = dataFP$N_bf_BC_1_CW_3[k],    N_bf_denom = dataFP$N_bf_BC_1_CW_3[k],    N_b = 0)
  dataFP$CD_NoB_BC_1_CW_5[k]    <- CD(N_f = dataFP$N_f_CW_5[k],    N_bf_nom = dataFP$N_bf_BC_1_CW_5[k],    N_bf_denom = dataFP$N_bf_BC_1_CW_5[k],    N_b = 0)
  dataFP$CD_NoB_BC_1_CW_10[k]   <- CD(N_f = dataFP$N_f_CW_10[k],   N_bf_nom = dataFP$N_bf_BC_1_CW_10[k],   N_bf_denom = dataFP$N_bf_BC_1_CW_10[k],   N_b = 0)
  dataFP$CD_NoB_BC_1_CW_2022[k] <- CD(N_f = dataFP$N_f_CW_2022[k], N_bf_nom = dataFP$N_bf_BC_1_CW_2022[k], N_bf_denom = dataFP$N_bf_BC_1_CW_2022[k], N_b = 0)
  #BC >= 5
  dataFP$CD_NoB_BC_5_CW_3[k]    <- CD(N_f = dataFP$N_f_CW_3[k],    N_bf_nom = dataFP$N_bf_BC_5_CW_3[k],    N_bf_denom = dataFP$N_bf_BC_5_CW_3[k],    N_b = 0)
  dataFP$CD_NoB_BC_5_CW_5[k]    <- CD(N_f = dataFP$N_f_CW_5[k],    N_bf_nom = dataFP$N_bf_BC_5_CW_5[k],    N_bf_denom = dataFP$N_bf_BC_5_CW_5[k],    N_b = 0)
  dataFP$CD_NoB_BC_5_CW_10[k]   <- CD(N_f = dataFP$N_f_CW_10[k],   N_bf_nom = dataFP$N_bf_BC_5_CW_10[k],   N_bf_denom = dataFP$N_bf_BC_5_CW_10[k],   N_b = 0)
  dataFP$CD_NoB_BC_5_CW_2022[k] <- CD(N_f = dataFP$N_f_CW_2022[k], N_bf_nom = dataFP$N_bf_BC_5_CW_2022[k], N_bf_denom = dataFP$N_bf_BC_5_CW_2022[k], N_b = 0)
  
  ###Normalized (original) CD###
  #BC >= 1 & Norm based on own Refs
  dataFP$CD_Norm_Own_Subtract_BC_1_CW_3[k]    <- CD(N_f = dataFP$N_f_CW_3[k],    N_bf_nom = dataFP$N_bf_BC_1_CW_3[k],    N_bf_denom = dataFP$N_bf_BC_1_CW_3[k],    N_b = max( (dataFP$N_b_CW_3[k] - dataFP$NREF_ownCalc[k]), 0) ) 
  dataFP$CD_Norm_Own_Subtract_BC_1_CW_5[k]    <- CD(N_f = dataFP$N_f_CW_5[k],    N_bf_nom = dataFP$N_bf_BC_1_CW_5[k],    N_bf_denom = dataFP$N_bf_BC_1_CW_5[k],    N_b = max( (dataFP$N_b_CW_5[k] - dataFP$NREF_ownCalc[k]), 0) )
  dataFP$CD_Norm_Own_Subtract_BC_1_CW_10[k]   <- CD(N_f = dataFP$N_f_CW_10[k],   N_bf_nom = dataFP$N_bf_BC_1_CW_10[k],   N_bf_denom = dataFP$N_bf_BC_1_CW_10[k],   N_b = max( (dataFP$N_b_CW_10[k] - dataFP$NREF_ownCalc[k]), 0) ) 
  dataFP$CD_Norm_Own_Subtract_BC_1_CW_2022[k] <- CD(N_f = dataFP$N_f_CW_2022[k], N_bf_nom = dataFP$N_bf_BC_1_CW_2022[k], N_bf_denom = dataFP$N_bf_BC_1_CW_2022[k], N_b = max( (dataFP$N_b_CW_2022[k] - dataFP$NREF_ownCalc[k]), 0) )
  #BC >= 1 & Norm based on average Refs in pub year
  dataFP$CD_Norm_FieldYear_Subtract_BC_1_CW_3[k]    <- CD(N_f = dataFP$N_f_CW_3[k],    N_bf_nom = dataFP$N_bf_BC_1_CW_3[k],    N_bf_denom = dataFP$N_bf_BC_1_CW_3[k],    N_b = max( (dataFP$N_b_CW_3[k] - dataFP$NREF_fieldAvgYear[k]), 0) ) 
  dataFP$CD_Norm_FieldYear_Subtract_BC_1_CW_5[k]    <- CD(N_f = dataFP$N_f_CW_5[k],    N_bf_nom = dataFP$N_bf_BC_1_CW_5[k],    N_bf_denom = dataFP$N_bf_BC_1_CW_5[k],    N_b = max( (dataFP$N_b_CW_5[k] - dataFP$NREF_fieldAvgYear[k]), 0) )
  dataFP$CD_Norm_FieldYear_Subtract_BC_1_CW_10[k]   <- CD(N_f = dataFP$N_f_CW_10[k],   N_bf_nom = dataFP$N_bf_BC_1_CW_10[k],   N_bf_denom = dataFP$N_bf_BC_1_CW_10[k],   N_b = max( (dataFP$N_b_CW_10[k] - dataFP$NREF_fieldAvgYear[k]), 0) ) 
  dataFP$CD_Norm_FieldYear_Subtract_BC_1_CW_2022[k] <- CD(N_f = dataFP$N_f_CW_2022[k], N_bf_nom = dataFP$N_bf_BC_1_CW_2022[k], N_bf_denom = dataFP$N_bf_BC_1_CW_2022[k], N_b = max( (dataFP$N_b_CW_2022[k] - dataFP$NREF_fieldAvgYear[k]), 0) )
}
  
##########################
#CORRECTIONS (based on citation window)

#Counts and CD values to NA for PY outside of scope
for (c in 1:nrow(dataFP)){
  if (dataFP$PY[c] >= 2018 ){
    #counts
    dataFP$N_f_CW_5[c] <- NA
    dataFP$N_b_CW_5[c] <- NA
    dataFP$N_bf_BC_1_CW_5[c] <- NA
    dataFP$N_bf_BC_5_CW_5[c] <- NA
    #CD indices
    dataFP$CD_BC_1_CW_5[c] <- NA
    dataFP$CD_BC_5_CW_5[c] <- NA
    dataFP$CD_D_BC_1_CW_5[c] <- NA
    dataFP$CD_NoB_BC_1_CW_5[c] <- NA
    dataFP$CD_NoB_BC_5_CW_5[c] <- NA
    dataFP$CD_Norm_Own_Subtract_BC_1_CW_5[c] <- NA
    dataFP$CD_Norm_FieldYear_Subtract_BC_1_CW_5[c] <- NA
    dataFP$CD_Norm_Own_Divide_BC_1_CW_5[c] <- NA
    dataFP$CD_Norm_FieldYear_Divide_BC_1_CW_5[c] <- NA
  }
  if (dataFP$PY[c] >= 2013 ){
    #counts
    dataFP$N_f_CW_10[c] <- NA
    dataFP$N_b_CW_10[c] <- NA
    dataFP$N_bf_BC_1_CW_10[c] <- NA
    dataFP$N_bf_BC_5_CW_10[c] <- NA
    #CD indices
    dataFP$CD_BC_1_CW_10[c] <- NA
    dataFP$CD_BC_5_CW_10[c] <- NA
    dataFP$CD_D_BC_1_CW_10[c] <- NA
    dataFP$CD_NoB_BC_1_CW_10[c] <- NA
    dataFP$CD_NoB_BC_5_CW_10[c] <- NA
    dataFP$CD_Norm_Own_Subtract_BC_1_CW_10[c] <- NA
    dataFP$CD_Norm_FieldYear_Subtract_BC_1_CW_10[c] <- NA
    dataFP$CD_Norm_Own_Divide_BC_1_CW_10[c] <- NA
    dataFP$CD_Norm_FieldYear_Divide_BC_1_CW_10[c] <- NA
  }
}

#calculate available citations (based on available data)
for (d in 1:nrow(dataFP)){
  if(dataFP$PY[d] <= 2019){
    dataFP$TC_3_ownCalc[d] <- (dataFP$N_f_CW_3[d] + dataFP$N_bf_BC_1_CW_3[d])
  } else {
    dataFP$TC_3_ownCalc[d] <- NA
  }
  if(dataFP$PY[d] <= 2017){
    dataFP$TC_5_ownCalc[d] <- (dataFP$N_f_CW_5[d] + dataFP$N_bf_BC_1_CW_5[d])
  } else {
    dataFP$TC_5_ownCalc[d] <- NA
  } 
  if(dataFP$PY[d] <= 2012){
    dataFP$TC_10_ownCalc[d] <- (dataFP$N_f_CW_10[d] + dataFP$N_bf_BC_1_CW_10[d])
  } else {
    dataFP$TC_10_ownCalc[d] <- NA
  }
  if(dataFP$PY[d] <= 2019){
    dataFP$TC_2022_ownCalc[d] <- (dataFP$N_f_CW_2022[d] + dataFP$N_bf_BC_1_CW_2022[d])
  } else {
    dataFP$TC_2022_ownCalc[d] <- NA
  }
}

#calculate citation and reference differences (based on available data)
for (e in 1:nrow(dataFP)){
  #refs
  dataFP$NREF_Diff[e] <- dataFP$NREF[e] - dataFP$NREF_ownCalc[e]
  #citations
  if(dataFP$PY[e] <= 2019){
    dataFP$TC_3_Diff[e] <- (dataFP$TC_3[e] - dataFP$TC_3_ownCalc[e])
  } else {
    dataFP$TC_3_Diff[e] <- NA
  }
  if(dataFP$PY[e] <= 2017){
    dataFP$TC_5_Diff[e] <- (dataFP$TC_5[e] - dataFP$TC_5_ownCalc[e])
  } else {
    dataFP$TC_5_Diff[e] <- NA
  } 
  if(dataFP$PY[e] <= 2012){
    dataFP$TC_10_Diff[e] <- (dataFP$TC_10[e] - dataFP$TC_10_ownCalc[e])
  } else {
    dataFP$TC_10_Diff[e] <- NA
  }
  if(dataFP$PY[e] <= 2019){
    dataFP$TC_2022_Diff[e] <- (dataFP$TC_2022[e] - dataFP$TC_2022_ownCalc[e])
  } else {
    dataFP$TC_2022_Diff[e] <- NA
  }
}

##########################
#FORMAT DATA

dataFP$Field <- "Marketing"
# dataFP$Field <- "AI"

#rename Marketing journals
dataFP$SO[dataFP$SO == "JOURNAL OF CONSUMER RESEARCH"] <- "JCR"
dataFP$SO[dataFP$SO == "JOURNAL OF MARKETING"] <- "JM"
dataFP$SO[dataFP$SO == "JOURNAL OF MARKETING RESEARCH"] <- "JMR"
dataFP$SO[dataFP$SO == "MARKETING SCIENCE"] <- "MKS"
#rename AI journals
dataFP$SO[dataFP$SO == "ARTIFICIAL INTELLIGENCE"] <- "AI"
dataFP$SO[dataFP$SO == "ARTIFICIAL INTELLIGENCE REVIEW"] <- "AIR"
dataFP$SO[dataFP$SO == "INTERNATIONAL JOURNAL OF COMPUTER VISION"] <- "IJCV"
dataFP$SO[dataFP$SO == "IEEE TRANSACTIONS ON PATTERN ANALYSIS AND MACHINE INTELLIGENCE"] <- "TPAML"

#reorder columns
dataFP <- dataFP[, c("DOI", "Title", "Authors", "NR", "SO",  "TC_3", "TC_3_ownCalc", "TC_3_Diff", "TC_5", "TC_5_ownCalc", "TC_5_Diff", "TC_10", "TC_10_ownCalc", "TC_10_Diff", "TC_2022", "TC_2022_ownCalc", "TC_2022_Diff", "TC", "PY", "PD", "NREF", "NREF_ownCalc", "NREF_Diff", "NREF_fieldAvgYear", "Field",
                     "N_f_CW_3"   , "N_b_CW_3"   , "N_bf_BC_1_CW_3"   , "N_bf_BC_5_CW_3"   , "CD_BC_1_CW_3"   , "CD_BC_5_CW_3"   , "CD_D_BC_1_CW_3"   , "CD_NoB_BC_1_CW_3"   , "CD_NoB_BC_5_CW_3"   , "CD_Norm_Own_Subtract_BC_1_CW_3"   , "CD_Norm_FieldYear_Subtract_BC_1_CW_3"   ,
                     "N_f_CW_5"   , "N_b_CW_5"   , "N_bf_BC_1_CW_5"   , "N_bf_BC_5_CW_5"   , "CD_BC_1_CW_5"   , "CD_BC_5_CW_5"   , "CD_D_BC_1_CW_5"   , "CD_NoB_BC_1_CW_5"   , "CD_NoB_BC_5_CW_5"   , "CD_Norm_Own_Subtract_BC_1_CW_5"   , "CD_Norm_FieldYear_Subtract_BC_1_CW_5"   ,
                     "N_f_CW_10"  , "N_b_CW_10"  , "N_bf_BC_1_CW_10"  , "N_bf_BC_5_CW_10"  , "CD_BC_1_CW_10"  , "CD_BC_5_CW_10"  , "CD_D_BC_1_CW_10"  , "CD_NoB_BC_1_CW_10"  , "CD_NoB_BC_5_CW_10"  , "CD_Norm_Own_Subtract_BC_1_CW_10"  , "CD_Norm_FieldYear_Subtract_BC_1_CW_10"  ,
                     "N_f_CW_2022", "N_b_CW_2022", "N_bf_BC_1_CW_2022", "N_bf_BC_5_CW_2022", "CD_BC_1_CW_2022", "CD_BC_5_CW_2022", "CD_D_BC_1_CW_2022", "CD_NoB_BC_1_CW_2022", "CD_NoB_BC_5_CW_2022", "CD_Norm_Own_Subtract_BC_1_CW_2022", "CD_Norm_FieldYear_Subtract_BC_1_CW_2022")]

# #WRITE FP data to file
# write_xlsx(dataFP, "Marketing_Results.xlsx")

#############################
#############################
#GRAPHS

#SELECT DATA
dataFP <- dataFP_MARKETING
#dataFP <- dataFP_AI

#CREATE ROBUSTENESS SAMPLE
dataFP_ROBUST <- dataFP[(dataFP$NREF_ownCalc >= 10) & (dataFP$N_f_CW_3 + dataFP$N_bf_BC_1_CW_3 >= 10), ] #based on available data

#############################

#AGGREGATE
dataFP_aggYear <- aggregate(cbind(CD_BC_1_CW_3,   CD_BC_5_CW_3,   CD_D_BC_1_CW_3,   CD_NoB_BC_1_CW_3,   CD_NoB_BC_5_CW_3,   CD_Norm_Own_Subtract_BC_1_CW_3,   CD_Norm_FieldYear_Subtract_BC_1_CW_3,
                                  CD_BC_1_CW_5,   CD_BC_5_CW_5,   CD_D_BC_1_CW_5,   CD_NoB_BC_1_CW_5,   CD_NoB_BC_5_CW_5,   CD_Norm_Own_Subtract_BC_1_CW_5,   CD_Norm_FieldYear_Subtract_BC_1_CW_5,
                                  CD_BC_1_CW_10,  CD_BC_5_CW_10,  CD_D_BC_1_CW_10,  CD_NoB_BC_1_CW_10,  CD_NoB_BC_5_CW_10,  CD_Norm_Own_Subtract_BC_1_CW_10,  CD_Norm_FieldYear_Subtract_BC_1_CW_10,
                                  CD_BC_1_CW_2022,CD_BC_5_CW_2022,CD_D_BC_1_CW_2022,CD_NoB_BC_1_CW_2022,CD_NoB_BC_5_CW_2022,CD_Norm_Own_Subtract_BC_1_CW_2022,CD_Norm_FieldYear_Subtract_BC_1_CW_2022,
                                  ) ~ PY #+ SO
                            , data = dataFP_ROBUST
                            , FUN = mean, na.rm = T, na.action = NULL)

#select Citation window

#grouped by year
dataFP_aggYearFilter <- dataFP_aggYear[, c("PY", "CD_BC_1_CW_3"   , "CD_BC_5_CW_3"   , "CD_D_BC_1_CW_3"   , "CD_NoB_BC_1_CW_3"   , "CD_NoB_BC_5_CW_3"   , "CD_Norm_Own_Subtract_BC_1_CW_3"   , "CD_Norm_FieldYear_Subtract_BC_1_CW_3")]    #3
dataFP_aggYearFilter <- dataFP_aggYear[, c("PY", "CD_BC_1_CW_5"   , "CD_BC_5_CW_5"   , "CD_D_BC_1_CW_5"   , "CD_NoB_BC_1_CW_5"   , "CD_NoB_BC_5_CW_5"   , "CD_Norm_Own_Subtract_BC_1_CW_5"   , "CD_Norm_FieldYear_Subtract_BC_1_CW_5")]    #5
dataFP_aggYearFilter <- dataFP_aggYear[, c("PY", "CD_BC_1_CW_10"  , "CD_BC_5_CW_10"  , "CD_D_BC_1_CW_10"  , "CD_NoB_BC_1_CW_10"  , "CD_NoB_BC_5_CW_10"  , "CD_Norm_Own_Subtract_BC_1_CW_10"  , "CD_Norm_FieldYear_Subtract_BC_1_CW_10")]   #10
dataFP_aggYearFilter <- dataFP_aggYear[, c("PY", "CD_BC_1_CW_2022", "CD_BC_5_CW_2022", "CD_D_BC_1_CW_2022", "CD_NoB_BC_1_CW_2022", "CD_NoB_BC_5_CW_2022", "CD_Norm_Own_Subtract_BC_1_CW_2022", "CD_Norm_FieldYear_Subtract_BC_1_CW_2022")] #2022

# #grouped by year, EXCLUDE NO_B
# dataFP_aggYearFilter <- dataFP_aggYear[, c("PY", "CD_BC_1_CW_3"   , "CD_BC_5_CW_3"   , "CD_D_BC_1_CW_3"   , "CD_Norm_Own_Subtract_BC_1_CW_3"   , "CD_Norm_FieldYear_Subtract_BC_1_CW_3")]    #3
# dataFP_aggYearFilter <- dataFP_aggYear[, c("PY", "CD_BC_1_CW_5"   , "CD_BC_5_CW_5"   , "CD_D_BC_1_CW_5"   , "CD_Norm_Own_Subtract_BC_1_CW_5"   , "CD_Norm_FieldYear_Subtract_BC_1_CW_5")]    #5
# dataFP_aggYearFilter <- dataFP_aggYear[, c("PY", "CD_BC_1_CW_10"  , "CD_BC_5_CW_10"  , "CD_D_BC_1_CW_10"  , "CD_Norm_Own_Subtract_BC_1_CW_10"  , "CD_Norm_FieldYear_Subtract_BC_1_CW_10")]   #10
# dataFP_aggYearFilter <- dataFP_aggYear[, c("PY", "CD_BC_1_CW_2022", "CD_BC_5_CW_2022", "CD_D_BC_1_CW_2022", "CD_Norm_Own_Subtract_BC_1_CW_2022", "CD_Norm_FieldYear_Subtract_BC_1_CW_2022")] #2022

#gather
dataFP_aggYearFilter <- gather(dataFP_aggYearFilter, key = "CD_metric", value = "CD_value", -1, na.rm = F)

#set label text
labels5 <- c(bquote(CD[1]), bquote(CD[5]), bquote(CD^"*"), bquote(CD[norm]^paper), bquote(CD[norm]^paste(field, "*", year)))
labels7 <- c(bquote(CD[1]), bquote(CD[5]), bquote(CD^"*"), bquote(CD[1]^nob), bquote(CD[5]^nob), bquote(CD[norm]^paper), bquote(CD[norm]^paste(field, "*", year)))

#BY YEAR
p1 <- ggplot(dataFP_aggYearFilter, aes(x = PY, y = CD_value, color = CD_metric)) +
  geom_line() +
  #geom_point(size = 0.5) +
  geom_hline(yintercept = 0, size = 0.5, color = "darkslategrey") +
  labs(title = "", x = "Publication year", y = "Average CD value") +
  theme_bw() +
  #ylim(-1,1) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_x_continuous(breaks = seq(1975, 2020, 5)) +
  scale_color_manual(name = "CD metric",
                     labels = labels7,
                     values = c("dodgerblue2", "#E31A1C", "green4", "#FF7F00", "#6A3D9A", "yellow3", "darkorange4", "black", "deeppink1"))

p1

#####################################
#grouped by year + journal

#AGGREGATE PUB YEAR
dataFP_aggYear <- aggregate(cbind(CD_BC_1_CW_3,   CD_BC_5_CW_3,   CD_D_BC_1_CW_3,   CD_NoB_BC_1_CW_3,   CD_NoB_BC_5_CW_3,   CD_Norm_Own_Subtract_BC_1_CW_3,   CD_Norm_FieldYear_Subtract_BC_1_CW_3,
                                  CD_BC_1_CW_5,   CD_BC_5_CW_5,   CD_D_BC_1_CW_5,   CD_NoB_BC_1_CW_5,   CD_NoB_BC_5_CW_5,   CD_Norm_Own_Subtract_BC_1_CW_5,   CD_Norm_FieldYear_Subtract_BC_1_CW_5,
                                  CD_BC_1_CW_10,  CD_BC_5_CW_10,  CD_D_BC_1_CW_10,  CD_NoB_BC_1_CW_10,  CD_NoB_BC_5_CW_10,  CD_Norm_Own_Subtract_BC_1_CW_10,  CD_Norm_FieldYear_Subtract_BC_1_CW_10,
                                  CD_BC_1_CW_2022,CD_BC_5_CW_2022,CD_D_BC_1_CW_2022,CD_NoB_BC_1_CW_2022,CD_NoB_BC_5_CW_2022,CD_Norm_Own_Subtract_BC_1_CW_2022,CD_Norm_FieldYear_Subtract_BC_1_CW_2022,
                                          ) ~ PY + SO
                              , data = dataFP_ROBUST
                              , FUN = mean, na.rm = T, na.action = NULL)

#create average
dataFP_aggYear_AVERAGE <- aggregate(cbind(CD_BC_1_CW_3,   CD_BC_5_CW_3,   CD_D_BC_1_CW_3,   CD_NoB_BC_1_CW_3,   CD_NoB_BC_5_CW_3,   CD_Norm_Own_Subtract_BC_1_CW_3,   CD_Norm_FieldYear_Subtract_BC_1_CW_3,
                                          CD_BC_1_CW_5,   CD_BC_5_CW_5,   CD_D_BC_1_CW_5,   CD_NoB_BC_1_CW_5,   CD_NoB_BC_5_CW_5,   CD_Norm_Own_Subtract_BC_1_CW_5,   CD_Norm_FieldYear_Subtract_BC_1_CW_5,
                                          CD_BC_1_CW_10,  CD_BC_5_CW_10,  CD_D_BC_1_CW_10,  CD_NoB_BC_1_CW_10,  CD_NoB_BC_5_CW_10,  CD_Norm_Own_Subtract_BC_1_CW_10,  CD_Norm_FieldYear_Subtract_BC_1_CW_10,
                                          CD_BC_1_CW_2022,CD_BC_5_CW_2022,CD_D_BC_1_CW_2022,CD_NoB_BC_1_CW_2022,CD_NoB_BC_5_CW_2022,CD_Norm_Own_Subtract_BC_1_CW_2022,CD_Norm_FieldYear_Subtract_BC_1_CW_2022,
                                          ) ~ PY
                                    , data = dataFP_ROBUST
                                    , FUN = mean, na.rm = T, na.action = NULL)
dataFP_aggYear_AVERAGE$SO <- "Average"
dataFP_aggYear <- rbind(dataFP_aggYear, dataFP_aggYear_AVERAGE)
names(dataFP_aggYear)[2] <- "Journal"

#select columns
dataFP_aggYearFilter <- dataFP_aggYear[, c("Journal", "PY", "CD_NoB_BC_5_CW_3")]
#gather
dataFP_aggYearFilter <- gather(dataFP_aggYearFilter, key = "CD_metric", value = "CD_value", -(1:2), na.rm = F)

#BY YEAR + JOURNAL
p2 <- ggplot(dataFP_aggYearFilter, aes(x = PY, y = CD_value, color = Journal, linetype = Journal)) +
  geom_line() +
  geom_point(size = 0.5) +
  geom_hline(yintercept = 0, size = 0.5, color = "darkslategrey") +
  #geom_vline(aes(xintercept = 0)) +
  labs(title = "", x = "Publication year", y = bquote("Average" ~ CD[5]^nob ~ "value")) +
  theme_bw() +
  #ylim(-1,1) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 18, face = "bold")) +
  scale_x_continuous(breaks = seq(1975, 2020, 5)) +
  # scale_color_manual(values = c("black", "dodgerblue2", "#E31A1C", "green4", "#FF7F00", "#6A3D9A", "yellow3", "darkorange4")) +
  # scale_linetype_manual(values = c("twodash", "solid", "solid", "solid", "solid"))
  scale_color_manual(values = c("dodgerblue2", "#E31A1C", "black", "green4", "#FF7F00", "#6A3D9A", "yellow3", "darkorange4")) +
  scale_linetype_manual(values = c("solid", "solid", "twodash", "solid", "solid"))

p2

##############
#Table with summary statistics

#select data
dataFP_ROBUST_ALL <- rbind(dataFP_MARKETING, dataFP_AI)

#CD to NA for PY outside of scope
for (a in 1:nrow(dataFP_ROBUST_ALL)){
    if( (is.na(dataFP_ROBUST_ALL$N_f_CW_3[a])) | (is.na(dataFP_ROBUST_ALL$N_bf_BC_1_CW_3[a])) |
        (dataFP_ROBUST_ALL$NREF_ownCalc[a] < 10) | 
        (dataFP$N_f_CW_3[a] + dataFP$N_bf_BC_1_CW_3[a] < 10) ){
      dataFP_ROBUST_ALL$CD_NoB_BC_5_CW_3[a] <- NA
    }
  if( (is.na(dataFP_ROBUST_ALL$N_f_CW_5[a])) | (is.na(dataFP_ROBUST_ALL$N_bf_BC_1_CW_5[a])) |
      (dataFP_ROBUST_ALL$NREF_ownCalc[a] < 10) | 
      (dataFP$N_f_CW_5[a] + dataFP$N_bf_BC_1_CW_5[a] < 10) ){
    dataFP_ROBUST_ALL$CD_NoB_BC_5_CW_5[a] <- NA
  }
  if( (is.na(dataFP_ROBUST_ALL$N_f_CW_10[a])) | (is.na(dataFP_ROBUST_ALL$N_bf_BC_1_CW_10[a])) |
      (dataFP_ROBUST_ALL$NREF_ownCalc[a] < 10) | 
      (dataFP$N_f_CW_10[a] + dataFP$N_bf_BC_1_CW_10[a] < 10) ){
    dataFP_ROBUST_ALL$CD_NoB_BC_5_CW_10[a] <- NA
  }
  if( (is.na(dataFP_ROBUST_ALL$N_f_CW_2022[a])) | (is.na(dataFP_ROBUST_ALL$N_bf_BC_1_CW_2022[a])) |
      (dataFP_ROBUST_ALL$NREF_ownCalc[a] < 10) | 
      (dataFP$N_f_CW_2022[a] + dataFP$N_bf_BC_1_CW_2022[a] < 10) ){
    dataFP_ROBUST_ALL$CD_NoB_BC_5_CW_2022[a] <- NA
  }
}

disruptSumStat <- dataFP_ROBUST_ALL %>% group_by(SO) %>%
  summarise(#Min._3 = min(CD_NoB_BC_5_CW_3, na.rm = T),
            Median_3 = median(CD_NoB_BC_5_CW_3, na.rm = T),
            Mean_3 = mean(CD_NoB_BC_5_CW_3, na.rm = T),
            #Max_3 = max(CD_NoB_BC_5_CW_3, na.rm = T),
            Std_3 = sd(CD_NoB_BC_5_CW_3, na.rm = T),
            #Min._5 = min(CD_NoB_BC_5_CW_5, na.rm = T),
            Median_5 = median(CD_NoB_BC_5_CW_5, na.rm = T),
            Mean_5 = mean(CD_NoB_BC_5_CW_5, na.rm = T),
            #Max_5 = max(CD_NoB_BC_5_CW_5, na.rm = T),
            Std_5 = sd(CD_NoB_BC_5_CW_5, na.rm = T),
            #Min._10 = min(CD_NoB_BC_5_CW_10, na.rm = T),
            Median_10 = median(CD_NoB_BC_5_CW_10, na.rm = T),
            Mean_10 = mean(CD_NoB_BC_5_CW_10, na.rm = T),
            #Max_10 = max(CD_NoB_BC_5_CW_10, na.rm = T),
            Std_10 = sd(CD_NoB_BC_5_CW_10, na.rm = T),
            #Min._2022 = min(CD_NoB_BC_5_CW_2022, na.rm = T),
            Median_2022 = median(CD_NoB_BC_5_CW_2022, na.rm = T),
            Mean_2022 = mean(CD_NoB_BC_5_CW_2022, na.rm = T),
            #Max_2022 = max(CD_NoB_BC_5_CW_2022, na.rm = T),
            Std_2022 = sd(CD_NoB_BC_5_CW_2022, na.rm = T))
disruptSumStat <- disruptSumStat %>% mutate_if(is.numeric, round, 2)

# #write df to xlsx
# write_xlsx(disruptSumStat, "temp.xlsx")


########################
#NREF COMPARISON

#NREF
dataFP_aggYear_NREF <- aggregate(cbind(NREF, NREF_ownCalc) ~ PY #+ SO
                                 , data = dataFP#_ROBUST
                                 , FUN = mean, na.rm = T, na.action = NULL)
#add diff column
dataFP_aggYear_NREF$Percentage <- (dataFP_aggYear_NREF$NREF_ownCalc / dataFP_aggYear_NREF$NREF) * 100

#gather
dataFP_aggYear_NREF <- gather(dataFP_aggYear_NREF, key = "NREF_metric", value = "NREF_value", -1, na.rm = F)

p3 <- ggplot(dataFP_aggYear_NREF, aes(x = PY, y = NREF_value, color = NREF_metric, linetype = NREF_metric)) +
  geom_line() +
  geom_point(size = 0.5) +
  geom_hline(yintercept = 0, size = 0.5, color = "darkslategrey") +
  labs(title = "", x = "Publication year", y = "Average reference count") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_y_continuous(sec.axis = sec_axis(~ . * 1, name = "Percentage available / actual")) +
  scale_x_continuous(breaks = seq(1975, 2020, 5)) +
  scale_linetype_manual(name = "Reference count",
                        labels = c("Actual", "Available in WoS", "% Actual"),
                        values = c("solid", "solid", "twodash")) +
  scale_color_manual(name = "Reference count",
                     labels = c("Actual", "Available in WoS", "% Actual"),
                     values = c("dodgerblue2", "#E31A1C", "black"))

p3


#####################################
#FIELD VS FIELD

dataFP_FIELDS <- rbind(dataFP_MARKETING, dataFP_AI)

#CREATE ROBUSTENESS SAMPLE
dataFP_FIELDS_ROBUST <- dataFP_FIELDS[which((dataFP_FIELDS$NREF_ownCalc >= 10) & (dataFP_FIELDS$N_f_CW_2022 + dataFP_FIELDS$N_bf_BC_1_CW_2022 >= 10)), ]

#AGGREGATE PUB YEAR
dataFP_FIELDS_aggYear <- aggregate(cbind(CD_BC_1_CW_3,   CD_BC_5_CW_3,   CD_D_BC_1_CW_3,   CD_NoB_BC_1_CW_3,   CD_NoB_BC_5_CW_3,   CD_Norm_Own_Subtract_BC_1_CW_3,   CD_Norm_FieldYear_Subtract_BC_1_CW_3,
                                        CD_BC_1_CW_5,   CD_BC_5_CW_5,   CD_D_BC_1_CW_5,   CD_NoB_BC_1_CW_5,   CD_NoB_BC_5_CW_5,   CD_Norm_Own_Subtract_BC_1_CW_5,   CD_Norm_FieldYear_Subtract_BC_1_CW_5,
                                        CD_BC_1_CW_10,  CD_BC_5_CW_10,  CD_D_BC_1_CW_10,  CD_NoB_BC_1_CW_10,  CD_NoB_BC_5_CW_10,  CD_Norm_Own_Subtract_BC_1_CW_10,  CD_Norm_FieldYear_Subtract_BC_1_CW_10,
                                        CD_BC_1_CW_2022,CD_BC_5_CW_2022,CD_D_BC_1_CW_2022,CD_NoB_BC_1_CW_2022,CD_NoB_BC_5_CW_2022,CD_Norm_Own_Subtract_BC_1_CW_2022,CD_Norm_FieldYear_Subtract_BC_1_CW_2022,
                                        ) ~ PY + Field
                                   , data = dataFP_FIELDS_ROBUST
                                   , FUN = mean, na.rm = T, na.action = NULL)

#select columns
dataFP_FIELDS_aggYearFilter <- dataFP_FIELDS_aggYear[, c("Field", "PY", "CD_NoB_BC_5_CW_2022")]
#gather
dataFP_FIELDS_aggYearFilter <- gather(dataFP_FIELDS_aggYearFilter, key = "CD_metric", value = "CD_value", -(1:2), na.rm = F)

#BY YEAR + FIELD
pField <- ggplot(dataFP_FIELDS_aggYearFilter, aes(x = PY, y = CD_value, color = Field)) +
  geom_line() +
  geom_point(size = 0.5) +
  geom_hline(yintercept = 0, size = 0.5, color = "darkslategrey") +
  labs(title = "", x = "Publication year", y = bquote("Average" ~ CD[5]^nob ~ "value")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 18, face = "bold")) +
  scale_x_continuous(breaks = seq(1975, 2020, 5)) +
  scale_color_manual(values = c("dodgerblue2", "#E31A1C", "green4", "#FF7F00", "#6A3D9A", "yellow3", "darkorange4"))

pField



#####################################
#REGRESSION

#select

dataRegression <- dataFP_MARKETING
#dataRegression <- dataFP_AI

#add number of authors
dataRegression$n_Authors <- str_count(dataRegression$Authors, ";") + 1

#create robustness samples
dataRegression3 <- dataRegression[which((dataRegression$NREF_ownCalc >= 10) & 
                                                      (dataRegression$N_f_CW_3 + dataRegression$N_bf_BC_1_CW_3 >= 10)), ]
dataRegression3 <- dataRegression3[, c("CD_NoB_BC_5_CW_3", "PY", "NREF", "NREF_Diff", "n_Authors", "ArticleLength", "TC_3_ownCalc")]
dataRegression3$PY <- as.factor(dataRegression3$PY)

dataRegression5 <- dataRegression[which((dataRegression$NREF_ownCalc >= 10) & 
                                                      (dataRegression$N_f_CW_5 + dataRegression$N_bf_BC_1_CW_5 >= 10)), ]
dataRegression5 <- dataRegression5[, c("CD_NoB_BC_5_CW_5", "PY", "NREF", "NREF_Diff", "n_Authors", "ArticleLength", "TC_5_ownCalc")]
dataRegression5$PY <- as.factor(dataRegression5$PY)

dataRegression10 <- dataRegression[which((dataRegression$NREF_ownCalc >= 10) & 
                                                    (dataRegression$N_f_CW_10 + dataRegression$N_bf_BC_1_CW_10 >= 10)), ]
dataRegression10 <- dataRegression10[, c("CD_NoB_BC_5_CW_10", "PY", "NREF", "NREF_Diff", "n_Authors", "ArticleLength", "TC_10_ownCalc")]
dataRegression10$PY <- as.factor(dataRegression10$PY)

dataRegression2022 <- dataRegression[which((dataRegression$NREF_ownCalc >= 10) & 
                                                      (dataRegression$N_f_CW_2022 + dataRegression$N_bf_BC_1_CW_2022 >= 10)), ]
dataRegression2022 <- dataRegression2022[, c("CD_NoB_BC_5_CW_2022", "PY", "NREF", "NREF_Diff", "n_Authors", "ArticleLength", "TC_2022_ownCalc")]
dataRegression2022$PY <- as.factor(dataRegression2022$PY)


#CW3
cdOLS3_A <- lm(CD_NoB_BC_5_CW_3 ~ PY
          , data = dataRegression3)
summary(cdOLS3_A)
cdOLS3_B <- lm(CD_NoB_BC_5_CW_3 ~ PY + NREF + NREF_Diff + n_Authors + ArticleLength
               , data = dataRegression3)
summary(cdOLS3_B)

#create prediction data
predictData_3 <- data.frame(levels(dataRegression3$PY),
                            mean(dataRegression3$NREF, na.rm = T),
                            mean(dataRegression3$NREF_Diff, na.rm = T),
                            mean(dataRegression3$n_Authors, na.rm = T),
                            mean(dataRegression3$ArticleLength, na.rm = T)
                            )
#predictData_3 <- data.frame(levels(dataRegression3$PY), 0,0,0,0)
names(predictData_3) <- names(dataRegression3)[-1]
predictData_3$PY <- as.factor(predictData_3$PY)
#predict
predict_cdOLS3_B<- data.frame("PY" =  levels(dataRegression3$PY), "CD_value" = predict(cdOLS3_B, predictData_3))

# #ONLY FOR FIELD VS FIELD comparison
# #add field column
# predict_cdOLS3_B$Field <- "Marketing"
# predict_cdOLS3_B$Field <- "AI"
# #merge for field comparison
# predict_cdOLS3_B_FIELD <- predict_cdOLS3_B
# predict_cdOLS3_B_FIELD <- rbind(predict_cdOLS3_B_FIELD, predict_cdOLS3_B)
# predict_cdOLS3_B_FIELD$PY <- as.integer(predict_cdOLS3_B_FIELD$PY)



#CW5
cdOLS5_A <- lm(CD_NoB_BC_5_CW_5 ~ PY
               , data = dataRegression5)
summary(cdOLS5_A)
cdOLS5_B <- lm(CD_NoB_BC_5_CW_5 ~ PY + NREF + NREF_Diff + n_Authors + ArticleLength
               , data = dataRegression5)
summary(cdOLS5_B)
#create prediction data
predictData_5 <- data.frame(levels(dataRegression5$PY), 
                            mean(dataRegression5$NREF, na.rm = T), 
                            mean(dataRegression5$NREF_Diff, na.rm = T),
                            mean(dataRegression5$n_Authors, na.rm = T), 
                            mean(dataRegression5$ArticleLength, na.rm = T))
#predictData_5 <- data.frame(levels(dataRegression5$PY), 0,0,0,0)
names(predictData_5) <- names(dataRegression5)[-1]
predictData_5$PY <- as.factor(predictData_5$PY)
#predict
predict_cdOLS5_B<- data.frame("PY" =  levels(dataRegression5$PY), "CD_value" = predict(cdOLS5_B, predictData_5))

# #ONLY FOR FIELD VS FIELD comparison
# #add field column
# predict_cdOLS5_B$Field <- "AI"
# predict_cdOLS5_B$Field <- "Marketing"
# #merge for field comparison
# predict_cdOLS5_B_FIELD <- predict_cdOLS5_B
# predict_cdOLS5_B_FIELD <- rbind(predict_cdOLS5_B_FIELD, predict_cdOLS5_B)
# predict_cdOLS5_B_FIELD$PY <- as.integer(predict_cdOLS5_B_FIELD$PY)



#CW10
cdOLS10_A <- lm(CD_NoB_BC_5_CW_10 ~ PY
               , data = dataRegression10)
summary(cdOLS10_A)
cdOLS10_B <- lm(CD_NoB_BC_5_CW_10 ~ PY + NREF + NREF_Diff + n_Authors + ArticleLength
               , data = dataRegression10)
summary(cdOLS10_B)
#create prediction data
predictData_10 <- data.frame(levels(dataRegression10$PY), 
                            mean(dataRegression10$NREF, na.rm = T), 
                            mean(dataRegression10$NREF_Diff, na.rm = T), 
                            mean(dataRegression10$n_Authors, na.rm = T), 
                            mean(dataRegression10$ArticleLength, na.rm = T))
#predictData_10 <- data.frame(levels(dataRegression10$PY), 0,0,0,0)
names(predictData_10) <- names(dataRegression10)[-1]
predictData_10$PY <- as.factor(predictData_10$PY)
#predict
predict_cdOLS10_B<- data.frame("PY" =  levels(dataRegression10$PY), "CD_value" = predict(cdOLS10_B, predictData_10))

# #ONLY FOR FIELD VS FIELD comparison
# #add field column
# predict_cdOLS10_B$Field <- "AI"
# predict_cdOLS10_B$Field <- "Marketing"
# #merge for field comparison
# predict_cdOLS10_B_FIELD <- predict_cdOLS10_B
# predict_cdOLS10_B_FIELD <- rbind(predict_cdOLS10_B_FIELD, predict_cdOLS10_B)
# predict_cdOLS10_B_FIELD$PY <- as.integer(predict_cdOLS10_B_FIELD$PY)




#CW2022
cdOLS2022_A <- lm(CD_NoB_BC_5_CW_2022 ~ PY
               , data = dataRegression2022)
summary(cdOLS2022_A)
cdOLS2022_B <- lm(CD_NoB_BC_5_CW_2022 ~ PY + NREF + NREF_Diff + n_Authors + ArticleLength
               , data = dataRegression2022)
summary(cdOLS2022_B)
#create prediction data
predictData_2022 <- data.frame(levels(dataRegression2022$PY), 
                             mean(dataRegression2022$NREF, na.rm = T), 
                             mean(dataRegression2022$NREF_Diff, na.rm = T), 
                             mean(dataRegression2022$n_Authors, na.rm = T), 
                             mean(dataRegression2022$ArticleLength, na.rm = T))
#predictData_2022<- data.frame(levels(dataRegression2022$PY),, 0,0,0,0)
names(predictData_2022) <- names(dataRegression2022)[-1]
predictData_2022$PY <- as.factor(predictData_2022$PY)
#predict
predict_cdOLS2022_B<- data.frame("PY" = levels(dataRegression2022$PY), "CD_value" = predict(cdOLS2022_B, predictData_2022))

# #ONLY FOR FIELD VS FIELD comparison
# #add field column
# predict_cdOLS2022_B$Field <- "AI"
# predict_cdOLS2022_B$Field <- "Marketing"
# #merge for field comparison
# predict_cdOLS2022_B_FIELD <- predict_cdOLS2022_B
# predict_cdOLS2022_B_FIELD <- rbind(predict_cdOLS2022_B_FIELD, predict_cdOLS2022_B)
# predict_cdOLS2022_B_FIELD$PY <- as.integer(predict_cdOLS2022_B_FIELD$PY)


#GRAPH FOR FIELD VS FIELD (single citation window at a time)
pPREDICT <- ggplot(predict_cdOLS3_B_FIELD, aes(x = PY, y = CD_value, color = Field)) +
  geom_line() +
  geom_point(size = 0.5) +
  geom_hline(yintercept = 0, size = 0.5, color = "darkslategrey") +
  labs(title = "", x = "Publication year", y = bquote("Average" ~ CD[5]^nob ~ "value")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 18, face = "bold")) +
  scale_x_continuous(breaks = seq(1975, 2020, 5)) +
  scale_color_manual(values = c("dodgerblue2", "#E31A1C", "green4", "#FF7F00", "#6A3D9A", "yellow3", "darkorange4"))

pPREDICT

#OLS output
texreg(
  list(cdOLS3_A, cdOLS3_B, cdOLS5_A, cdOLS5_B, cdOLS10_A, cdOLS10_B, cdOLS2022_A, cdOLS2022_B),
  custom.header = list(
    "M1" = 1,
    "M2" = 2,
    "M3" = 3,
    "M4" = 4,
    "M5" = 5,
    "M6" = 6,
    "M7" = 7,
    "M8" = 8
  ),
  custom.model.names = c("TEST1", "TEST2", "TEST3", "TEST4", "TEST5", "TEST6", "TEST7", "TEST8"),
  caption = "Estimation results for OLS",
  caption.above = TRUE,
  center = TRUE,
  bold = 0.05,
  label = "tab:OLS",
  float.pos = "H",
  single.row = TRUE,
  #,longtable = TRUE
  include.fstatistic = T
  #include.rmse = T
)
