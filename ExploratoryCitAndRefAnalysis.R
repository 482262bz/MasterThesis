library(readxl)
library(writexl)
library(foreign)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)

#############################
#LOAD DATA

#Marketing
dataFP_report <- read_excel("Marketing_FP_CitationReport.xlsx")
dataFP_core <- read.dbf("Marketing_FP_core.dbf", as.is = F)

#AI
dataFP_report <- read_excel("AI_FP_CitationReport.xlsx")
dataFP_core <- read.dbf("AI_FP_core.dbf", as.is = F)

#############################
#WRANGLE DATA

#clean up DOI column (for merging)
dataFP_report$DOI <- str_trim(tolower(dataFP_report$DOI))
dataFP_core$DOI <- str_trim(tolower(dataFP_core$DOI))
#merge
dataFP <- merge(dataFP_report, dataFP_core, by = "DOI")
rm(dataFP_report, dataFP_core)
#keep relevant columns only
dataFP <- dataFP[ , c("DOI", "Title", "Authors", "NR", "SO", "TC", "PY", "PD", "NREF", as.character(seq(1975, 2023)))]
#set Publication year column to int
dataFP$PY <- as.integer(as.character(dataFP$PY))
#set factor columns to char
dataFP <- dataFP %>% mutate(across(where(is.factor), as.character))
#remove missing DOIs
dataFP <- dataFP[!is.na(dataFP$DOI), ]
#remove duplicate DOIs
dataFP <- dataFP[!duplicated(dataFP$DOI), ]
#remove FPs published >= 2020 & < 1975 (minimal Citation Window of 3 years)
dataFP <- dataFP[dataFP$PY >= 1975 & dataFP$PY < 2020, ]

#add citation count for different windows
for (a in 1:nrow(dataFP)){
  if(dataFP$PY[a] <= 2019){
    dataFP$TC_3[a] <- rowSums(dataFP[a, as.character(seq(dataFP$PY[a], (dataFP$PY[a] + 3)))])
  } else {
    dataFP$TC_3[a] <- NA
  }
  if(dataFP$PY[a] <= 2017){
    dataFP$TC_5[a] <- rowSums(dataFP[a, as.character(seq(dataFP$PY[a], (dataFP$PY[a] + 5)))])
  } else {
    dataFP$TC_5[a] <- NA
  } 
  if(dataFP$PY[a] <= 2012){
    dataFP$TC_10[a] <- rowSums(dataFP[a, as.character(seq(dataFP$PY[a], (dataFP$PY[a] + 10)))])
  } else {
    dataFP$TC_10[a] <- NA
  }
  if(dataFP$PY[a] <= 2019){
    dataFP$TC_2022[a] <- rowSums(dataFP[a, as.character(seq(dataFP$PY[a], 2022))])
  } else {
    dataFP$TC_2022[a] <- NA
  }
}

#reorder columns
dataFP <- dataFP[ , c("DOI", "Title", "Authors", "NR", "SO", "TC_3", "TC_5", "TC_10", "TC_2022", "TC", "PY", "PD", "NREF")] #, as.character(seq(1975, 2023)))]

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

#############################
#PUBLICATION COUNT by YEAR (& JOURNAL)

#complete sample
pubCount <- dataFP %>% group_by(SO, PY) %>%
  summarise(count3 = sum(!is.na(TC_3)),
            count5 = sum(!is.na(TC_5)),
            count10 = sum(!is.na(TC_10)),
            count2022 = sum(!is.na(TC_2022)))

# #ROBUSTNESS SAMPLE
# pubCount <- dataFP %>% group_by(SO, PY) %>%
#   summarise(count3 = sum((NREF_ownCalc >= 10) & (N_f_CW_3 + N_bf_BC_1_CW_3 >= 10), na.rm = T),
#             count5 = sum((NREF_ownCalc >= 10) & (N_f_CW_5 + N_bf_BC_1_CW_5 >= 10), na.rm = T),
#             count10 = sum((NREF_ownCalc >= 10) & (N_f_CW_10 + N_bf_BC_1_CW_10 >= 10), na.rm = T),
#             count2022 = sum((NREF_ownCalc >= 10) & (N_f_CW_2022 + N_bf_BC_1_CW_2022 >= 10), na.rm = T))

pubCount <- pivot_wider(data = pubCount, 
                        names_from = SO, 
                        values_from = c("count3", "count5", "count10", "count2022"))
pubCount[nrow(pubCount) + 1, 2:ncol(pubCount)] <- as.list(colSums(pubCount[2:ncol(pubCount)], na.rm = T)) #journal totals


#create table for pub count of single citation window
dataFP_filtered <- dataFP[!is.na(dataFP$TC_3), ]
pubCount <- dataFP_filtered %>% group_by(SO, PY) %>%
  summarise(count = n())
pubCount <- spread(pubCount, key = "SO", value = count)
pubCount$`% Total` <- 0
for (i in 1:nrow(pubCount)){
  pubCount$`% Total`[i] <- round(sum(pubCount[i, 2:5], na.rm = T) / sum(pubCount[,2:5], na.rm = T) * 100, 2)
}
pubCount <- gather(pubCount, key = "Journal", value = "Publications", -1, na.rm = F)
# pubCount[nrow(pubCount) + 1, 2:6] <- as.list(colSums(pubCount[2:6], na.rm = T)) #journal totals
names(pubCount)[1] <- "Publication year"

#create graph for pub count of single citation window
pubCount$Publications[pubCount$Journal == "% Total"] <- pubCount$Publications[pubCount$Journal == "% Total"] * 10
p1 <- ggplot(pubCount, aes(x = `Publication year`, y = Publications, color = Journal, linetype = Journal)) +
  geom_line() +
  labs(title = "", x = "Publication year", y = "Publication count") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 18, face = "bold")) +
  scale_x_continuous(breaks = seq(1975, 2020, 5)) +
  scale_y_continuous(sec.axis = sec_axis(~ . * 0.1, name = "Percentage of total sample")) +
  scale_color_manual(values = c("black", "dodgerblue2", "#E31A1C", "green4","#FF7F00")) +
  scale_linetype_manual(values = c("twodash", "solid", "solid", "solid", "solid"))
p1

#############################
#CITATION COUNT (for single citation window)

dataFP_filtered <- dataFP[!is.na(dataFP$TC_2022), ]
citCountWindow <- dataFP_filtered$TC_2022

citCount <- aggregate(x = citCountWindow, by = list(dataFP_filtered$SO, dataFP_filtered$PY), FUN = mean)
citCount <- spread(citCount, key = "Group.1", value = "x")
for (j in 1:nrow(citCount)){
  citCount$Average[j] <- rowMeans(citCount[j, 2:5], na.rm = T)
}
citCount <- gather(citCount, key = "Journal", value = "Citations", -1, na.rm = F)
citCount$Citations <- round(citCount$Citations, 0)
names(citCount)[1] <- "Publication year"

# #create df with averages for FIELDS
# aiAVG <- citCount[citCount$Journal == "Average",]
# aiAVG$Journal <- "AI"
# marketingAVG <- citCount[citCount$Journal == "Average",]
# marketingAVG$Journal <- "Marketing"
# citCount <- rbind(aiAVG, marketingAVG)
# names(citCount)[2] <- "Field"

###GRAPH###
#p2 <- ggplot(citCount, aes(x = `Publication year`, y = Citations, color = Journal, linetype = Journal)) + #journal level
p2 <- ggplot(citCount, aes(x = `Publication year`, y = Citations, color = Field, linetype = Field)) + #field level
  geom_line() +
  labs(title = "", x = "Publication year", y = "Average citation count") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 18, face = "bold")) +
  scale_x_continuous(breaks = seq(1975, 2020, 5)) +
  scale_color_manual(values = c("dodgerblue2", "#E31A1C", "black", "green4","#FF7F00")) +
  scale_linetype_manual(values = c("solid", "solid", "twodash", "solid", "solid"))
p2

#############################
#REFERNCE COUNT (for single citation window)

refCount <- aggregate(x = dataFP_filtered$NREF, by = list(dataFP_filtered$SO, dataFP_filtered$PY), FUN = mean)
refCount <- spread(refCount, key = "Group.1", value = "x")
for (j in 1:nrow(refCount)){
  refCount$Average[j] <- rowMeans(refCount[j, 2:5], na.rm = T)
}
refCount <- gather(refCount, key = "Journal", value = "References", -1, na.rm = F)
refCount$References <- round(refCount$References, 0)
names(refCount)[1] <- "Publication year"

# #create df with averages
# aiAVG <- refCount[refCount$Journal == "Average",]
# aiAVG$Journal <- "AI"
# marketingAVG <- refCount[refCount$Journal == "Average",]
# marketingAVG$Journal <- "Marketing"
# refCount <- rbind(aiAVG, marketingAVG)
# names(refCount)[2] <- "Field"

###GRAPH###
p3 <- ggplot(refCount, aes(x = `Publication year`, y = References, color = Journal, linetype = Journal)) + #journal level
#p3 <- ggplot(refCount, aes(x = `Publication year`, y = References, color = Field, linetype = Field)) + #field level
  geom_line() +
  labs(title = "", x = "Publication year", y = "Average reference count") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 18, face = "bold")) +
  scale_x_continuous(breaks = seq(1975, 2020, 5)) +
  # scale_color_manual(values = c("dodgerblue2", "#E31A1C")) + #field level
  # scale_linetype_manual(values = c("solid", "solid")) #field level
  scale_color_manual(values = c("dodgerblue2", "#E31A1C", "black", "green4","#FF7F00")) + #journal level
  scale_linetype_manual(values = c("solid", "solid", "twodash", "solid", "solid")) #journal level
p3

#############################
#CITATIONS SUMSTAT

citSumStat <- dataFP_filtered %>% #group_by(SO) %>%
  summarise(Field = "AI",
            #Min._3 = min(TC_3, na.rm = T),
            Median_3 = median(TC_3, na.rm = T),
            Mean_3 = mean(TC_3, na.rm = T),
            Max._3 = max(TC_3, na.rm = T),
            #Min._5 = min(TC_5, na.rm = T),
            Median_5 = median(TC_5, na.rm = T),
            Mean_5 = mean(TC_5, na.rm = T),
            Max._5 = max(TC_5, na.rm = T),
            #Min._10 = min(TC_10, na.rm = T),
            Median_10 = median(TC_10, na.rm = T),
            Mean_10 = mean(TC_10, na.rm = T),
            Max._10 = max(TC_10, na.rm = T),
            #Min._2022 = min(TC_2022, na.rm = T),
            Median_2022 = median(TC_2022, na.rm = T),
            Mean_2022 = mean(TC_2022, na.rm = T),
            Max._2022 = max(TC_2022, na.rm = T))
citSumStat <- citSumStat %>% mutate_if(is.numeric, round, 0)

#create df with averages
aiAVG <- citSumStat
marketingAVG <- citSumStat
citSumStat <- rbind(aiAVG, marketingAVG)


#############################
#REFERENCES SUMSTAT

#single citation window
dataFP_filtered <- dataFP
refSumStat <- dataFP_filtered %>% #group_by(SO) %>%
  summarise(Field = "Marketing",
            Min. = min(NREF),
            Median = median(NREF),
            Mean = mean(NREF),
            Max. = max(NREF))
refSumStat <- refSumStat %>% mutate_if(is.numeric, round, 0)

#create df with averages
aiAVG <- refSumStat
marketingAVG <- refSumStat
refSumStat <- rbind(aiAVG, marketingAVG)

#############################
# #write df to xlsx
# write_xlsx(pubCount, "temp.xlsx")
