library(data.table)
library(stringr)

#get file list
file_list <- list.files(path = "")

#init blank data.table
CP <- data.table()

#read files
for (i in 1:length(file_list)){
  print(i)
  
  #read
  temp_data <- fread(file_list[i], quote = "")
  
  #filter and modify
  temp_data <- tolower(temp_data[,c("DI", "PY", "CR")])
  
  #replace
  temp_data$CR <- str_extract_all(temp_data$CR, "(10\\.[^\\s,\\]\\n\\;]+)(?=[\\s,\\]\\n]*)")
  
  #combine
  CP <- rbindlist(list(CP, temp_data))
}

#remove missing DOIs
CP <- CP[!is.na(CP$DI), ]
CP <- CP[DI != "", ]
#remove duplicate DOIs
CP <- CP[!duplicated(CP$DI), ]
#DOIS toLower
CP$DI <- tolower(CP$DI)
CP$CR <- lapply(CP$CR, tolower)
#rename cols
names(CP) <- c("CP_DOI","CP_PY","CR")
