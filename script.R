#libs
library(xlsx)
library(tidyverse)


processor <- function(name) {
  #read the data from input directory in wd
  data <- read.csv2(paste0(getwd(), "/input/", name), 
                    header = T,
                    stringsAsFactors = F,
                    fileEncoding = 'cp1251')
  
  #create subset to actual email and 11th grade in the moment of Tournament
  sub <- subset(data, data$e.mail != "" & (data$Класс == '11'))
  
  #write a .xlsx table with filtered data
  write.xlsx(x = sub, 
             file = paste0(getwd(), "/output/candidates_", name, ".xlsx"), 
             sheetName = "Candidates")
}

#read the name of simple processing files
vec <- list.files(path = paste0(getwd(), "/input/"))

#apply function to all of them
lapply(vec, processor)


#advanced processing: one string -- one team, with all of candidates
#requires pre-processing: name, school, etc need to be marked with "_cap" postfix accorded to captain (first colums)
#and "2", "3", etc postfixes to another participants

processorAdvanced <- function(name) {
  #read the data
  data <- read.csv2(paste0(getwd(), "/input/", name), 
                    header = T,
                    stringsAsFactors = F,
                    fileEncoding = 'cp1251')
  #delete all unneeded columns (region, year of participant, schools, mentors et al)
  data <- data[-c(1:6)]
  data <- data %>% select(-contains("X"))
  
  #group data by pistfix in column (see preprocessing)
  exprVector <- c("_cap", "2", "3", "4", "5", "6")
  listCandidates <- lapply(exprVector, function(x) {data <- data[, grepl(x, names(data))]})
  names(listCandidates) <- exprVector
  
  #delete postfixes^ they're no more needed
  names(listCandidates[[1]]) <- unlist(lapply(names(listCandidates[[1]]), function(x) { str_replace_all(x, "_cap", "") }))
  for (i in c(2:length(listCandidates))) {
    names(listCandidates[[i]]) <- unlist(lapply(names(listCandidates[[i]]), function(x) { str_replace_all(x, "\\d", "") }))
  }
  
  #create subset by class and actual email
  listCandidatesFiltered <- lapply(listCandidates, function(x) {x <- subset(x, (x$email != "") & (x$class == 11))})
  
  #saving six files with filtered candidates
  xlsxNames <- lapply(names(listCandidatesFiltered), function(x) {paste0("candidates_regetap2024_", x, ".xlsx")})
  for (i in c(1:length(listCandidatesFiltered))) {
    write.xlsx(x = listCandidatesFiltered[[i]], file = paste0(getwd(), "/output/", xlsxNames[i]), sheetName = xlsxNames[i])
  }
}


processorAdvanced("regetap2024.csv")


#special case: class is not only 11th. Using the >= filteration: >=9 for two-years distance, >=8 for three-years, etc.
processorTwoYears <- function(name) {
  data <- read.csv2(paste0(getwd(), "/input/", name), 
                    header = T,
                    stringsAsFactors = F,
                    fileEncoding = 'cp1251')
  
  sub <- subset(data, data$e.mail != "" & (data$Класс >= '9'))
  write.xlsx(x = sub, 
             file = paste0(getwd(), "/output/candidates_", name, ".xlsx"), 
             sheetName = "Candidates")
}

processorTwoYears("year2022.csv")
