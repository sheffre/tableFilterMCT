#libs
library(xlsx)
library(tidyverse)
# #data
# data <- read.csv2(paste0(getwd(), "/data.csv"), 
#                  header = T,
#                  stringsAsFactors = F,
#                  fileEncoding = 'cp1251') 
# 
# 
# sub <- subset(data, data$e.mail != "" & data$Класс == '11')
# 
# write.csv(sub, file = paste0(getwd(), "/candidates.csv"), fileEncoding = 'cp1251')
# 
# write.xlsx(x = sub, 
#            file = paste0(getwd(), "/candidates.xlsx"), 
#            sheetName = "Candidates")
# 
# 

processor <- function(name) {
  data <- read.csv2(paste0(getwd(), "/input/", name), 
                    header = T,
                    stringsAsFactors = F,
                    fileEncoding = 'cp1251')
  
  sub <- subset(data, data$e.mail != "" & (data$Класс == '11'))
  write.xlsx(x = sub, 
             file = paste0(getwd(), "/output/candidates_", name, ".xlsx"), 
             sheetName = "Candidates")
}


vec <- list.files(path = paste0(getwd(), "/input/"))

lapply(vec, processor)


processorAdvanced <- function(name) {
  data <- read.csv2(paste0(getwd(), "/input/", name), 
                    header = T,
                    stringsAsFactors = F,
                    fileEncoding = 'cp1251')

  data <- data[-c(1:6)]
  data <- data %>% select(-contains("X"))
  
  exprVector <- c("_cap", "2", "3", "4", "5", "6")
  listCandidates <- lapply(exprVector, function(x) {data <- data[, grepl(x, names(data))]})
  
  names(listCandidates) <- exprVector
  
  names(listCandidates[[1]]) <- unlist(lapply(names(listCandidates[[1]]), function(x) { str_replace_all(x, "_cap", "") }))
  
  for (i in c(2:length(listCandidates))) {
    names(listCandidates[[i]]) <- unlist(lapply(names(listCandidates[[i]]), function(x) { str_replace_all(x, "\\d", "") }))
  }
  
  listCandidatesFiltered <- lapply(listCandidates, function(x) {x <- subset(x, (x$email != "") & (x$class == 11))})
  
  xlsxNames <- lapply(names(listCandidatesFiltered), function(x) {paste0("candidates_regetap2024_", x, ".xlsx")})
  
  for (i in c(1:length(listCandidatesFiltered))) {
    write.xlsx(x = listCandidatesFiltered[[i]], file = paste0(getwd(), "/output/", xlsxNames[i]), sheetName = xlsxNames[i])
  }
}


processorAdvanced("regetap2024.csv")


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
