
library(readxl)
library(dplyr)
library(stringr)

path <- "D:/All Downloads/microarray-tab/"     #### Deleted. Insert a new path if required.
files <- list.files(path)

# for(h in 1:length(files)){
for(h in c(14,15,18,21,22)){
   
   temp.path <- paste0(path,files[h])
   
   temp.data <- read.delim(temp.path, header=FALSE)
   temp.data <- temp.data[-c(1:3),]
   rownames(temp.data) <- 1:nrow(temp.data)
   
   d <- ncol(temp.data)
   
   ## For most datasets
   if(FALSE){
      gt <- temp.data[,d-1]
      tab.data.cleaned <- cbind(gt, temp.data[,-c(d-1,d)])
   }
   
   ## For datasets 1,2,7,9,24
   if(FALSE){
      gt <- temp.data[,d]
      tab.data.cleaned <- cbind(gt, temp.data[,-d])
   }
   
   ## For datasets 14,15,18,21,22
   if(TRUE){
      # gt <- temp.data[,d]
      # tab.data.cleaned <- cbind(gt, temp.data[,-d])
      tab.data.cleaned <- temp.data
   }
   
   
   excel.data <- tab.data.cleaned %>% 
      labels.rename() %>% 
      as.matrix() %>% 
      apply(c(1,2), function(val) return(as.numeric(val))) %>%
      as.data.frame()
   
   print(dim(excel.data))
   
   colnames(excel.data) <- sapply(1:ncol(excel.data), function(val) paste0("V",val))
   excel.data <- excel.data %>% arrange(V1)
   
   csv.path <- paste0("D:\\All Downloads\\microarray-csv\\error-first\\", 
                      str_remove(files[h],".tab"),".csv")
   
   write.csv(excel.data, 
             csv.path,
             row.names = FALSE)
   
   print(h)
}



labels.rename <- function(X){
   
   X <- as.matrix(X)
   
   if (length(setdiff(unique(X[,1]), 1:length(unique(X[,1])))) == 0) {
      return(X)
   }
   
   original.labels <- X[,1] %>% as.character()
   new.label.names <- 1 : length(unique(original.labels))
   
   X[,1] <- new.label.names[as.factor(original.labels)] %>% as.numeric()
   
   return(as.data.frame(X))
}


