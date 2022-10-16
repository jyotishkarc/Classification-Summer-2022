
library(readxl)
library(dplyr)
library(stringr)

path <- "D:/All Downloads/compcancer-rest/"     #### Deleted. Insert a new path if required.
files <- list.files(path)

for(h in 1:length(files)){
   
   temp.path <- paste0(path,files[h])
   
   temp.data <- read.delim(temp.path, header=FALSE)
   
   excel.data <- t(temp.data)[-1,-1] %>% 
                     labels.rename() %>% 
                     as.matrix() %>% 
                     apply(c(1,2), function(val) return(as.numeric(val))) %>%
                     as.data.frame()
   
   excel.path <- paste0("D:\\All Downloads\\", 
                        str_remove(files[h],".txt"),".xlsx")
   
   writexl::write_xlsx(excel.data,
                       path = excel.path)
   
   print(h)
}











