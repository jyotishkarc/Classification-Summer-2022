
library(readxl)
library(writexl)


path <- "G:/Projects/Prof. Subhajit Dutta (Summer, 2022)/Results/Simulated-ALL-Results-newest/FRESHEST/"

files <- list.files(path)[1:5]

res.our <- list()

for(h in 1:5){
   
   temp <- rio::import_list(paste0(path, files[h]))
   
   res.our[[h]] <- matrix(NA, nrow = 2*length(names(temp)), ncol = 3)
   
   for(k in 1:length(names(temp))){
      
      res.our[[h]][(2*k-1), 1:3] <- temp[[k]][52, 1:3] %>% as.numeric()
      res.our[[h]][(2*k), 1:3] <- rep(NA,3)
   }
   
   res.our[[h]] <- res.our[[h]] %>% as.data.frame()
   
   colnames(res.our[[h]]) <- c("del.1","del.2","del.3")
}

writexl::write_xlsx(res.our, path = paste0("G:\\Projects\\Prof. Subhajit Dutta (Summer, 2022)\\Results\\Simulated-ALL-Results-newest\\FRESHEST\\res-Summary.xlsx"))
