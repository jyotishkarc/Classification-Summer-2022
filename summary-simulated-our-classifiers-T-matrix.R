
library(readxl)
library(writexl)


path <- "D:/My Documents/R/R Codes/Classification using Data-Adaptive Energy Distance (Summer, 2022)/Classification-Summer-2022/Results/Simulated/delta-1,2,3/"

examples.target <- c(1:4,7)
files <- list.files(path)[examples.target]

res.T <- list()

for(h in 1:5){
   
   temp <- rio::import_list(paste0(path, files[h]))
   
   res.T[[h]] <- matrix(NA, nrow = 2*length(names(temp)), ncol = 3)
   
   for(k in 1:length(names(temp))){
      
      res.T[[h]][(2*k-1), ] <- temp[[k]] %>% colMeans() %>% as.numeric()
      res.T[[h]][2*k, ] <- rep(NA,3)
   }
   
   res.T[[h]] <- res.T[[h]] %>% as.data.frame()
   colnames(res.T[[h]]) <- c("T.FF","T.FG","T.GG")
}

writexl::write_xlsx(res.T, path = paste0("C:\\Users\\JYOTISHKA\\Desktop\\T-Summary.xlsx"))
