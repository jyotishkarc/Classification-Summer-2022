
library(readxl)
library(writexl)


path <- "G:/Projects/Prof. Subhajit Dutta (Summer, 2022)/Results/Simulated-ALL-Results-newest/FRESHEST/T-matrix/"

files <- list.files(path)[1:5]

res.T <- list()

for(h in 1:5){
   
   temp <- rio::import_list(paste0(path, files[h]))
   
   res.T[[h]] <- matrix(NA, nrow = 2*length(names(temp)), ncol = 3)
   
   for(k in 1:length(names(temp))){
      
      res.T[[h]][(2*k-1), ] <- temp[[k]] %>% colMeans() %>% as.numeric()
      res.T[[h]][2*k, ] <- rep(NA,3)
   }
   
   res.T[[h]] <- res.T[[h]] %>% as.data.frame()
   
   # rownames(res.T[[h]]) <- c(5,10,25,50,100,250,500,1000)
   colnames(res.T[[h]]) <- c("T.FF","T.FG","T.GG")
}

writexl::write_xlsx(res.T, path = paste0("G:\\Projects\\Prof. Subhajit Dutta (Summer, 2022)\\Results\\Simulated-ALL-Results-newest\\FRESHEST\\T-Summary.xlsx"))
