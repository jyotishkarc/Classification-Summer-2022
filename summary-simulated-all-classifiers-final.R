
library(readxl)
library(writexl)
library(rio)


############################## Error rates with standard errors

path <- paste0(getwd(),"/Results/Simulated/Combined-delta-1,2,3-and-Pop/")
examples.target <- c(1:4,7)
files <- list.files(path)[examples.target]

res.our <- res.pop <- res.all <- list()
dims <- c(5,10,25,50,100,250,500,1000)

for(h in 1:5){
   
   temp <- rio::import_list(paste0(path, files[h]))
   
   res.our[[h]] <- matrix(NA, nrow = 2*length(names(temp)), ncol = 3)
   res.pop[[h]] <- matrix(NA, nrow = 2*length(names(temp)), ncol = 7)
   
   for(k in 1:length(names(temp))){
      min.nnet.ind <- which.min(temp[[k]][102, 13:20])[1] %>% as.numeric() + 12
      
      res.our[[h]][(2*k-1), ] <- temp[[k]][102, 1:3] %>% 
                                          as.numeric() %>% round(4) %>% as.character()
      res.pop[[h]][(2*k-1), ] <- temp[[k]][102, c(4,5,10:12,min.nnet.ind,21)] %>% 
                                          as.numeric() %>% round(4) %>% as.character()
      
      res.our[[h]][(2*k), ] <- paste0("(",temp[[k]][103, 1:3] %>% 
                                          as.numeric() %>% round(4),")")
      res.pop[[h]][(2*k), ] <- paste0("(",temp[[k]][103, c(4,5,10:12,min.nnet.ind,21)] %>% 
                                          as.numeric() %>% round(4),")")
   }
   
   res.our[[h]] <- res.our[[h]] %>% as.data.frame()
   res.pop[[h]] <- res.pop[[h]] %>% as.data.frame()
   
   res.all[[h]] <- cbind(res.our[[h]],res.pop[[h]]) %>%
                           apply(1:2, as.character) %>% as.data.frame()
   
   colnames(res.all[[h]]) <- c('del.1','del.2','del.3',
                               'Bayes',
                               'GLMNET','NN-RAND',
                               'SVM-LIN','SVM-RBF',
                               'N-Net','1-NN')
   
   print(h)
}

#### Separated tables
res.all.final.separate <- res.all %>% lapply(function(list.val){
                                             cbind("d" = rep(dims, each = 2),
                                                   list.val)
                                          })

names(res.all.final.separate) <- examples.target %>% sapply(function(ex) paste0("Ex-",ex))

writexl::write_xlsx(res.all.final.separate, 
                    paste0("C:\\Users\\JYOTISHKA\\Desktop\\summary-simulated-all-separate-new.xlsx"))

#### Combined table
res.all.final.stacked <- cbind("Example" = rep(1:5, each = 16),
                               "d" = rep(rep(dims, each = 2), times = 5),
                                     rbind(res.all[[1]],
                                           res.all[[2]],
                                           res.all[[3]],
                                           res.all[[2]],
                                           res.all[[5]]))

writexl::write_xlsx(res.all.final.stacked, 
                    paste0("C:\\Users\\JYOTISHKA\\Desktop\\summary-simulated-all-stacked-new.xlsx"))



############################## T-matrix

path.T <- paste0(getwd(),"/Results/Simulated/delta-1,2,3/T-matrix/")
files.T <- list.files(path.T)[1:5]

res.T <- matrix(NA, nrow = 5*2, ncol = 6)

for(h in 1:5){
   
   print("A")
   temp.T <- rio::import_list(paste0(path.T, files.T[h]))
   
   print("AA")
   T.max.ind <- which.max(temp.T[[8]][102,1:3])
   T.max.check <- ifelse(T.max.ind == 2, "TRUE", "FALSE")
   
   print("AAA")
   res.T[(2*h-1), ] <- c(h, temp.T[[8]][102,1:3] %>% 
                                 as.numeric() %>% round(5) %>% as.character(),
                            T.max.check, h)
   
   res.T[(2*h), ] <- c(h, paste0("(",temp.T[[8]][103,1:3] %>% 
                                 as.numeric() %>% round(6),")"),
                          T.max.check, h)
   print("AAAA")
}

res.T <- res.T %>% as.data.frame()
colnames(res.T) <- c("Example", "T_FF", "T_FG", "T_GG", "which.max", "thm 3.4-y/n")

writexl::write_xlsx(res.T, 
                    paste0("C:\\Users\\JYOTISHKA\\Desktop\\summary-simulated-T-1000.xlsx"))






























