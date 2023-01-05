
library(rio)
library(dplyr)
library(ggplot2)

path.our <- "C:/Users/JYOTISHKA/Desktop/delta0-TwoClass-simulated/"
# path.our <- "G:/Projects/Prof. Subhajit Dutta (Summer, 2022)/Results/All-Latest-Results/Simulated/"
files.path.our <- list.files(path.our)[1:5]

path.pop <- "C:/Users/JYOTISHKA/Desktop/all-classifiers-TwoClass-simulated-new/"
files.path.pop <- list.files(path.pop)

plt <- df.summary.error <- df.summary.se <- list()

for(h in 1:5){
   
   df.list.our <- rio::import_list(paste0(path.our, files.path.our[h]))
   df.list.pop <- rio::import_list(paste0(path.pop, files.path.pop[h]))
   
   ifelse(files.path.our[h] == "all-C01-vs-C02.xlsx", 
          iterations <- 50, iterations <- 100)
   
   df.summary.error[[h]] <-
         df.summary.se[[h]] <- data.frame("d" = c(5,10,25,50,100,250,500,1000),
                                          "del.0" = rep(NA,8),
                                          # "del.1" = rep(NA,8),
                                          # "del.2" = rep(NA,8),
                                          # "del.3" = rep(NA,8),
                                          "BYS" = rep(NA,8),
                                          "GLMNET" = rep(NA,8),
                                          "NNRAND" = rep(NA,8),
                                          "SVMLIN" = rep(NA,8),
                                          "SVMRBF" = rep(NA,8),
                                          "NNet" = rep(NA,8),
                                          "ONN" = rep(NA,8))
   
   for(i in 1:8){
      del.errors <- df.list.our[[i]][iterations + 2, ] %>% as.numeric()
      pop.errors <- df.list.pop[[i]][iterations + 2, c(4,5,10:21)] %>% as.numeric()
      
      del.se <- df.list.our[[i]][iterations + 3, ] %>% as.numeric()
      pop.se <- df.list.pop[[i]][iterations + 3, c(4,5,10:21)] %>% as.numeric()
      
      df.summary.error[[h]][i, -1] <- c(del.errors, pop.errors[1:5], 
                                        min(pop.errors[6:13]), pop.errors[14])
      df.summary.se[[h]][i, -1] <- c(del.se, pop.se[1:5], 
                                        min(pop.se[6:13]), pop.se[14])
   }
   
   plt[[h]] <- df.summary.error[[h]] %>%
                  reshape2::melt(id = "d") %>% 
                  ggplot(aes(x = d)) +
                  geom_line(aes(y = value, color = variable), linewidth = 1)
}











