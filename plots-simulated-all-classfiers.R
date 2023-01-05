
library(rio)
library(dplyr)
library(ggplot2)

path <- "/Users/aytijhyasaha/Desktop/projects/Classification-Summer-2022/Results/Simulated/"
files.path <- list.files(path)[1:5]

plt <- list()

for(h in 1:5){
   
   df.list <- rio::import_list(paste0(path, files.path[h]))
   
   ifelse(files.path[h] == "all-C01-vs-C02.xlsx", 
          iterations <- 50, iterations <- 100)
   
   df.summary[[h]] <- data.frame("d" = c(5,10,25,50,100,250,500,1000),
                                 "del.1" = rep(NA,8),
                                 "del.2" = rep(NA,8),
                                 "del.3" = rep(NA,8),
                                 "del.1.se" = rep(NA,8),
                                 "del.2.se" = rep(NA,8),
                                 "del.3.se" = rep(NA,8))
   
   for(i in 1:8){
      del <- df.list[[i]][iterations + 2, ] %>% as.numeric()
      del.se <- df.list[[i]][iterations + 3, ] %>% as.numeric()
      
      df.summary[[h]][i, -1] <- c(del,del.se)
   }
   
   plt[[h]] <- df.summary[[h]] %>%
                  ggplot(aes(x = d)) +
                  geom_line(aes(y = del.1, color = 'red')) +
                  geom_line(aes(y = del.2, color = 'blue')) +
                  geom_line(aes(y = del.3, color = 'green'))
}











