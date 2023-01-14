
library(readxl)
library(writexl)
library(dplyr)
library(stringr)
library(rio)
library(googlesheets4)

path <- "G:/Projects/Prof. Subhajit Dutta (Summer, 2022)/Results/"

# path.simulated <- paste0(path, "simulated-ALL-Results-newest/")
path.UCR <- paste0(path, "UCR/")
path.CompCancer <- paste0(path, "CompCancer/")
path.Microarray <- paste0(path, "Microarray-ALL-Results-newest/")

results.simulated <- list.files(path.simulated)
results.UCR <- list.files(path.UCR) %>% setdiff("UCR-ALL-T-matrix")
results.CompCancer <- list.files(path.CompCancer) %>% setdiff("CompCancer-ALL-T-matrix")
results.Microarray <- list.files(path.Microarray) %>% setdiff("Microarray-ALL-T-matrix")

path.datasets <- "D:/My Documents/Datasets/"

data.summary.UCR <- read.csv(paste0(path.datasets,"UCR Database/UCR-DataSummary.csv"), 
                             stringsAsFactors = FALSE)
data.summary.CompCancer <- read_excel(paste0(path.datasets,
                                             "CompCancer Database/CompCancer-DataSummary.xlsx"))
data.summary.Microarray <- read_excel(paste0(path.datasets,
                                             "Microarray Database/Microarray-DataSummary.xlsx"))


################################################################## UCR

UCR <- FALSE

if(UCR == TRUE){
   
   pop.UCR <- read_excel("G:/Projects/Prof. Subhajit Dutta (Summer, 2022)/Results/UCR - Pop - 11.10.2021.xlsx")
   
   df.names <- results.UCR %>% 
      str_remove("-our.xlsx") %>%
      str_remove("-our-MV.xlsx") %>%
      intersect(pop.UCR$Dataset)
   
   res.all <- matrix(NA, nrow = length(df.names), ncol = 10)
   dataset.details <- matrix(0, nrow = length(df.names), ncol = 3)
   
   for(h in 1:length(df.names)){
      
      print(h)
      
      temp.name <- paste0(df.names[h],"-our.xlsx")
      temp.name.MV <- paste0(df.names[h],"-our-MV.xlsx")
      
      if(temp.name %in% results.UCR){df.our <- read_excel(paste0(path.UCR,temp.name))}
      if(temp.name.MV %in% results.UCR){df.our <- read_excel(paste0(path.UCR,temp.name.MV))}
      
      res.our <- df.our[102,] %>% as.numeric() %>% sapply(function(x) floor(x*10^5)/10^5)
      
      res.pop <- pop.UCR %>% filter(Dataset == df.names[h])
      res.pop.targets <- c(res.pop$GLMNET, res.pop$RP, 
                           res.pop$SVMlin, res.pop$SVMRBF, 
                           res.pop$Nnet, res.pop$`1NN`, res.pop$SAVG) %>% as.numeric()
      
      dataset.summary <- data.summary.UCR %>% filter(Name == df.names[h])
      dataset.details[h,] <- c(as.integer(dataset.summary$Class), 
                               as.integer(dataset.summary$Train) + 
                                  as.integer(dataset.summary$Test), 
                               as.integer(dataset.summary$Length))
      
      res.all[h,] <- c(res.our, res.pop.targets)
   }
   
   summary.UCR <- data.frame(df.names, dataset.details, res.all) %>% arrange(X1)
   colnames(summary.UCR) <- c("Dataset","Class","N","d",
                              "del.1","del.2","del.3",
                              "GLMNET","RP","SVMLIN","SVMRBF","NNet","1NN","SAVG")
}



################################################################## Microarray

MICROARRAY <- TRUE

if(MICROARRAY == TRUE){
   
   path.real.data.all <- "G:/Projects/Prof. Subhajit Dutta (Summer, 2022)/Results/Popular-Classifiers-All-Databases/RealDataAll.xlsx"
   pop.MA <- rio::import_list(path.real.data.all)$MICROARRAY
   
   df.names <- results.Microarray %>% 
      str_remove(".csv") %>% 
      str_remove("-our.xlsx") %>% 
      str_remove("-our-MV.xlsx") %>%
      intersect(pop.MA$datasets)
   
   res.all <- matrix(NA, nrow = length(df.names), ncol = 10)
   dataset.details <- matrix(0, nrow = length(df.names), ncol = 3)
   
   for(h in 1:length(df.names)){
      
      print(h)
      
      temp.name <- paste0(df.names[h],"-our.xlsx")
      temp.name.MV <- paste0(df.names[h],"-our-MV.xlsx")
      
      if(temp.name %in% results.Microarray){df.our <- read_excel(paste0(path.Microarray,
                                                                        temp.name))}
      if(temp.name.MV %in% results.Microarray){df.our <- read_excel(paste0(path.Microarray,
                                                                           temp.name.MV))}
      
      res.our <- df.our[102,] %>% as.numeric() %>% sapply(function(x) floor(x*10^5)/10^5)
      
      res.pop <- pop.MA %>% filter(datasets == df.names[h])
      res.pop.targets <- c(res.pop$GLMNET...14, res.pop$RP...15, 
                           res.pop$SVMlin...16, res.pop$SVMRBF...17, 
                           res.pop$Nnet...18, res.pop$kNN...21, 
                           res.pop$SAVG...20) %>% as.numeric()
      
      dataset.summary <- data.summary.Microarray %>% filter(Dataset == df.names[h])
      dataset.details[h,] <- c(as.integer(dataset.summary$J), 
                               as.integer(dataset.summary$N),
                               as.integer(dataset.summary$d))
      
      res.all[h,] <- c(res.our, res.pop.targets)
   }
   
   summary.MA <- data.frame(df.names, dataset.details, res.all) %>% arrange(X1)
   colnames(summary.MA) <- c("Dataset","Class","N","d",
                             "del.1","del.2","del.3",
                             "GLMNET","RP","SVMLIN","SVMRBF","NNet","1NN","SAVG")
}

gs4_create("results-MA-all-classifiers-31-12-2022",
           sheets = list(Microarray = summary.MA))


################################################################## CompCancer

COMPCANCER <- TRUE

if(COMPCANCER == TRUE){
   
   path.real.data.all <- "G:/Projects/Prof. Subhajit Dutta (Summer, 2022)/Results/RealDataAll.xlsx"
   pop.CC <- rio::import_list(path.real.data.all)$COMPCANCER
   
   df.names <- results.CompCancer %>% 
      str_remove("-our.xlsx") %>% 
      str_remove("-our-MV.xlsx") %>% 
      intersect(pop.CC$datasets)
   
   res.all <- matrix(NA, nrow = length(df.names), ncol = 10)
   dataset.details <- matrix(0, nrow = length(df.names), ncol = 3)
   
   for(h in 1:length(df.names)){
      
      print(h)
      
      temp.name <- paste0(df.names[h],"-our.xlsx")
      temp.name.MV <- paste0(df.names[h],"-our-MV.xlsx")
      
      if(temp.name %in% results.CompCancer){df.our <- read_excel(paste0(path.CompCancer,
                                                                        temp.name))}
      if(temp.name.MV %in% results.CompCancer){df.our <- read_excel(paste0(path.CompCancer,
                                                                           temp.name.MV))}
      
      res.our <- df.our[102,] %>% as.numeric() %>% sapply(function(x) floor(x*10^5)/10^5)
      
      res.pop <- pop.CC %>% filter(datasets == df.names[h])
      res.pop.targets <- c(res.pop$GLMNET...2, res.pop$RP...3, 
                           res.pop$SVMlin...4, res.pop$SVMRBF...5, 
                           res.pop$Nnet...6, res.pop$kNN...7, 
                           res.pop$SAVG...16) %>% as.numeric()
      
      dataset.summary <- data.summary.CompCancer %>% filter(Dataset == df.names[h])
      dataset.details[h,] <- c(as.integer(dataset.summary$J), 
                               as.integer(dataset.summary$N),
                               as.integer(dataset.summary$d))
      
      res.all[h,] <- c(res.our, res.pop.targets)
   }
   
   summary.CC <- data.frame(df.names, dataset.details, res.all) %>% arrange(X1)
   colnames(summary.CC) <- c("Dataset","Class","N","d",
                             "del.1","del.2","del.3",
                             "GLMNET","RP","SVMLIN","SVMRBF","NNet","1NN","SAVG")
}

##################################################################

res.final <- gs4_create("results-real-all-classifiers-31-12-2022",
                        sheets = list(UCR = summary.UCR,
                                      CompCancer = summary.CC,
                                      Microarray = summary.MA))


