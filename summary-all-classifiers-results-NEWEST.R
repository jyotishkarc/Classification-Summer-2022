
library(readxl)
library(writexl)
library(dplyr)
library(stringr)

path <- "G:/Projects/Prof. Subhajit Dutta (Summer, 2022)/Results/"

path.simulated <- paste0(path, "simulated-ALL-Results-newest/")
path.UCR <- paste0(path, "UCR-ALL-Results-newest/")
path.CompCancer <- paste0(path, "CompCancer-ALL-Results-newest/")
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

if(TRUE){
   
   pop.UCR <- read_excel("D:/All Downloads/UCR - 11.10.2021.xlsx")
   
   df.names <- results.UCR %>% str_remove("-our.xlsx") %>% intersect(pop.UCR$Dataset)
   
   res.all <- matrix(NA, nrow = length(df.names), ncol = 10)
   dataset.details <- matrix(0, nrow = length(df.names), ncol = 3)
   
   for(h in 1:length(df.names)){
      
      df.our <- read_excel(paste0(path.UCR,df.names[h],"-our.xlsx"))
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





