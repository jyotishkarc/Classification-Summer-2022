

path <- "C:/Users/JYOTISHKA/Desktop/"

path.simulated <- paste0(path, "all-classifiers-TwoClass-simulated-new/")
path.UCR <- paste0(path, "UCR-TwoClass-Results/")
path.CompCancer <- paste0(path, "CompCancer-TwoClass-New-Results/")

results.simulated <- list.files(path.simulated)
results.UCR <- list.files(path.UCR)
results.CompCancer <- list.files(path.CompCancer)


columns <- c('del.1','del.2','del.3',
             'GLMNET',
             'RF1','RF2','RF3','RF4',
             'NNRAND',
             'SVMLIN','SVMRBF',
             'NN-lg-1','NN-lg-3','NN-lg-5','NN-lg-10',
             'NN-R-1','NN-R-3','NN-R-5','NN-R-10',
             'ONN')

summary.UCR <- summary.CompCancer <- matrix(0, nrow = 3*length(results.UCR), ncol = 21)

for(val in 1:length(results.UCR)){
   temp.file <- read_excel(paste0(path.UCR, results.UCR[val]))
   summary.UCR[(3*(val - 1) + 1):(3*(val - 1) + 3), ] <- 
      cbind(c(results.UCR[val],NA,NA), rbind(temp.file[52:53, ], rep(NA,20))) %>% as.matrix()
}


for(val in 1:length(results.CompCancer)){
   temp.file <- read_excel(paste0(path.CompCancer, results.CompCancer[val]))
   summary.CompCancer[(3*(val - 1) + 1):(3*(val - 1) + 3), ] <- 
      cbind(c(results.CompCancer[val],NA,NA), rbind(temp.file[52:53, ], rep(NA,20))) %>% as.matrix()
}


colnames(summary.UCR) <-  colnames(summary.CompCancer) <- c("Name",columns)


writexl::write_xlsx(summary.UCR %>% as.data.frame(), "C:\\Users\\JYOTISHKA\\Desktop\\UCR-TwoClass-Results.xlsx")

writexl::write_xlsx(summary.CompCancer %>% as.data.frame(), "C:\\Users\\JYOTISHKA\\Desktop\\CompCancer-TwoClass-Results.xlsx")

