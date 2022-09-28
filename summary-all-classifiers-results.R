
library(readxl)
library(writexl)
library(dplyr)
library(stringr)

path <- "C:/Users/JYOTISHKA/Desktop/"

path.simulated <- paste0(path, "NEWEST/")
path.UCR <- paste0(path, "NEWEST - UCR/")
path.CompCancer <- paste0(path, "NEWEST - CompCancer/")

results.simulated <- list.files(path.simulated)
results.UCR <- list.files(path.UCR)
results.CompCancer <- list.files(path.CompCancer)


################################################################################# UCR

if(TRUE) {
   
   columns <- c('del.1','del.2','del.3',
                'del.1.boot','del.2.boot','del.3.boot',
                'GLMNET',
                'RF1','RF2','RF3','RF4',
                'NNRAND',
                'SVMLIN','SVMRBF',
                'NN-lg-1','NN-lg-3','NN-lg-5','NN-lg-10',
                'NN-R-1','NN-R-3','NN-R-5','NN-R-10',
                'ONN')
   
   path.UCR.stats <- "G:/Datasets/Classification Datasets/"
   # path.UCR.stats <- "E:/JRC-2022/Classification-Summer-2022-JRC/Datasets/UCR/"
   
   UCR.stats <- read.csv(paste0(path.UCR.stats,"UCR-DataSummary.csv"), 
                         stringsAsFactors = FALSE)
   files.TwoClass <- UCR.stats$Name[which(UCR.stats$Class == 2)]
   
   JMLR.UCR.TwoClass <- c("FiftyWords","ACSF1","Adiac","Arrowhead","Beef","BeetleFly",
                 "BirdChicken","Car","CBF","CinCECGtorso","Coffee","Computers",
                 "CricketX","CricketY","DiatomSizeReduction","DistalPhalanxOutlineAgeGroup",
                 "DistalPhalanxOutlineCorrect","DistalPhalanxTW","Earthquakes","ECG200",
                 "ECGFiveDays","EOGHorizontalSignal","EOGVerticalSignal","EthanolLevel",
                 "FaceFour","FISH","GunPoint1","Ham","Handoutlines","Haptics","Herring",
                 "HouseTwenty","InlineSkate","InsectEPGRegularTrain","ItalyPowerDemand",
                 "LargeKitchenAppliances","Lighting2","Lighting7","MEAT","MedicalImages",
                 "MiddlePhalanxOutlineAgeGroup","MiddlePhalanxOutlineCorrect",
                 "MiddlePhalanxTW","MoteStrain","OliveOil","OSUleaf","PigAirwayPressure",
                 "PigArtPressure","PigCVP","Plane","ProximalPhalanxOutlineAgeGroup",
                 "ProximalPhalanxOutlineCorrect","ProximalPhalanxTW","RefrigerationDevices",
                 "ScreenType","ShapeletSim","ShapesAll","SmallKitchenAppliances",
                 "SonyAIBORobotSurface","SonyAIBORobotSurfaceII","Strawberry","SwedishLeaf",
                 "syntheticcontrol","ToeSegmentation1","ToeSegmentation2","Trace",
                 "TwoLeadECG","Wine","WordsSynonyms","Worms1","WormsTwoClass")
   
   files.TwoClass.JMLR <- intersect(JMLR.UCR.TwoClass,files.TwoClass)
   
   res.summary.UCR <- matrix(0, nrow = 3*length(results.UCR), 
                             ncol = length(columns) + 3)
   
   for(val in 1:length(results.UCR)){
      temp.file <- read_excel(paste0(path.UCR, results.UCR[val]))
      
      temp.stats <- UCR.stats %>% 
         filter(Name %in% files.TwoClass.JMLR) %>% 
         filter(Name == results.UCR[val] %>% 
                   stringr::str_remove(".xlsx"))
      
      res.summary.UCR[(3*(val - 1) + 1):(3*(val - 1) + 3), ] <- 
         cbind(c(NA,temp.stats$Name,NA), 
               c(NA,temp.stats$Train + temp.stats$Test,NA),
               c(NA,temp.stats$Length,NA),
               rbind(rep(NA,20),temp.file[52:53, ])) %>% as.matrix()
   }
   
   colnames(res.summary.UCR) <- c("Name","N","d",columns)
}

writexl::write_xlsx(res.summary.UCR %>% as.data.frame(), "C:\\Users\\JYOTISHKA\\Desktop\\RES-newest\\UCR-TwoClass-Results-newest.xlsx")


#####################################################################################

########################################################################## CompCancer

if(TRUE){
   
   columns <- c('del.1','del.2','del.3',
                'del.1.boot','del.2.boot','del.3.boot',
                'GLMNET',
                'RF1','RF2','RF3','RF4',
                'NNRAND',
                'SVMLIN','SVMRBF',
                'NN-lg-1','NN-lg-3','NN-lg-5','NN-lg-10',
                'NN-R-1','NN-R-3','NN-R-5','NN-R-10',
                'ONN')
   
   res.summary.CompCancer <- matrix(0, nrow = 3*length(results.CompCancer), 
                                    ncol = length(columns) + 1)
   
   for(val in 1:length(results.CompCancer)){
      temp.file <- read_excel(paste0(path.CompCancer, results.CompCancer[val]))
      
      res.summary.CompCancer[(3*(val - 1) + 1):(3*(val - 1) + 3), ] <- 
         cbind(c(NA,results.CompCancer[val],NA), 
               rbind(rep(NA,20), temp.file[52:53, ])) %>% as.matrix()
   }
   
   colnames(res.summary.CompCancer) <- c("Name",columns)
}

writexl::write_xlsx(res.summary.CompCancer %>% as.data.frame(), "C:\\Users\\JYOTISHKA\\Desktop\\CompCancer-TwoClass-Results-newest.xlsx")

#####################################################################################


# writexl::write_xlsx(res.summary.UCR %>% as.data.frame(), "C:\\Users\\JYOTISHKA\\Desktop\\UCR-TwoClass-Results.xlsx")

# writexl::write_xlsx(res.summary.CompCancer %>% as.data.frame(), "C:\\Users\\JYOTISHKA\\Desktop\\CompCancer-TwoClass-Results.xlsx")


########################################################################## Simulated

if(FALSE){
   
   columns <- c('del.1','del.2','del.3',
                'del.1.boot','del.2.boot','del.3.boot',
                'BYS',
                'GLMNET',
                'RF1','RF2','RF3','RF4',
                'NNRAND',
                'SVMLIN','SVMRBF',
                'NN-lg-1','NN-lg-3','NN-lg-5','NN-lg-10',
                'NN-R-1','NN-R-3','NN-R-5','NN-R-10',
                'ONN')
   
   res.summary.simulated <- matrix(0, nrow = 3*length(results.simulated), 
                                    ncol = length(columns) + 1)
   
   for(val in 1:length(results.simulated)){
      temp.file <- read_excel(paste0(path.simulated, results.simulated[val]))
      
      if(val == 1){
         res.summary.simulated[(3*(val - 1) + 1):(3*(val - 1) + 6), ] <- 
            cbind(c(NA,results.simulated[val],NA), 
                  rbind(rep(NA,23), temp.file[52:53, ])) %>% as.matrix()
      }
      else{
         res.summary.simulated[(3*(val - 1) + 1):(3*(val - 1) + 6), ] <- 
            cbind(c(NA,results.simulated[val],NA), 
                  rbind(rep(NA,23), temp.file[102:103, ])) %>% as.matrix()
      } 
      
   }
   
   colnames(res.summary.CompCancer) <- c("Name",columns)
}

#####################################################################################




