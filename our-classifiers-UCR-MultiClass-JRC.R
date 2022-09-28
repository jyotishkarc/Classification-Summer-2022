
library(doParallel)
library(dplyr)
library(readxl)
library(writexl)


# start.time <- proc.time()

no.cores <- round(detectCores()*0.75)
cl <- makeCluster(spec = no.cores, type = 'PSOCK')
registerDoParallel(cl)


rho <- function(a,b,c){
   if (prod(a == c)== 1 || prod(b == c) == 1){
      return(0)
   }else{
      temp <- sign((a-c) * (b-c)) * (-1)
      return(temp %>% replace(temp == -1, 0) %>% mean())
   }
}




################################################### Classifier Function for Test Observations

classify.parallel <- function(Z, X, Y,
                              T.FF, T.GG, T.FG,
                              W, S_FG,
                              A,B){
   # print("Classification starting")
   R <- nrow(Z)
   Q <- rbind(X,Y)
   n <- nrow(X)
   m <- nrow(Y)
   # M <- nrow(B)
   
   clusterExport(cl, c('R','n','m'), envir = environment())
   
   
   ########################################### T_FZ
   
   ##### T_FZ.rho.fun
   T_FZ.rho.fun <- function(vec){
      i <- vec[1]
      j <- vec[2]
      
      return(sum(sapply(1:(n+m),function(val){
         rho(Z[i,],X[j,],Q[val,])
      })))
   }
   
   index.mat <- cbind(rep(1:R, each = n),rep(1:n, times = R))
   
   T_FZ <- index.mat %>%
      parApply(cl, ., 1, T_FZ.rho.fun) %>%
      matrix(nrow = R, ncol = n, byrow = TRUE) %>% 
      rowMeans() / (n+m)
   
   
   ########################################### T_FZ
   
   ##### T_GZ.rho.fun
   T_GZ.rho.fun <- function(vec){
      i <- vec[1]
      j <- vec[2]
      
      return(sum(sapply(1:(n+m),function(val){
         rho(Z[i,],Y[j,],Q[val,])
      })))
   }
   
   index.mat <- cbind(rep(1:R, each = m),rep(1:m, times = R))
   
   T_GZ <- index.mat %>%
      parApply(cl, ., 1, T_GZ.rho.fun) %>%
      matrix(nrow = R, ncol = m, byrow = TRUE) %>% 
      rowMeans() / (n+m)
   
   
   L_FZ <- T_FZ - rep(T.FF, R)/2
   L_GZ <- T_GZ - rep(T.GG, R)/2
   S_Z <- L_FZ + L_GZ - T.FG
   
   W0_FG <- W[[1]]
   
   prac.label.1 <- prac.label.2 <- prac.label.3 <- rep(0, R)
   
   #### CLASSIFIER 1
   delta1_Z <- L_GZ - L_FZ
   
   prac.label.1[which(delta1_Z > 0)] <- A
   prac.label.1[which(delta1_Z <= 0)] <- B
   
   #### CLASSIFIER 2
   delta2_Z <- W0_FG * delta1_Z + S_FG * S_Z
   
   prac.label.2[which(delta2_Z > 0)] <- A
   prac.label.2[which(delta2_Z <= 0)] <- B
   
   #### CLASSIFIER 3
   delta3_Z <- W0_FG * sign(delta1_Z) + S_FG * sign(S_Z)
   
   prac.label.3[which(delta3_Z > 0)] <- A
   prac.label.3[which(delta3_Z <= 0)] <- B
   
   prac.label <- list(prac.label.1, prac.label.2, prac.label.3)
   
   return(prac.label)
}
##################################################################################

#################

labels.rename <- function(X){
   
   X <- as.matrix(X)
   
   if (length(setdiff(unique(X[,1]), 1:length(unique(X[,1])))) == 0) {
      return(X)
   }
   
   original.labels <- X[,1] %>% as.character()
   new.label.names <- 1 : length(unique(original.labels))
   
   X[,1] <- new.label.names[as.factor(original.labels)] %>% as.numeric()
   
   return(as.data.frame(X))
}

#################

clusterEvalQ(cl, {library(magrittr)})
clusterExport(cl, ls())

#################

JMLR.UCR <- c("FiftyWords","ACSF1","Adiac","Arrowhead","Beef","BeetleFly",
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

#################

print("Hello")

iterations <- 100

path <- "G:/Datasets/Classification Datasets/"
# path <- "E:/JRC-2022/Classification-Summer-2022-JRC/Datasets/UCR/"

UCR.stats <- read.csv(paste0(path,"UCR-DataSummary.csv"), stringsAsFactors = FALSE)
# files.TwoClass <- UCR.stats$Name[which(UCR.stats$Class == 2)]
# files.TwoClass <- intersect(JMLR.UCR,files.TwoClass)

path.UCR <- paste0(path,"UCRArchive_2018/")
files.UCR <- list.files(path.UCR)


time.UCR <- rep(0, length(files.UCR))

for(h in 1:length(files.UCR)){
   
   print(h)
   print(files.UCR[h])
   print(Sys.time())
   
   start.time.sys <- Sys.time()
   start.time.proc <- proc.time()
   
   init.train.data <- read.delim(paste0(path.UCR, 
                                        files.UCR[h], "/",
                                        files.UCR[h], "_TRAIN.tsv"), 
                                 header = FALSE, 
                                 stringsAsFactors = FALSE)
   
   init.test.data <- read.delim(paste0(path.UCR, 
                                       files.UCR[h], "/",
                                       files.UCR[h], "_TEST.tsv"), 
                                header = FALSE, 
                                stringsAsFactors = FALSE)
   
   dataset <- rbind(init.train.data, 
                    init.test.data) %>% labels.rename() %>% 
                                          as.data.frame() %>% 
                                          arrange(V1) %>% as.matrix()
   
   J <- dataset[,1] %>% unique() %>% length()
   
   ranges <- train.index <- test.index <- prac.label <- list()
   
   for(j in 1:J){
      ranges[[j]] <- which(dataset[,1] == j)
   }
   
   print("Dataset extracted")
   
   N <- nrow(dataset)
   d <- ncol(dataset) - 1
   
   classes.mat <- cbind(rep(1:J, each = J), 
                        rep(1:J, times = J)) %>% as.data.frame() %>% filter(V1 < V2)
   
   for(u in 1:iterations){
      
      train.index[[u]] <- test.index[[u]] <- list()
      
      for(j in 1:J){
         train.index[[u]][[j]] <- sample(ranges[[j]], ceiling(length(ranges[[j]])/2))
         test.index[[u]][[j]] <- setdiff(ranges[[j]], train.index[[u]][[j]])
      }
   }
   
   ground.truth <- dataset[,1] %>% unlist() %>% as.numeric()
   
   
   for(u in 1:iterations){
      
      
      res.list <- list()
      
      # cat("Started - Our Classifiers:")
      print(Sys.time())
      
      ############################################## Our Classifiers
      
      
      for(ind in 1:nrow(classes.mat)){   
         if(u %% 1 == 0) print(u)
         start.time <- proc.time()[3]
         
         A <- classes.mat[ind, 1] 
         B <- classes.mat[ind, 2]
         
         class.A <- which(ground.truth == A)
         class.B <- which(ground.truth == B)
         
         n <- round(length(class.A) * 0.5)
         m <- round(length(class.B) * 0.5)
         
         error.prop.1 <- error.prop.2 <- error.prop.3 <- c()
         
         
         X <- dataset[train.index[[u]][[A]], -1]
         Y <- dataset[train.index[[u]][[B]], -1]
         Q <- rbind(X,Y)
         
         Z <- dataset[c(test.index[[u]][[A]],
                        test.index[[u]][[B]]), -1]    ## Test Obs.
         
         # if (u %% 1 == 0) {print(u)}
         
         n <- nrow(X)
         m <- nrow(Y)
         
         clusterExport(cl, c('X','Y','Q','M','n','m','rho'))
         
         
         ########################################### T.FG
         
         ##### T.FG.rho.fun
         T.FG.rho.fun <- function(vec){
            i <- vec[1]
            j <- vec[2]
            
            return(sum(sapply(1:(n+m),function(val){
               rho(X[i,],Y[j,],Q[val,])
            })))
         }
         
         index.mat <- cbind(rep(1:n, each = m),rep(1:m, times = n))
         
         T.FG <- index.mat %>% 
            parApply(cl, ., 1, T.FG.rho.fun) %>% sum() / ((n+m)*n*m)
         
         
         ########################################### T.FF
         
         ##### T.FF.rho.fun
         T.FF.rho.fun <- function(vec){
            i <- vec[1]
            j <- vec[2]
            
            return(sum(sapply(1:(n+m),function(val){
               rho(X[i,],X[j,],Q[val,])
            })))
         }
         
         index.mat <- cbind(rep(1:n, each = n),rep(1:n, times = n))
         
         T.FF <- index.mat %>% 
            parApply(cl, ., 1, T.FF.rho.fun) %>% sum() / ((n+m)*n*(n-1))
         
         
         ########################################### T.GG
         
         ##### T.GG.rho.fun
         T.GG.rho.fun <- function(vec){
            i <- vec[1]
            j <- vec[2]
            
            return(sum(sapply(1:(n+m),function(val){
               rho(Y[i,],Y[j,],Q[val,])
            })))
         }
         
         index.mat <- cbind(rep(1:m, each = m),rep(1:m, times = m))
         
         T.GG <- index.mat %>% 
            parApply(cl, ., 1, T.GG.rho.fun) %>% sum() / ((n+m)*m*(m-1))
         
         
         ########## W
         W0_FG <- 2 * T.FG - T.FF - T.GG
         W1_FG <- W0_FG^2 / 2 + (T.FF - T.GG)^2 / 2
         W2_FG <- W0_FG / 2 + abs(T.FF - T.GG) / 2
         
         W <- list(W0_FG, W1_FG, W2_FG)
         S_FG <- T.FF - T.GG
         
         
         ########## Test Observations
         
         ground.label <- ground.truth[test.index[[u]]]
         
         clusterExport(cl, c('Z'))
         
         ############################################################### Classification
         prac.label[[ind]] <- classify.parallel(Z, X, Y,
                                                T.FF, T.GG, T.FG,
                                                W, S_FG,
                                                A,B)
         ###############################################################
         
         res.mat[[A]][[1]] <- prac.label[[ind]][[1]][1:length(test.index[[u]][[A]])]
         res.mat[[A]][[2]] <- prac.label[[ind]][[2]][1:length(test.index[[u]][[A]])]
         res.mat[[A]][[3]] <- prac.label[[ind]][[3]][1:length(test.index[[u]][[A]])]
         
         res.mat[[B]][[1]] <- prac.label[[ind]][[1]][1:length(test.index[[u]][[B]])]
         res.mat[[B]][[2]] <- prac.label[[ind]][[2]][1:length(test.index[[u]][[B]])]
         res.mat[[B]][[3]] <- prac.label[[ind]][[3]][1:length(test.index[[u]][[B]])]
      }
      
      
      
      
      
   }
   
   
   
   
   
   cat("Started - Popular Classifiers:")
   print(Sys.time())
   
   ############################################## Popular Classifiers
   
   
   result.all <- cbind(error.prop.1, error.prop.2, error.prop.3,
                       as.data.frame(result)) %>% as.data.frame()
   
   colnames(result.all) <- c('del.1','del.2','del.3',
                             'del.1.boot','del.2.boot','del.3.boot',
                             'GLMNET',
                             'RF1','RF2','RF3','RF4',
                             'NNRAND',
                             'SVMLIN','SVMRBF',
                             'NN-lg-1','NN-lg-3','NN-lg-5','NN-lg-10',
                             'NN-R-1','NN-R-3','NN-R-5','NN-R-10',
                             'ONN')
   
   rownames(result.all) <- 1:iterations
   
   res.list <- rbind(result.all, rep(NA, ncol(result.all)), 
                     apply(result.all, 2, mean), 
                     apply(result.all, 2, sciplot::se))
   
   # result.folder.path <- "C:\\Users\\JYOTISHKA\\Desktop\\CompCancer-TwoClass-Results\\"
   
   result.folder.path <- "E:\\JRC-2022\\Classification-Summer-2022-JRC\\Results\\Real\\UCR\\TwoClass\\NEWEST\\"
   
   write_xlsx(x = res.list,
              path = paste0(result.folder.path, files.TwoClass[h],".xlsx"))
   
   end.time <- proc.time()[3]
   time.UCR[h] <- end.time - start.time
   print(time.UCR[h])
   cat("Ended:")
   print(Sys.time())
   print(files.TwoClass[h])
   cat("\n\n")
}


stopCluster(cl)
gc()

