
library(doParallel)
library(magrittr)
library(readxl)
library(writexl)

library(glmnet)            #### GLMNET
library(RandPro)           #### Random Projection
library(e1071)             #### SVM (Linear & RBF Kernel)
library(randomForest)      #### Random Forest
library(nnet)              #### Neural Networks
library(class)             #### One Nearest Neighbour


# start.time <- proc.time()

no.cores <- round(detectCores()*0.75)
cl <- makeCluster(spec = no.cores, type = 'PSOCK')
registerDoParallel(cl)

# rho <- function(a,b,c){
#    if (prod(a == c)== 1 || prod(b == c) == 1){
#       return(0)
#    }else{
#       return(acos(sum((a-c)*(b-c)) / sqrt(sum((a-c)^2) * sum((b-c)^2))) / pi)
#    }
# }

rho <- function(a,b,c){
   if (prod(a == c)== 1 || prod(b == c) == 1){
      return(0)
   }else{
      temp <- sign((a-c) * (b-c)) * (-1)
      return(temp %>% replace(temp == -1, 0) %>% mean())
   }
}




################################################### Classifier Function for Test Observations

classify.parallel <- function(Z, X, Y, #B,
                              T.FF, T.GG, T.FG,
                              # T.FF.boot, T.GG.boot, T.FG.boot,
                              W, #W.boot,
                              S_FG, #S_FG.boot
                              ){
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
   
   
   # ##### T_FZ.rho.boot
   # T_FZ.rho.boot <- function(vec){
   #    i <- vec[1]
   #    j <- vec[2]
   #    
   #    return(sum(sapply(1:M,function(val){
   #       rho(Z[i,],X[j,],B[val,])
   #    })))
   # }
   # 
   # index.mat <- cbind(rep(1:R, each = n),rep(1:n, times = R))
   # 
   # T_FZ.boot <- index.mat %>%
   #    parApply(cl, ., 1, T_FZ.rho.boot) %>%
   #    matrix(nrow = R, ncol = n, byrow = TRUE) %>% 
   #    rowMeans() / M
   
   
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
   
   
   # ##### T_GZ.rho.boot
   # T_GZ.rho.boot <- function(vec){
   #    i <- vec[1]
   #    j <- vec[2]
   #    
   #    return(sum(sapply(1:M,function(val){
   #       rho(Z[i,],Y[j,],B[val,])
   #    })))
   # }
   # 
   # index.mat <- cbind(rep(1:R, each = m),rep(1:m, times = R))
   # 
   # T_GZ.boot <- index.mat %>%
   #    parApply(cl, ., 1, T_GZ.rho.boot) %>%
   #    matrix(nrow = R, ncol = m, byrow = TRUE) %>% 
   #    rowMeans() / M
   
   
   
   L_FZ <- T_FZ - rep(T.FF, R)/2
   L_GZ <- T_GZ - rep(T.GG, R)/2
   S_Z <- L_FZ + L_GZ - T.FG
   
   # L_FZ.boot <- T_FZ.boot - rep(T.FF.boot, R)/2
   # L_GZ.boot <- T_GZ.boot - rep(T.GG.boot, R)/2
   # S_Z.boot <- L_FZ.boot + L_GZ.boot - T.FG.boot
   
   W0_FG <- W[[1]]
   # W1_FG <- W[[2]]
   # W2_FG <- W[[3]]
   
   # W0_FG.boot <- W.boot[[1]]
   # # W1_FG.boot <- W.boot[[2]]
   # # W2_FG.boot <- W.boot[[3]]
   
   prac.label.1 <- prac.label.2 <- prac.label.3 <- 
      # prac.label.1.boot <- prac.label.2.boot <- prac.label.3.boot <- 
      rep(0, R)
   
   #### CLASSIFIER 1
   delta1_Z <- L_GZ - L_FZ
   # delta1_Z.boot <- L_GZ.boot - L_FZ.boot
   
   prac.label.1[which(delta1_Z > 0)] <- 1
   # prac.label.1.boot[which(delta1_Z.boot > 0)] <- 1
   prac.label.1[which(delta1_Z <= 0)] <- 2
   # prac.label.1.boot[which(delta1_Z.boot <= 0)] <- 2
   
   #### CLASSIFIER 2
   delta2_Z <- W0_FG * delta1_Z + S_FG * S_Z
   # delta2_Z.boot <- W0_FG.boot * delta1_Z.boot + S_FG.boot * S_Z.boot
   
   prac.label.2[which(delta2_Z > 0)] <- 1
   # prac.label.2.boot[which(delta2_Z.boot > 0)] <- 1
   prac.label.2[which(delta2_Z <= 0)] <- 2
   # prac.label.2.boot[which(delta2_Z.boot <= 0)] <- 2
   
   #### CLASSIFIER 3
   delta3_Z <- W0_FG * sign(delta1_Z) + S_FG * sign(S_Z)
   # delta3_Z.boot <- W0_FG.boot * sign(delta1_Z.boot) + S_FG.boot * sign(S_Z.boot)
   
   prac.label.3[which(delta3_Z > 0)] <- 1
   # prac.label.3.boot[which(delta3_Z.boot > 0)] <- 1
   prac.label.3[which(delta3_Z <= 0)] <- 2
   # prac.label.3.boot[which(delta3_Z.boot <= 0)] <- 2
   
   prac.label <- list(prac.label.1, prac.label.2, prac.label.3,
                      # prac.label.1.boot, prac.label.2.boot, prac.label.3.boot
                      )
   
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
   
   return(X)
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
                    init.test.data) %>% as.matrix() %>% labels.rename()
   
   J <- dataset[,1] %>% unique() %>% length()
   
   print("Dataset extracted")
   
   N <- nrow(dataset)
   d <- ncol(dataset) - 1
   
   classes.mat <- cbind(rep(1:J, each = J), 
                        rep(1:J, times = J)) %>% as.data.frame() %>% filter(V1 < V2)
   
   ground.truth <- dataset[,1] %>% unlist() %>% as.numeric()
   
   for(ind in 1:nrow(classes.mat)){
      
      A <- classes.mat[ind, 1] 
      B <- classes.mat[ind, 2]
      
      class.A <- which(ground.truth == A)
      class.B <- which(ground.truth == B)   
      
      n <- round(length(class.A) * 0.5)
      m <- round(length(class.B) * 0.5)
      
      train.index <- test.index <- list()
      
      for(u in 1:iterations){
         train.index[[u]] <- c(sample(class.A, n),sample(class.B, m))
         test.index[[u]] <- setdiff(1:N, train.index[[u]])
      }
      
      error.prop.1 <- error.prop.2 <- error.prop.3 <- c()
      
      
      
      
      
      
   }
   
   
   
   res.list <- list()
   
   cat("Started - Our Classifiers:")
   print(Sys.time())
   
   ############################################## Our Classifiers
   
   for(u in 1:iterations){
      
      if(u %% 1 == 0) print(u)
      start.time <- proc.time()[3]
      
      X <- dataset[which(dataset[train.index[[u]],1] == 1), -1]
      Y <- dataset[which(dataset[train.index[[u]],1] == 2), -1]
      Q <- rbind(X,Y)
      B <- Q[sample(1:nrow(Q), size = M, replace = TRUE), ]
      
      Z <- dataset[test.index[[u]], -1]     ## Test Observations
      
      # if (u %% 1 == 0) {print(u)}
      
      n <- nrow(X)
      m <- nrow(Y)
      
      clusterExport(cl, c('X','Y','Q','B','M','n','m','rho'))
      
      
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
      
      # ##### T.FG.rho.boot
      # T.FG.rho.boot <- function(vec){
      #    i <- vec[1]
      #    j <- vec[2]
      #    
      #    return(sum(sapply(1:M, function(val){
      #       rho(X[i,],Y[j,],B[val,])
      #    })))
      # }
      # 
      # index.mat <- cbind(rep(1:n, each = m),rep(1:m, times = n))
      # 
      # T.FG.boot <- index.mat %>% 
      #    parApply(cl, ., 1, T.FG.rho.boot) %>% sum() / (M*n*m)
      
      
      
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
      
      
      # ##### T.FF.rho.boot
      # T.FF.rho.boot <- function(vec){
      #    i <- vec[1]
      #    j <- vec[2]
      #    
      #    return(sum(sapply(1:M, function(val){
      #       rho(X[i,],X[j,],B[val,])
      #    })))
      # }
      # 
      # index.mat <- cbind(rep(1:n, each = n),rep(1:n, times = n))
      # 
      # T.FF.boot <- index.mat %>% 
      #    parApply(cl, ., 1, T.FF.rho.boot) %>% sum() / (M*n*(n-1))
      
      
      
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
      
      
      # ##### T.GG.rho.boot
      # T.GG.rho.boot <- function(vec){
      #    i <- vec[1]
      #    j <- vec[2]
      #    
      #    return(sum(sapply(1:M,function(val){
      #       rho(Y[i,],Y[j,],B[val,])
      #    })))
      # }
      # 
      # index.mat <- cbind(rep(1:m, each = m),rep(1:m, times = m))
      # 
      # T.GG.boot <- index.mat %>% 
      #    parApply(cl, ., 1, T.GG.rho.boot) %>% sum() / (M*m*(m-1))
      
      
      ########## W
      W0_FG <- 2 * T.FG - T.FF - T.GG
      W1_FG <- W0_FG^2 / 2 + (T.FF - T.GG)^2 / 2
      W2_FG <- W0_FG / 2 + abs(T.FF - T.GG) / 2
      
      W <- list(W0_FG, W1_FG, W2_FG)
      S_FG <- T.FF - T.GG
      
      # ########## W.boot
      # W0_FG.boot <- 2 * T.FG.boot - T.FF.boot - T.GG.boot
      # W1_FG.boot <- W0_FG.boot^2 / 2 + (T.FF.boot - T.GG.boot)^2 / 2
      # W2_FG.boot <- W0_FG.boot / 2 + abs(T.FF.boot - T.GG.boot) / 2
      # 
      # W.boot <- list(W0_FG.boot, W1_FG.boot, W2_FG.boot)
      # S_FG.boot <- T.FF.boot - T.GG.boot
      
      
      
      ########## Test Observations
      
      ground.label <- ground.truth[test.index[[u]]]
      
      clusterExport(cl, c('Z'))
      
      ##################################################################### Classification
      prac.label <- classify.parallel(Z, X, Y, #B,
                                      T.FF, T.GG, T.FG,
                                      # T.FF.boot, T.GG.boot, T.FG.boot,
                                      W, #W.boot,
                                      S_FG, #S_FG.boot
                                      )
      #####################################################################
      
      error.prop.1[u] <- length(which(ground.label != prac.label[[1]])) / nrow(Z)
      error.prop.2[u] <- length(which(ground.label != prac.label[[2]])) / nrow(Z)
      error.prop.3[u] <- length(which(ground.label != prac.label[[3]])) / nrow(Z)
      
      # error.prop.1.boot[u] <- length(which(ground.label != prac.label[[4]])) / nrow(Z)
      # error.prop.2.boot[u] <- length(which(ground.label != prac.label[[5]])) / nrow(Z)
      # error.prop.3.boot[u] <- length(which(ground.label != prac.label[[6]])) / nrow(Z)
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

