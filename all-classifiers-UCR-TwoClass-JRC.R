

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

#################

rho <- function(a,b,c){
   if (prod(a == c)== 1 || prod(b == c) == 1){
      return(0)
   }else{
      temp <- sign((a-c) * (b-c)) * (-1)
      return(temp %>% replace(temp == -1, 0) %>% mean())
   }
}

#################

classify.parallel <- function(Z, X, Y, T.FF, T.GG, T.FG, W, S_FG){
   
   # print("Classification starting")
   R <- nrow(Z)
   Q <- rbind(X,Y)
   n <- nrow(X)
   m <- nrow(Y)
   
   clusterExport(cl, c('R','n','m'), envir = environment())
   
   
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
      rowMeans() / (n+m-1)
   
   
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
      rowMeans() / (n+m-1)
   
   
   L_FZ <- T_FZ - rep(T.FF, R)/2
   L_GZ <- T_GZ - rep(T.GG, R)/2
   
   S_Z <- L_FZ + L_GZ - T.FG
   
   W0_FG <- W[[1]]
   # W1_FG <- W[[2]]
   # W2_FG <- W[[3]]
   
   prac.label.1 <- prac.label.2 <- prac.label.3 <- rep(0, R)
   
   #### CLASSIFIER 1
   delta1_Z <- L_GZ - L_FZ
   
   prac.label.1[which(delta1_Z > 0)] <- 1
   prac.label.1[which(delta1_Z <= 0)] <- 2
   
   #### CLASSIFIER 2
   delta2_Z <- W0_FG * delta1_Z + S_FG * S_Z
   
   prac.label.2[which(delta2_Z > 0)] <- 1
   prac.label.2[which(delta2_Z <= 0)] <- 2
   
   #### CLASSIFIER 3
   delta3_Z <- W0_FG * sign(delta1_Z) + S_FG * sign(S_Z)
   
   prac.label.3[which(delta3_Z > 0)] <- 1
   prac.label.3[which(delta3_Z <= 0)] <- 2
   
   prac.label <- list(prac.label.1, prac.label.2, prac.label.3)
   
   return(prac.label)
}

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

iterations <- 50

# path <- "G:/Datasets/Classification Datasets/"
path <- "E:/JRC-2022/Classification-Summer-2022-JRC/Datasets/UCR/"

UCR.stats <- read.csv(paste0(path,"UCR-DataSummary.csv"), stringsAsFactors = FALSE)
files.TwoClass <- UCR.stats$Name[which(UCR.stats$Class == 2)]

files.TwoClass <- intersect(JMLR.UCR,files.TwoClass)

path.UCR <- paste0(path,"UCRArchive_2018/")
files.UCR <- list.files(path.UCR)


for(h in 5:length(files.TwoClass)){
   
   print(h)
   print(files.TwoClass[h])
   print(Sys.time())
   
   start.time.sys <- Sys.time()
   start.time.proc <- proc.time()
   
   init.train.data <- read.delim(paste0(path.UCR, 
                                        files.TwoClass[h], "/",
                                        files.TwoClass[h], "_TRAIN.tsv"), 
                                 header = FALSE, 
                                 stringsAsFactors = FALSE)
   
   init.test.data <- read.delim(paste0(path.UCR, 
                                       files.TwoClass[h], "/",
                                       files.TwoClass[h], "_TEST.tsv"), 
                                header = FALSE, 
                                stringsAsFactors = FALSE)
   
   dataset <- rbind(init.train.data, init.test.data) %>% as.matrix() %>% labels.rename()
   
   print("Dataset extracted")
   
   N <- nrow(dataset)
   d <- ncol(dataset) - 1
   
   ground.truth <- dataset[,1] %>% unlist() %>% as.numeric()
   
   class.1 <- which(ground.truth == 1)
   class.2 <- which(ground.truth == 2)
   
   n <- m <- round(min(c(length(class.1),length(class.2))) * 0.5)
   
   train.index <- test.index <- list()
   
   for(u in 1:iterations){
      train.index[[u]] <- c(sample(class.1, n),sample(class.2, m))
      test.index[[u]] <- setdiff(1:N, train.index[[u]])
   }
   
   
   error.prop.1 <- error.prop.2 <- error.prop.3 <- c()
   res.list <- list()
   
   ############################################## Our Classifiers START
   
   for(u in 1:iterations){
      
      if(u %% 1 == 0) print(u)
      
      X <- dataset[which(dataset[train.index[[u]],1] == 1), -1]
      Y <- dataset[which(dataset[train.index[[u]],1] == 2), -1]
      Q <- rbind(X,Y)
      
      Z <- dataset[test.index[[u]], -1]     ## Test Observations
      
      n <- nrow(X)
      m <- nrow(Y)
      
      clusterExport(cl, c('X','Y','Q','n','m'))
      
      
      #####
      T.FG.rho.fun <- function(vec){
         i <- vec[1]
         j <- vec[2]
         
         return(sum(sapply(1:(n+m),function(val){
            rho(X[i,],Y[j,],Q[val,])
         })))
      }
      
      index.mat <- cbind(rep(1:n, each = m),rep(1:m, times = n))
      
      T.FG <- index.mat %>% 
         parApply(cl, ., 1, T.FG.rho.fun) %>%
         matrix(nrow = n, ncol = m, byrow = T) %>% sum() / ((n+m-2)*n*m)
      
      
      #####
      T.FF.rho.fun <- function(vec){
         i <- vec[1]
         j <- vec[2]
         
         return(sum(sapply(1:(n+m),function(val){
            rho(X[i,],X[j,],Q[val,])
         })))
      }
      
      index.mat <- cbind(rep(1:n, each = n),rep(1:n, times = n))
      
      T.FF <- index.mat %>% 
         parApply(cl, ., 1, T.FF.rho.fun) %>% sum() / ((n+m-2)*n*(n-1))
      
      
      #####
      T.GG.rho.fun <- function(vec){
         i <- vec[1]
         j <- vec[2]
         
         return(sum(sapply(1:(n+m),function(val){
            rho(Y[i,],Y[j,],Q[val,])
         })))
      }
      
      index.mat <- cbind(rep(1:m, each = m),rep(1:m, times = m))
      
      T.GG <- index.mat %>% 
         parApply(cl, ., 1, T.GG.rho.fun) %>% sum() / ((n+m-2)*m*(m-1))
      
      
      ########## W
      W0_FG <- 2 * T.FG - T.FF - T.GG
      W1_FG <- W0_FG^2 / 2 + (T.FF - T.GG)^2 / 2
      W2_FG <- W0_FG / 2 + abs(T.FF - T.GG) / 2
      
      W <- list(W0_FG, W1_FG, W2_FG)
      S_FG <- T.FF - T.GG
      
      
      ########## Test Observations
      
      ground.label <- ground.truth[test.index[[u]]]
      
      clusterExport(cl, c('Z'))
      
      prac.label <- classify.parallel(Z, X, Y, T.FF, T.GG, T.FG, W, S_FG)
      
      error.prop.1[u] <- length(which(ground.label != prac.label[[1]])) / nrow(Z)
      error.prop.2[u] <- length(which(ground.label != prac.label[[2]])) / nrow(Z)
      error.prop.3[u] <- length(which(ground.label != prac.label[[3]])) / nrow(Z)
   }
   
   print(Sys.time() - start.time.sys)
   
   ############################################## Our Classifiers END
   #
   #
   #
   ############################################## Popular Classifiers START
   
   result <- foreach(u = 1:iterations,
                     .combine = rbind,
                     .packages = c('glmnet','RandPro','e1071','randomForest',
                                   'class','nnet')) %dopar% 
      {
         X <- dataset[which(dataset[train.index[[u]],1] == 1), -1]
         Y <- dataset[which(dataset[train.index[[u]],1] == 2), -1]
         
         n <- nrow(X)
         m <- nrow(Y)
         
         train.sample <- rbind(X,Y)
         train.label <- c(rep(1,n), rep(2,m))
         
         test.sample <- Z <- dataset[test.index[[u]], -1]     ## Test Observations
         test.label <- dataset[test.index[[u]], 1]
         
         # if (u %% 1 == 0) {print(u)}
         
         
         ################################ GLMNET
         
         mdl <- cv.glmnet(
            x = train.sample,
            y = as.factor(train.label),
            family = 'binomial',
            # family = ifelse(J==2, 'binomial', 'multinomial'),
            type.measure = 'class'
         )
         
         lam.opt <- mdl$lambda[which.min(mdl$cvm)]
         
         pred.lbl <- predict(
            object = mdl,
            newx = as.matrix(test.sample),
            type = 'class',
            s = lam.opt
         )
         
         e_glm <- mean(pred.lbl != test.label)
         
         
         ################################ Random Projection NN
         
         mdl2 <- RandPro::classify(
            train_data = as.matrix(train.sample),
            test_data = as.matrix(test.sample),
            train_label = as.factor(train.label),
            test_label = as.factor(test.label),
            eps = 0.1
         )
         
         e_rnd = 1 - as.numeric(mdl2$overall[1])
         
         
         ################################ SVM Linear
         
         fit1 <- svm(x = train.sample, y = as.factor(train.label),
                     kernel = "linear", gamma = 1/d)
         p_1 <- as.numeric(predict(fit1, test.sample))
         
         e_SVM_lin <- mean(p_1 != test.label)
         
         
         ################################ SVM RBF
         
         fit4 <- svm(x = train.sample, y = as.factor(train.label),
                     kernel = "radial")
         
         p_1 <- as.numeric(predict(fit4, test.sample))
         
         e_SVM_rbf <- mean(p_1 != test.label)
         
         
         ################################ Random Forest
         
         fit1 <- randomForest(train.sample, as.factor(train.label),
                              ntree = 5000, mtry = d^0.1)
         fit2 <- randomForest(train.sample, as.factor(train.label),
                              ntree = 5000, mtry = d^0.25)
         fit3 <- randomForest(train.sample, as.factor(train.label),
                              ntree = 5000, mtry = d^0.5)
         fit4 <- randomForest(train.sample, as.factor(train.label),
                              ntree = 5000, mtry = d^0.75)
         
         p_1 <- as.numeric(predict(object = fit1, newdata = test.sample,
                                   type = 'class'))
         p_2 <- as.numeric(predict(object = fit2, newdata = test.sample,
                                   type = 'class'))
         p_3 <- as.numeric(predict(object = fit3, newdata = test.sample,
                                   type = 'class'))
         p_4 <- as.numeric(predict(object = fit4, newdata = test.sample,
                                   type = 'class'))
         
         e_RF_1 <- mean(p_1 != test.label)
         e_RF_2 <- mean(p_2 != test.label)
         e_RF_3 <- mean(p_3 != test.label)
         e_RF_4 <- mean(p_4 != test.label)
         
         
         ################################ Neural Networks
         
         ##### Logistic Activation
         
         Q <- data.frame(rbind(train.sample, test.sample))
         targets <- class.ind(c(train.label, test.label))
         
         mdl.nnet.logistic.1 <- nnet(Q[1:(n+m),], targets[1:(n+m),], size = 1,
                                     decay = 5e-4, maxit = 100, MaxNWts = 30000,
                                     linout = FALSE)
         
         mdl.nnet.logistic.3 <- nnet(Q[1:(n+m),], targets[1:(n+m),], size = 3,
                                     decay = 5e-4, maxit = 100, MaxNWts = 30000,
                                     linout = FALSE)
         
         mdl.nnet.logistic.5 <- nnet(Q[1:(n+m),], targets[1:(n+m),], size = 5,
                                     decay = 5e-4, maxit = 100, MaxNWts = 30000,
                                     linout = FALSE)
         
         mdl.nnet.logistic.10 <- nnet(Q[1:(n+m),], targets[1:(n+m),], size = 10,
                                      decay = 5e-4, maxit = 100, MaxNWts = 30000,
                                      linout = FALSE)
         
         p_log_1 <- predict(mdl.nnet.logistic.1, Q[-c(1:(n+m)),])
         p_log_3 <- predict(mdl.nnet.logistic.3, Q[-c(1:(n+m)),])
         p_log_5 <- predict(mdl.nnet.logistic.5, Q[-c(1:(n+m)),])
         p_log_10 <- predict(mdl.nnet.logistic.10, Q[-c(1:(n+m)),])
         
         e_nnet_log_1 <- mean(apply(p_log_1, 1, which.max) != test.label)
         e_nnet_log_3 <- mean(apply(p_log_3, 1, which.max) != test.label)
         e_nnet_log_5 <- mean(apply(p_log_5, 1, which.max) != test.label)
         e_nnet_log_10 <- mean(apply(p_log_10, 1, which.max) != test.label)
         
         
         # ##### ReLU Activation
         
         mdl.nnet.ReLU.1 <- nnet(Q[1:(n+m),], targets[1:(n+m),], size = 1,
                                 decay = 5e-4, maxit = 100, MaxNWts = 30000,
                                 linout = TRUE)
         
         mdl.nnet.ReLU.3 <- nnet(Q[1:(n+m),], targets[1:(n+m),], size = 3,
                                 decay = 5e-4, maxit = 100, MaxNWts = 30000,
                                 linout = TRUE)
         
         mdl.nnet.ReLU.5 <- nnet(Q[1:(n+m),], targets[1:(n+m),], size = 5,
                                 decay = 5e-4, maxit = 100, MaxNWts = 30000,
                                 linout = TRUE)
         
         mdl.nnet.ReLU.10 <- nnet(Q[1:(n+m),], targets[1:(n+m),], size = 10,
                                  decay = 5e-4, maxit = 100, MaxNWts = 30000,
                                  linout = TRUE)
         
         p_ReLU_1 <- predict(mdl.nnet.ReLU.1, Q[-c(1:(n+m)),])
         p_ReLU_3 <- predict(mdl.nnet.ReLU.3, Q[-c(1:(n+m)),])
         p_ReLU_5 <- predict(mdl.nnet.ReLU.5, Q[-c(1:(n+m)),])
         p_ReLU_10 <- predict(mdl.nnet.ReLU.10, Q[-c(1:(n+m)),])
         
         e_nnet_ReLU_1 <- mean(apply(p_ReLU_1, 1, which.max) != test.label)
         e_nnet_ReLU_3 <- mean(apply(p_ReLU_3, 1, which.max) != test.label)
         e_nnet_ReLU_5 <- mean(apply(p_ReLU_5, 1, which.max) != test.label)
         e_nnet_ReLU_10 <- mean(apply(p_ReLU_10, 1, which.max) != test.label)
         
         
         ################################ One Nearest Neighbour
         
         mdl.onn <- class::knn1(train = train.sample,
                                test = test.sample,
                                cl = as.factor(train.label))
         
         e_onn <- mean(mdl.onn != test.label)
         
         
         ################################
         
         return(c(e_glm,
                  e_RF_1, e_RF_2, e_RF_3, e_RF_4,
                  e_rnd,
                  e_SVM_lin,
                  e_SVM_rbf,
                  e_nnet_log_1, e_nnet_log_3, e_nnet_log_5, e_nnet_log_10,
                  e_nnet_ReLU_1, e_nnet_ReLU_3, e_nnet_ReLU_5, e_nnet_ReLU_10,
                  e_onn
         ))
      }
   
   ############################################## Popular Classifiers END
   
   
   result.all <- cbind(error.prop.1, error.prop.2, error.prop.3,
                       as.data.frame(result)) %>% as.data.frame()
   
   colnames(result.all) <- c('del.1','del.2','del.3',
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
   
   # result.folder.path <- "C:\\Users\\JYOTISHKA\\Desktop\\UCR-TwoClass-Results\\"
   
   result.folder.path <- "E:\\JRC-2022\\Classification-Summer-2022-JRC\\Results\\Real\\UCR\\TwoClass\\"
   
   print(h)
   print(files.TwoClass[h])
   
   write_xlsx(x = res.list,
              path = paste0(result.folder.path, files.TwoClass[h],".xlsx"))
   
   print(Sys.time() - start.time.sys)
   
   end.time <- proc.time()[3]- start.time.proc
   print(end.time)
}


stopCluster(cl)
gc()






