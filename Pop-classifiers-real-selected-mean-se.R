


library(doParallel)
library(dplyr)
library(readxl)
library(writexl)

library(glmnet)            #### GLMNET
library(RandPro)           #### Random Projection
library(e1071)             #### SVM (Linear & RBF Kernel)
library(randomForest)      #### Random Forest
library(nnet)              #### Neural Networks
library(class)             #### One Nearest Neighbour


# start.time <- proc.time()

no.cores <- round(detectCores()*0.55)
cl <- makeCluster(spec = no.cores, type = 'PSOCK')
registerDoParallel(cl)

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
# clusterExport(cl, ls())



#################

final.real.data <- read.csv("E:/JRC-2022/Classification-Summer-2022-JRC/Results/Real/final-real-data-for-reporting.csv")

dataset.names <- final.real.data$Dataset





print("Hello")

iterations <- 20


# path.CompCancer <- "D:/My Documents/Datasets/CompCancer Database/Database/"
# path.Microarray <- "D:/My Documents/Datasets/Microarray Database/Database/"
# path.UCR <- "D:/My Documents/Datasets/UCR Database/UCRArchive_2018/"


path.CompCancer <- "E:/JRC-2022/Classification-Summer-2022-JRC/Datasets/CompCancer Database/Database/"
path.Microarray <- "E:/JRC-2022/Classification-Summer-2022-JRC/Datasets/Microarray Database/Database/"
path.UCR <- "E:/JRC-2022/Classification-Summer-2022-JRC/Datasets/UCR/UCRArchive_2018/"



for(h in 1:length(dataset.names)){
   # for(h in 1:1){
   
   pos <- which(final.real.data$Dataset == dataset.names[h])
   
   print(paste0(final.real.data$Database[pos]," : ",dataset.names[h]))
   
   
   if(final.real.data$Database[pos] == "CompCancer"){
      
      dataset <- paste0(path.CompCancer, dataset.names[h],"_database.xlsx") %>% 
         read_excel() %>% 
         labels.rename() %>% 
         as.matrix() %>%
         apply(c(1,2), function(val) as.numeric(val))
      
      print("CompCancer")
   }
   
   
   if(final.real.data$Database[pos] == "Microarray"){
      
      dataset <- paste0(path.Microarray, dataset.names[h],".csv") %>% 
         read.csv() %>% 
         na.omit() %>%
         labels.rename() %>%
         arrange(V1) %>%
         as.matrix() %>%
         apply(c(1,2), function(val) as.numeric(val))
      
      print("Microarray")
   }
   
   
   if(final.real.data$Database[pos] == "UCR"){
      
      init.train.data <- read.delim(paste0(path.UCR, 
                                           dataset.names[h], "/",
                                           dataset.names[h], "_TRAIN.tsv"), 
                                    header = FALSE, 
                                    stringsAsFactors = FALSE)
      
      init.test.data <- read.delim(paste0(path.UCR, 
                                          dataset.names[h], "/",
                                          dataset.names[h], "_TEST.tsv"), 
                                   header = FALSE, 
                                   stringsAsFactors = FALSE)
      
      dataset <- rbind(init.train.data, 
                       init.test.data) %>% as.matrix() %>% labels.rename()
      
      print("UCR")
   }
   
   
   print(h)
   print(Sys.time())
   
   start.time.sys <- Sys.time()
   start.time.proc <- proc.time()
   
   print("Dataset extracted")
   
   N <- nrow(dataset)
   d <- ncol(dataset) - 1
   
   ground.truth <- dataset[,1] %>% unlist() %>% as.numeric()
   
   train.index <- test.index <- list()
   
   for(u in 1:iterations){
      train.index[[u]] <- sample(1:N, floor(N/2), replace = FALSE)
      test.index[[u]] <- setdiff(1:N, train.index[[u]])
   }
   
   
   print(Sys.time() - start.time.sys)
   
   ############################################## Popular Classifiers START
   
   result <- foreach(u = 1:iterations,
                     .combine = rbind,
                     .packages = c('glmnet','RandPro','e1071','randomForest',
                                   'class','nnet')) %dopar% 
      {
         train.sample <- dataset[train.index[[u]],-1]
         train.label <- dataset[train.index[[u]],1]
         
         test.sample <- Z <- dataset[test.index[[u]], -1]     ## Test Observations
         test.label <- dataset[test.index[[u]], 1]
         
         # if (u %% 1 == 0) {print(u)}
         
         
         ################################ GLMNET
         
         mdl <- cv.glmnet(
            x = train.sample,
            y = as.factor(train.label),
            # family = 'multinomial',
            family = ifelse(length(unique(train.label))==2, 'binomial', 'multinomial'),
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
         
         mdl.nnet.logistic.1 <- nnet(Q[1:length(train.label),], 
                                     targets[1:length(train.label),], size = 1,
                                     decay = 5e-4, maxit = 100, MaxNWts = 30000,
                                     linout = FALSE)
         
         mdl.nnet.logistic.3 <- nnet(Q[1:length(train.label),], 
                                     targets[1:length(train.label),], size = 3,
                                     decay = 5e-4, maxit = 100, MaxNWts = 30000,
                                     linout = FALSE)
         
         mdl.nnet.logistic.5 <- nnet(Q[1:length(train.label),], 
                                     targets[1:length(train.label),], size = 5,
                                     decay = 5e-4, maxit = 100, MaxNWts = 30000,
                                     linout = FALSE)
         
         mdl.nnet.logistic.10 <- nnet(Q[1:length(train.label),], 
                                      targets[1:length(train.label),], size = 10,
                                      decay = 5e-4, maxit = 100, MaxNWts = 30000,
                                      linout = FALSE)
         
         p_log_1 <- predict(mdl.nnet.logistic.1, Q[-c(1:length(train.label)),])
         p_log_3 <- predict(mdl.nnet.logistic.3, Q[-c(1:length(train.label)),])
         p_log_5 <- predict(mdl.nnet.logistic.5, Q[-c(1:length(train.label)),])
         p_log_10 <- predict(mdl.nnet.logistic.10, Q[-c(1:length(train.label)),])
         
         e_nnet_log_1 <- mean(apply(p_log_1, 1, which.max) != test.label)
         e_nnet_log_3 <- mean(apply(p_log_3, 1, which.max) != test.label)
         e_nnet_log_5 <- mean(apply(p_log_5, 1, which.max) != test.label)
         e_nnet_log_10 <- mean(apply(p_log_10, 1, which.max) != test.label)
         
         
         # ##### ReLU Activation
         
         mdl.nnet.ReLU.1 <- nnet(Q[1:length(train.label),], 
                                 targets[1:length(train.label),], size = 1,
                                 decay = 5e-4, maxit = 100, MaxNWts = 30000,
                                 linout = TRUE)
         
         mdl.nnet.ReLU.3 <- nnet(Q[1:length(train.label),], 
                                 targets[1:length(train.label),], size = 3,
                                 decay = 5e-4, maxit = 100, MaxNWts = 30000,
                                 linout = TRUE)
         
         mdl.nnet.ReLU.5 <- nnet(Q[1:length(train.label),], 
                                 targets[1:length(train.label),], size = 5,
                                 decay = 5e-4, maxit = 100, MaxNWts = 30000,
                                 linout = TRUE)
         
         mdl.nnet.ReLU.10 <- nnet(Q[1:length(train.label),], 
                                  targets[1:length(train.label),], size = 10,
                                  decay = 5e-4, maxit = 100, MaxNWts = 30000,
                                  linout = TRUE)
         
         p_ReLU_1 <- predict(mdl.nnet.ReLU.1, Q[-c(1:length(train.label)),])
         p_ReLU_3 <- predict(mdl.nnet.ReLU.3, Q[-c(1:length(train.label)),])
         p_ReLU_5 <- predict(mdl.nnet.ReLU.5, Q[-c(1:length(train.label)),])
         p_ReLU_10 <- predict(mdl.nnet.ReLU.10, Q[-c(1:length(train.label)),])
         
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
   
   
   result.all <- as.data.frame(result)
   
   colnames(result.all) <- c('GLMNET',
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
   
   # result.folder.path <- "C:\\Users\\JYOTISHKA\\Desktop\\final-results-real\\"
   
   result.folder.path <- "E:\\JRC-2022\\Classification-Summer-2022-JRC\\Results\\Real\\Reporting\\"
   
   print(h)
   
   write_xlsx(x = res.list,
              path = paste0(result.folder.path,
                            final.real.data$Database[pos],"_",
                            dataset.names[h],".xlsx"))
   
   print(Sys.time() - start.time.sys)
   
   end.time <- proc.time()[3]- start.time.proc
   print(end.time)
   
   gc()
}


stopCluster(cl)
gc()

