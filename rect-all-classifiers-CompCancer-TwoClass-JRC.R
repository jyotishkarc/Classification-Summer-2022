

library(doParallel)
# library(rio)
library(dplyr)
library(readxl)
library(writexl)

# path.datasets <- "D:/My Documents/Datasets/CompCancer Database/TwoClass/"
path.datasets <- "E:/JRC-2022/Classification-Summer-2022-JRC/Datasets/CompCancer Database/TwoClass/"

# path.wrong.results <- "C:/Users/JYOTISHKA/Desktop/CompCancer-TwoClass-Results/"
# path.new.results <- "C:\\Users\\JYOTISHKA\\Desktop\\CompCancer-TwoClass-New-Results\\"

path.wrong.results <- "E:/JRC-2022/Classification-Summer-2022-JRC/Results/Real/CompCancer/TwoClass/"
path.new.results <- "E:\\JRC-2022\\Classification-Summer-2022-JRC\\Results\\Real\\CompCancer\\TwoClassNew\\"

files.wrong <- list.files(path.wrong.results)

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


for(k in 3:length(files.wrong)){
   
   start.time <- proc.time()
   print(k)
   print(files.wrong[k])
   print(Sys.time())
   
   file.wrong <- paste0(path.wrong.results,files.wrong[k]) %>% read_excel() %>% as.matrix()
   
   file.wrong.pop <- file.wrong[ ,-c(1:3)]
   
   iterations <- 50
   
   dataset <- paste0(path.datasets, files.wrong[k]) %>% 
      read_excel() %>% 
      as.matrix() %>%
      apply(c(1,2), function(val) as.numeric(val))
   
   N <- nrow(dataset)
   d <- ncol(dataset) - 1
   
   ground.truth <- dataset[,1] %>% unlist() #%>% as.numeric()
   
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
   
   ############################################## Our Classifiers
   
   for(u in 1:iterations){
      
      if(u %% 4 == 0) print(u)
      
      X <- dataset[which(dataset[train.index[[u]],1] == 1), -1]
      Y <- dataset[which(dataset[train.index[[u]],1] == 2), -1]
      Q <- rbind(X,Y)
      
      Z <- dataset[test.index[[u]], -1]     ## Test Observations
      
      # if (u %% 1 == 0) {print(u)}
      
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
   
   new.result <- cbind(c(error.prop.1, NA, mean(error.prop.1), sciplot::se(error.prop.1)),
                       c(error.prop.2, NA, mean(error.prop.2), sciplot::se(error.prop.2)),
                       c(error.prop.3, NA, mean(error.prop.3), sciplot::se(error.prop.3)),
                       file.wrong.pop) %>% as.data.frame()
   
   colnames(new.result) <- c('del.1','del.2','del.3',
                             'GLMNET',
                             'RF1','RF2','RF3','RF4',
                             'NNRAND',
                             'SVMLIN','SVMRBF',
                             'NN-lg-1','NN-lg-3','NN-lg-5','NN-lg-10',
                             'NN-R-1','NN-R-3','NN-R-5','NN-R-10',
                             'ONN')
   
   print(files.wrong[k])
   print(k)
   print(proc.time() - start.time)
   
   writexl::write_xlsx(new.result,
                       path = paste0(path.new.results,files.wrong[k]))
   
}
