
## Our classifiers including bootstrapped rho, with appended results of popular classifiers
## Date: 28.08.2022

library(doParallel)
library(rio)
library(dplyr)
library(readxl)
library(writexl)
library(EnvStats)

################################################################ Multi-threading
no.cores <- round(detectCores()*0.75)                       ####
cl <- makeCluster(spec = no.cores, type = 'PSOCK')          ####
registerDoParallel(cl)                                      ####
################################################################

# path.wrong.results <- "C:/Users/JYOTISHKA/Desktop/all-classifiers-TwoClass-simulated-old/"
# path.new.results <- "C:\\Users\\JYOTISHKA\\Desktop\\all-classifiers-TwoClass-simulated-new\\"

path.wrong.results <- "E:/JRC-2022/Classification-Summer-2022-JRC/Results/Simulated/"
path.newest.results <- "E:\\JRC-2022\\Classification-Summer-2022-JRC\\Results\\Simulated\\NEWEST\\"

files.wrong <- list.files(path.wrong.results)

################################################################ rho function
rho <- function(a,b,q){                                     ####
   if (prod(a == q)== 1 || prod(b == q) == 1){              ####
      return(0)                                             ####
   }else{                                                   ####
      temp <- sign((a-q) * (b-q)) * (-1)                    ####
      return(temp %>% replace(temp == -1, 0) %>% mean())    ####
   }                                                        ####
}                                                           ####
################################################################


################################################### Classifier Function for Test Observations

classify.parallel <- function(Z, X, Y, B, T.FF, T.GG, T.FG, W, S_FG){
   # print("Classification starting")
   R <- nrow(Z)
   Q <- rbind(X,Y)
   n <- nrow(X)
   m <- nrow(Y)
   M <- nrow(B)
   
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
   
   
   ##### T_FZ.rho.boot
   T_FZ.rho.boot <- function(vec){
      i <- vec[1]
      j <- vec[2]
      
      return(sum(sapply(1:M,function(val){
         rho(Z[i,],X[j,],B[val,])
      })))
   }
   
   index.mat <- cbind(rep(1:R, each = n),rep(1:n, times = R))
   
   T_FZ.boot <- index.mat %>%
      parApply(cl, ., 1, T_FZ.rho.boot) %>%
      matrix(nrow = R, ncol = n, byrow = TRUE) %>% 
      rowMeans() / M
   
   
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
   
   
   ##### T_GZ.rho.boot
   T_GZ.rho.boot <- function(vec){
      i <- vec[1]
      j <- vec[2]
      
      return(sum(sapply(1:M,function(val){
         rho(Z[i,],Y[j,],B[val,])
      })))
   }
   
   index.mat <- cbind(rep(1:R, each = m),rep(1:m, times = R))
   
   T_GZ.boot <- index.mat %>%
      parApply(cl, ., 1, T_GZ.rho.boot) %>%
      matrix(nrow = R, ncol = m, byrow = TRUE) %>% 
      rowMeans() / M
   
   
   
   L_FZ <- T_FZ - rep(T.FF, R)/2
   L_GZ <- T_GZ - rep(T.GG, R)/2
   S_Z <- L_FZ + L_GZ - T.FG
   
   L_FZ.boot <- T_FZ.boot - rep(T.FF.boot, R)/2
   L_GZ.boot <- T_GZ.boot - rep(T.GG.boot, R)/2
   S_Z.boot <- L_FZ.boot + L_GZ.boot - T.FG.boot
   
   W0_FG <- W[[1]]
   # W1_FG <- W[[2]]
   # W2_FG <- W[[3]]
   
   W0_FG.boot <- W.boot[[1]]
   # W1_FG.boot <- W.boot[[2]]
   # W2_FG.boot <- W.boot[[3]]
   
   prac.label.1 <- prac.label.2 <- prac.label.3 <- 
      prac.label.1.boot <- prac.label.2.boot <- prac.label.3.boot <- rep(0, R)
   
   #### CLASSIFIER 1
   delta1_Z <- L_GZ - L_FZ
   delta1_Z.boot <- L_GZ.boot - L_FZ.boot
   
   prac.label.1[which(delta1_Z > 0)] <- 1
   prac.label.1.boot[which(delta1_Z.boot > 0)] <- 1
   prac.label.1[which(delta1_Z <= 0)] <- 2
   prac.label.1.boot[which(delta1_Z.boot <= 0)] <- 2
   
   #### CLASSIFIER 2
   delta2_Z <- W0_FG * delta1_Z + S_FG * S_Z
   delta2_Z.boot <- W0_FG.boot * delta1_Z.boot + S_FG.boot * S_Z.boot
   
   prac.label.2[which(delta2_Z > 0)] <- 1
   prac.label.2.boot[which(delta2_Z.boot > 0)] <- 1
   prac.label.2[which(delta2_Z <= 0)] <- 2
   prac.label.2.boot[which(delta2_Z.boot <= 0)] <- 2
   
   #### CLASSIFIER 3
   delta3_Z <- W0_FG * sign(delta1_Z) + S_FG * sign(S_Z)
   delta3_Z.boot <- W0_FG.boot * sign(delta1_Z.boot) + S_FG.boot * sign(S_Z.boot)
   
   prac.label.3[which(delta3_Z > 0)] <- 1
   prac.label.3.boot[which(delta3_Z.boot > 0)] <- 1
   prac.label.3[which(delta3_Z <= 0)] <- 2
   prac.label.3.boot[which(delta3_Z.boot <= 0)] <- 2
   
   prac.label <- list(prac.label.1, prac.label.2, prac.label.3,
                      prac.label.1.boot, prac.label.2.boot, prac.label.3.boot)
   
   return(prac.label)
}
##################################################################################


# iterations <- 100

n <- 20
m <- 20
ns <- 100
ms <- 100

M <- 1000

d.seq <- c(5,10,25,50,100,250,500,1000)

########## 

for(h in 1:length(files.wrong)){
   
   print(files.wrong[h])
   print(Sys.time())
   
   file.wrong <- paste0(path.wrong.results,files.wrong[h]) %>% import_list()
   file.wrong.pop <- file.wrong %>% lapply(function(sheet) return(sheet[ ,-c(1:3)]))
   
   res.list <- list()
   
   for(k in 1:length(d.seq)){
      
      error.prop.1 <- error.prop.2 <- error.prop.3 <- c()
      
      start.time <- proc.time()
      
      ############################################## Our Classifiers
      
      d <- d.seq[k]
      
      if(h == 1){
         iterations <- 50
      }else{
         iterations <- 100
      }
      
      for(u in 1:iterations){
         
         n <- 20
         m <- 20
         ns <- 100
         ms <- 100
         
         # M <- 1000
         
         if(u %% 5 == 0) {print(u)}
         
         if(h == 1){
            set.seed(u)
            
            X <- matrix(rcauchy((n+ns)*d, 0, 1), nrow = n+ns, ncol = d, byrow = TRUE)
            Y <- matrix(rcauchy((m+ms)*d, 0, 2), nrow = m+ms, ncol = d, byrow = TRUE)
         }
         
         if(h == 2){
            set.seed(u)
            
            X <- matrix(rcauchy((n+ns)*d, 0, 1), nrow = n+ns, ncol = d, byrow = TRUE)
            Y <- matrix(rcauchy((m+ms)*d, 1, 1), nrow = m+ms, ncol = d, byrow = TRUE)
         }
         
         if(h == 3){
            set.seed(u)
            
            X <- matrix(rnorm((n + ns) * d, 0, sqrt(3)),
                        nrow = n + ns,
                        ncol = d,
                        byrow = TRUE)
            
            Y <- matrix(rt((m + ms) * d, df = 3),
                        nrow = m + ms,
                        ncol = d,
                        byrow = TRUE)
         }
         
         if(h == 4){
            set.seed(u)
            
            X <- matrix(rnorm((n+ns)*d, 1, sqrt(1)), nrow = n+ns, ncol = d, byrow = TRUE)
            Y <- matrix(rnorm((m+ms)*d, 1, sqrt(2)), nrow = m+ms, ncol = d, byrow = TRUE)
         }
         
         if(h == 5){
            set.seed(u)
            
            X <- matrix(rpareto((n + ns) * d, location = 1, shape = 1),
                        nrow = n + ns,
                        ncol = d,
                        byrow = TRUE)
            
            Y <- matrix(rpareto((m + ms) * d, location = 1.25, shape = 1),
                        nrow = m + ms,
                        ncol = d,
                        byrow = TRUE)
         }
         
         Z <- rbind(X[(n+1):(n+ns),], Y[(m+1):(m+ms),])     ## Test Observations
         
         X <- X[1:n,]
         Y <- Y[1:m,]
         Q <- rbind(X,Y)
         B <- Q[sample(1:nrow(Q), size = M, replace = TRUE), ]
         
         
         clusterEvalQ(cl, {library(magrittr)})
         clusterExport(cl, c('X','Y','Q','B','n','m','M','rho'))
         
         
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
         
         ##### T.FG.rho.boot
         T.FG.rho.boot <- function(vec){
            i <- vec[1]
            j <- vec[2]
            
            return(sum(sapply(1:M, function(val){
               rho(X[i,],Y[j,],B[val,])
            })))
         }
         
         index.mat <- cbind(rep(1:n, each = m),rep(1:m, times = n))
         
         T.FG.boot <- index.mat %>% 
            parApply(cl, ., 1, T.FG.rho.boot) %>% sum() / (M*n*m)
         
         
         
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
         
         
         ##### T.FF.rho.boot
         T.FF.rho.boot <- function(vec){
            i <- vec[1]
            j <- vec[2]
            
            return(sum(sapply(1:M, function(val){
               rho(X[i,],X[j,],B[val,])
            })))
         }
         
         index.mat <- cbind(rep(1:n, each = n),rep(1:n, times = n))
         
         T.FF.boot <- index.mat %>% 
            parApply(cl, ., 1, T.FF.rho.boot) %>% sum() / (M*n*(n-1))
         
         
         
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
         
         
         ##### T.GG.rho.boot
         T.GG.rho.boot <- function(vec){
            i <- vec[1]
            j <- vec[2]
            
            return(sum(sapply(1:M,function(val){
               rho(Y[i,],Y[j,],B[val,])
            })))
         }
         
         index.mat <- cbind(rep(1:m, each = m),rep(1:m, times = m))
         
         T.GG.boot <- index.mat %>% 
            parApply(cl, ., 1, T.GG.rho.boot) %>% sum() / (M*m*(m-1))
         
         
         
         
         ########## W
         W0_FG <- 2 * T.FG - T.FF - T.GG
         W1_FG <- W0_FG^2 / 2 + (T.FF - T.GG)^2 / 2
         W2_FG <- W0_FG / 2 + abs(T.FF - T.GG) / 2
         
         W <- list(W0_FG, W1_FG, W2_FG)
         S_FG <- T.FF - T.GG
         
         ########## W.boot
         W0_FG.boot <- 2 * T.FG.boot - T.FF.boot - T.GG.boot
         W1_FG.boot <- W0_FG.boot^2 / 2 + (T.FF.boot - T.GG.boot)^2 / 2
         W2_FG.boot <- W0_FG.boot / 2 + abs(T.FF.boot - T.GG.boot) / 2
         
         W.boot <- list(W0_FG.boot, W1_FG.boot, W2_FG.boot)
         S_FG.boot <- T.FF.boot - T.GG.boot
         
         
         
         
         
         ########## Classifcation of Test Observations
         
         ground.label <- c(rep(1,ns), rep(2,ms))
         clusterExport(cl, c('Z'))
         
         ##################################################################### Classification
         prac.label <- classify.parallel(Z, X, Y, 
                                         T.FF, T.GG, T.FG,
                                         T.FF.boot, T.GG.boot, T.FG.boot,
                                         W, W.boot,
                                         S_FG, S_FG.boot)
         #####################################################################
         
         error.prop.1[u] <- length(which(ground.label != prac.label[[1]])) / (ns + ms)
         error.prop.2[u] <- length(which(ground.label != prac.label[[2]])) / (ns + ms)
         error.prop.3[u] <- length(which(ground.label != prac.label[[3]])) / (ns + ms)
         
         error.prop.1.boot[u] <- length(which(ground.label != prac.label[[4]])) / (ns + ms)
         error.prop.2.boot[u] <- length(which(ground.label != prac.label[[5]])) / (ns + ms)
         error.prop.3.boot[u] <- length(which(ground.label != prac.label[[6]])) / (ns + ms)
      }
      
      res.list[[k]] <- cbind(c(error.prop.1, NA, mean(error.prop.1), 
                               sciplot::se(error.prop.1)),
                             c(error.prop.2, NA, mean(error.prop.2), 
                               sciplot::se(error.prop.2)),
                             c(error.prop.3, NA, mean(error.prop.3), 
                               sciplot::se(error.prop.3)),
                             c(error.prop.1.boot, NA, mean(error.prop.1.boot), 
                               sciplot::se(error.prop.1.boot)),
                             c(error.prop.2.boot, NA, mean(error.prop.2.boot), 
                               sciplot::se(error.prop.2.boot)),
                             c(error.prop.3.boot, NA, mean(error.prop.3.boot), 
                               sciplot::se(error.prop.3.boot)),
                           file.wrong.pop[[k]]) %>% as.data.frame()
      
      colnames(res.list[[k]]) <- c('del.1','del.2','del.3',
                                   'del.1.boot','del.2.boot','del.3.boot',
                                   'BYS',
                                   'GLMNET',
                                   'RF1','RF2','RF3','RF4',
                                   'NNRAND',
                                   'SVMLIN','SVMRBF',
                                   'NN-lg-1','NN-lg-3','NN-lg-5','NN-lg-10',
                                   'NN-R-1','NN-R-3','NN-R-5','NN-R-10',
                                   'ONN')
      
      print(proc.time() - start.time)
   }
   
   res.list <- list("d=5" = res.list[[1]],
                    "d=10" = res.list[[2]],
                    "d=25" = res.list[[3]],
                    "d=50" = res.list[[4]],
                    "d=100" = res.list[[5]],
                    "d=250" = res.list[[6]],
                    "d=500" = res.list[[7]],
                    "d=1000" = res.list[[8]])
   
   writexl::write_xlsx(res.list,
                       path = paste0(path.newest.results,files.wrong[h]))
   
   print(files.wrong[h])
   print(Sys.time())
   cat("\n\n")
}










########## Extracting unordered pairs from ordered pairs

if (FALSE) {
   index.mat %>%
      as.data.frame() %>%
      mutate(dx = pmin(V1, V2), dy = pmax(V1, V2)) %>%
      distinct(dx, dy) %>%
      filter(dx != dy)
}
