
## Our classifiers with appended results of popular classifiers
## Date: 28.08.2022

library(doParallel)
library(rio)
library(dplyr)
library(readxl)
library(writexl)
# library(EnvStats)

################################################################ Multi-threading
no.cores <- round(detectCores()*0.75)                       ####
cl <- makeCluster(spec = no.cores, type = 'PSOCK')          ####
registerDoParallel(cl)                                      ####
################################################################

path.new.results <- "C:\\Users\\JYOTISHKA\\Desktop\\all-classifiers-TwoClass-simulated-new\\"

# path.newest.results <- "E:\\JRC-2022\\Classification-Summer-2022-JRC\\Results\\Simulated\\NEWEST\\MORE FRESH\\"


################################################################ rho function
rho <- function(a,b,c){
   if (prod(a == c)== 1 || prod(b == c) == 1){
      return(0)
   }else{
      temp <- sign((a-c) * (b-c)) * (-1)
      return(temp %>% replace(temp == -1, 0) %>% mean())
   }
}
################################################################


################################################### Classifier Function for Test Observations

classify.parallel <- function(Z, X, Y,
                              T.FF, T.GG, T.FG,
                              W, S_FG){
   
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
# clusterExport(cl, ls())

##################################################################################


# iterations <- 4

n <- 20
m <- 20
ns <- 100
ms <- 100

d.seq <- c(5,10,25,50,100,250,500,1000)
# d.seq <- c(5,10,25)

########## 

T.mat <- list()


for(h in c(1:1)){
   
   cat("example - ",h)
   print(Sys.time())
   
   # file.wrong <- paste0(path.wrong.results,files.wrong[h]) %>% import_list()
   # file.wrong.pop <- file.wrong %>% lapply(function(sheet) return(sheet[ ,-c(1:3)]))
   
   res.list <- T.mat[[h]] <- list()
   
   for(k in 1:length(d.seq)){
      
      error.prop.1 <- error.prop.2 <- error.prop.3 <- c()
      
      start.time <- proc.time()
      
      ############################################## Our Classifiers
      
      d <- d.seq[k]
      
      iterations <- 10
      
      T.mat[[h]][[k]] <- matrix(NA, nrow = iterations, ncol = 3)
      
      ##### Iterations starting
      
      for(u in 1:iterations){
         
         n <- 20
         m <- 20
         ns <- 100
         ms <- 100
         
         # M <- 1000
         
         if(u %% 1 == 0) {print(u)}
         
         # if(h == 1){
         #    set.seed(u)
         #    
         #    X <- matrix(1/(1-runif((n+ns)*d, 0, 1)), nrow = n+ns, ncol = d, byrow = TRUE)
         #    Y <- matrix(1.25/(1-runif((m+ms)*d, 0, 1)), nrow = m+ms, ncol = d, byrow = TRUE)
         # }
         
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

         # if(h == 5){
         #    set.seed(u)
         #
         #    X <- matrix(rpareto((n + ns) * d, location = 1, shape = 1),
         #                nrow = n + ns,
         #                ncol = d,
         #                byrow = TRUE)
         #
         #    Y <- matrix(rpareto((m + ms) * d, location = 1.25, shape = 1),
         #                nrow = m + ms,
         #                ncol = d,
         #                byrow = TRUE)
         # }

         if(h == 5){
            set.seed(u)

            X <- matrix(rlnorm((n + ns) * d, meanlog = 1, sdlog = 1),
                        nrow = n + ns,
                        ncol = d,
                        byrow = TRUE)

            Y <- matrix(rlnorm((m + ms) * d, meanlog = 1.25, sdlog = 1),
                        nrow = m + ms,
                        ncol = d,
                        byrow = TRUE)
         }
         
         Z <- rbind(X[(n+1):(n+ns),], Y[(m+1):(m+ms),])     ## Test Observations
         
         X <- X[1:n,]
         Y <- Y[1:m,]
         Q <- rbind(X,Y)
         
         clusterEvalQ(cl, {library(magrittr)})
         clusterExport(cl, c('X','Y','Q','n','m','rho'))
         
         
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
         
         T.mat[[h]][[k]][u,] <- c(T.FF, T.FG, T.GG)
         
         ########## W
         W0_FG <- 2 * T.FG - T.FF - T.GG
         W1_FG <- W0_FG^2 / 2 + (T.FF - T.GG)^2 / 2
         W2_FG <- W0_FG / 2 + abs(T.FF - T.GG) / 2
         
         W <- list(W0_FG, W1_FG, W2_FG)
         S_FG <- T.FF - T.GG
         
         
         ########## Classifcation of Test Observations
         
         ground.label <- c(rep(1,ns), rep(2,ms))
         clusterExport(cl, c('Z'))
         
         ##################################################################### Classification
         prac.label <- classify.parallel(Z, X, Y, 
                                         T.FF, T.GG, T.FG,
                                         W, S_FG)
         #####################################################################
         
         error.prop.1[u] <- length(which(ground.label != prac.label[[1]])) / (ns + ms)
         error.prop.2[u] <- length(which(ground.label != prac.label[[2]])) / (ns + ms)
         error.prop.3[u] <- length(which(ground.label != prac.label[[3]])) / (ns + ms)
      }
      
      res.list[[k]] <- cbind(c(error.prop.1, NA, mean(error.prop.1), 
                               sciplot::se(error.prop.1)),
                             c(error.prop.2, NA, mean(error.prop.2), 
                               sciplot::se(error.prop.2)),
                             c(error.prop.3, NA, mean(error.prop.3), 
                               sciplot::se(error.prop.3))) %>% as.data.frame()
                             #file.wrong.pop[[k]]) 
                             
      
      # colnames(res.list[[k]]) <- c('del.1','del.2','del.3',
      #                              'BYS',
      #                              'GLMNET',
      #                              'RF1','RF2','RF3','RF4',
      #                              'NNRAND',
      #                              'SVMLIN','SVMRBF',
      #                              'NN-lg-1','NN-lg-3','NN-lg-5','NN-lg-10',
      #                              'NN-R-1','NN-R-3','NN-R-5','NN-R-10',
      #                              'ONN')
      
      colnames(res.list[[k]]) <- c('del.1','del.2','del.3')
      
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
   
   # res.list <- list("d=5" = res.list[[1]],
   #                  "d=10" = res.list[[2]],
   #                  "d=25" = res.list[[3]])
   
   writexl::write_xlsx(res.list,
                       path = paste0(path.newest.results,"res-", h, ".xlsx"))
   
   # rio::export(T.mat[[h]], paste0("E:/JRC-2022/Classification-Summer-2022-JRC/Results/Simulated/NEWEST/MORE FRESH/T-",h,".xlsx"))
   
   rio::export(T.mat[[h]], paste0("C:/Users/JYOTISHKA/T-",h,".xlsx"))
   
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
