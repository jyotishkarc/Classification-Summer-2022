

library(doParallel)
library(rio)
library(dplyr)
library(readxl)
library(writexl)
library(EnvStats)

no.cores <- round(detectCores() * 0.75)
cl <- makeCluster(spec = no.cores, type = 'PSOCK')
registerDoParallel(cl)

# path.wrong.results <- "C:/Users/JYOTISHKA/Desktop/all-classifiers-TwoClass-simulated-old/"
# path.new.results <- "C:\\Users\\JYOTISHKA\\Desktop\\all-classifiers-TwoClass-simulated-new\\"

path.wrong.results <- "E:/JRC-2022/Classification-Summer-2022-JRC/Results/Simulated/"
path.new.results <- "E:\\JRC-2022\\Classification-Summer-2022-JRC\\Results\\Simulated\\NEW\\"

files.wrong <- list.files(path.wrong.results)

rho <- function(a,b,c){
   if (prod(a == c)== 1 || prod(b == c) == 1){
      return(0)
   }else{
      temp <- sign((a-c) * (b-c)) * (-1)
      return(temp %>% replace(temp == -1, 0) %>% mean())
   }
}

rho.bar.hat <- function(U,V,X,Y){
   if(U == V){
      return(0)
   }else{
      n <- nrow(X)
      m <- nrow(Y)
      d <- ncol(X)
      
      S <- rep(0,d)
      
      for(k in 1:d){
         S[k] <- c(sapply(1:n, function(val){rho(U[k],V[k],X[val,k])}),
                   sapply(1:m, function(val){rho(U[k],V[k],Y[val,k])}))
      }
      
      return(sum(S) / (d * (n+m)))
   }
}


classify.parallel <- function(Z, X, Y, T.FF, T.GG, T.FG, W, S_FG){
   # print("Classification starting")
   
   R <- nrow(Z)
   n <- nrow(X)
   m <- nrow(Y)
   
   # clusterExport(cl, c('R','n','m'), envir = environment())
   
   T_FZ <- T_GZ <- rep(0,R)
   
   for(r in 1:R){
      S_1 <- S_2 <- 0
      
      for(i in 1:n){
         S_1 <- S_1 + rho.bar.hat(X[i,],Z[r,],X,Y)
      }
      
      for(j in 1:m){
         S_2 <- S_2 + rho.bar.hat(Y[j,],Z[r,],X,Y)
      }
      
      T_FZ[r] <- S_1 / n
      T_GZ[r] <- S_2 / m
   }
   
   
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

# iterations <- 100

n <- 20
m <- 20
ns <- 100
ms <- 100

d.seq <- c(5,10,25,50,100,250,500,1000)


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
            
            X <- matrix(rpareto((n + ns) * d, location =  1, shape = 1),
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
         # Q <- rbind(X,Y)
         
         clusterExport(cl, c('X','Y','n','m'))
         
         T.FG <- T.FF <- T.GG <- 0
         
         #####
         # index.mat <- cbind(rep(1:n, each = m),rep(1:m, times = n))
         
         for(i in 1:n){
            for(j in 1:m){
               T.FG <- T.FG + rho.bar.hat(X[i,],Y[j,],X,Y)
            }
         }
         
         T.FG <- T.FG / (n*m)
         
         #####
         # index.mat <- cbind(rep(1:n, each = n),rep(1:n, times = n))
         
         for(i in 1:n){
            for(j in 1:n){
               T.FF <- T.FF + rho.bar.hat(X[i,],X[j,],X,Y)
            }
         }
         
         T.FF <- T.FF / (n*(n-1))
         
         #####
         # index.mat <- cbind(rep(1:m, each = m),rep(1:m, times = m))
         
         for(i in 1:m){
            for(j in 1:m){
               T.GG <- T.GG + rho.bar.hat(Y[i,],Y[j,],X,Y)
            }
         }
         
         T.GG <- T.GG / (m*(m-1))
         
         
         ########## W
         W0_FG <- 2 * T.FG - T.FF - T.GG
         W1_FG <- W0_FG^2 / 2 + (T.FF - T.GG)^2 / 2
         W2_FG <- W0_FG / 2 + abs(T.FF - T.GG) / 2
         
         W <- list(W0_FG, W1_FG, W2_FG)
         S_FG <- T.FF - T.GG
         
         
         ########## Test Observations
         
         ground.label <- c(rep(1,ns), rep(2,ms))
         
         clusterExport(cl, c('Z'))
         
         prac.label <- classify.parallel(Z, X, Y, T.FF, T.GG, T.FG, W, S_FG)
         
         error.prop.1[u] <- length(which(ground.label != prac.label[[1]])) / (ns + ms)
         error.prop.2[u] <- length(which(ground.label != prac.label[[2]])) / (ns + ms)
         error.prop.3[u] <- length(which(ground.label != prac.label[[3]])) / (ns + ms)
      }
      
      res.list[[k]] <- cbind(c(error.prop.1, NA, mean(error.prop.1), sciplot::se(error.prop.1)),
                             c(error.prop.2, NA, mean(error.prop.2), sciplot::se(error.prop.2)),
                             c(error.prop.3, NA, mean(error.prop.3), sciplot::se(error.prop.3)),
                             file.wrong.pop[[k]]) %>% as.data.frame()
      
      colnames(res.list[[k]]) <- c('del.1','del.2','del.3',
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
                       path = paste0(path.new.results,files.wrong[h]))
   
   print(files.wrong[h])
   cat("\n\n")
   
}
