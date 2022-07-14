
library(doParallel)
library(magrittr)

start.time <- proc.time()

no.cores <- round(detectCores()*0.70)
cl <- makeCluster(spec = no.cores, type = 'PSOCK')
registerDoParallel(cl)

rho.0 <- function(a,b,c){
   if (prod(a == c)== 1 || prod(b == c) == 1){
      return(0)
   }else{
      return(acos(sum((a-c)*(b-c)) / sqrt(sum((a-c)^2) * sum((b-c)^2))) / pi)
   }
}

rho.hat <- function(){
   ## There's nothing here
}


error.prop <- c()


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
                                    rho.0(Z[i,],X[j,],Q[val,])
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
                                    rho.0(Z[i,],Y[j,],Q[val,])
      })))
   }
   
   index.mat <- cbind(rep(1:R, each = m),rep(1:m, times = R))
   
   T_GZ <- index.mat %>%
               parApply(cl, ., 1, T_FZ.rho.fun) %>%
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


clusterExport(cl, ls())

# t1 <- proc.time()
error.prop.1 <- error.prop.2 <- error.prop.3 <- c()

for(u in 1:5){
   n <- 20
   m <- 20
   ns <- 100
   ms <- 100
   
   d <- 500
   
   X <- matrix(rcauchy((n+ns)*d, 2, 1), nrow = n+ns, ncol = d, byrow = TRUE)
   Y <- matrix(rcauchy((m+ms)*d, 3, 1), nrow = m+ms, ncol = d, byrow = TRUE)
   
   Z <- rbind(X[(n+1):(n+ns),], Y[(m+1):(m+ms),])     ## Test Observations
   
   X <- X[1:n,]
   Y <- Y[1:m,]
   Q <- rbind(X,Y)
   
   if (u %% 1 == 0) {print(u)}
   
   ##### A_XY
   
   T.FG.rho.fun <- function(vec){
      i <- vec[1]
      j <- vec[2]
      
      return(sum(sapply(1:(n+m),function(val){
         rho.0(X[i,],Y[j,],Q[val,])
      })))
   }
   
   
   clusterExport(cl, c('X','Y','Q','n','m'))
   
   
   index.mat <- cbind(rep(1:n, each = m),rep(1:m, times = n))
   
   # a <- matrix(parApply(cl,indx.mat,1,T.FG.rho.fun), n, m, byrow = T)/((n+m-2)*n*m)
   # T.FG = sum(a)
   
   T.FG <- index.mat %>% 
               parApply(cl, ., 1, T.FG.rho.fun) %>%
               matrix(nrow = n, ncol = m, byrow = T) %>% sum() / ((n+m-2)*n*m)
   
   
   ##### A_XX
   
   T.FF.rho.fun <- function(vec){
      i <- vec[1]
      j <- vec[2]
      
      return(sum(sapply(1:(n+m),function(val){
                                    rho.0(X[i,],X[j,],Q[val,])
      })))
   }
   
   index.mat <- cbind(rep(1:n, each = n),rep(1:n, times = n))
   # T.FF <- sum(parApply(cl,indx.mat,1,T.FF.rho.fun))/((n+m-2)*n*(n-1))
   
   T.FF <- index.mat %>% 
               parApply(cl, ., 1, T.FF.rho.fun) %>% sum() / ((n+m-2)*n*(n-1))
   
   ##### A_YY
   
   T.GG.rho.fun <- function(vec){
      i <- vec[1]
      j <- vec[2]
      
      return(sum(sapply(1:(n+m),function(val){
                                    rho.0(Y[i,],Y[j,],Q[val,])
      })))
   }
   
   index.mat <- cbind(rep(1:m, each = m),rep(1:m, times = m))
   # T.GG <- sum(parApply(cl,indx.mat,1,T.GG.rho.fun))/((n+m-2)*m*(m-1))
   
   T.GG <- index.mat %>% 
               parApply(cl, ., 1, T.GG.rho.fun) %>% sum() / ((n+m-2)*m*(m-1))
   
   
   ##### L
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
   
   # print((proc.time() - t1)/u) #avgtime required per iteration
}

error.prop.mean <- list(mean(error.prop.1), mean(error.prop.2), mean(error.prop.3))
error.prop.sd <- list(sd(error.prop.1), sd(error.prop.2), sd(error.prop.3))

exec.time <- proc.time() - start.time

print("Cauchy")
print(exec.time)
print(error.prop.mean)

stopCluster(cl)
gc()
