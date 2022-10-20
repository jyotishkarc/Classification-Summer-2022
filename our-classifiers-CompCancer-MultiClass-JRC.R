
library(doParallel)
library(dplyr)
library(stringr)
library(readxl)
library(writexl)
library(rio)

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

Mode <- function(x) {
   ux <- unique(x)
   ux[which.max(tabulate(match(x, ux)))]
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
# clusterExport(cl, ls())

#################

print("Hello")

iterations <- 100

path <- "G:/Datasets/CompCancer Database/"
# path <- "E:/JRC-2022/Classification-Summer-2022-JRC/Datasets/UCR/"

CC.stats <- read_excel(paste0(path,"CompCancer-DataSummary.xlsx"))
CC.implement <- CC.stats %>% arrange(J)

path.CC <- paste0(path,"Database/")
files.CC.all <- list.files(path.CC)

files.CC <- union(paste0(CC.implement$Dataset,"_database.xlsx"), files.CC.all)

time.CC <- rep(0, length(files.CC))

# for(h in 35:length(files.CC)){
# for(h in c(12,30,33)){
for(h in c(35)){
   
   print(h)
   print(files.CC[h])
   print(Sys.time())
   
   start.time.sys <- Sys.time()
   start.time.proc <- proc.time()[3]
   
   ########## Reading the datasets
   
   dataset <- paste0(path.CC, files.CC[h]) %>% 
                  read_excel() %>% 
                  na.omit() %>%
                  labels.rename() %>%
                  as.data.frame() %>%
                  arrange(V1) %>%
                  as.matrix() %>%
                  apply(c(1,2), function(val) as.numeric(val))
   
   J <- dataset[,1] %>% unique() %>% length()
   
   ranges <- train.index <- test.index <- prac.label <- res.mat <- list()
   
   for(j in 1:J){
      ranges[[j]] <- which(dataset[,1] == j)
   }
   
   T.mat <- list()
   
   print("Dataset extracted")
   
   N <- nrow(dataset)
   d <- ncol(dataset) - 1
   
   classes.mat <- cbind(rep(1:J, each = J), 
                        rep(1:J, times = J)) %>% as.data.frame() %>% filter(V1 < V2)
   
   for(ind in 1:nrow(classes.mat)){
      T.mat[[ind]] <- matrix(0, nrow = iterations, ncol = 3)
   }
   
   for(u in 1:iterations){
      
      train.index[[u]] <- test.index[[u]] <- list()
      
      for(j in 1:J){
         train.index[[u]][[j]] <- sample(ranges[[j]], ceiling(length(ranges[[j]])/2))
         test.index[[u]][[j]] <- setdiff(ranges[[j]], train.index[[u]][[j]])
      }
   }
   
   ground.truth <- dataset[,1] %>% unlist() %>% as.numeric()
   
   error.prop <- matrix(0, nrow = iterations, ncol = 3)
   
   ############ Independent iterations starting
   
   for(u in 1:iterations){
      
      if(u %% 1 == 0){
         print(u)
         print(Sys.time())
         
         if(u %% 20 == 0) print(h)
      }
      
      res.list <- list()
      
      ############################################## Our Classifiers
      
      classes.mat <- classes.mat %>% as.matrix()
      
      for(j in 1:J){
         res.mat[[j]] <- list()
         
         res.mat[[j]][[1]] <- NA
         res.mat[[j]][[2]] <- NA
         res.mat[[j]][[3]] <- NA
      }
      
      ########## Lopping over the Jc2 many pairs of classes
      
      for(ind in 1:nrow(classes.mat)){
         
         cat(classes.mat[ind,1],"vs",classes.mat[ind,2],"\n")
         start.time <- proc.time()[3]
         
         A <- classes.mat[ind, 1] 
         B <- classes.mat[ind, 2]
         
         class.A <- which(ground.truth == A)
         class.B <- which(ground.truth == B)
         
         n <- round(length(class.A) * 0.5)
         m <- round(length(class.B) * 0.5)
         
         X <- dataset[train.index[[u]][[A]], -1]
         Y <- dataset[train.index[[u]][[B]], -1]
         Q <- rbind(X,Y)
         
         Z <- dataset[c(test.index[[u]][[A]],
                        test.index[[u]][[B]]), -1]    ## Test Obs.
         
         n <- nrow(X)
         m <- nrow(Y)
         
         clusterExport(cl, c('X','Y','Q','n','m','rho'))
         
         
         ################################# T.FG
         
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
         
         
         ################################# T.FF
         
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
         
         
         ################################# T.GG
         
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
         
         
         ########## T.matrix
         
         T.mat[[ind]][u, ] <- c(T.FF, T.FG, T.GG)
         
         
         ########## W
         W0_FG <- 2 * T.FG - T.FF - T.GG
         W1_FG <- W0_FG^2 / 2 + (T.FF - T.GG)^2 / 2
         W2_FG <- W0_FG / 2 + abs(T.FF - T.GG) / 2
         
         W <- list(W0_FG, W1_FG, W2_FG)
         S_FG <- T.FF - T.GG
         
         
         ########## Test Observations
         
         clusterExport(cl, c('Z'))
         
         ############################################################### Classification
         
         prac.label <- classify.parallel(Z, X, Y, 
                                         T.FF, T.GG, T.FG,
                                         W, S_FG,
                                         A, B)
         
         ###############################################################
         
         res.mat[[A]][[1]] <- res.mat[[A]][[1]] %>% 
            rbind(prac.label[[1]][1:length(test.index[[u]][[A]])]) %>% na.omit()
         res.mat[[A]][[2]] <- res.mat[[A]][[2]] %>% 
            rbind(prac.label[[2]][1:length(test.index[[u]][[A]])]) %>% na.omit()
         res.mat[[A]][[3]] <- res.mat[[A]][[3]] %>% 
            rbind(prac.label[[3]][1:length(test.index[[u]][[A]])]) %>% na.omit()
         
         res.mat[[B]][[1]] <- res.mat[[B]][[1]] %>% 
            rbind(prac.label[[1]][(length(test.index[[u]][[A]]) + 1) :
               (length(test.index[[u]][[A]])+length(test.index[[u]][[B]]))]) %>% na.omit()
         res.mat[[B]][[2]] <- res.mat[[B]][[2]] %>% 
            rbind(prac.label[[2]][(length(test.index[[u]][[A]]) + 1) :
               (length(test.index[[u]][[A]])+length(test.index[[u]][[B]]))]) %>% na.omit()
         res.mat[[B]][[3]] <- res.mat[[B]][[3]] %>% 
            rbind(prac.label[[3]][(length(test.index[[u]][[A]]) + 1) :
               (length(test.index[[u]][[A]])+length(test.index[[u]][[B]]))]) %>% na.omit()
      }
      
      
      for(g in 1:3){
         error.prop[u,g] <- sapply(1:J, function(j){
            sum(apply(res.mat[[j]][[g]], 2, Mode) != j)
         }) %>% sum() / length(unlist(test.index[[u]]))
      }
   }
   
   
   print(Sys.time())
   
   ##############################################
   
   result.all <- error.prop
   
   colnames(result.all) <- c('del.1','del.2','del.3')
   rownames(result.all) <- 1:iterations
   
   res.list <- rbind(result.all, rep(NA, ncol(result.all)), 
                     apply(result.all, 2, mean), 
                     apply(result.all, 2, sciplot::se)) %>% as.data.frame()
   
   result.folder.path <- "G:\\Projects\\Prof. Subhajit Dutta (Summer, 2022)\\Results\\CompCancer-ALL-Results-newest\\"
   
   # result.folder.path <- "E:\\JRC-2022\\Classification-Summer-2022-JRC\\Results\\Real\\UCR\\UCR-ALL-Results-newest\\"
   
   write_xlsx(x = res.list,
              path = paste0(result.folder.path, 
                            str_remove(files.CC[h],"_database.xlsx"),
                            "-our.xlsx"))
   
   
   T.mat.folder.path <- paste0(result.folder.path,"CompCancer-ALL-T-matrix\\")
   
   for(ind in 1:nrow(classes.mat)){
      colnames(T.mat[[ind]]) <- c("T_FF","T_FG","T_GG")
      names(T.mat)[[ind]] <- paste0(classes.mat[ind,1]," vs ",classes.mat[ind,2])
   }
   
   T.mat.filename <- paste0(T.mat.folder.path, 
                            str_remove(files.CC[h],"_database.xlsx"),
                            "-T-matrix.xlsx")
   
   export(T.mat, T.mat.filename)
   
   end.time.proc <- proc.time()[3]
   time.CC[h] <- end.time.proc - start.time.proc
   print(time.CC[h])
   cat("Ended:")
   print(Sys.time())
   print(files.CC[h])
   cat("\n\n")
}


# 16,20,21,27,29,12,30,33,35

stopCluster(cl)
gc()





