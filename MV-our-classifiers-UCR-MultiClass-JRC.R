
library(doParallel)
library(dplyr)
library(stringr)
library(readxl)
library(writexl)
library(rio)

# start.time <- proc.time()

no.cores <- round(detectCores()*0.60)
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

iterations <- 10

path <- "G:/Datasets/UCR Database/"
# path <- "E:/JRC-2022/Classification-Summer-2022-JRC/Datasets/UCR/"

UCR.stats <- read.csv(paste0(path,"UCR-DataSummary.csv"), stringsAsFactors = FALSE)

omitted.ID <- c(1,7:9,12:14,16:18,21,23,24,26,27,29,30,33,37,42,44,49,50,
                53:55,58,62,66,68,75:80,82,85:89,92,96,97,99:104,113:120,126)

# UCR.implement <- UCR.stats[-omitted.ID, ]
UCR.implement <- UCR.stats[!(UCR.stats$ID %in% omitted.ID), ] %>% arrange(Class)

path.UCR <- paste0(path,"UCRArchive_2018/")
files.UCR.all <- list.files(path.UCR)
files.UCR <- intersect(UCR.implement$Name, files.UCR.all)

time.UCR <- rep(0, length(files.UCR))

for(h in 16:length(files.UCR)){
   # for(h in c(12,30,33)){
   # for(h in c(15,16)){
   
   print(h)
   print(files.UCR[h])
   print(Sys.time())
   
   start.time.sys <- Sys.time()
   start.time.proc <- proc.time()[3]
   
   ########## Reading the datasets
   
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
   
   prac.label.MV <- list()
   error.prop <- matrix(0, nrow = iterations, ncol = 3)
   
   ############ Independent iterations starting
   
   for(u in 1:iterations){
      
      if(u %% 1 == 0){
         print(u)
         print(Sys.time())
         
         if(u %% 20 == 0) print(h)
      }
      
      prac.label.MV[[u]] <- list()
      
      ############################################## Our Classifiers
      
      classes.mat <- classes.mat %>% as.matrix()
      
      
      ########## Test Observations
      
      Z.gt <- dataset[unlist(test.index[[u]]), 1]
      Z <- dataset[unlist(test.index[[u]]), -1]
      
      res.mat <- list()
      
      for(j in 1:3){
         res.mat[[j]] <- matrix(NA, nrow = nrow(classes.mat), ncol = length(Z.gt))
      }
      
      
      ########## Looping over the Jc2 many pairs of classes
      
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
         
         res.mat[[1]][ind, ] <- prac.label[[1]] %>% as.numeric()
         res.mat[[2]][ind, ] <- prac.label[[2]] %>% as.numeric()
         res.mat[[3]][ind, ] <- prac.label[[3]] %>% as.numeric()
      }
      
      prac.label.MV[[u]][[1]] <- res.mat[[1]] %>% apply(2, Mode) %>% as.numeric()
      prac.label.MV[[u]][[2]] <- res.mat[[2]] %>% apply(2, Mode) %>% as.numeric()
      prac.label.MV[[u]][[3]] <- res.mat[[3]] %>% apply(2, Mode) %>% as.numeric()
      
      for(g in 1:3){
         error.prop[u,g] <- length(which(prac.label.MV[[u]][[g]] != Z.gt)) / length(Z.gt)
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
   
   rownames(res.list) <- 1:(iterations+3)
   
   # result.folder.path <- "G:\\Projects\\Prof. Subhajit Dutta (Summer, 2022)\\Results\\UCR-ALL-Results-MV\\"
   
   result.folder.path <- "E:\\JRC-2022\\Classification-Summer-2022-JRC\\Results\\Real\\UCR\\UCR-ALL-Results-MV\\"
   
   write_xlsx(x = res.list,
              path = paste0(result.folder.path, 
                            str_remove(files.UCR[h],"_database.xlsx"),
                            "-our-MV.xlsx"))
   
   
   T.mat.folder.path <- paste0(result.folder.path,"UCR-ALL-T-matrix\\")
   
   for(ind in 1:nrow(classes.mat)){
      colnames(T.mat[[ind]]) <- c("T_FF","T_FG","T_GG")
      names(T.mat)[[ind]] <- paste0(classes.mat[ind,1]," vs ",classes.mat[ind,2])
   }
   
   T.mat.filename <- paste0(T.mat.folder.path, 
                            str_remove(files.UCR[h],"_database.xlsx"),
                            "-T-matrix.xlsx")
   
   export(T.mat, T.mat.filename)
   
   end.time.proc <- proc.time()[3]
   time.UCR[h] <- end.time.proc - start.time.proc
   print(time.UCR[h])
   cat("Ended:")
   print(Sys.time())
   print(files.UCR[h])
   cat("\n\n")
   
   gc()
}


# 16,20,21,27,29,12,30,33,35

stopCluster(cl)
gc()





