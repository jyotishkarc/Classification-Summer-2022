library(magrittr)
library(writexl)
path = "/Users/aytijhyasaha/Desktop/Datasets/"
filenames=list.files(path)

labels.rename <- function(X){
   
   X <- as.matrix(X)
   
   if (length(setdiff(unique(X[,1]), 1:length(unique(X[,1])))) == 0) {
      return(X)
   }
   
   original.labels <- X[,1] %>% as.character()
   new.label.names <- 1 : length(unique(original.labels))
   
   X[,1] <- new.label.names[as.factor(original.labels)]
   
   return(X)
}

X=list()
X[[1]]=armstrong.2002.v1_database

X[[1]]=t(as.matrix(X[[1]]))[-1,]
X[[1]]=cbind(X[[1]],X[[1]][,2]) %>% labels.rename()
write_xlsx(X[[1]] %>% as.data.frame(), 
              path = paste0("/Users/aytijhyasaha/Desktop/projects/Classification-Summer-2022/","Dataset",1,".xlsx"))

