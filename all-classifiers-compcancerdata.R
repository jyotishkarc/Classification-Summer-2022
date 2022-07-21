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
X[[2]]=chen.2002_database
X[[3]]=chowdary.2006_database
X[[4]]=golub.1999.v1_database
X[[5]]=gordon.2002_database
X[[6]]=laiho.2007_database
X[[7]]=nutt.2003.v2_database
X[[8]]=nutt.2003.v2_database
X[[9]]=pomeroy.2002.v1_database
X[[10]]=shipp.2002.v1_database
X[[11]]=singh.2002_database
X[[12]]=west.2001_database
X[[13]]=yeoh.2002.v1_database

filenames=gsub('txt','xlsx',filenames)
for(i in 1:13){
   X[[i]]=t(as.matrix(X[[i]]))[-1,]%>% labels.rename()
   write_xlsx(X[[i]] %>% as.data.frame(), 
              path = paste0("/Users/aytijhyasaha/Desktop/projects/Classification-Summer-2022/Compcancer-datasets/",filenames[i]))
   
}


