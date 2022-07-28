

path <- "C:/Users/JYOTISHKA/Desktop/"

path.simulated <- paste0(path, "all-classifiers-TwoClass-simulated-new/")
path.UCR <- paste0(path, "UCR-TwoClass-Results/")
path.CompCancer <- paste0(path, "CompCancer-TwoClass-Results/")

results.simulated <- list.files(path.simulated)
results.UCR <- list.files(path.UCR)
results.CompCancer <- list.files(path.CompCancer)


A <- sapply(1:length(results.UCR), function(val){
  
  temp.file <- read_excel(paste0(path.UCR, results.UCR[val]))
  return(temp.file[c(52,53),])
})



