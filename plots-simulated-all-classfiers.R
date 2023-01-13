
library(rio)
library(dplyr)
library(ggplot2)

path.our <- "C:/Users/JYOTISHKA/Desktop/delta0-TwoClass-simulated/"
# path.our <- "G:/Projects/Prof. Subhajit Dutta (Summer, 2022)/Results/All-Latest-Results/Simulated/"

files.path.our <- list.files(path.our)[1:5]

path.pop <- "C:/Users/JYOTISHKA/Desktop/all-classifiers-TwoClass-simulated-new/"
files.path.pop <- list.files(path.pop)

examples <- c("Cauchy(0,1) vs Cauchy(0,2)",
              "Cauchy(0,1) vs Cauchy(0,2)",
              expression(paste("Normal(0,3) vs ",t[3])),
              "Normal(1,1) vs Normal(1,2)",
              "Lognormal(1,1) vs Lognormal(1.25,1)")

plt <- df.summary.error <- df.summary.se <- list()

for(h in 1:5){
   
   df.list.our <- rio::import_list(paste0(path.our, files.path.our[h]))
   df.list.pop <- rio::import_list(paste0(path.pop, files.path.pop[h]))
   
   iterations <- 100
   
   df.summary.error[[h]] <-
         df.summary.se[[h]] <- data.frame("d" = c(5,10,25,50,100,250,500,1000),
                                          "del.0" = rep(NA,8),
                                          # "del.1" = rep(NA,8),
                                          # "del.2" = rep(NA,8),
                                          # "del.3" = rep(NA,8),
                                          "BYS" = rep(NA,8),
                                          "GLMNET" = rep(NA,8),
                                          "NNRAND" = rep(NA,8),
                                          "SVMLIN" = rep(NA,8),
                                          "SVMRBF" = rep(NA,8),
                                          "NNet" = rep(NA,8),
                                          "ONN" = rep(NA,8))
   
   for(i in 1:8){
      del.errors <- df.list.our[[i]][iterations + 2, ] %>% as.numeric()
      pop.errors <- df.list.pop[[i]][iterations + 2, c(4,5,10:21)] %>% as.numeric()
      
      del.se <- df.list.our[[i]][iterations + 3, ] %>% as.numeric()
      pop.se <- df.list.pop[[i]][iterations + 3, c(4,5,10:21)] %>% as.numeric()
      
      df.summary.error[[h]][i, -1] <- c(del.errors, pop.errors[1:5], 
                                        min(pop.errors[6:13]), pop.errors[14])
      df.summary.se[[h]][i, -1] <- c(del.se, pop.se[1:5], 
                                        min(pop.se[6:13]), pop.se[14])
      
      df.summary.error[[h]]$d <- df.summary.se[[h]]$d <- 1:8
   }
   
   ## Ex-1: C(0,1)-vs-C(0,2)
   ## Ex-2: C(0,1)-vs-C(1,1)
   ## Ex-3: N(0,3)-vs-t(3)
   ## Ex-4: N(1,1)-vs-N(1,2)
   ## Ex-5: lgn(1,1)-vs-lgn(1.25,1)
   
   if(TRUE){
      plt[[h]] <- df.summary.error[[h]] %>%
                     reshape2::melt(id = "d") %>% 
                     cbind("se" = df.summary.se[[h]] %>% 
                                    reshape2::melt(id = "d") %>% 
                                    select(value) %>% 
                                    unlist() %>% 
                                    as.numeric()) %>%
                     ggplot(aes(x = d)) +
                     geom_line(aes(y = value, color = variable), linewidth = 0.9) +
                     geom_errorbar(aes(ymin = value - se, ymax = value + se), width=0.08,
                                 position = position_dodge(0.05)) + 
                     scale_x_discrete(name = "Dimension",
                                      limits = c("5","10","25","50","100",
                                                 "250","500","1000")) +
                     scale_color_discrete(labels = c(eval(rlang::parse_exprs("delta[0]")),
                                                     'Bayes',
                                                     'GLMNET','NN-RAND',
                                                     'SVM-Linear','SVM-RBF',
                                                     'Neural Net','1-NN')) +
                     ggtitle(examples[h]) +
                     theme_light() +
                     theme(legend.title = element_blank(),
                           plot.title = element_text(face = "bold", hjust = 0.5))
   }
   
   print(h)
}


ggpubr::ggarrange(plt[[4]] + ylab("Misclassification Rate"),
                  plt[[3]] + ylab(""), 
                  ncol = 2, 
                  common.legend = TRUE, legend = "bottom")







