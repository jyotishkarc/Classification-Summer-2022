
library(rio)
library(dplyr)
library(ggplot2)
library(latex2exp)

path.our <- paste0(getwd(),"/Results/Simulated/delta-0/")
files.path.our <- list.files(path.our)[1:2]

path.pop <- paste0(getwd(),"/Results/Simulated/Combined-delta-1,2,3-and-Pop/")
files.path.pop <- list.files(path.pop)[1:2]

## Ex-1: N(1,1)-vs-N(1,2)
## Ex-2: N(0,3)-vs-t(3)
## Ex-3: C(0,1)-vs-C(1,1)
## Ex-4: C(0,1)-vs-C(0,2)
## Ex-7: outlier

examples <- c(1:2)

plt <- df.summary.error <- df.summary.se <- list()

for(h in 1:2){
   
   df.list.our <- rio::import_list(paste0(path.our, files.path.our[h]))
   df.list.pop <- rio::import_list(paste0(path.pop, files.path.pop[h]))
   
   iterations <- 100
   
   df.summary.error[[h]] <-
         df.summary.se[[h]] <- data.frame("d" = c(5,10,25,50,100,250,500,1000),
                                          "del.0" = rep(NA,8),
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
   
   gg_colors_0 <- c('mediumorchid',
                    'grey1', 
                    '#FFD13B','darkorange',
                    'khaki4','hotpink1',
                    'lightsalmon','lightseagreen')
                           
   gg_colors_0 <- setNames(gg_colors_0, names(df.summary.error[[h]])[-1])
   
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
                     geom_point(aes(y = value, color = variable)) +
                     # geom_errorbar(aes(ymin = value - se, ymax = value + se), width=0.08,
                                   # position = position_dodge(0.05)) +
                     scale_x_discrete(name = "Dimension (d)",
                                      limits = c("5","10","25","50","100",
                                                 "250","500","1000")) +
                     # scale_x_discrete(name = "Dimension (d)",
                     #                  limits = c("","10","25","50","",
                     #                             "250","","1000")) +
                     # scale_x_log10() +
                     scale_color_manual(labels = c(eval(rlang::parse_exprs("delta[0]")),
                                                   'Bayes',
                                                   'GLMNET','NN-RAND',
                                                   'SVM-LIN','SVM-RBF',
                                                   'N-NET','1-NN'),
                                        values = gg_colors_0) +
                     ggtitle(paste0("Example ", h)) +
                     theme_light() +
                     theme(legend.title = element_blank(),
                           legend.text = element_text(#face = "bold", 
                                                      hjust = 0.5,
                                                      size = 13),
                           legend.position = "bottom",
                           plot.title = element_text(face = "bold",
                                                     size = 14,
                                                     hjust = 0.5),
                           axis.title = element_text(size = 12, face = "bold"),
                           axis.text = element_text(size = 13)) +
                     guides(colour = guide_legend(nrow = 1))
   }
   
   print(h)
}


# ggpubr::ggarrange(plt[[4]] + ylab("Misclassification Probability"),
#                   plt[[3]] + ylab(""), 
#                   ncol = 2, 
#                   common.legend = TRUE, legend = "bottom")

cat("delta-0 vs Popular Classifiers")


ggpubr::ggarrange(plt[[1]] + 
                     ylab(TeX("$\\textbf{Misclassification~~Probability}~(\\Delta)$")),
                  plt[[2]] + ylab(""),
                  nrow = 1, ncol = 2, 
                  common.legend = TRUE, legend = "bottom")
# 
