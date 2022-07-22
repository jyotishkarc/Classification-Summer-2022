

library(trendecon)
library(dplyr)
library(reshape2)


proc_keyword_init("russia ukraine war", "")
proc_keyword_init("russia and ukraine war", "")
proc_keyword_init("russia and ukraine", "")
proc_keyword_init("russia war in ukraine", "")



proc_index(c("russia ukraine war", "russia and ukraine war", 
             "russia and ukraine", "russia war in ukraine"), "", "xyz")

