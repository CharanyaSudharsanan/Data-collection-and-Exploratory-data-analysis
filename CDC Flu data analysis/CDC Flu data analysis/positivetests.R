---
title: "Influenza Positive Tests Reported to CDC by U.S "
date: "3/9/2018"
output: html_document
author : Charanya Sudharsanan, 
         Prachi Shah
---
```{r}
library(ggplot2)
library(reshape2)
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)


pt <- read.csv('positivetests.csv', header = T)
meltdata = melt(pt,id.vars= c("Week","TotalTested"),measure.vars=c("A.Subtyping.not.performed.","A.H3N2v.","A..H1N1.pdm09","A.H3.","B","BVIC","BYAM"))
plot1 <- ggplot(meltdata, aes(x=as.factor(Week), y=value, fill=variable)) + theme_bw() + geom_bar(position="stack", stat="identity", colour = "black", size = 0.5) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot1 <- plot1 + ggtitle("Influenza Positive Tests Reported to CDC by U.S. \nPublic Health Laboratories 2017-2018 Season") +theme(plot.title = element_text(hjust = 0.5))+
  xlab("Week") + ylab("Number of Positive Specimens")
plot1 <- plot1 + theme(legend.title = element_blank())

plot1 <- plot1 + scale_fill_manual(values=c("yellow", "purple", "orange","red","darkgreen","green1","green2"), 
                    breaks=c("A.Subtyping.not.performed.","A..H1N1.pdm09","A.H3N2v.","A.H3.","B","BVIC","BYAM"),
                       labels=c("A(subtyping not performed)","A(H1N1)pdm09","H3N2v","A(H3N2)","B(lineage not performed)","B(Victoria Lineage)","B(Yamagata Lineage)"))


plot1 <- plot1 + scale_x_discrete(expand = c(0,0), breaks = seq(201740,201820,2)) + scale_y_continuous(expand=c(0,0), breaks = seq(0,4000,500))
plot1 + theme(axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank())
```

