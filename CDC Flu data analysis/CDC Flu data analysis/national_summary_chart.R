---
title: "National Summary"
date: "3/9/2018"
output: html_document
author : Charanya Sudharsanan, Prachi Shah
---
```{r}

library(ggplot2)

dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)

natsum <- read.csv("national_summary.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

options(repr.plot.width=10, repr.plot.height=20)

NSPlot <- ggplot(natsum, aes(x = as.factor(natsum$Week))) +
geom_bar(aes(y=natsum$Total_A + natsum$Total_B),stat = "identity", fill = "yellow2", color = "black")+geom_bar(aes(y=natsum$Total_B),stat = "identity", fill = "darkgreen", color = "black")+geom_line(aes(y=X..Positive*18000/30, group =1), colour= "black", size=0.8)+geom_line(aes(y=natsum$Percent.Positive.A*18000/30, group =1), colour= "yellow3", linetype = 2, size=0.5)+geom_line(aes(y=(natsum$Percent.Positive.A-0.3)*18000/30, group =1), colour= "black", linetype = 2, size=0.5, alpha="0.4")+geom_line(aes(y=natsum$Percent.Positive.B*18000/30, group =1), colour= "darkblue", linetype = "dotted", size=0.5)+geom_line(aes(y=(natsum$Percent.Positive.B-0.2)*18000/32, group =1), colour= "black", linetype = "dotted", size=0.5, alpha="0.4")+scale_colour_manual(name = "Type",values = c(yellow,green),labels = c('c2','c1'))+
labs(title="              Influenza Positive Tests Reported to CDC by U.S.\n Clinical Laboratories, National Summary, 2017-2018 Season\n\n\n", size = 1)+
xlab("Week") +
scale_fill_manual("legend", values = c(Percent.Positive.A,Percent.Positive.B))+
scale_y_continuous(expand = c(0,0),name="Number Of Positive Specimens",sec.axis = sec_axis(~ . * 30 / 18000, name = "Percent Positive", breaks=seq(0,30, by=2)), breaks=seq(0,18000, by=2000))+
scale_x_discrete(expand = c(0,0), breaks=seq(201740,201820, by=2))+
coord_cartesian(ylim = c(0, 18000))+theme_minimal()+theme(axis.line = element_line(colour = "black"),axis.text.x = element_text( color="black",angle= 45),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+theme(legend.position = "Right", legend.text = element_text(color = 'red',size = 10))
NSPlot

```

