---
title: "Pneumonia & Influenza Mortality"
date: "3/9/2018"
output: html_document
author : Charanya Sudharsanan, Prach Shah
---
```{r}
library("reshape2")
library("ggplot2")
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)


data <- read.csv("P&I.csv", header=T)
colnames(data) <- c("Year", "Week","P_Deaths","Expected","Threshold","AllDeaths","PneumoniaDeaths","InfluenzaDeaths")
mort_df<-subset(data,select=c('Year','Week','P_Deaths', 'Expected', 'Threshold'))
mort_df$combined_x=paste(mort_df$Year,mort_df$Week,sep=" ")
melt_df <- melt(mort_df[,c('combined_x','P_Deaths','Expected','Threshold')],id.vars = "combined_x")
options(repr.plot.width=12, repr.plot.height=5)
ggplot(mort_df,aes(x=Week))+geom_line(aes(y=P_Deaths,group=1,colour="Red"),size=0.9)+theme_classic()+
geom_line(aes(y=Expected,group=1,colour="Black"),size=0.8)+
geom_line(data=mort_df[!is.na(mort_df$Expected),],aes(y=Expected,group=1,colour="Black"))+xlab(label = "MMWR Week")+ylab(label = "% of All Deaths Due to P&I")+coord_cartesian(expand = TRUE)+
geom_line(aes(y=Threshold,group=1,colour="Black"),size=0.8)+  scale_colour_manual(values=c("black","red","black"))+
facet_wrap(~Year, nrow = 1,strip.position="bottom")+ theme(panel.spacing = unit(-0.9, "lines"))+ xlim(0,53)+
ggtitle("Pneumonia and Influenza Mortality from \n National Center for Health Statistics Mortality Surveillance System\nData through the week ending January 13,2018,as of february 1, 2018")+theme(plot.title = element_text(hjust = 0.5))+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
theme(plot.title = element_text(size = 12, face = "bold"))+theme(legend.position = "none",panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 
```


