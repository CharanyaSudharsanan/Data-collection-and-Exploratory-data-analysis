---
title: "Pediatric Deaths"
date: "3/9/2018"
output: html_document
author : Charanya Sudharsanan,Prach Shah
---
```{r}
library(reshape2)
library(ggplot2)

dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)

pdtable <- read.csv("pddeath.csv", header=T)
pd_df <- melt(pdtable[,c('WEEK.NUMBER','CURRENT.WEEK.DEATHS','PREVIOUS.WEEKS.DEATHS')],id.vars =1 )
agg_df<-setNames(aggregate(cbind(pdtable$'CURRENT.WEEK.DEATHS', pdtable$'PREVIOUS.WEEKS.DEATHS') ~ pdtable$'SEASON', data=pdtable, FUN=sum), c("Season","Current_week_deaths","Previous_week_deaths"))
row_sum<-rowSums(agg_df[,c("Current_week_deaths", "Previous_week_deaths")])
options(repr.plot.width=25, repr.plot.height=10)
ggplot(data=pd_df,aes(x=WEEK.NUMBER))+geom_bar(aes(y=value,fill=variable),stat="Identity",width=0.9, colour="black",position = "stack")+ theme_minimal(base_size=6)+theme_bw()+theme(axis.text=element_text(size=3),
        axis.title=element_text(size=8,face="bold")) +theme(plot.title = element_text(size = 50, face = "bold"))+
theme(axis.text.x = element_text(angle = 90))+scale_fill_manual(values=c('skyblue','darkgreen'))+xlab('Week of Death')+
ylab('Number of deaths')+
annotate("text", x=25, y=18.5, label= paste("2014-2015, \n Number of Deaths = ", row_sum[1]), size = 2) + annotate("text", x = 70, y=18.5, label = paste("2015-2016, \n Number of Deaths = ", row_sum[2]), size = 2)+ annotate("text", x = 118, y=18.5, label = paste("2016-2017, \n Number of Deaths = ", row_sum[3]), size = 2)+ annotate("text", x = 168, y=18.5, label = paste("2017-2018, \n Number of Deaths = ", row_sum[4]), size = 2)+ggtitle('Number of Influenza-Associcated Pediatric Deaths\nby Week of Death:2014-2015 Season to Present')+theme(plot.title = element_text(hjust = 0.5))+scale_x_discrete(breaks =c( "2014-40", "2014-46", "2014-52", "2015-05", "2015-11", "2015-17", "2015-23", "2015-29", "2015-35", "2015-41", "2015-47", "2016-01", "2016-07", "2016-13", "2016-19", "2016-25", "2016-31", "2016-37", "2016-43", "2016-49", "2017-03", "2017-09", "2017-15","2017-21", "2017-27", "2017-33", "2017-39", "2017-45", "2017-51", "2018-05", "2018-11", "2018-17", "2018-23", "2018-29", "2018-35", "2014-41", "2014-47", "2014-53", "2015-06", "2015-12", "2015-18", "2015-24", "2015-30", "2015-36", "2015-42", "2015-48", "2016-02", "2016-08", "2016-14", "2016-20", "2016-26", "2016-32", "2016-38", "2016-44", "2016-50", "2017-04", "2017-10","2017-16", "2017-22", "2017-28","2017-34", "2017-40", "2017-46", "2017-52", "2018-06", "2018-12","2018-18","2018-24","2018-30","2018-36"))+
theme(plot.title = element_text(size = 15, face = "bold"))+theme(legend.title = element_blank(),legend.position = "bottom")+theme(
  panel.background = element_rect(fill = NA))
```

