setwd("C://Apps//Rshiny//data")
data<-read.csv("final2.csv")

library(sqldf)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(dplyr)
library(magrittr)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(BSDA)
#Admission入學方式 Ad_star繁星，Ad_apply學測，Ad_exam指考
windowsFonts(BL = windowsFont("微軟正黑體"))
Ad_star<-data %>%
  filter(入學管道=='繁星推薦一般考生'|入學管道=='繁星推薦原住民')
Ad_apply <-sqldf("select * from data where 入學管道碼 like 'B2%'")
Ad_exam <- data %>% 
  filter(入學管道碼=='B1D'|入學管道碼=='B1P')

piechartmaker <- function(x,y){
  datalist<-list()
  for (i in (1:5)){
    if(i<5){
      datalist[i]<-x %>% 
        filter(score<110-i*10&score>=100-i*10)%>% 
        summarise(n=n())
    }
    else{
      datalist[i]<-x%>% 
        filter(score<60)%>% 
        summarise(n=n())
    }
  }
  datalist<- unlist(datalist)
  names <- c("100~90","89~80","79~70","69~60","60以下")
  datalist<- round(datalist/sum(datalist)*100)
  df<-data.frame(value=datalist,group=names)
  df2 <- df %>% 
    mutate(csum = rev(cumsum(rev(value))), 
           pos = value/2 + lead(csum, 1),
           pos = if_else(is.na(pos), value/2, pos))
    g<- ggplot(df, aes(x = "" , y = value, fill = fct_inorder(group))) +
      geom_col(width = 1, color = 1) +
      coord_polar(theta = "y") +
      scale_fill_brewer(palette = "Pastel1") +
      geom_label_repel(data = df2,
                       aes(y = pos, label = paste0(value, "%")),
                       size = 2.5, nudge_x = 1, show.legend = FALSE) +
      guides(fill = guide_legend(title = "分數",nrow = 5, title.position = "top")) +
      theme_void()+theme(legend.position = "bottom",plot.title = element_text(size=15, face="bold",hjust=0.5,vjust=3,family = "BL"),legend.key.size = unit(3, 'mm'), legend.title = element_text(size=12,family = "BL"),legend.text = element_text(size=8,family = "BL"))+
      labs(title=y)
  return(g)
}

g1 <- piechartmaker(Ad_star,"繁星推薦")
g2 <- piechartmaker(Ad_apply,"申請入學")
g3 <- piechartmaker(Ad_exam,"指考")
jpeg(file= "全校學生成績分布.jpeg", width = 756, height = 600,res=200)
grid.arrange(g1,g2,g3 ,nrow=1,ncol=3)
dev.off()

