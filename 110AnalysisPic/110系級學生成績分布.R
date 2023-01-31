library(tidyverse)
library(ggplot2)
library(ggrepel)
library(grid)
library(gridExtra)

setwd("C://Apps//Rshiny//data")
data <- read.csv('final2.csv')
windowsFonts(BL = windowsFont("微軟正黑體"))
# data 資料集
# department 科系
# type 入學管道
piechartmaker <- function(data, department, type) {
  data <- data %>%
    filter(系所名 == department)
  if (type == '繁星推薦') {
    d1 <- data %>%
      filter(入學管道 == '繁星推薦一般考生' | 入學管道 == '繁星推薦原住民')
  }
  else if (type == '申請入學') {
    d1 <- data %>%
      filter(grepl('申請入學', 入學管道))
  }
  else if (type == '指考') {
    d1 <- data %>%
      filter(入學管道 == '學士班指考' | 入學管道 == '重點科別公費生考試入學')
  }
  else {
    return('type error!')
  }
  
  datalist <- list()
  for (i in (1:5)) {
    if (i < 5) {
      datalist[i] <- d1 %>% 
        filter(score < 110-i*10 & score >= 100-i*10) %>% 
        summarise(n = n())
    }
    else {
      datalist[i] <- d1 %>% 
        filter(score < 60)%>% 
        summarise(n = n())
    }
  }
  datalist <- unlist(datalist)
  names <- c('100~90', '89~80', '79~70', '69~60', '60以下')
  datalist <- round(datalist/sum(datalist)*100)
  df <- data.frame(value = datalist, group = names)
  df2 <- df %>% 
    mutate(csum = rev(cumsum(rev(value))), 
           pos = value/2 + lead(csum, 1),
           pos = if_else(is.na(pos), value/2, pos))
  g <- ggplot(df, aes(x = '', y = value, fill = fct_inorder(group))) +
        geom_col(width = 1, color = 1) +
        coord_polar(theta = 'y') +
        scale_fill_brewer(palette = 'Pastel1') +
        geom_label_repel(data = df2,
                     aes(y = pos, label = paste0(value, '%')),
                     size = 2.5, nudge_x = 1, show.legend = FALSE) +
        guides(fill = guide_legend(title = '分數', nrow = 5, title.position = 'top')) +
        theme_void() + theme(legend.position = 'bottom', plot.title = element_text(size = 15, face = 'bold', hjust = 0.5, vjust = 3,family = "BL"), legend.key.size = unit(3, 'mm'), legend.title = element_text(size = 12,family = "BL"), legend.text = element_text(size = 8,family = "BL")) +
        labs(title = type)
  return(g)
}

mergeplot <- function(data, department) {
  g1 <- piechartmaker(data, department, type = '繁星推薦')
  g2 <- piechartmaker(data, department, type = '申請入學')
  g3 <- piechartmaker(data, department, type = '指考')
  
  jpeg(file = paste0(department, '學生成績分布.jpeg'), width = 756, height = 600, res = 200)
  grid.arrange(g1, g2, g3 ,nrow = 1, ncol = 3)
  dev.off()
}
setwd("C://Apps//Rshiny//www")
dlist <- unique(data[['系所名']])
for (d in dlist) {
  mergeplot(data = data, department = d)
}

