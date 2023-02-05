library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(grid)
library(gridExtra)
library(devtools)
library(highcharter)
library(billboarder)
library(tidyverse)
library(RColorBrewer)
#for(i in c(1:57))
#devtools::install_github('jbkunst/highcharter')
#devtools::install_github('dreamRs/billboarder')
src = "//Users//guanruilin//Documents//Rshiny//data//final3.csv"

院 <- c("請選擇院所", "傳播學院", "外國語文學院", "教育學院", "文學院", "民生學院", "法律學院", "理工學院", "社會科學院", "管理學院", "織品服裝學院", "藝術學院", "醫學院")
系 <- c("請選擇系所")
系所<-c("請選擇系所","影像傳播學系", "新聞傳播學系", "廣告傳播學系","英國語文學系", "德語語文學系", "法國語文學系", "西班牙語文學系", "日本語文學系", "義大利語文學系"
      ,"體育學系體育學組", "體育學系運動競技組", "體育學系運動健康管理組","圖書資訊學系", "教育領導與科技發展學士學位學程","中國文學系", "歷史學系", "哲學系","兒童與家庭學系", "餐旅管理學系", "食品科學系", "營養科學系",
      "法律學系", "財經法律學系", "學士後法律學系","數學系應用數學組", "數學系資訊數學組","物理學系光電物理組", "物理學系物理組","化學系", "生命科學系", "資訊工程學系", "電機工程學系", "醫學資訊與創新應用學士學位學程", "人工智慧與資訊安全學士學位學程",
      "社會學系", "社會工作學系", "經濟學系", "宗教學系", "心理學系", "天主教研修學士學位學程", "企業管理學系", "會計學系", "統計資訊學系", "金融與國際企業學系", "資訊管理學系",
      "織品服裝學系織品設計組", "織品服裝學系服飾設計組", "織品服裝學系織品服飾行銷組", "音樂學系", "應用美術學系", "景觀設計學系",
      "醫學系", "護理學系", "公共衛生學系", "臨床心理學系", "職能治療學系", "呼吸治療學系")
傳播學院系 <- c("影像傳播學系", "新聞傳播學系", "廣告傳播學系")
外國語文學院系 <- c("英國語文學系", "德語語文學系", "法國語文學系", "西班牙語文學系", "日本語文學系", "義大利語文學系")
教育學院系 <- c("體育學系體育學組", "體育學系運動競技組", "體育學系運動健康管理組","圖書資訊學系", "教育領導與科技發展學士學位學程")
文學院系 <- c("中國文學系", "歷史學系", "哲學系")
民生學院系 <- c("兒童與家庭學系", "餐旅管理學系", "食品科學系", "營養科學系")
法律學院系 <- c("法律學系", "財經法律學系", "學士後法律學系")
理工學院系 <- c("數學系應用數學組", "數學系資訊數學組","物理學系光電物理組", "物理學系物理組","化學系", "生命科學系", "資訊工程學系", "電機工程學系", "醫學資訊與創新應用學士學位學程", "人工智慧與資訊安全學士學位學程")
社會科學院系 <- c("社會學系", "社會工作學系", "經濟學系", "宗教學系", "心理學系", "天主教研修學士學位學程")
管理學院系 <- c("企業管理學系", "會計學系", "統計資訊學系", "金融與國際企業學系", "資訊管理學系")
織品服裝學院系 <- c("織品服裝學系織品設計組", "織品服裝學系服飾設計組", "織品服裝學系織品服飾行銷組")
藝術學院系 <- c("音樂學系", "應用美術學系", "景觀設計學系")
醫學院系 <- c("醫學系", "護理學系", "公共衛生學系", "臨床心理學系", "職能治療學系", "呼吸治療學系")
# Define UI ----
ui <- navbarPage(
  "各系110年度三大入學管道學業表現",
  theme = shinythemes::shinytheme("flatly"),
  #shinythemes::themeSelector("flatly"),
  #theme = shinythemes::themeSelector("flatly"),
  #theme = shinytheme("cerulean"),
  #titlePanel("各系110年度三大入學管道學業表現"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "theme2.css")
  ),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(6,
               checkboxInput("v0", label = "全校分析結果",value=FALSE))),
      fluidRow(
        column(6,
               selectInput("v1", label = "請選擇院所", choices = 院, multiple = FALSE)),
        column(6,
               selectInput("v2", label = "請選擇系所", choices = 系所, multiple = FALSE, selected = " ")),
        textOutput("selected_v1"),#在底下server內新增對應Rcode
        textOutput("selected_v2"),#在底下server內新增對應Rcode
      ),
      # fluidRow(
      #   column(6,
      #     submitButton("分析結果")
      #   )
      # )
    ),
    mainPanel(
      h3("本頁面探討全校三大入學管道之學生的總平均學業表現，將學生學業平均分數分成以下五大級距，根據所得出的比例數據做分析。"),
      tabsetPanel(
        tabPanel("全校之分析結果",
                 fluidRow(
                          column(4,uiOutput("imageOfAll1")),
                          column(4,uiOutput("imageOfAll2")),
                          column(4,uiOutput("imageOfAll3"))),
                 textOutput("explanationOfAll")),
        tabPanel("各院之分析結果",
                 fluidRow(
                          column(4,uiOutput("imageOfCol1")),
                          column(4,uiOutput("imageOfCol2")),
                          column(4,uiOutput("imageOfCol3"))),
                 textOutput("explanationOfCollege")),
        tabPanel("該院各系所的分析結果",
                 fluidRow(
                   column(4,uiOutput("imageOfDepartment1")),
                   column(4,uiOutput("imageOfDepartment2")),
                   column(4,uiOutput("imageOfDepartment3"))),
                   textOutput("explanationOfDepartment"))
      )
    )
  )
)
server <- function(input, output, session){
  #自動更新院選項
  #observe({
  #if(!is.null(input$v2))
  #updateSelectInput(session, "v1", 
  #choices = 院[!(院 %in% input$v2)], 
  #selected = isolate(input$v1))
  #})
  #自動更新系選項
  observe({
    if(!is.null(input$v1))
      if((input$v1)=="傳播學院")
        updateSelectInput(session, "v2", 
                          choices = 傳播學院系[!(傳播學院系 %in% input$v1)], 
                          selected = isolate(input$v2))
    else if((input$v1)=="外國語文學院")
      updateSelectInput(session, "v2", 
                        choices = 外國語文學院系[!(外國語文學院系 %in% input$v1)], 
                        selected = isolate(input$v2))
    else if((input$v1)=="教育學院")
      updateSelectInput(session, "v2", 
                        choices = 教育學院系[!(教育學院系 %in% input$v1)], 
                        selected = isolate(input$v2))
    else if((input$v1)=="文學院")
      updateSelectInput(session, "v2", 
                        choices = 文學院系[!(文學院系 %in% input$v1)], 
                        selected = isolate(input$v2))
    else if((input$v1)=="民生學院")
      updateSelectInput(session, "v2", 
                        choices = 民生學院系[!(民生學院系 %in% input$v1)], 
                        selected = isolate(input$v2))
    else if((input$v1)=="法律學院")
      updateSelectInput(session, "v2", 
                        choices = 法律學院系[!(法律學院系 %in% input$v1)], 
                        selected = isolate(input$v2))
    else if((input$v1)=="理工學院")
      updateSelectInput(session, "v2", 
                        choices = 理工學院系[!(理工學院系 %in% input$v1)], 
                        selected = isolate(input$v2))
    else if((input$v1)=="社會科學院")
      updateSelectInput(session, "v2", 
                        choices = 社會科學院系[!(社會科學院系 %in% input$v1)], 
                        selected = isolate(input$v2))
    else if((input$v1)=="管理學院")
      updateSelectInput(session, "v2", 
                        choices = 管理學院系[!(管理學院系 %in% input$v1)], 
                        selected = isolate(input$v2))
    else if((input$v1)=="織品服裝學院")
      updateSelectInput(session, "v2", 
                        choices = 織品服裝學院系[!(織品服裝學院系 %in% input$v1)], 
                        selected = isolate(input$v2))
    else if((input$v1)=="藝術學院")
      updateSelectInput(session, "v2", 
                        choices = 藝術學院系[!(藝術學院系 %in% input$v1)], 
                        selected = isolate(input$v2))
    else if((input$v1)=="醫學院")
      updateSelectInput(session, "v2", 
                        choices = 醫學院系[!(醫學院系 %in% input$v1)], 
                        selected = isolate(input$v2))
  })
  #取得使用者所選取的院所
  output$selected_v2 <- renderText({ 
    paste("您已選擇:",input$v2)
  })
  output$selected_v1 <- renderText({ 
    paste("您已選擇:",input$v1)
  })
  #the 1st one is choosen and the 2nd one is absent
  #if((is.null(input$v1)==FALSE) & (is.null(input$v2)==TRUE)){
  setwd("//Users//guanruilin//Documents//Rshiny//data")
  data <- read.csv('final2.csv')
  piechartmaker <- function(data, department, type) {
    data <- data %>%
      filter(學院名 == department)
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
  #顯示:只選擇各院後之圖片(沒選擇系所)

  #全校
  output$imageOfAll1 <- renderUI({
    if(input$v0=="TRUE"){
      x = readr::read_csv(src) %>%
        filter(departmentNum >= '1', departmentNum <= '57')
      cc1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(cc1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
  })
  output$imageOfAll2 <- renderUI({
    if(input$v0=="TRUE"){
      x = readr::read_csv(src) %>%
        filter(departmentNum >= '1', departmentNum <= '57')
      cc2 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(cc2) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
  })
  output$imageOfAll3 <- renderUI({
    if(input$v0=="TRUE"){
      x = readr::read_csv(src) %>%
        filter(departmentNum >= '1', departmentNum <= '57')
      cc3 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(cc3) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
  })
  output$explanationOfAll <- renderText({
    if(input$v0=="TRUE"){
      paste("全校110學年度入學三大入學生學業表現可知，平均在80分以上之比例高低為：繁星推薦(66%)>個人申請(42%)>指考分發(34%)，可推論110學年度傳播學院繁星推薦入學的學生學業表現相對優良。")
    }
  })
  #院
  output$imageOfCol1 <- renderUI({
    if(input$v1 == "傳播學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('IG'))
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
      #繁星篩選
      z1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      #繁星做圖
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v1 == "外國語文學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('FG'))
      z1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v1 == "教育學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('KG'))
      z1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v1 == "文學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('CG'))
      z1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
      
    }
    else if(input$v1 == "民生學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('HG'))
      z1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
      
    }
    else if(input$v1 == "法律學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('JG'))
      z1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v1 == "理工學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('SG'))
      z1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
      
    }
    else if(input$v1 == "社會科學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('WG'))
      z1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
      
    }
    else if(input$v1 == "管理學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('MG'))
      z1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
      
    }
    else if(input$v1 == "織品服裝學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('OG'))
      z1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
      
    }
    else if(input$v1 == "藝術學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('AG'))
      z1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
      
    }
    else if(input$v1 == "醫學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('DG'))
      z1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
      
    }
  })
  output$imageOfCol2 <- renderUI({
    if(input$v1 == "傳播學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('IG'))
      z2 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z2) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v1 == "外國語文學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('FG'))
      z2 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z2) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v1 == "教育學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('KG'))
      z2 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z2) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v1 == "文學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('CG'))
      z2 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z2) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v1 == "民生學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('HG'))
      z2 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z2) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v1 == "法律學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('JG'))
      z2 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z2) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v1 == "理工學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('SG'))
      z2 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z2) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v1 == "社會科學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('WG'))
      z2 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z2) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v1 == "管理學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('MG'))
      z2 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z2) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v1 == "織品服裝學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('OG'))
      z2 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z2) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v1 == "藝術學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('AG'))
      z2 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z2) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v1 == "醫學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('DG'))
      z2 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z2) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
  })
  output$imageOfCol3 <- renderUI({
    if(input$v1 == "傳播學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('IG'))
      z3 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z3) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v1 == "外國語文學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('FG'))
      z3 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z3) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v1 == "教育學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('KG'))
      z3 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z3) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v1 == "文學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('CG'))
      z3 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z3) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v1 == "民生學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('HG'))
      z3 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z3) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v1 == "法律學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('JG'))
      z3 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z3) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v1 == "理工學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('SG'))
      z3 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z3) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v1 == "社會科學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('WG'))
      z3 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z3) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v1 == "管理學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('MG'))
      z3 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z3) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v1 == "織品服裝學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('OG'))
      z3 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z3) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v1 == "藝術學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('AG'))
      z3 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z3) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v1 == "醫學院"){
      x = readr::read_csv(src) %>%
        filter(院別碼 %in% c('DG'))
      z3 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z3) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
  })
  output$explanationOfCollege <- renderText({
    if(input$v1 == "傳播學院"){            
      paste("110學年度入學三大入學生學業表現可知，110學年度入學生總平均在80分以上之比例高低為：繁星推薦(92%)>個人申請(49%)>指考分發(47%)，可推論110學年度傳播學院繁星推薦入學的學生學業表現相對優良。")
    }
    else if(input$v1 == "外國語文學院"){
      paste("外國語文學院110學年度入學三大入學生學業表現可知，110學年度入學生總平均在80分以上之比例高低為：繁星推薦(69%)>個人申請(48%)>指考分發(38%)，可推論110學年度外國語文學院繁星推薦入學的學生學業表現相對優良。")
    }
    else if(input$v1 == "教育學院"){
      paste("教育學院110學年度入學三大入學生學業表現可知，110學年度入學生總平均在80分以上之比例高低為：繁星推薦(93%)>個人申請(66%)>指考分發(53%)，可推論110學年度教育學院繁星推薦入學的學生學業表現相對優良。")
    }
    else if(input$v1 == "文學院"){
      paste("文學院110學年度入學三大入學生學業表現可知，110學年度入學生總平均在80分以上之比例高低為：繁星推薦(60%)>個人申請(38%)>指考分發(27%)，可推論110學年度文學院繁星推薦入學的學生學業表現相對優良。")
    }
    else if(input$v1 == "民生學院"){
      paste("民生學院110學年度入學三大入學生學業表現可知，110學年度入學生總平均在80分以上之比例高低為：繁星推薦(66%)>個人申請(43%)>指考分發(37%)，可推論110學年度民生學院繁星推薦入學的學生學業表現相對優良。")
    }
    else if(input$v1 == "法律學院"){
      paste("法律學院110學年度入學三大入學生學業表現可知，110學年度入學生總平均在80分以上之比例高低為：繁星推薦(53%)>個人申請(27%)>指考分發(18%)，可推論110學年度法律學院繁星推薦入學的學生學業表現相對優良。")
    }
    else if(input$v1 == "理工學院"){
      paste("理工學院110學年度入學三大入學生學業表現可知，110學年度入學生總平均在80分以上之比例高低為：繁星推薦(52%)>個人申請(28%)>指考分發(17%)，可推論110學年度理工學院繁星推薦入學的學生學業表現相對優良。")
    }
    else if(input$v1 == "社會科學院"){
      paste("社會科學院110學年度入學三大入學生學業表現可知，110學年度入學生總平均在80分以上之比例高低為：繁星推薦(60%)>個人申請(46%)>指考分發(30%)，可推論110學年度社會科學院繁星推薦入學的學生學業表現相對優良。")
    }
    else if(input$v1 == "管理學院"){
      paste("管理學院110學年度入學三大入學生學業表現可知，110學年度入學生總平均在80分以上之比例高低為：繁星推薦(69%)>指考分發(39%)>個人申請(34%)，可推論110學年度管理學院繁星推薦入學的學生學業表現相對優良。")
    }
    else if(input$v1 == "織品服裝學院"){
      paste("織品服裝學院110學年度入學三大入學生學業表現可知，110學年度入學生總平均在80分以上之比例高低為：繁星推薦(67%)>個人申請(44%)>指考分發(18%)，可推論110學年度織品服裝學院繁星推薦入學的學生學業表現相對優良。")
    }
    else if(input$v1 == "藝術學院"){
      paste("藝術學院110學年度入學三大入學生學業表現可知，110學年度入學生總平均在80分以上之比例高低為：繁星推薦(69%)>個人申請(48%)>指考分發(38%)，可推論110學年度藝術學院繁星推薦入學的學生學業表現相對優良。")
    }
    else if(input$v1 == "醫學院"){
      paste("醫學院110學年度入學三大入學生學業表現可知，110學年度入學生總平均在80分以上之比例高低為：繁星推薦(84%)>個人申請(68%)>指考分發(66%)，可推論110學年度醫學院繁星推薦入學的學生學業表現相對優良。")
    }
  })
  #系
  output$imageOfDepartment1 <- renderUI({
    if(input$v2 == "影像傳播學系"){            
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('5'))
      e1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(e1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
     
    }                                        
    else if(input$v2 == "新聞傳播學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('6'))
      f1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(f1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "廣告傳播學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('7'))
      g1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(g1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "英國語文學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('11'))
      k1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "德語語文學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('16'))
      p1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(p1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "法國語文學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('12'))
      l1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(l1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "西班牙語文學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('13'))
      m1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(m1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "日本語文學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('14'))
      n1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(n1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "義大利語文學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('15'))
      o1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(o1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "體育學系體育學組"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('8'))
      h1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(h1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "體育學系運動競技組"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('9'))
      i1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(i1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "體育學系運動健康管理組"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('10'))
      j1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(j1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "圖書資訊學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('2'))
      d1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(d1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "教育領導與科技發展學士學位學程"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('55'))
      bc1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(bc1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "中國文學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('1'))
      a1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(a1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "歷史學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('2'))
      b1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(b1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "哲學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('2'))
      c1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(c1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "兒童與家庭學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('30'))
      ad1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ad1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "餐旅管理學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('29'))
      ac1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ac1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "食品科學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('45'))
      as1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(as1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "營養科學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('46'))
      at1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(at1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "法律學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('31'))
      ae1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ae1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "財經法律學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('35'))
      ai1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ai1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "學士後法律學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('36'))
      aj1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(aj1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "數學系應用數學組"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('18'))
      r1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(r1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "數學系資訊數學組"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('17'))
      q1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(q1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "物理學系光電物理組"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('28'))
      ab1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ab1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "物理學系物理組"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('27'))
      aa1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(aa1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "化學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('19'))
      s1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(s1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "生命科學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('26'))
      z1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "資訊工程學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('25'))
      y1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(y1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "電機工程學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('24'))
      x1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(x1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
      
    }
    else if(input$v2 == "醫學資訊與創新應用學士學位學程"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('56'))
      bd1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(x1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
      
    }
    else if(input$v2 == "人工智慧與資訊安全學士學位學程"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('57'))
      be1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(be1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "社會學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('32'))
      af1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(af1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
     
    }
    else if(input$v2 == "社會工作學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('33'))
      ag1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ag1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "經濟學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('34'))
      ah1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ah1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
      
    }
    else if(input$v2 == "宗教學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('47'))
      au1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(au1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "心理學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('20'))
      t1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(t1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "天主教研修學士學位學程"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('54'))
      bb1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(bb1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "企業管理學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('37'))
      ak1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ak1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "會計學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('38'))
      al1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(al1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "統計資訊學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('41'))
      ao1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ao1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "金融與國際企業學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('40'))
      an1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(an1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "資訊管理學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('39'))
      am1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(am1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "織品服裝學系織品設計組"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('21'))
      u1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(u1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "織品服裝學系服飾設計組"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('23'))
      w1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(w1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "織品服裝學系織品服飾行銷組"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('22'))
      v1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(v1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "音樂學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('42'))
      ap1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ap1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "應用美術學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('43'))
      aq1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(aq1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "景觀設計學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('44'))
      ar1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ar1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "醫學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('50'))
      ax1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ax1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "護理學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('48'))
      av1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(av1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "公共衛生學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('49'))
      aw1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(aw1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "臨床心理學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('51'))
      ay1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ay1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "職能治療學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('52'))
      az1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(az1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
    else if(input$v2 == "呼吸治療學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('53'))
      ba1 = x %>% filter(入學管道碼 %in% c('B6C')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ba1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "繁星推薦", position = "center")
    }
  })
  output$imageOfDepartment2 <- renderUI({
    if(input$v2 == "影像傳播學系"){            
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('5'))
      e1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(e1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }                                        
    else if(input$v2 == "新聞傳播學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('6'))
      f1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(f1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "廣告傳播學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('7'))
      g1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(g1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "英國語文學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('11'))
      k1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(k1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "德語語文學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('16'))
      p1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(p1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "法國語文學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('12'))
      l1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(l1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "西班牙語文學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('13'))
      m1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(m1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "日本語文學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('14'))
      n1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(n1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "義大利語文學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('15'))
      o1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(a1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "體育學系體育學組"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('8'))
      h1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(h1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "體育學系運動競技組"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('9'))
      i1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(i1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "體育學系運動健康管理組"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('10'))
      j1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(j1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "圖書資訊學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('2'))
      d1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(d1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "教育領導與科技發展學士學位學程"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('55'))
      bc1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(bc1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "中國文學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('1'))
      a1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(a1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "歷史學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('2'))
      b1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(b1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "哲學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('2'))
      c1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(c1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "兒童與家庭學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('30'))
      ad1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ad1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "餐旅管理學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('29'))
      ac1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ac2) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "食品科學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('45'))
      as1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(as1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "營養科學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('46'))
      at1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(at1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "法律學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('31'))
      ae1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ae1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "財經法律學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('35'))
      ai1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ai1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "學士後法律學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('36'))
      aj1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(aj1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "數學系應用數學組"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('18'))
      r1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(r1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "數學系資訊數學組"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('17'))
      q1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(q1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "物理學系光電物理組"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('28'))
      ab1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ab1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "物理學系物理組"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('27'))
      aa1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(aa1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "化學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('19'))
      s1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(s1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "生命科學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('26'))
      z1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "資訊工程學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('25'))
      y1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(y1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "電機工程學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('24'))
      x1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(x1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "醫學資訊與創新應用學士學位學程"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('56'))
      bd1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(bd1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "人工智慧與資訊安全學士學位學程"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('57'))
      be1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(be1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "社會學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('32'))
      af1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(af1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "社會工作學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('33'))
      ag1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ag1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "經濟學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('34'))
      ah1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ah1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "宗教學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('47'))
      au1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(au1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "心理學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('20'))
      t1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(t1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "天主教研修學士學位學程"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('54'))
      bb1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(bb1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "企業管理學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('37'))
      ak1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ak1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "會計學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('38'))
      al1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(al1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "統計資訊學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('41'))
      ao1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ao1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "金融與國際企業學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('40'))
      an1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(an1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "資訊管理學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('39'))
      am1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(am1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "織品服裝學系織品設計組"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('21'))
      u1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(u1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "織品服裝學系服飾設計組"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('23'))
      w1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(w1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "織品服裝學系織品服飾行銷組"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('22'))
      v1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(v1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "音樂學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('42'))
      ap1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ap1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "應用美術學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('43'))
      aq1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(aq1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "景觀設計學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('44'))
      ar1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ar1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "醫學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('50'))
      ax1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ax1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "護理學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('48'))
      av1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(av1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "公共衛生學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('49'))
      aw1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(aw1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "臨床心理學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('51'))
      ay1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ay1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "職能治療學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('52'))
      az1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(az1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
    else if(input$v2 == "呼吸治療學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('53'))
      ba1 = x %>% filter(入學管道碼 %in% c('B2D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ba1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "個人申請", position = "center")
    }
  })
  output$imageOfDepartment3 <- renderUI({
    if(input$v2 == "影像傳播學系"){            
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('5'))
      e1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(e1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }                                        
    else if(input$v2 == "新聞傳播學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('6'))
      f1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(f1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "廣告傳播學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('7'))
      g1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(g1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "英國語文學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('11'))
      k1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(k1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "德語語文學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('16'))
      p1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(p1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "法國語文學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('12'))
      l1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(l1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "西班牙語文學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('13'))
      m1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(m1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "日本語文學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('14'))
      n1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(n1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "義大利語文學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('15'))
      o1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(o1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "體育學系體育學組"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('8'))
      h1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(h1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "體育學系運動競技組"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('9'))
      i1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(i1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "體育學系運動健康管理組"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('10'))
      j1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(j1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "圖書資訊學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('2'))
      d1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(d1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "教育領導與科技發展學士學位學程"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('55'))
      bc1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(bc1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "中國文學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('1'))
      a1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(a1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "歷史學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('2'))
      b1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(b1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "哲學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('2'))
      c1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(c1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "兒童與家庭學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('30'))
      ad1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ad1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "餐旅管理學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('29'))
      ac1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ac1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "食品科學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('45'))
      as1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(as1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "營養科學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('46'))
      at1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(at1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "法律學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('31'))
      ae1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ae1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "財經法律學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('35'))
      ai1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ai1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "學士後法律學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('36'))
      aj1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(aj1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "數學系應用數學組"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('18'))
      r1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(r1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "數學系資訊數學組"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('17'))
      q1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(q1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "物理學系光電物理組"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('28'))
      ab1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ab1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "物理學系物理組"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('27'))
      aa1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(aa1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "化學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('19'))
      s1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(s1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "生命科學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('26'))
      z1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(z1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "資訊工程學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('25'))
      y1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(y1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "電機工程學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('24'))
      x1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(x1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "醫學資訊與創新應用學士學位學程"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('56'))
      bd1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(bd1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "人工智慧與資訊安全學士學位學程"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('57'))
      be1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(be1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "社會學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('32'))
      af1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(af1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "社會工作學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('33'))
      ag1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ag1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "經濟學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('34'))
      ah1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ah1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "宗教學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('47'))
      au1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(au1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "心理學系"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('20'))
      t1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(t1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "天主教研修學士學位學程"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('54'))
      bb1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(bb1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "企業管理學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('37'))
      ak1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ak1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "會計學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('38'))
      al1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(al1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "統計資訊學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('41'))
      ao1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ao1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "金融與國際企業學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('40'))
      an1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(an1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "資訊管理學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('39'))
      am1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(am1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "織品服裝學系織品設計組"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('21'))
      u1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(u1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "織品服裝學系服飾設計組"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('23'))
      w1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(w1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "織品服裝學系織品服飾行銷組"){
      x = readr::read_csv(src) %>%
        #filter可取用選擇的資料範圍 選所有系所編號(1~57)
        filter(departmentNum %in% c('22'))
      v1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(v1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "音樂學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('42'))
      ap1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ap1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "應用美術學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('43'))
      aq1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(aq1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "景觀設計學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('44'))
      ar1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ar1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "醫學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('50'))
      ax1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ax1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "護理學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('48'))
      av1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(av1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "公共衛生學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('49'))
      aw1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(aw1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "臨床心理學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('51'))
      ay1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ay1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "職能治療學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('52'))
      az1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(az1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
    else if(input$v2 == "呼吸治療學系"){
      x = readr::read_csv(src) %>%
        filter(departmentNum %in% c('53'))
      ba1 = x %>% filter(入學管道碼 %in% c('B1D')) %>%
        group_by(scoreStratified) %>%
        summarise(CNT = n())
      billboarder(height = 250, width = 280) %>% 
        bb_piechart(ba1) %>%
        bb_legend(position = 'right') %>%
        bb_colors_manual("0~59分" = "#9955FF", "60~69分" = "#009FCC", "70~79分" = "#00AA00", "80~89分" = "#FF5511", "90~100分" = "#FF0000") %>%
        bb_title(text = "指考", position = "center")
    }
  })
  output$explanationOfDepartment <- renderText({
    if(input$v2 == "影像傳播學系"){            
      paste("影像傳播學系110學年度入學三大入學生學業表現可知，110學年度入學生總平均在80分以上之比例高低為：繁星推薦(75%)>指考分發(67%)>個人申請(43%)，可推論110學年度影像傳播學系繁星推薦入學的學生學業表現相對優良。")
    }                                        
    else if(input$v2 == "新聞傳播學系"){
      paste("新聞傳播學系110學年度入學三大入學生學業表現可知，110學年度入學生總平均在80分以上之比例高低為：繁星推薦(100%)>個人申請(65%)>指考分發(39%)，可推論110學年度新聞傳播學系繁星推薦的學生學業表現相對優良。")
    }
    else if(input$v2 == "廣告傳播學系"){
      paste("廣告傳播學系110學年度入學三大入學生學業表現可知，110學年度入學生總平均在80分以上之比例高低為：繁星推薦(100%)>個人申請(43%)>指考分發(32%)，可推論110學年度廣告傳播學系繁星入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "英國語文學系"){
      paste("英國語文學系110學年度入學生總平均在80分以上之比例高低為：繁星推薦(78%)>個人申請(44%)>指考分發(39%)，可推論110學年度英國語文學系繁星入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "德語語文學系"){
      paste("德語語文學系110學年度入學生總平均在80分以上之比例高低為：繁星推薦(87%)>個人申請(44%)>指考分發(31%)，可推論110學年度德語語文學系繁星入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "法國語文學系"){
      paste("法國語文學系110學年度入學生總平均在80分以上之比例高低為：個人申請(55%)>繁星推薦(50%)>指考分發(44%)，可推論110學年度法國語文學系申請入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "西班牙語文學系"){
      paste("西班牙語文學系110學年度入學生總平均在80分以上之比例高低為：個人申請(58%)>繁星推薦(56%) >指考分發(29%)，可推論110學年度西班牙語文學系申請入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "日本語文學系"){
      paste("日本語文學系110學年度入學生總平均在80分以上之比例高低為：繁星推薦(76%)>個人申請(59%) >指考分發(57%)，可推論110學年度日本語文學系繁星入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "義大利語文學系"){
      paste("義大利語文學系110學年度入學生總平均在80分以上之比例高低為：繁星推薦(55%)>指考分發(18%)>個人申請(15%)，可推論110學年度義大利語文學系繁星入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "體育學系體育學組"){
      paste("體育學系體育學組110學年度入學三大入學生學業表現可知，110學年度入學生總平均在80分以上之比例高低為：個人申請(50%)=指考分發(50%)，可推論110學年度體育學系體育學組學生學業表現與入學管道無明顯關聯。")
    }
    else if(input$v2 == "體育學系運動競技組"){
      paste("")
    }
    else if(input$v2 == "體育學系運動健康管理組"){
      paste("體育學系運動健康管理組110學年度入學三大入學生學業表現可知，110學年度入學生總平均在80分以上之比例高低為：個人申請(60%)>繁星推薦(50%)>指考分發(42%)，可推論110學年度體育學系運動健康管理組個人申請入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "圖書資訊學系"){
      paste("圖書資訊學系110學年度入學三大入學生學業表現可知，110學年度入學生總平均在80分以上之比例高低為：繁星推薦(100%)>個人申請(72%)>指考分發(36%)，可推論110學年度圖書資訊學系繁星推薦入學的學生學業表現相對優良")
    }
    else if(input$v2 == "教育領導與科技發展學士學位學程"){
      paste("教育領導與科技發展學士學位學程110學年度入學三大入學生學業表現可知，110學年度入學生總平均在80分以上之比例高低為：繁星推薦(100%)>指考分發(91%)>個人申請(83%)，可推論110學年度教育領導與科技發展學士學位學程繁星入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "中國文學系"){
      paste("中國文學系110學年度入學三大入學生學業表現 可知，110學年度入學生總平均在80分以上之比例高低為：繁星推薦(61%)>個人申請(31%)>指考分發(16%)，可推論110學年度中國文學系繁星入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "歷史學系"){
      paste("歷史學系110年度入學三大入學生學業表現可知，110學年度入學生總平均在80分以上之比例高低為：繁星推薦(58%)>個人申請(52%)>指考分發(37%)，可推論110學年度歷史學系繁星推薦的學生學業表現相對優良。")
    }
    else if(input$v2 == "哲學系"){
      paste("哲學系110年度入學三大入學生學業表現可知，110學年度入學生總平均在80分以上之比例高低為：繁星推薦(60%)>個人申請(35%)>指考分發(23%)，可推論110學年度哲學系繁星推薦的學生學業表現相對優良")
    }
    else if(input$v2 == "兒童與家庭學系"){
      paste("兒童與家庭學系110學年度入學三大入學生學業表現 可知，110學年度入學生總平均在80分以上之比例高低為：指考分發(95%)>個人申請(80%)>繁星推薦(67%)，可推論110學年度兒童與家庭學系指考分發的學生學業表現相對優良")
    }
    else if(input$v2 == "餐旅管理學系"){
      paste("餐旅管理學系110學年度入學三大入學生學業表現可知，110學年度入學生總平均在80分以上之比例高低為：繁星推薦(65%)>個人申請(30%)>指考分發(19%)，可推論110學年度餐旅管理學系繁星入學的學生學業表現相對優良")
    }
    else if(input$v2 == "食品科學系"){
      paste("食品科學系110學年度入學三大入學生學業表現 可知，110學年度入學生總平均在80分以上之比例高低為：繁星推薦(44%)>個人申請(10%)>指考分發(3%)，可推論110學年度食品科學系繁星入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "營養科學系"){
      paste("營養科學系110學年度入學三大入學生學業表現 可知，110學年度入學生總平均在80分以上之比例高低為：繁星推薦(89%)>個人申請(65%)>指考分發(50%)，可推論110學年度營養科學系繁星入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "法律學系"){
      paste("法律學系110學年度入學生總平均在80分以上之比例高低為：繁星推薦(55%)>個人申請(29%)>指考分發(14%)，可推論110學年度法律學系繁星入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "財經法律學系"){
      paste("財經法律學系110學年度入學生總平均在80分以上之比例高低為：繁星推薦(50%)>指考分發(25%)>個人申請(24%)，可推論110學年度財經法律學系繁星入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "學士後法律學系"){
      paste("")
    }
    else if(input$v2 == "數學系應用數學組"){
      paste("數學系應用數學組110學年度入學三大入學生學業表現可知，110學年度入學生總平均在80分以上之比例高低為：繁星推薦(74%)>指考分發(16%)>個人申請(14%)，可推論110學年度數學系應用數學組繁星推薦入學的學生學業表現相對優良")
    }
    else if(input$v2 == "數學系資訊數學組"){
      paste("數學系資訊數學組110學年度入學三大入學生學業表現可知，110學年度入學生總平均在80分以上之比例高低為：繁星推薦(25%)>個人申請(22%)>指考分發(14%)，可推論110學年度數學系資訊數學組繁星推薦入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "物理學系光電物理組"){
      paste("物理學系光電物理組110學年度入學三大入學生學業表現可知，110學年度入學生總平均在80分以上之比例高低為：繁星推薦(69%)>個人申請(18%)>指考分發(12%)，可推論110學年度物理學系光電物理組繁星推薦入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "物理學系物理組"){
      paste("物理學系物理組110學年度入學三大入學生學業表現可知，110學年度入學生總平均在80分以上之比例高低為：個人申請(17%)>繁星推薦(0%)=指考分發(0%)，可推論110學年度物理學系物理組個人申請入學的學生學業表現相對優良。")    
    }
    else if(input$v2 == "化學系"){
      paste("化學系110學年度入學三大入學生學業表現 可知，110學年度入學生總平均在80分以上之比例高低為：繁星推薦(59%)>指考分發(23%)>個人申請(20%)，可推論110學年度化學系繁星入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "生命科學系"){
      paste("生命科學系110學年度入學三大入學生學業表現可知，110年度入學生總平均在80分以上之比例高低為：繁星推薦(88%)>個人申請(43%)>指考分發(36%)，可推論110學年度生命科學系繁星入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "資訊工程學系"){
      paste("資訊工程學系110學年度入學三大入學生學業表現 可知，110學年度入學生總平均在80分以上之比例高低為：繁星推薦(37%)>個人申請(35%)>指考分發(22%)，可推論110學年度資訊工程學系繁星推薦入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "電機工程學系"){
      paste("電機工程學系110學年度入學三大入學生學業表現可知，110學年度入學生總平均在80分以上之比例高低為：繁星推薦(56%)>個人申請(26%)>指考分發(14%)，可推論110學年度電機工程學系繁星入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "醫學資訊與創新應用學士學位學程"){
      paste("醫學資訊與創新應用學士學位學程110學年度入學三大入學生學業表現可知，110學年度入學生總平均在80分以上之比例高低為：繁星推薦(57%)>個人申請(45%)>指考分發(5%)，可推論110學年度醫學資訊與創新應用學士學位學程繁星推薦入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "人工智慧與資訊安全學士學位學程"){
      paste("")
    }
    else if(input$v2 == "社會學系"){
      paste("社會學系110學年度入學生總平均在80分以上之比例高低為：繁星推薦(80%)>個人申請(76%)>指考分發(54%)，可推論110學年度社會學系繁星入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "社會工作學系"){
      paste("社會工作學系110學年度入學生總平均在80分以上之比例高低為：繁星推薦(33%)>個人申請(28%)>指考分發(19%)，可推論110學年度社會工作學系繁星入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "經濟學系"){
      paste("經濟學系110學年度入學生總平均在80分以上之比例高低為：繁星推薦(50%)> 個人申請(28%)>指考分發(19%)，可推論110學年度經濟學系繁星入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "宗教學系"){
      paste("宗教學系110學年度入學生總平均在80分以上之比例高低為：繁星推薦(100%)>個人申請(48%)>指考分發(38%)，可推論110學年度宗教學系繁星入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "心理學系"){
      paste("心理學系110學年度入學生總平均在80分以上之比例高低為：繁星推薦(78%)>個人申請(51%)>指考分發(47%)，可推論110學年度心理學系繁星入學的學生學業表現相對優良")
    }
    else if(input$v2 == "天主教研修學士學位學程"){
      paste("")
    }
    else if(input$v2 == "企業管理學系"){
      paste("企業管理學系110學年度入學生總平均在80分以上之比例高低為：繁星推薦(83%)>指考分發(46%)>個人申請(35%)，可推論110學年度企業管理系繁星入學的學生學業表現相對優良")
    }
    else if(input$v2 == "會計學系"){
      paste("會計學系110學年度入學生總平均在80分以上之比例高低為：繁星推薦(65%)>個人申請(45%)>指考分發(42%)，可推論110學年度會計系繁星入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "統計資訊學系"){
      paste("統計資訊學系110學年度入學生總平均在80分以上之比例高低為：繁星推薦(67%)>指考分發(24%)>個人申請(23%)，可推論110學年度統計資訊系繁星入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "金融與國際企業學系"){
      paste("金融與國際企業學系110學年度入學生總平均在80分以上之比例高低為：繁星推薦(67%)>指考分發(39%)>個人申請(34%)，可推論110學年度金融與國際企業學系繁星入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "資訊管理學系"){
      paste("資訊管理學系110學年度入學生總平均在80分以上之比例高低為：繁星推薦(58%)>個人申請(38%)>指考分發(36%)，可推論110學年度資訊管理系繁星入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "織品服裝學系織品設計組"){
      paste("織品服裝學系織品設計組110學年度入學生總平均在80分以上之比例高低為：繁星推薦(60%)>個人申請(47%)>指考分發(8%)，可推論110學年度織品服裝學系織品設計組繁星入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "織品服裝學系服飾設計組"){
      paste("織品服裝學系服飾設計組110學年度入學生總平均在80分以上之比例高低為：繁星推薦(100%)>個人申請(34%)>指考分發(11%)，可推論110學年度織品服裝學系服飾設計組繁星入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "織品服裝學系織品服飾行銷組"){
      paste("織品服裝學系織品服飾行銷組110學年度入學生總平均在80分以上之比例高低為：繁星推薦(46%)=個人申請(46%)>指考分發(24%)，可推論110學年度織品服裝學系織品服飾行銷組繁星入學、個人申請的學生學業表現相對優良。")
    }
    else if(input$v2 == "音樂學系"){
      paste("音樂學系110學年度入學生總平均在80分以上之比例高低為：繁星推薦(100%)>個人申請(72%)>指考分發(42%)，可推論110學年度音樂學系繁星入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "應用美術學系"){
      paste("應用美術學系110學年度入學生總平均在80分以上之比例高低為：繁星推薦(83%)>個人申請(65%)>指考分發(54%)，可推論110學年度應用美術學系繁星入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "景觀設計學系"){
      paste("景觀設計學系110學年度入學生總平均在80分以上之比例高低為：繁星推薦(33%)>指考分發(19%)>個人申請(16%)，可推論110學年度景觀設計學系繁星入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "醫學系"){
      paste("醫學系110學年度入學生總平均在80分以上之比例高低為：繁星推薦(100%)>指考分發(95%)>個人申請(67%)，可推論110學年度醫學系個人繁星推薦的學生學業表現相對優良。")
    }
    else if(input$v2 == "護理學系"){
      paste("護理學系110學年度入學生總平均在80分以上之比例高低為：繁星推薦(71%)>個人申請(55%)>指考分發(35%)，可推論110學年度護理學系繁星推薦入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "公共衛生學系"){
      paste("公共衛生學系110學年度入學生總平均在80分以上之比例高低為：繁星推薦(78%) >指考分發(50%) >個人申請(43%)，可推論110學年度公共衛生學系繁星入學的學生學業表現相對優良。")
    }
    else if(input$v2 == "臨床心理學系"){
      paste("臨床心理學系110學年度入學生總平均在80分以上之比例高低為：個人申請(75%)>繁星推薦(73%)=指考分發(73%)，可推論110學年度臨床心理學系個人申請的學生學業表現相對優良")
    }
    else if(input$v2 == "職能治療學系"){
      paste("職能治療學系110學年度入學生總平均在80分以上之比例高低為： 繁星推薦(100%)>個人申請(88%)>指考分發(79%)，可推論110學年度職能治療學系繁星推薦的學生學業表現相對優良。")
    }
    else if(input$v2 == "呼吸治療學系"){
      paste("呼吸治療學系110學年度入學生總平均在80分以上之比例高低為：繁星推薦(100%)>指考分發(84%)>個人申請(70%)，可推論110學年度呼吸治療學系繁星入學的學生學業表現相對優良。")
    }
    
  })
}
shinyApp(ui = ui, server = server)
