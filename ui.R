## ui.R ##
#install.packages("data.table", INSTALL_opts = c('--no-lock'))
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(data.table)
marketData1<-fread("busanmarket19.csv")
marketData2<-fread("busanmarket15.csv")
marketData3<-fread("busanmarket16.csv")
marketData4<-fread("busanmarket17.csv")
marketData5<-fread("busanmarket18.csv")
timepop<-fread("timepop.csv",stringsAsFactors = F,dec=",")
sexpop<-fread("sexpop.csv",stringsAsFactors = F,dec = ",")
agepop<-fread("agepop.csv",stringsAsFactors = F,dec = ",")
weekpop<-fread("week.csv",stringsAsFactors = F,dec = ",")
commutepop<-fread("commutepop.csv",stringsAsFactors = F,dec = ",")
consume<-fread("consume.csv",stringsAsFactors = F,dec = ",")
avggain<-fread("avggain.csv",stringsAsFactors = F,dec = ",")

district<-arrange(distinct(marketData1,시군구명),시군구명)
district<-c("선택",district)

MainCategory<-distinct(marketData1,상권업종대분류명)
MainCategory<-c("선택",MainCategory)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("상권분석", tabName = "commerce", icon = icon("store-alt"))
    
    
  )
)

body <- dashboardBody(
  
  tabItems(
    tabItem(tabName = "commerce",
            fluidRow(
              column(width=9,
                     box(width=NULL, solidHeader = TRUE,
                         leafletOutput("mymap", height=800)
                     ),
                     tabBox(width=NULL,title="업종 증감률",
                            tabPanel("전체",plotOutput("barplot1"),
                                     br(),
                                     tableOutput("value1")),
                            tabPanel("구단위",plotOutput("barplot2"),
                                     br(),
                                     tableOutput("value2")),
                            tabPanel("동단위",plotOutput("barplot3"),
                                     br(),
                                     tableOutput("value3"))
                     ),
                     tabBox(width=NULL,title="유동인구",
                            tabPanel("평균 유동인구",plotOutput("weekpop")),
                            tabPanel("연령별",plotOutput("agepop")),
                            tabPanel("시간별",plotOutput("timepop")),
                            tabPanel("성별",plotOutput("sexpop")),
                            tabPanel("출/퇴근",plotOutput("commutepop"))
                            
                            
                     ),
                     tabBox(width=NULL,title="구매력",
                            tabPanel("평균소득",plotOutput("avggain")),
                            tabPanel("카드 사용량",plotOutput("consume"))
                            
                     )
                     
              ),
              column(width=3,
                     
                     box(width=NULL,
                         selectInput(inputId="district",label=h3("지역 선택"),
                                     choices=district
                                     
                         ),
                         selectInput(inputId="town",label=h3("행정동 선택"),
                                     choices=NULL
                         ),
                         selectInput(inputId="main",label=h3("상권업종대분류명"),
                                     choices=MainCategory,
                                     selected=NULL
                         ),
                         selectInput(inputId="middle",label=h3("상권업종중분류명"),
                                     choices=NULL,
                                     selected=NULL
                         ),
                         selectInput(inputId="sub",label=h3("상권업종소분류명"),
                                     choices=NULL,
                                     selected=NULL
                         ),
                         br(),
                         column(12,align="right", 
                                actionButton("action",width =100,label="확인")
                         )
                     )
                     
                     
              )
              
              
            )
            
    )
  )
)

# Put them together into a dashboardPage
ui<-dashboardPage(
  dashboardHeader(),
  sidebar,
  body
)


