## server.R ##
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
#install.packages("DT")
library(DT)

server<-function(input,output,session){
  
  town<-reactive({
    c("선택",arrange(distinct(filter(marketData1,시군구명==input$district),법정동명),법정동명))
  })
  observe({
    if(!input$district=="선택"){
      updateSelectInput(session,"town",selected=NULL,choices=town())
    }
  })
  
  middle<-reactive({
    c("선택",distinct(filter(marketData1,상권업종대분류명==input$main),상권업종중분류명))
    
  })
  observe({
    if(!input$main=="선택"){
      updateSelectInput(session,"middle",selected=NULL,choices=middle())
    }
  })
  
  sub<-reactive({
    if(!input$middle=="선택"&&!input$main=="선택"){
      c("전체",distinct(filter(marketData1,상권업종중분류명==input$middle),상권업종소분류명))
    }
    
  })
  
  observe({
    if(!input$middle==""){
      updateSelectInput(session,"sub",selected=NULL,choices=sub())
    }
  })
  # output$value<-renderPrint(town())
  output$mymap<-renderLeaflet({
    leaflet()%>%setView(lng=129.075041,lat=35.179967,zoom=15)%>%addTiles()
  })
  
  
  
  
  observeEvent(input$action,{
    
    
    if(input$district=="선택"){
      showNotification("지역명을 선택해주세요.",type="warning",duration=3)
    }else if(input$main=="선택"){
      showNotification("업종 대분류를 선택해주세요.",type="warning",duration=3)
    }else if(input$middle=="선택"){
      showNotification("업종 중분류를 선택해주세요.",type="warning",duration=3)
    }
    else{
      
      mark<-filter(marketData1,법정동명==input$town)
      mark<-filter(mark,상권업종대분류명==input$main)
      mark<-filter(mark,상권업종중분류명==input$middle)
      mark<-filter(mark,상권업종소분류명==input$sub)
      #tp<-filter(timepop,구군==input$district)
      content<-paste(sep=" ",
                     "상호명 : ",mark$상호명,
                     "<br/>",
                     "지점명 : ",mark$지점명,
                     "<br/>",
                     "주소 : ",mark$도로명주소)
      
      
      
      if(nrow(mark)==0){
        showNotification("선택하신 지역에는 해당 업종이 없습니다.",type="warning",duration=3)
      }else{
        plotsexPop<-eventReactive(input$action,{
          
          pop<-filter(sexpop,구군==input$district)
          
          # 
          # pop<-select(pop,남자,여자)
          # pop<-t(pop)
          # 
          # barplot(pop,names=c(1:12),beside = T,col=c("blue","red"),ylim=c(0,300000))
          # legend(0,300000,c("남자","여자"),fill=c("blue","red"),cex=1)
          # pop<-filter(sexpop,구군=='사하구')
          pop<-select(pop,남자,여자)/31
          pop<-as.integer(pop)
          pie(pop,labels=pop,col=c("blue","red"),main="남/녀 유동인구",radius=1)
          legend(0.3,1,c("남자","여자"), fill=c("blue","red"))
          
          
        })
        
        plotTimePop<-eventReactive(input$action,{
          tp<-filter(timepop,구군==input$district)
          pop<-c(as.vector(tp[,5]))/31
          t<-as.vector(tp[,4])
          barplot(pop,names.arg=t,xlab="시간대별 유동인구")
        })
        
        plotAgePop<-eventReactive(input$action,{
          pop<-subset(agepop,구군==input$district)$유동인구
          
          barplot(pop,names.arg=c("10대","20대","30대","40대","50대","60대","70대"),xlab="연령별 유동인구",col=rainbow(7))
          
          
        })
        plotWeekPop<-eventReactive(input$action,{
          
          
          pop<-filter(weekpop,구군==input$district)
          barplot(c(pop$`평균 유동인구`)
                  ,names.arg = c("일요일","월요일","화요일","수요일","목요일","금요일","토요일"),xlab="평균 유동인구",col=rainbow(7))
          
          
        })
        plotcommutePop<-eventReactive(input$action,{
          
          pop<-filter(commutepop,구군==input$district)
          go<-as.integer(mean(subset(pop,구분=='출근')$유동인구)/30)
          out<-as.integer(mean(subset(pop,구분=='퇴근')$유동인구)/30)
          
          pie(c(go,out),labels = c(go,out), col=c("green","yellow"),main="출/퇴근 유동인구",radius=1)
          legend(0.3,1,c("출근","퇴근"), fill=c("green","yellow"))
          
        })
        
        plotavggain<-eventReactive(input$action,{
          g<-(filter(avggain,구군==input$district)$평균소득)/10
          
          plot(g,ann=F,axes=F,col='red',type='o')+
            axis(1,at=1:5,lab=c("14년","15년","16년","17년","18년"))+
            axis(2,ylim=c(0,30000))+
            title(main='지역 평균소득',xlab='년도',ylab='금액(단위 : 만원)')
          
        })
        
        plotconsum<-eventReactive(input$action,{
          con<-subset(consume,구군==input$district)$금액/300000
          plot(con,ann=F,axes=F,col='red',type='o')+
            axis(1,at=1:5,lab=c("14년","15년","16년","17년","18년"))+
            axis(2,ylim=c(0,10000))+
            title(main='카드사용금액',xlab='년도',ylab='금액(단위 : 억원)')
        })
        
        PlotMarketDistrict<-eventReactive(input$action,{
          S15<-nrow(filter(marketData2,시군구명==input$district,상권업종소분류명==input$sub))
          S16<-nrow(filter(marketData3,시군구명==input$district,상권업종소분류명==input$sub))
          S17<-nrow(filter(marketData4,시군구명==input$district,상권업종소분류명==input$sub))
          S18<-nrow(filter(marketData5,시군구명==input$district,상권업종소분류명==input$sub))
          S19<-nrow(filter(marketData1,시군구명==input$district,상권업종소분류명==input$sub))
          market<-c(S15,S16,S17,S18,S19)
          colors<-c("yellow")
          for(i in 2:length(market))
          {if(market[i]>market[i-1]){colors<-c(colors,"blue")}
            else if(market[i]<market[i-1]){colors<-c(colors,"red")}
            else {colors<-c(colors,"yellow")}
          }
          barplot(market,names.arg=c('15년','16년','17년','18년','19년'),xlab="연도별 업종수",col=colors)
        })
        
        PlotMarketAll<-eventReactive(input$action,{
          S15<-nrow(filter(marketData2,상권업종소분류명==input$sub))
          S16<-nrow(filter(marketData3,상권업종소분류명==input$sub))
          S17<-nrow(filter(marketData4,상권업종소분류명==input$sub))
          S18<-nrow(filter(marketData1,상권업종소분류명==input$sub))
          S19<-nrow(filter(marketData5,상권업종소분류명==input$sub))
          market<-c(S15,S16,S17,S18,S19)
          colors<-c("yellow")
          for(i in 2:length(market))
          {if(market[i]>market[i-1]){colors<-c(colors,"blue")}
            else if(market[i]<market[i-1]){colors<-c(colors,"red")}
            else {colors<-c(colors,"yellow")}
          }
          barplot(c(S15,S16,S17,S18,S19),names.arg=c('15년','16년','17년','18년','19년'),xlab="연도별 업종수",col=colors)
        })
        
        PlotMarketTown<-eventReactive(input$action,{
          S15<-nrow(filter(marketData2,법정동명==input$town,상권업종소분류명==input$sub))
          S16<-nrow(filter(marketData3,법정동명==input$town,상권업종소분류명==input$sub))
          S17<-nrow(filter(marketData4,법정동명==input$town,상권업종소분류명==input$sub))
          S18<-nrow(filter(marketData1,법정동명==input$town,상권업종소분류명==input$sub))
          S19<-nrow(filter(marketData5,법정동명==input$town,상권업종소분류명==input$sub))
          market<-c(S15,S16,S17,S18,S19)
          colors<-c("yellow")
          for(i in 2:length(market))
          {if(market[i]>market[i-1]){colors<-c(colors,"blue")}
            else if(market[i]<market[i-1]){colors<-c(colors,"red")}
            else {colors<-c(colors,"yellow")}
          }
          barplot(c(S15,S16,S17,S18,S19),names.arg=c('15년','16년','17년','18년','19년'),xlab="연도별 업종수",col=colors)
          
        })
        
        
        
        ValMarketDistrict<-eventReactive(input$action,{
          구분<-c('대분류','중분류','소분류')
          업종<-c(input$main,input$middle,input$sub)
          
          Y15업소수<-c(B15<-nrow(filter(marketData2,시군구명==input$district,상권업종대분류명==input$main)),
                    M15<-nrow(filter(marketData2,시군구명==input$district,상권업종중분류명==input$middle)),
                    S15<-nrow(filter(marketData2,시군구명==input$district,상권업종소분류명==input$sub)))
          
          Y16업소수<-c(B16<-nrow(filter(marketData3,시군구명==input$district,상권업종대분류명==input$main)),
                    M16<-nrow(filter(marketData3,시군구명==input$district,상권업종중분류명==input$middle)),
                    S16<-nrow(filter(marketData3,시군구명==input$district,상권업종소분류명==input$sub)))
          Y16증감률<-c((B16-B15)/B15*100,(M16-M15)/M15*100,(S16-S15)/S15*100)
          Y17업소수<-c(B17<-nrow(filter(marketData4,시군구명==input$district,상권업종대분류명==input$main)),
                    M17<-nrow(filter(marketData4,시군구명==input$district,상권업종중분류명==input$middle)),
                    S17<-nrow(filter(marketData4,시군구명==input$district,상권업종소분류명==input$sub)))
          Y17증감률<-c((B17-B16)/B16*100,(M17-M16)/M16*100,(S17-S16)/S16*100)
          Y18업소수<-c(B18<-nrow(filter(marketData1,시군구명==input$district,상권업종대분류명==input$main)),
                    M18<-nrow(filter(marketData1,시군구명==input$district,상권업종중분류명==input$middle)),
                    S18<-nrow(filter(marketData1,시군구명==input$district,상권업종소분류명==input$sub)))
          Y18증감률<-c((B18-B17)/B17*100,(M18-M17)/M17*100,(S18-S17)/S17*100)
          Y19업소수<-c(B19<-nrow(filter(marketData5,시군구명==input$district,상권업종대분류명==input$main)),
                    M19<-nrow(filter(marketData5,시군구명==input$district,상권업종중분류명==input$middle)),
                    S19<-nrow(filter(marketData5,시군구명==input$district,상권업종소분류명==input$sub)))
          Y19증감률<-c((B19-B18)/B18*100,(M19-M18)/M18*100,(S19-S18)/S18*100)
          data.frame(구분,업종,Y15업소수,Y16업소수,Y16증감률,Y17업소수,Y17증감률,Y18업소수,Y18증감률,Y19업소수,Y19증감률)
          
        })
        ValMarketAll<-eventReactive(input$action,{
          
          구분<-c('대분류','중분류','소분류')
          업종<-c(input$main,input$middle,input$sub)
          
          Y15업소수<-c(B15<-nrow(filter(marketData2,상권업종대분류명==input$main)),
                    M15<-nrow(filter(marketData2,상권업종중분류명==input$middle)),
                    S15<-nrow(filter(marketData2,상권업종소분류명==input$sub)))
          
          Y16업소수<-c(B16<-nrow(filter(marketData3,상권업종대분류명==input$main)),
                    M16<-nrow(filter(marketData3,상권업종중분류명==input$middle)),
                    S16<-nrow(filter(marketData3,상권업종소분류명==input$sub)))
          Y16증감률<-c((B16-B15)/B15*100,(M16-M15)/M15*100,(S16-S15)/S15*100)
          Y17업소수<-c(B17<-nrow(filter(marketData4,상권업종대분류명==input$main)),
                    M17<-nrow(filter(marketData4,상권업종중분류명==input$middle)),
                    S17<-nrow(filter(marketData4,상권업종소분류명==input$sub)))
          Y17증감률<-c((B17-B16)/B16*100,(M17-M16)/M16*100,(S17-S16)/S16*100)
          Y18업소수<-c(B18<-nrow(filter(marketData1,상권업종대분류명==input$main)),
                    M18<-nrow(filter(marketData1,상권업종중분류명==input$middle)),
                    S18<-nrow(filter(marketData1,상권업종소분류명==input$sub)))
          Y18증감률<-c((B18-B17)/B17*100,(M18-M17)/M17*100,(S18-S17)/S17*100)
          Y19업소수<-c(B19<-nrow(filter(marketData5,상권업종대분류명==input$main)),
                    M19<-nrow(filter(marketData5,상권업종중분류명==input$middle)),
                    S19<-nrow(filter(marketData5,상권업종소분류명==input$sub)))
          Y19증감률<-c((B19-B18)/B18*100,(M19-M18)/M18*100,(S19-S18)/S18*100)
          data.frame(구분,업종,Y15업소수,Y16업소수,Y16증감률,Y17업소수,Y17증감률,Y18업소수,Y18증감률,Y19업소수,Y19증감률)
          
        })
        
        ValMarketTown<-eventReactive(input$action,{
          구분<-c('대분류','중분류','소분류')
          업종<-c(input$main,input$middle,input$sub)
          
          Y15업소수<-c(B15<-nrow(filter(marketData2,법정동명==input$town,상권업종대분류명==input$main)),
                    M15<-nrow(filter(marketData2,법정동명==input$town,상권업종중분류명==input$middle)),
                    S15<-nrow(filter(marketData2,법정동명==input$town,상권업종소분류명==input$sub)))
          
          Y16업소수<-c(B16<-nrow(filter(marketData3,법정동명==input$town,상권업종대분류명==input$main)),
                    M16<-nrow(filter(marketData3,법정동명==input$town,상권업종중분류명==input$middle)),
                    S16<-nrow(filter(marketData3,법정동명==input$town,상권업종소분류명==input$sub)))
          Y16증감률<-c((B16-B15)/B15*100,(M16-M15)/M15*100,(S16-S15)/S15*100)
          Y17업소수<-c(B17<-nrow(filter(marketData4,법정동명==input$town,상권업종대분류명==input$main)),
                    M17<-nrow(filter(marketData4,법정동명==input$town,상권업종중분류명==input$middle)),
                    S17<-nrow(filter(marketData4,법정동명==input$town,상권업종소분류명==input$sub)))
          Y17증감률<-c((B17-B16)/B16*100,(M17-M16)/M16*100,(S17-S16)/S16*100)
          Y18업소수<-c(B18<-nrow(filter(marketData1,법정동명==input$town,상권업종대분류명==input$main)),
                    M18<-nrow(filter(marketData1,법정동명==input$town,상권업종중분류명==input$middle)),
                    S18<-nrow(filter(marketData1,법정동명==input$town,상권업종소분류명==input$sub)))
          Y18증감률<-c((B18-B17)/B17*100,(M18-M17)/M17*100,(S18-S17)/S17*100)
          Y19업소수<-c(B19<-nrow(filter(marketData5,법정동명==input$town,상권업종대분류명==input$main)),
                    M19<-nrow(filter(marketData5,법정동명==input$town,상권업종중분류명==input$middle)),
                    S19<-nrow(filter(marketData5,법정동명==input$town,상권업종소분류명==input$sub)))
          Y19증감률<-c((B19-B18)/B18*100,(M19-M18)/M18*100,(S19-S18)/S18*100)
          data.frame(구분,업종,Y15업소수,Y16업소수,Y16증감률,Y17업소수,Y17증감률,Y18업소수,Y18증감률,Y19업소수,Y19증감률)
          
        })
        
        
        
        output$barplot1<-renderPlot(PlotMarketAll())
        output$barplot2<-renderPlot(PlotMarketDistrict())
        output$barplot3<-renderPlot(PlotMarketTown())
        output$value3<-renderTable(ValMarketTown())
        output$value2<-renderTable(ValMarketDistrict())
        output$value1<-renderTable(ValMarketAll())
        output$timepop<-renderPlot(plotTimePop())
        output$sexpop<-renderPlot(plotsexPop())
        output$agepop<-renderPlot(plotAgePop())
        output$weekpop<-renderPlot(plotWeekPop())
        output$commutepop<-renderPlot(plotcommutePop())
        output$avggain<-renderPlot(plotavggain())
        output$consume<-renderPlot(plotconsum())
        output$mymap<-renderLeaflet({
          
          leaflet()%>%addTiles()%>%addMarkers(lng=mark$경도, lat=mark$위도,popup=content)
        })
        
        
      }
    }
    
    
  })
  
}

