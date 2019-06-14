# input
# file dw1 dw2
# scale1 method1 p1 link k1 h which done1 
# scale2 seed  k2 algorithm iteration nstart done2
# scale3 metric k3 done3 

# output
# mp1 sdp1 sdt1 mmod1 mmod1_1
# mp2 sdp2 sdt2 mod2
# mp3 sdp3 sdt3 mod3
# mtxt1 mdat1

library(shiny)
library(shinydashboard)
library(reshape)
library(DT)
library(ggplot2)
#library(amap)
library(cluster)
library(vegan)

ui <- dashboardPage(
  
  dashboardHeader(title = "clust"),
  
  dashboardSidebar(
    fileInput("file","",accept = ".csv",buttonLabel = "查找", placeholder = "传入文件…"),
    
    sidebarMenu(
      menuItem("层次聚类", tabName = "hclust", icon = icon("line-chart"),
               badgeLabel = "main", badgeColor = "red",selected=T),
      menuItem("k 均值法", tabName = "kmeans", icon = icon("line-chart"),
               badgeLabel = "main", badgeColor = "red"),
      menuItem("k 中心法", tabName = "kmedoids", icon = icon("line-chart"),
               badgeLabel = "test", badgeColor = "orange"),
      menuItem("主数据", tabName = "data", icon = icon("table")),
      menuItem("原理&知识", icon = icon("book"),startExpanded=T,
               menuSubItem("算法公式",tabName = "main_knowledge", icon = icon("blank")),      
               menuSubItem("趣知识",tabName = "fun_knowledge", icon = icon("blank"))
      )
    ),
    
    br(),hr(),br(),
    verbatimTextOutput("mtxt1",placeholder = T),
    br(),
    fluidRow(HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
             downloadButton("dw1","写出模型"),
             downloadButton("dw2","写出结果"))
    ),
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "hclust",fluidPage(
        tabBox(title = tagList(icon("gear"), "模型&误差"),width = 9,side="right",
               tabPanel(title = "主图",plotOutput("mp1")),
               tabPanel(title = "误差图",plotOutput("sdp1")),
               tabPanel(title = "聚类结果",dataTableOutput("sdt1")),
               tabPanel(title = "谱系树",verbatimTextOutput("mmod1_1",placeholder = T)),
               tabPanel(title = "模型",verbatimTextOutput("mmod1",placeholder = T))                   
        ),
        box(title = "聚类控制",width = 3,collapsible = T,
            verticalLayout(checkboxInput("scale1","标准化",T),
                           selectInput("method1","距离算法",
                                       c("manhattan","euclidean","maximum","minkowski",
                                         "mahalanobis","canberra","binary","jaccard",
                                         "correlation","pearson","spearman",
                                         "abscorrelation","abspearson","kendall"),
                                       "euclidean"),
                           selectInput("link","连接方法",
                                       c("ward.D","ward.D2","single","complete",
                                         "average","median","mcquitty",
                                         "centroid")),
                           numericInput("p1","闵氏距离的乘方项",3,3,100,1),
                           numericInput("k1","分组数目",4,2,10,1)
            ),
            hr(),
            splitLayout(numericInput("h","子分支高度",3,0,1000,.01),
                        numericInput("which","子分支组别",2,1,200,1)
            ),
            actionButton("done1","确定")
        )
      )),
      
      tabItem(tabName = "kmeans",fluidPage(
        tabBox(title = tagList(icon("gear"), "模型&误差"),width = 9,side="right",
               tabPanel(title = "主图",plotOutput("mp2")),
               tabPanel(title = "误差图",plotOutput("sdp2")),
               tabPanel(title = "聚类结果",dataTableOutput("sdt2")),
               tabPanel(title = "模型",verbatimTextOutput("mmod2",placeholder = T))
        ),
        box(title = "聚类控制",width = 3,collapsible = T,
            verticalLayout(checkboxInput("scale2","标准化",T),
                           numericInput("seed","设定随机数种子",0,10000,1),
                           selectInput("algorithm","聚类算法",
                                       c("Hartigan-Wong", "Lloyd","MacQueen")),
                           numericInput("k2","分组数目",4,2,10,1),
                           numericInput("iteration","最大迭代次数",10,2,100,1),
                           numericInput("nstart","最大重复次数",1,1,100,1),
                           actionButton("done2","确定")
            )
        )
      )),
      
      tabItem(tabName = "kmedoids",fluidPage(
        tabBox(title = tagList(icon("gear"), "模型&误差"),width = 9,side="right",
               tabPanel(title = "主图",plotOutput("mp3")),
               tabPanel(title = "误差图",plotOutput("sdp3")),
               tabPanel(title = "聚类结果",dataTableOutput("sdt3")),
               tabPanel(title = "模型",verbatimTextOutput("mmod3",placeholder = T))
        ),
        box(title = "聚类控制",width = 3,collapsible = T,
            verticalLayout(checkboxInput("scale3","标准化",T),
                           selectInput("metric","聚类算法",c("euclidean","manhattan")),
                           numericInput("k3","分组数目",4,2,10,1),
                           actionButton("done3","确定")
            )
        )
      )),
      
      tabItem(tabName = "data",
              box(title = "当前数据",solidHeader = T,width = 12,height = "580px",
                  dataTableOutput("mdat1"))
      ),
      
      tabItem(tabName = "main_knowledge",
              HTML("<center><h1>聚类中的距离与聚类过程计算公式<h1></center>"),
              tags$iframe(src="hclust.html",width="100%",height= "560px")
      ),
      
      tabItem(tabName = "fun_knowledge",
              fluidPage(
                HTML("<font size=6><center>聚类分析知多少</center></font>"),
                fluidRow(
                  box(status = "info",width = 5,
                      solidHeader = T,collapsible = T,collapsed = T,
                      title = "欧氏距离",img(src="euclidean.png")),
                  box(width = 2,background = "navy",
                      HTML("<font size=6><center>VS</center></font>")),
                  box(status = "primary",width = 5,
                      solidHeader = T,collapsible = T,collapsed = T,
                      title = "马氏距离",img(src="Mahalanobis.png"))
                ),
                fluidRow(
                  column(width = 3,
                         box(status = "warning",width = 12,height="400px",
                             solidHeader = T,collapsible = T,
                             title = "层次聚类 VS 动态聚类",
                             p(strong("层次聚类"),"的基本思想是，将n个样本各自作为一类，
                               并规定样本之间的距离与类与类之间的距离，
                               然后将距离最近的两类合并成一个新类，
                               计算新类与其它类的距离，重复进行两个新类的合并，
                               每次减少一类，直至所有的样本合并为一类。"),
                             p(strong("动态聚类"),"的基本思想是，开始先随机抽取k个质心，
                               按照最小距离准则先粗略的分一下类，
                               然后按照某种最优原则移动质心，修改不合理的分类，
                               直至分得比较合理为止，形成一个最终的分类结果。"))
                             ),
                  box(status = "danger",width = 5,solidHeader = T,collapsible = T,
                      title = "动态聚类示意",img(src="kmeans.jpg")),
                  column(width = 4,
                         box(status = "success",width = 12,solidHeader = T,collapsible = T,
                             title = "层次聚类示意",img(src="hclust.jpg",width=280)),
                         box(status = "warning",width = 12,
                             solidHeader = T,collapsible = T,height="170px",
                             title = "分类 VS 聚类",
                             p(strong("分类"),"是根据一些给定的已知类别的样本，
                               拟合某种目标函数，
                               使它能够对未知类别的样本进行分类。
                               属于有监督的学习"),
                             p(strong("聚类"),"是通过某种算法把一组未知类别
                               的样本划分成若干类别,
                               属于无监督的学习。"))
                             ))
                             ))
                         ))
  )


server <- function(input, output) {
  
  md<-read.csv("./mtcars.csv",stringsAsFactors=F)

  output$mtxt1<-renderText({
    if(!is.null(input$file)){
      tmp <-read.csv(input$file$datapath, stringsAsFactors=F)
      colnames(tmp)[1]<-"id"
      md <<- tmp
      paste("当前传入文件： ",input$file$name)
    }
  })
  
  output$mdat1<-renderDataTable({
    datatable(md,rownames = F)
  })
  
  mdist<-function(dat,method,p=3){
    if(method == "jaccard"){
      dat<-as.matrix(dat)
      d<-data.frame()
      for(i in 1:nrow(dat)){
        for(j in i:nrow(dat)){
          a <- dat[i,]==dat[j,]
          d[j,i]<-sum(!a,na.rm = T)/length(a[!is.na(a)])
        }
      }
      d<-as.dist(d)
    }else if(method == "mahalanobis"){
      dat<-as.matrix(dat)
      d<-data.frame()
      mcov <- cov(dat)
      for(i in 1:nrow(dat)){
        for(j in i:nrow(dat)){
          d[j,i]<-sqrt((dat[i,] - dat[j,]) %*% mcov %*% (dat[i,] - dat[j,]))
        }
      }
      d<-as.dist(d)
    }else if(method == "minkowski"){
      d<-dist(dat,method = "minkowski",p=p)
    }else
      d<-dist(dat,method = method)
    return(d)
  }
  
  observeEvent(input$done1,{
    
    if(input$scale1)md[,-1]<-scale(md[,-1])
    d<-mdist(md[,-1],input$method1,input$p1)
    
    mod<<-hclust(d,input$link)
    mod$labels<<-as.character(md[,1])
    mod$cutree<<-cutree(mod,input$k1)
    mod$dist.method<<-input$method1
    mod$dend<<-as.dendrogram(mod)
    
    group<<-as.factor(mod$cutree)
    
    output$mp1 <- renderPlot({
      
      mlayout<-matrix(rep(1,9),3)
      mlayout[1,1]<-2
      
      op<-par(no.readonly = T)
      
      layout(mlayout)
      
      par(mar=c(2,1,3,10))
      plot(cut(mod$dend,input$h)$lower[[input$which]],horiz = T)

      par(mar=c(0,3,2,0))
      plot(mod,labels = F)
      rect.hclust(mod, k = input$k1,border = 3)
      rect.hclust(mod,h = input$h,border = 8)
      rect.hclust(mod,h = input$h,which = input$which)
      graphics::box(which = "figure",col="gray",lwd=3)     
      graphics::box(which = "figure",col="black",lwd=1)     
      
      par(op)
    })
    
    output$sdp1 <- renderPlot({
      md$group<-group
      md1<-melt(md,id=c("id","group"))
      ggplot(md1,aes(variable,value,group=id,color=group))+geom_line(alpha=.2)
    })
    
    output$sdt1<-renderDataTable({
      datatable(as.data.frame(mod$cutree),options = list(scroll=T,scrollY=350))
    })
    
    output$mmod1_1<-renderPrint(str(mod$dend))
    output$mmod1<-renderPrint(str(mod[1:8]))
    
  })
  
  observeEvent(input$done2,{
    
    if(input$seed != 0)set.seed(input$seed)
    if(input$scale2)md[,-1]<-scale(md[,-1])
    
    mod<<-kmeans(md[,-1],input$k2,iter.max = input$iteration,nstart = input$nstart,
                 algorithm = input$algorithm)
    names(mod$clust)<<-md[,1]
    
    group<<-as.factor(mod$cluster)
    
    fit <- cascadeKM(md[,-1], 1, 10, iter = 1000)
    
    output$mp2 <- renderPlot({
      plot(fit,sortg = T)
    })
    
    output$sdp2 <- renderPlot({    
      md$group<<-group
      md1<-melt(md,id=c("id","group"))
      ggplot(md1,aes(variable,value,group=id,color=group))+geom_line(alpha=.2)
    })
    
    output$sdt2<-renderDataTable({
      datatable(as.data.frame(mod$cluster),options = list(scroll=T,scrollY=350))
    })
    
    output$mmod2<-renderPrint(str(mod))
    
  })
  
  observeEvent(input$done3,{
    
    if(input$scale3)md[,-1]<-scale(md[,-1])
    
    mod<<-pam(md[,-1],input$k3,metric = input$metric)
    
    group<<-as.factor(mod$clustering)
    
    output$mp3 <- renderPlot({
      layout(matrix(c(1,2),1))
      plot(mod)
      layout(matrix(1))
    })
    
    output$sdp3 <- renderPlot({ 
      md$group<-group
      md1<-melt(md,id=c("id","group"))
      ggplot(md1,aes(variable,value,group=id,color=group))+geom_line(alpha=.2)
    })
    
    output$sdt3<-renderDataTable({
      datatable(as.data.frame(mod$clustering),options = list(scroll=T,scrollY=350))
    })
    
    output$mmod3<-renderPrint(str(mod))
    
  })
  
  output$dw1 <- downloadHandler(
    filename = function() {
      paste('model_', Sys.Date(), '.txt', sep='')
    },
    content = function(con) {
      sink(con)
      options(max.print = 100000)
      print("______总体结果______",quote = F)
      cat("\n")
      str(mod)
      cat("\n","\n","\n","\n")
      print("______具体结果______",quote = F)
      cat("\n")
      for(i in 1:length(mod)){
        print(names(mod[i]),quote = F)
        cat("\n")
        print(mod[[i]])
        cat("\n","\n","\n","\n")
        }
      sink(NULL)
    },
    contentType = "text/csv"
  )
  output$dw2 <- downloadHandler(
    filename = function() {
      paste("result_", Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(cbind(md,group), con, row.names = F)
    },
    contentType = "text/csv"
  )
  
}

shinyApp(ui, server)



