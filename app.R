library(shiny)
library(bslib)
library(ggplot2)
library(viridis)
library(dplyr)
library(stringr)
library(hrbrthemes)

ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "cerulean"),
  tags$head(HTML("
                 <title>爱丽丝暴击了吗</title> <link rel='icon' type='png' href='logo.png'> 
    ")), #tags$title also creates the <title></title> element
  # tags$img(
  #   src = "arisu.jpg",
  #   style = 'position: absolute'
  # ),
  
  titlePanel(h1("爱丽丝今天暴击了吗？",align="middle",
                style="background-color:#2893e0;
                padding:0px;
                color:white;
                padding-left:15px;
                margin-left:-15px;
                margin-right:-15px;
                ")),
  hr(),
  
  sidebarLayout(
    ## 左面板UI -----
    sidebarPanel(
      p(
        img(src="arisu.jpg",height=120,width=180),
        align="middle"
      ),
      h2("就不暴击",align="middle"),
      br(),
      div(
        fluidRow(
          column(6,actionButton("crit","暴击了！😎")),
          column(6,actionButton("nocrit","没暴击！😅"))
        ),
        align="middle"
      ),
      br(),
      div(
        column(12,plotOutput("barplot",height="220px",inline=T)),
        align="middle"
      ),
      br(),
      div(
        plotOutput("lineplot",height="220px",inline=T),
        align="middle"
      ),
      br(),
      conditionalPanel(
        condition = "output.showDownload",
        br(),
        helpText("这个暴击率你满意了吗"),
        downloadButton("download",label="保存战果")
      )
    ),
    ## 右面板UI -----
    mainPanel(
      fluidRow(
        column(12,dataTableOutput("table"))
      ),
      hr(),
      div(
        fluidRow(
          column(4,tableOutput("table2")),
          column(4,textOutput("timeelapsed")),
          column(4,textOutput("critrate"))
        ),
        align="middle"
      ),
      hr(),
      fluidRow(
        column(12,dataTableOutput("table3"))
      )
    )
  )
)

server <- function(input,output){
  crit_record <- reactiveVal(
    data.frame(time=character(),crit=integer(),stringsAsFactors = FALSE)
  )
  
  # Add 1 crit record
  observeEvent(input$crit,{
    crit_record() %>% 
      add_row(
        time=Sys.time() %>% str_sub(.,start=1),
        crit=1
      ) %>% 
      crit_record()
  })
  # Add 1 no crit record
  observeEvent(input$nocrit,{
    crit_record() %>% 
      add_row(
        time=Sys.time() %>% str_sub(.,start=1),
        crit=0
      ) %>% 
      crit_record()
  })
  
  ## 暴击计数表 -----
  output$table <- renderDataTable({
    req(crit_record(),(input$crit>0|input$nocrit>0))
    
    a <- crit_record()
    colnames(a) <- c("时间","票要炸了,你暴击了吗")
    return(a)
  },options = list(paging=FALSE,
                   scrollY=200,
                   searching=FALSE,
                   info=FALSE,
                   autoWidth=TRUE,
                   columnDefs = list(list(width = '300px', targets = "_all"))))
  
  crit_freq <- reactive({
    req(nrow(crit_record())>0,input$crit>0|input$nocrit>0)
    
    tbl <- table(crit_record() %>% select(crit)) %>% as.data.frame()
    colnames(tbl) <- c("crit","freq")
    tbl$crit <- factor(tbl$crit)
    return(tbl)
  })
  
  crit_rate <- reactive({
    req(nrow(crit_record())>0,crit_freq())
    
    if (nrow(crit_freq())==1 & crit_freq()[1,1]==1) {
      rate <- 1
    } else if (nrow(crit_freq())==1 & crit_freq()[1,1]==0){
      rate <- 0
    } else {
      rate <- crit_freq()[2,2]/sum(crit_freq()[,2])
    }
    rate <- paste("暴击率",round(rate,4)*100,"%",sep="")
    return(rate)
  })
  
  crit_rate_record <- reactiveVal(
    data.frame(time=character(),rate=numeric())
  )
  
  observeEvent(crit_rate(),{
    crit_rate_record() %>% 
      add_row(
        time=crit_record()[nrow(crit_record()),1],
        rate=as.numeric(str_extract(crit_rate(),"\\d+"))
      ) %>% 
      crit_rate_record()
  })
  
  ## 暴击率表 -----
  output$table3 <- renderDataTable({
    req(crit_rate(),crit_rate_record())
    
    a <- crit_rate_record()
    colnames(a) <- c("时间","暴击率")
    
    return(a)
  },options = list(paging=FALSE,
                    scrollY=200,
                    searching=FALSE,
                    info=FALSE,
                    autoWidth=TRUE,
                    columnDefs = list(list(width = '300px', targets = "_all"))))
  
  ## 暴击率图 -----
  crit_plot <- reactive({
    req(nrow(crit_rate_record())>1)
    
    a <- crit_rate_record() %>% 
      mutate(ind = seq_along(rate))
    plt <- ggplot(a,aes(x=ind,y=rate)) +
      geom_line(color="#f45a5a") +
      geom_point(color="#f45a5a",shape=1,size=3,stroke=1) +
      ggtitle("你们有这样的暴击率吗") +
      ylab("暴击率(%)") +
      theme_ipsum() +
      theme(axis.line = element_line(colour = "black", linewidth = 0.5,
                                     linetype = "solid"),
            plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"),
            plot.title = element_text(hjust = 0.5,size=16),
            axis.text.x = element_blank(),
            axis.title.x = element_blank())
    
    return(plt)
  })
  
  output$lineplot <- renderPlot({
    req(crit_plot())
    
    crit_plot()
  },height=200,width=250)
  
  ## 暴击率播报 -----
  output$critrate <- renderText({
    req(crit_rate())
    
    crit_rate()
  })
  
  ## 暴击次数表 -----
  output$table2 <- renderTable({
    a <- crit_freq()
    colnames(a) <- c("暴击了吗","暴了几次")
    
    return(a)
  },rownames = F, digits = 2, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  timeelapsed <- reactive({
    req(nrow(crit_record()>1))
    
    time_elapsed <- difftime(crit_record()[nrow(crit_record()),1],crit_record()[1,1],unit="mins") %>% as.numeric() %>% round(.,digits=2)
    
    return(time_elapsed)
  })
  
  ## 时间经过播报 -----
  output$timeelapsed <- renderText({
    req(timeelapsed())
    
    a <- paste("🔔已经过",timeelapsed(),"分钟")
    return(a)
  })
  
  ## 柱状图 -----
  freq_plot <- reactive({
    req(crit_record(),crit_freq())
    plt <- ggplot(crit_freq(),aes(x=crit,y=freq,fill=crit)) +
      geom_bar(stat="identity",width=0.4) +
      xlab("暴击了吗") +
      ylab("暴了几次") +
      labs(fill="你暴了吗") +
      ggtitle("爱丽丝你这把有没有好好打？") +
      theme_ipsum() +
      theme(axis.line = element_line(colour = "black", linewidth = 0.5,
                                     linetype = "solid"),
            plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"),
            plot.title = element_text(hjust = 0.2,size=16),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1))
    
    return(plt)
  })
  
  output$barplot <- renderPlot({
    req(freq_plot())
    
    freq_plot()
  },height=200,width=250)
  
  # Download button
  outputName <- reactive({
    req(nrow(crit_record()>0),timeelapsed())
    
    paste("本人于",crit_record()[nrow(crit_record()),1],"凹出爱丽丝",crit_rate(),"耗时",timeelapsed(),"分钟","望周知.csv",sep="")
  })
  
  output$showDownload <- reactive({
    req(crit_record(),crit_freq(),crit_rate(),outputName())
    TRUE
  })
  outputOptions(output, "showDownload", suspendWhenHidden = FALSE)
  
  output$download <- downloadHandler(
    filename = function() {
      outputName()
    },
    content = function(file) {
      write.csv(crit_record(), file, row.names = TRUE)
    }
  )
}

shinyApp(ui = ui, server = server)