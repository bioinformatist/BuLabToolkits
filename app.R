library(shiny)
library(data.table)

# This function is copied from https://stackoverflow.com/a/19786996
freadClip <- function(...) {
  X <- tempfile()
  writeLines(readLines("clipboard"), X)
  fread(X, ...)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("复制行菌"),
   
   tags$p('切换窗口到表格操作工具内（比如MS Excel）,',
          tags$br(),
          '拖动鼠标选取一块区域，复制到剪贴板（Ctrl-C），',
          tags$br(),
          '然后回到该界面点击粘贴按钮'),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         actionButton("pastedDone",
                      "粘贴！",
                      icon('clipboard')),
         
         uiOutput('pickCol')
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tableOutput('pastedTable'),
        tableOutput('repeatedTable')
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$pastedDone, {
    pastedData <- reactive({
      freadClip()
    })
    
    output$pastedTable <- renderTable({
      pastedData()
    }, caption = '原表格')
    
    output$pickCol <- renderUI({
      tagList(
        selectInput('selectedCol', '请选出帅气的一列指定重复次数：', colnames(pastedData())),
        actionButton('doRepetition',
                     '重复！',
                     icon('files-o')),
        checkboxInput('delCol', '删除这帅气的列吧！', TRUE)
      )
    })
    
    observeEvent(input$doRepetition, {
      times <- reactive({
        input$seletedCol
      })
      
      output$repeatedTable <- renderTable({
        if (input$delCol) {
          pastedData()[rep(1:.N, times)][, times := NULL]
        } else {
          pastedData()[rep(1:.N, times)]
        }
      })
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

