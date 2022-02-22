# install.packages('shiny')

library(shiny)

ui <- fluidPage(
    selectInput('dataset',   #사용하는 데이터 셋
                label = '데이터셋',   #페이지의 레이블명
                choices = ls('package:datasets')),   #선택박스 생성
    verbatimTextOutput('summary'),   #output$summary를 verbatimText 형태로 출력
    tableOutput('table'),   #output$table를 table 형태로 출력
)

server <- function(input, output, session) {
    output$summary <- renderPrint({
        dataset <- get(input$dataset, 'package:datasets')
        summary(dataset)
    })
    
    output$table <- renderTable({
        dataset <- get(input$dataset, 'package:datasets')
        dataset
    })
}

shinyApp(ui = ui, server = server)









































