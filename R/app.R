library(shinydashboard)

app2 <- shinyApp(
  
  ui = dashboardPage(
    
    dashboardHeader(title = "New Haven Crime Statistics",titleWidth=500),
    
    dashboardSidebar(
        selectInput("crime", "Select Crime:", 
                    crime.vector),
        selectInput("time.agg", "Time aggregation:", 
                    c('day','week','month')),
        sliderInput("opacity", "Opacity:", 
                    min=0, max=1, value=0.02)
      ),
    dashboardBody(
      fluidRow(
                    box(tabPanel("Map", plotOutput("map1")), width=4),
                    box(tabPanel("Time", plotOutput("plot_time")), width=4),
                    box(tabPanel("Date", plotOutput("plot_date")), width=4)
        )
      )
  ), 
  
  server = function(input, output) {
    
    output$map1 = renderPlot({
      p1 <- crimeplot.fun(select.crimes=input$crime,
                          opacity1=input$opacity)
      p1
    })
    
    output$plot_time = renderPlot({
      sub1<-n2[n2$NIBRS_Offe==input$crime,]
      p2 <- ggplot(sub1, aes(x=Hr)) + 
        geom_histogram(binwidth=1)+
        theme_classic()+
        ggtitle('Time of Crime') +
        xlab('Time of Day')
      p2
    })
    
    output$plot_date = renderPlot({
      sub1<-n2[n2$NIBRS_Offe==input$crime,]
      sub1$Date_Occur <- as.Date(sub1$Date_Occur, '%m/%d/%Y')
      sub1$date.week <- floor_date( sub1$Date_Occur, unit=input$time.agg)
      
      sub2 <- sub1 %>%
        group_by(date.week) %>%
        summarize(N_crimes=n())
      sub2 <- sub2[!is.na(sub2$date.week),]
      
      p3 <- ggplot(sub2, aes(x=date.week, y=N_crimes))+
        geom_line() +
        theme_classic() +
        ylab('N crimes')+
        xlab('Date')
      p3
    })
      
  }
)
