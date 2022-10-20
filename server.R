library(shiny)
library(tidyverse)
body<-read.csv("BodyFat.csv")
body$ABDOMEN_INCH <- body$ABDOMEN/2.54
body1<-body[-c(39,42,48,76,96,182),]
mod <- lm(BODYFAT ~ 0+ABDOMEN_INCH+HEIGHT, data = body1)
body1$fitted <- round(mod$fitted.values,2)
body1$ABDOMEN_INCH <- round(body1$ABDOMEN_INCH,2)

reset_selection <- function(x, brush) {
  brushedPoints(x, brush, allRows = TRUE)$selected_
}

scatter <- function(x, selected_) {
  x %>%
    mutate(selected_ = selected_) %>%
    ggplot() +
    geom_point(aes(BODYFAT, fitted)) +
    geom_abline(slope=1,intercept=0,color='red',size=1)+
    xlim(0,47)+
    ylim(0,47)
}

function(input, output) {
  output$f3 <- renderText('Your estimated BODYFAT is:')
  output$f2 <- renderText(paste(as.character({round(1.62645*input$AB-0.57235*input$H,2)}),'%'))
  output$f1 <- renderText('Our model is: BODYFAT = 1.63*ABDOMEN(inch) - 0.57*HEIGHT(inch)')
  observeEvent(input$do, {
    output$f4 <- renderText('BODYFAT vs Fitted BODYFAT')
    selected <- reactiveVal(rep(TRUE, nrow(body1)))
    observeEvent(
      input$plot_brush,
      selected(reset_selection(body1, input$plot_brush))
    )
    
    output$plot <- renderPlot(scatter(body1, selected()))
    output$table <- renderDataTable(filter(body1, selected())[,c("BODYFAT","ABDOMEN_INCH","HEIGHT","fitted")])
  })
}