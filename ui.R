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

fluidPage(
  titlePanel('BODYFAT caculator'),
  numericInput("AB", "ABDOMEN (in inches)", 0),
  numericInput("H", "HEIGHT (in inches)", 0),
  textOutput("f3"),
  textOutput("f2"),
  tags$head(tags$style("#f2{color: red;
                                 font-size: 30px;
                                 }")),
  textOutput("f1"),
  actionButton("do", "Show Dataset"),
  textOutput("f4"),
  plotOutput("plot", brush = "plot_brush"),
  dataTableOutput("table")
)