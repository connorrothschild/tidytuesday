library(shiny)
library(ggplot2)
library(plotly)

theme_set(theme_minimal())

source("./source_code_for_shiny.R")

ui <- fluidPage(
  
  # Define UI for application 
    titlePanel(
      h1("A College Degree Lowers Job Automation Risk",
        style = "font-family: 'Helvetica Neue';
        font-size: 20px; font-weight: 500; line-height: 1.1;")),
           # plot object for intro
    mainPanel(
      fluidRow(
        div(
           plotlyOutput("plot", height = '600px', width = '850px')
           ),align="left"),
           # hr(),
           p("DATA: FREY & OSBORNE, BUREAU OF LABOR STATISTICS",
           style = "font-family: 'Helvetica Neue';
        font-size: 8px; font-weight: 500; line-height: 1.1;")
           )  

)

# server
server <- function(input, output, session) {
  
  output$plot <- renderPlotly ({
    
  introggPlot <- data %>% 
    mutate(text = glue::glue('<span style="font-size:16px;font-weight:bold">{data$occupation}</span>',
                             '\n<b>Number employed:</b> {scales::comma(data$TOT_EMP)}',
                             '\n<b>Computerization prob:</b> {data$probability}%',
                             '\n<b>Education:</b> {data$typicaled}',
                             sep = "\n")) %>% 
    filter(typicaled != "Some college, no degree") %>%
    ggplot(aes(x=probability, y=A_MEDIAN, size=TOT_EMP, fill=typicaled, text = text)) +
    geom_point(color = "black", alpha = .97, stroke = .1) +
    scale_size(range = c(1, 10), guide = 'legend') +
    # this is the hackiest thing i've ever done i'm so sorry
    geom_segment(aes(x = 0, y = 54000, xend = 100, yend = 58000), size = .1) +
    # ylab("Median Income") +
    xlab("") +
    # xlab("\nProbability of Automation") +
    # ggtitle("Likelihood of Job Automation vs Median Income") +
    labs(size= "", alpha = "", fill = "") + 
    scale_fill_manual(values = cols, labels = c('No formal educational credential', 'High school diploma or equivalent', "Some college, no degree",
                                                 "Associate's degree", "Postsecondary nondegree award",
                                                 "Bachelor's degree", "Master's degree",
                                                 "Doctoral or professional degree")) +
    scale_y_continuous(
      # labels=scales::dollar_format(prefix="$"), 
      # adding limit to be -10000 for extra padding so i can keep text annotations below plot
                      limits = c(-1000,240000), 
                       breaks = c(20000, 40000, 60000, 80000, 100000, 120000, 140000, 160000, 180000, 200000, 220000),
                       labels = c("20K", "40K", "60K", "80K", "100K", "120K", "140K", "160K", "180K", "200K", "220K")) +
    scale_x_continuous(
      # labels=scales::number_format(suffix="%"), 
                      limits = c(0,100), 
                       breaks = c(10,20,30,40,50,60,70,80,90),
                       labels = c(10,20,30,40,50,60,70,80,"90%")) +
    theme(axis.line.x = ggplot2::element_blank(), 
          axis.line.y = ggplot2::element_blank(),
          axis.text = element_text(colour = "black", size = 8))
  
  # Convert into ggplotly
  introPlot <- ggplotly(introggPlot, tooltip = 'text', sort = FALSE) %>%
    layout(
      legend = list(orientation = "h",   
                     xanchor = "left",
                      x = 0, y = 100,
                     traceorder = "normal",
                     itemsizing = "constant",
                     tracegroupgap = 0,
                     font = list(size = 9.5)),
      font = list(family = 'Helvetica Neue',
                  color = "black"),
      margin = list(r=10, l=10, pad = 0),
      hoverlabel = list(bgcolor = 'white', color = 'black'),
      yaxis = list(
        tickfont = element_blank(),
        overlaying = "y",
        side = "right",
        title = ""
      )) %>% 
    add_annotations(
      x = 1, y = 7500,
      xref = "x",
      yref = "y",
      text = "<b>Low paid,\nleast vulnerable</b>",
      xanchor = 'center',
      align = 'left',
      font = list(size = 10), 
      showarrow = F
    ) %>%
    add_annotations(
      x = 100, y = 7500,
      xref = "x",
      yref = "y",
      text = "<b>Low paid,\nmost vulnerable</b>",
      xanchor = 'center',
      align = 'right',
      font = list(size = 10), 
      showarrow = F
    ) %>%
    add_annotations(
      x = 1, y = 230000,
      xref = "x",
      yref = "y",
      text = "<b>Best paid,\nleast vulnerable</b>",
      xanchor = 'center',
      align = 'left',
      font = list(size = 10), 
      showarrow = F
    ) %>%
    add_annotations(
      x = 100, y = 230000,
      xref = "x",
      yref = "y",
      text = "<b>Best paid,\nmost vulnerable</b>",
      xanchor = 'center',
      align = 'right',
      font = list(size = 10), 
      showarrow = F
    ) %>%
    # least, most likely to be automated, etc
    add_annotations(
      x = -5, y = -5000,
      xref = "x",
      yref = "y",
      text = glue::glue(sprintf('\u2190'), "Least likely to be automated"),
      xanchor = 'left',
      align = 'left',
      font = list(size = 10), 
      showarrow = F
    ) %>%
    add_annotations(
      x = 105, y = -5000,
      xref = "x",
      yref = "y",
      text = glue::glue("Most likely to be automated", sprintf('\u2192')),
      xanchor = 'right',
      align = 'right',
      font = list(size = 10), 
      showarrow = F
    ) %>%
    add_annotations(
      x = 99, y = 243000,
      xref = "x",
      yref = "y",
      text = "Average annual wage",
      xanchor = 'center',
      align = 'right',
      font = list(size = 10), 
      showarrow = F
    ) %>%
    add_annotations(
      x = subset(data$probability, data$occupation == "Chief Executives"),
      y = subset(data$A_MEDIAN, data$occupation == "Chief Executives"),
      text = "Chief Executives",
      xref = "x",
      yref = "y",
      xanchor = 'center',
      align = 'right',
      font = list(size = 10), 
      showarrow = TRUE,
      arrowhead = 0,
      ax = 50,
      ay = 25
    ) %>% 
    add_annotations(
      x = subset(data$probability, data$occupation == "Cashiers"),
      y = subset(data$A_MEDIAN, data$occupation == "Cashiers"),
      text = "Cashiers",
      xref = "x",
      yref = "y",
      xanchor = 'center',
      align = 'right',
      font = list(size = 10), 
      showarrow = TRUE,
      arrowhead = 0,
      ax = 40,
      ay = -50
    ) %>% 
    config(displaylogo = F, showSendToCloud = F, displayModeBar = F)

  })
}
# Run the application
shinyApp(ui = ui, server = server)
