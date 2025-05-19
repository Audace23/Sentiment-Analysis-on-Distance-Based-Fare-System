# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))
#Install packages
install.packages(c("shiny", "plotly",
                   "wordcloud2", "bs4Dash",
                   "dplyr")
                 )
library(shiny)
library(plotly)
library(wordcloud2)
library(bs4Dash)
library(dplyr)
#Load data
frequent_words<- read.csv("www/frequent_words.csv")
all_with_sentiment<- read.csv("www/all_with_sentiment.csv")
all_with_sentiment_pie<- read.csv("www/all_with_sentiment_pie.csv")
#===============================================================================
# Building the word_cloud
set.seed(123)
frequent_words <- frequent_words %>% 
  select(2,3)
Word_Cloud<-wordcloud2(data = frequent_words, size = 1.5, color = 'random-light', backgroundColor = "navy")
Word_Cloud

# Function to create pie chart
create_pie <- function(df) {
  all_with_sentiment_pie %>%
    count(sentiment) %>%
    mutate(
      sentiment = factor(sentiment, levels = c("positive", "negative")),
      color = case_when(
        sentiment == "positive" ~ "#28a745",
        sentiment == "negative" ~ "#dc3545",
        TRUE ~ "#999999"
      )
    ) %>%
    plot_ly(
      labels = ~sentiment,
      values = ~n,
      type = "pie",
      marker = list(colors = ~color)
    )
}

# Build a list of pie charts by source
pie_charts <- list()
unique_sources <- unique(all_with_sentiment$source)

for (src in unique_sources) {
  pie_charts[[src]] <- create_pie(all_with_sentiment_pie %>% filter(source == src))
}


#===============================================================================
#UI Section 
ui_hack <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Snowfall animation */
      body {
        background: #ffffff;
      }

      #title-box {
        position: relative;
        overflow: hidden;
      }

      #snowflakes {
        position: absolute;
        width: 100%;
        height: 100%;
        pointer-events: none;
        z-index: 0;
      }

      .snowflake {
        color: white;
        font-size: 1.5em;
        position: absolute;
        top: -10px;
        animation: fall 10s linear infinite;
        z-index: 0;
      }

      @keyframes fall {
        0% { transform: translateY(0); }
        100% { transform: translateY(100vh); }
      }

      .snowflake:nth-child(1) { left: 5%; animation-delay: 0s; }
  .snowflake:nth-child(2) { left: 15%; animation-delay: 1s; }
  .snowflake:nth-child(3) { left: 25%; animation-delay: 2s; }
  .snowflake:nth-child(4) { left: 35%; animation-delay: 0.5s; }
  .snowflake:nth-child(5) { left: 45%; animation-delay: 1.5s; }
  .snowflake:nth-child(6) { left: 55%; animation-delay: 2.5s; }
  .snowflake:nth-child(7) { left: 65%; animation-delay: 0.8s; }
  .snowflake:nth-child(8) { left: 75%; animation-delay: 1.8s; }
  .snowflake:nth-child(9) { left: 85%; animation-delay: 2.8s; }
  .snowflake:nth-child(10) { left: 10%; animation-delay: 0.3s; }
  .snowflake:nth-child(11) { left: 40%; animation-delay: 1.3s; }
  .snowflake:nth-child(12) { left: 90%; animation-delay: 2.3s; }
    "))
  ),
  
  # ROW 1: Title Header
  fluidRow(
    tags$div(
      id = "title-box",
      style = "background-color: #CADBEE; padding: 20px; border-top: 3px solid #044ee3; border-radius: 10px; width: 100%; position: relative;",
      tags$div(id = "snowflakes",
               span(class = "snowflake", "❄"),
               span(class = "snowflake", "❄"),
               span(class = "snowflake", "❄"),
               span(class = "snowflake", "❄"),
               span(class = "snowflake", "❄"),
               span(class = "snowflake", "❄")
      ),
      tags$div(
        "PUBLIC SENTIMENT ON DISTANCE-BASED TRANSPORT FARE IN RWANDA",
        style = "text-align: center; font-weight: bold; font-size: 25px; color: #112a46; position: relative; z-index: 1;"
      )
    )
  ),
  
  # ROW 2: Two half-width visual boxes
  fluidRow(
    column(6,
           bs4Dash::box(
             title = tags$div("SENTIMENT DISTRIBUTION BY SOURCE",
                              style = "text-align: center; font-weight: bold; font-size: 18px; color: #112a46;"),
             width = 12,
             solidHeader = TRUE,
             status = NULL,
             style = "border-top: 3px solid #044ee3;",
             collapsible = FALSE,
             selectInput("sourceInput", "Choose Data Source", 
                         choices = c("All", "News", "Social Media"),
                         selected = "All"),
             plotlyOutput("sentimentPie", height = "300px"))
    ),
    column(6,
           bs4Dash::box(
             title = tags$div("SENTIMENT DEPENDENCY ANALYSIS",
                              style = "text-align: center; font-weight: bold; font-size: 18px; color: #112a46;"),
             width = 12,
             solidHeader = TRUE,
             status = NULL,
             style = "border-top: 3px solid #044ee3;",
             collapsible = FALSE,
             
             tags$h4("Sentiment Distribution by Source", 
                     style = "text-align: center; margin-top: 10px; font-size: 16px; color: #112a46;"),
             
             tags$div(
               style = "padding: 10px; border-radius: 5px; background-color: #f8f9fa;",
               tags$table(
                 style = "width: 100%; border-collapse: collapse;",
                 tags$thead(
                   tags$tr(
                     tags$th(style = "border: 1px solid #ddd; padding: 8px; text-align: left; background-color: #f2f2f2;", ""),
                     tags$th(style = "border: 1px solid #ddd; padding: 8px; text-align: center; background-color: #f2f2f2;", "positive"),
                     tags$th(style = "border: 1px solid #ddd; padding: 8px; text-align: center; background-color: #f2f2f2;", "neutral"),
                     tags$th(style = "border: 1px solid #ddd; padding: 8px; text-align: center; background-color: #f2f2f2;", "negative")
                   )
                 ),
                 tags$tbody(
                   tags$tr(
                     tags$td(style = "border: 1px solid #ddd; padding: 8px; font-weight: bold;", "News Articles"),
                     tags$td(style = "border: 1px solid #ddd; padding: 8px; text-align: center;", "27"),
                     tags$td(style = "border: 1px solid #ddd; padding: 8px; text-align: center;", "307"),
                     tags$td(style = "border: 1px solid #ddd; padding: 8px; text-align: center;", "7")
                   ),
                   tags$tr(
                     tags$td(style = "border: 1px solid #ddd; padding: 8px; font-weight: bold;", "Social Media"),
                     tags$td(style = "border: 1px solid #ddd; padding: 8px; text-align: center;", "8"),
                     tags$td(style = "border: 1px solid #ddd; padding: 8px; text-align: center;", "163"),
                     tags$td(style = "border: 1px solid #ddd; padding: 8px; text-align: center;", "13")
                   ),
                   tags$tr(
                     tags$td(style = "border: 1px solid #ddd; padding: 8px; font-weight: bold;", "Total"),
                     tags$td(style = "border: 1px solid #ddd; padding: 8px; text-align: center;", "35"),
                     tags$td(style = "border: 1px solid #ddd; padding: 8px; text-align: center;", "470"),
                     tags$td(style = "border: 1px solid #ddd; padding: 8px; text-align: center;", "20")
                   )
                 )
               )
             ),
             
             tags$div(
               style = "margin-top: 20px; padding: 10px; background-color: #f8f9fa; border-left: 3px solid #044ee3;",
               tags$h4("Statistical Analysis", 
                       style = "margin-top: 0; font-size: 16px; color: #000000; text-decoration: underline; font-weight : bold"),
               tags$p(style = "margin-bottom: 0;",
                      "Using Chi-Square test at 95% level of significance (χ² = 10.19, p-value = 0.0373), there is a statistically significant dependence between sentiment and the platform through which the message is sent. This suggests that the distribution of positive, negative, and neutral sentiments differs significantly between articles and social media posts regarding distance-based transport fares in Rwanda.")
             )
           )
    )
  ),
  # ROW 3: Word Cloud + Placeholder
  fluidRow(
    column(4,
           bs4Dash::box(
             title = tags$div("INFRASTRUCTURE",
                              style = "text-align: center; font-weight: bold; font-size: 18px; color: #112a46;"),
             width = 12,
             solidHeader = TRUE,
             status = NULL,
             style = "border-top: 3px solid #044ee3;",
             collapsible = FALSE,
             tags$div(
               style = "text-align: center;",
               # Image with responsive sizing
               tags$img(
                 src = "Aside.png",  # Make sure the image is in your www/ folder
                 style = "max-width: 100%; height: auto; margin-bottom: 15px;",
                 alt = "Commuters in Kigali"
               ),
               #  a clickable "Read more" link
               tags$div(
                 style = "margin-top: 10px;",
                 tags$a(
                   href = "https://www.newtimes.co.rw/article/22352/news/infrastructure/commuters-all-smiles-as-kigali-launches-distance-based-fare-system",  # Same URL as above
                   target = "_blank",
                   "Read full article →",
                   style = "color: #044ee3; font-size: 14px;"
                 )
               )
             )
           )
    ),
    column(8,
           bs4Dash::box(
             title = tags$div("FREQUENTLY USED WORDS IN PUBLIC REACTIONS",
                              style = "text-align: center; font-weight: bold; font-size: 18px; color: #112a46;"),
             width = 12,
             solidHeader = TRUE,
             status = NULL,
             style = "border-top: 3px solid #044ee3;",
             collapsible = FALSE,
             # Adding CSS to ensure the wordcloud fills the container
             tags$style(HTML("
           #wordCloud {
             width: 100% !important;
             height: 400px !important;
           }
           .wordcloud2 > canvas {
             width: 100% !important;
             height: 100% !important;
           }
         ")),
             wordcloud2Output("wordCloud", height = "400px", width = "100%")
           )
    )
  ),
  # ROW 4: Top Concerns and Misconceptions
  fluidRow(
    bs4Dash::box(
      width = 12,
      solidHeader = TRUE,
      status = NULL,
      style = "border-top: 3px solid #044ee3;",
      collapsible = FALSE,
      fluidRow(
        column(6,
               tags$div("Top 5 Concerns", style = "font-weight: bold; font-size: 16px;"),
               tags$ul(
                 tags$li(textOutput("concern1")),
                 tags$li(textOutput("concern2")),
                 tags$li(textOutput("concern3")),
                 tags$li(textOutput("concern4")),
                 tags$li(textOutput("concern5"))
               )
        ),
        column(6,
               tags$div("Top 5 Misconceptions", style = "font-weight: bold; font-size: 16px;"),
               tags$ul(
                 tags$li(textOutput("misconception1")),
                 tags$li(textOutput("misconception2")),
                 tags$li(textOutput("misconception3")),
                 tags$li(textOutput("misconception4")),
                 tags$li(textOutput("misconception5"))
               )
        )
      )
    )
  ),
  
  # ROW 5: Recommendations Box
  fluidRow(
    bs4Dash::box(
      solidHeader = TRUE,
      width = 12,
      style = "background-color: #CADBEE; padding: 20px; border-top: 3px solid #044ee3; border-radius: 10px; width: 100%; position: relative;",
      tags$div(
        "RECOMMENDATIONS",
        style = "text-align: center; font-weight: bold; font-size: 25px; color: #000000; position: relative; z-index: 1;"
      ),
      uiOutput("recommendations")
    )
  ),
  # Footer
  div(style = "background-color: black; color: white; padding: 20px; text-align: center; margin-top: 30px;",
      tags$div(
        tags$a(href = "https://rw.linkedin.com/company/rwanda-ict-chamber", target = "_blank",
               icon("linkedin"), style = "color: white; margin: 0 10px; font-size: 20px;"),
        tags$a(href = "https://x.com/rwictchamber/status/1920817517477400891", target = "_blank",
               icon("twitter"), style = "color: white; margin: 0 10px; font-size: 20px;")
      ),
      tags$p("TECH ASSOCIATES HACKATHON | © 2025", style = "margin-top: 10px; font-weight: bold;")
  )
)

server_hack <- function(input, output, session) {
  # Pie Chart for Sentiment Distribution
  output$sentimentPie <- renderPlotly({
    selected_source <- input$sourceInput
    
    # Filter data based on selection
    if (selected_source == "All") {
      filtered_data <- all_with_sentiment_pie
    } else {
      # Match the selection with the actual values in your dataset
      source_mapping <- c("News" = "Articles", "Social Media" = "Twitter (X)")
      data_source <- source_mapping[selected_source]
      filtered_data <- all_with_sentiment_pie %>% filter(source == data_source)
    }
    
    # Create pie chart with filtered data
    filtered_data %>%
      count(sentiment) %>%
      mutate(
        sentiment = factor(sentiment, levels = c("positive", "negative")),
        color = case_when(
          sentiment == "positive" ~ "#28a745",
          sentiment == "negative" ~ "#dc3545",
          TRUE ~ "#999999"
        )
      ) %>%
      plot_ly(
        labels = ~sentiment,
        values = ~n,
        type = "pie",
        marker = list(colors = ~color)
      )
  })
  
  # Use the pre-calculated contingency table
  output$contingencyTable <- DT::renderDataTable({
    DT::datatable(
      contingency_data,
      options = list(
        dom = 't',  # Only show table, no search/pagination
        ordering = FALSE,
        columnDefs = list(
          list(className = 'dt-center', targets = 1:3),  # Center numbers
          list(className = 'dt-left', targets = 0)  # Left align source column
        )
      ),
      rownames = FALSE
    )
  })
  
  # Use pre-calculated Chi-Square test results
  output$chiSquareTest <- renderText({
    chi_interpretation
  })
  
  # Word_Cloud rendering
  output$wordCloud <- renderWordcloud2({
    wordcloud2(data = frequent_words, 
               size = 0.8,  
               color = 'random-light', 
               backgroundColor = "navy")
  })
  
  # Rest of your server code remains unchanged
  # Top concerns (short summary table)
  output$topConcerns <- renderTable({
    data.frame(
      Concern = c("High fare on long trips", "Payment confusion", "No clear info", "Tech issues with tap & go", "Lack of awareness")
    )
  })
  
  # Top 5 Concerns (detailed)
  output$concern1 <- renderText({"Lack of clarity on how fares are calculated"})
  output$concern2 <- renderText({"Increased costs for low-income commuters"})
  output$concern3 <- renderText({"Poor communication before rollout"})
  output$concern4 <- renderText({"Longer trips now perceived as unfairly expensive"})
  output$concern5 <- renderText({"Routes under this system are few"})
  
  # Top 5 Misconceptions
  output$misconception1 <- renderText({"Belief that the new system increases fares for all routes"})
  output$misconception2 <- renderText({"Assumption that fares change randomly"})
  output$misconception3 <- renderText({"Thinking the system is only for Kigali"})
  output$misconception4 <- renderText({"Assuming it tracks exact GPS location in real-time"})
  output$misconception5 <- renderText({"Confusing distance-based fare with time-based billing"})
  
  # Recommendations UI
  output$recommendations <- renderUI({
    HTML(
      '
    <div style="display: flex; justify-content: space-between; color: navy; font-size: 16px;">
      <ul style="list-style-type: none; padding-left: 0; flex: 1;">
        <li><i class="fas fa-bullhorn" style="margin-right: 10px;"></i> Enhance public communication on fare structure and benefits.</li>
        <li><i class="fas fa-calculator" style="margin-right: 10px;"></i> Launch a fare calculator for transparency.</li>
        <li><i class="fas fa-hand-holding-usd" style="margin-right: 10px;"></i> Ensure fare subsidies for students and vulnerable groups.</li>
      </ul>
      <ul style="list-style-type: none; padding-left: 20px; flex: 1;">
        <li><i class="fas fa-chalkboard-teacher" style="margin-right: 10px;"></i> Educate drivers and conductors on policy enforcement.</li>
        <li><i class="fas fa-comment-dots" style="margin-right: 10px;"></i> Conduct regular feedback assessments via social and news platforms.</li>
      </ul>
    </div>
    '
    )
  })
}
# Setting the TimeOUt
Sys.setenv(R_SHINY_LAUNCH_TIMEOUT = "300")
shinyApp(ui= ui_hack, server_hack)
