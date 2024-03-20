library(fpp3)
library(plotly)
library(shiny)

# TESLA DATA - google Trends

# Path where data is
file_path <- "Tesla.csv"

# Data starts in 3rd row, skip first 2 rows
g_trends <- read.csv(file_path, skip = 2)

# Rename columns
names(g_trends) <- c("Month", "Interest")

# Convert Month to date
g_trends$Month <- yearmonth(g_trends$Month)

# Convert to tsibble
g_trends <- tsibble(g_trends, index=Month)

# Google labels low numbers as "<1"
# Convert those to 0s and ensure the column is numbers
g_trends$Interest <- as.numeric(
  ifelse(g_trends$Interest == "<1", 0, g_trends$Interest)
)

# from 2001 to 2016
tesla_df <- g_trends%>% 
  filter(year(Month) <= 2016 & year(Month) >= 2004) 


# GLOBAL TEMPERATURE DATA

global_temp <- read.csv("monthly_global_temp.csv")

#head(global_temp)

global_temp <- global_temp %>% 
  mutate(Date = yearmonth(as.Date(Date))) %>% 
  tsibble(index=Date, key=Source) %>% 
  filter(year(Date) >= 2004) 

# cross corr
g_trends_ts <- ts(tesla_df$Interest)
global_temp_ts <- ts(global_temp$Mean)

# Calculate cross-correlation
# cross_corr <- ccf(g_trends_ts, global_temp_ts)



# Build Shiny App:
library(shiny)


ui <- navbarPage(
  # title = "Global Temperature App",
  tags$style(HTML("
    body {
      background-color: black;
      color: white; /* white font */
    }
    .navbar {
      background-color: black !important;
    }
    .navbar-nav > li > a:hover {
      background-image: linear-gradient(to right, #333A73, #FC6736) !important; /* Gradient color on hover */
      color: white !important; /* White text color on hover */
    }
  ")),
  
  tabPanel("Background and Instructions",
           tags$div(
             style = "background-image: linear-gradient(to right, #333A73, #FC6736); padding: 10px; text-align: center;",
             titlePanel(
               tags$span(
                 "Global Temperature Time-Series App",
                 style = "color: white; sans-serif; font-size: 40px; font-weight: bold;"
               )
             )
           ),
           tags$div(
             HTML("<h3>Background Information:</h3>"),
             tags$ul(
               tags$p("Background: Data are included from the GISS Surface Temperature (GISTEMP) analysis and the global 
                      component of Climate at a Glance (GCAG). Data Source link: ",
                      tags$a(href = "https://datahub.io/core/global-temp#readme", "https://datahub.io/core/global-temp#readme")
               ),
               tags$p("The following time series plot illustrates the Global mean temperature for each month from Jan 2004 to Dec 2016."),
               tags$p("The plot demonstrates that the mean temperature has increased and decreased over time. It reached its maximum value in 2016."),
             )
           ),
           tags$div(
             HTML("<h3>Instructions to use the App:</h3>"),
             tags$ul(
               tags$li("Users can click on the above 'Plots' tab to display various plots including Seasonality, Autocorrelation, and Classical Decomposition."),
               tags$li("Users can select the 'Additional Feature: Tesla Google Trends and Global Mean Temperature' tab to view an interesting relationship between Tesla's Google Trends and Global Mean Temperature."),
             )
           ),
           plotOutput("Temp")
  ),
  
  tabPanel("Plots",
           radioButtons("Choose_Plot", label = h3("Choose Plot:", style = "color: #007BFF; font-size: 24px; font-weight: bold;"),
                        choices = list("Seasonality Plot" = "Seasonality", 
                                       "Autocorrelation Plot" = "Autocorrelation", 
                                       "Classical Decomposition" = "Decomposition"), 
                        selected = "Seasonality", 
                        inline=TRUE),
           plotOutput("Plot_3"),
           mainPanel(uiOutput("Plot_3_interpretation"))
  ),
  
  tabPanel("Additional Feature: Tesla Google Trends and Global Mean Temperature",
           plotOutput("Tesla"), 
           plotOutput("Global_temp_trend"),
           mainPanel(uiOutput("teslatext")),
           plotOutput("cross_corr")
  )
)


server <- function(input, output, session) {
  
  output$Temp <- renderPlot({
    global_temp %>%   
      autoplot(Mean)+
      labs(title = "Global Temperature: Jan 2004 to Dec 2016", x="Date", y="Global Temperature Mean") +
      theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
            plot.background = element_rect(color = "black")) 
    
  })
  
  output$Plot_3 <- renderPlot({
    if(input$Choose_Plot == "Seasonality") {
      global_temp %>%   
        gg_season(Mean) +
        labs(title = "Global Temperature: Seasonality", x="Date", y="Global Temperature Mean") +
        theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
              plot.background = element_rect(color = "black"))
    } else if(input$Choose_Plot == "Autocorrelation") {
      global_temp %>%   
        ACF(Mean) %>% 
        autoplot() +
        labs(title = "Global Temperature: Autocorrelation", x="Date", y="Global Temperature Mean") +
        theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
              plot.background = element_rect(color = "black"))
    } else if(input$Choose_Plot == "Decomposition") {
      global_temp %>%   
        model(classical_decomposition(Mean, type="additive")) %>% 
        components() %>% 
        autoplot() +
        labs(title = "Global Temperature: Classical Decomposition", x="Date", y="Global Temperature Mean") +
        theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
              plot.background = element_rect(color = "black"))
    }
  })
  
  output$Plot_3_interpretation <- renderUI({
    if(input$Choose_Plot == "Seasonality") {
      tags$ul(
        tags$li("The global mean temperature does not have strong seasonality."),
        tags$li("The mean temperature is higher in the month of Jan, Feb, and March as compared to other months."),
        tags$li("The top 'pink' line represents the later years of the dataset which are 2015 and 2016. The mean temp is the highest in those years compared to previous years."),
        tags$li("It is fairly steady from May to Dec, with a slight increase towards the end of the year.")
      )
    } else if(input$Choose_Plot == "Autocorrelation") {
      tags$ul(
        tags$li("The Autocorrelation Plot demonstrates a strong trend in the global monthly mean temperature."),
        tags$li("The plot shows that the values are strongly similar to other values that are close to each other in terms of time-period as compared to the value of the same day of previous or next year."),
        tags$li("Both GISTEMP and GCAG's data show a strong trend and weak seasonality.")
      )
    } else if(input$Choose_Plot == "Decomposition") {
      tags$ul(
        tags$li("The first plot is the original time series plot. The 'Additive' decomposition has been used because the trend is fairly linear and the seasonality does not change with level."),
        tags$li("The second plot is the trend plot which shows a significant upward trend in mean global temp."),
        tags$li("The third is the seasonality plot. The seasonal variation in GISTEMP is higher compared to that of GCAG."),
        tags$li("The last plot is irregularity which demonstrates randomness in the dataset. A slightly higher value is seen towards the end which is likely due to the increase in temperature in 2016.")
      )
    } 
  })
  
  output$Tesla <- renderPlot({
    tesla_df %>% 
      model(classical_decomposition(Interest, type="additive")) %>% 
      components() %>% 
      autoplot(trend)+
      labs(title = "Tesla Google Trend: Jan 2004 to Dec 2016", x="Date", y="Tesla Google Trend") +
      theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
            plot.background = element_rect(color = "black"))
  })
  
  output$Global_temp_trend <- renderPlot({
    global_temp %>%
      model(classical_decomposition(Mean, type = "additive")) %>%
      components() %>%
      autoplot(trend) +
      labs(title = "Global Temperature Trend: Jan 2004 to Dec 2016", x = "Date", y = "Global Temperature Mean") +
      theme(
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        plot.background = element_rect(color = "black"),
        legend.position = c(0.2, 0.8),
        legend.background = element_rect
        (fill = "white"),
      ) +
      guides(color = guide_legend(title = "Legend Title")) 
  })
  
  output$teslatext <- renderUI({
    tags$ul(
      tags$p("The trends of both data look similar from 2012 to 2016 because both values increased since 2012 and reached max in 2016."),
      tags$li("This relationship is not directly linked (Not a 'Cause and Effect' relationship) because both tesla and global temp have different reasons for increase in value from 2012 to 2016."),
      tags$li("In 2016, Tesla gained popularity following the launch of its Model 3, which quickly became the most sought-after electric car globally.
              Additionally, the mean global temperature reached its highest point in February 2016 due to various factors, 
              including long-term global warming and the strong El Niño phenomenon.\n"),
      tags$p(""),
      tags$p("\nCross Correlation Plot: "),
      tags$li("The Cross Correlation plot below shows that Global Temperature and Tesla's Google trends are correlated to each other."),
      tags$li("The lines representing the strength of correlation are outside of the blue lines which suggests a significant correlation between Tesla's Google trend and Global Temperature."),
    )
  })
  
  output$cross_corr <- renderPlot({
    cross_corr <- ccf(g_trends_ts, global_temp_ts)
    plot(cross_corr, main = "Cross-Correlation between Tesla Google Trends and Global Temperature")
  })
  
}

shinyApp(ui, server)
