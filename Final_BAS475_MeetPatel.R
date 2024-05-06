library(fpp3)
library(plotly)
library(shiny)
library(urca)

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


# Forecast Model (GISTEMP):

gistemp <- global_temp %>% 
  filter(Source == "GISTEMP")

gistemp %>% 
  autoplot() %>% 
  ggplotly()

gistemp %>% 
  tail(-24)


# train and test data filtering
test_df <- gistemp %>% 
  tail(24)

train_df <- gistemp %>% 
  head(-24)

# Cross Validation
df_cv <- train_df %>% 
  stretch_tsibble(.init = 5*12, .step = 2*12) 


fit <- df_cv %>%
  model(Drift_with_Snaive = SNAIVE(Mean ~ drift()),
        tslm = TSLM(Mean~ trend() + season()),
        ets = ETS(Mean),
        arima = ARIMA(Mean))

fit_fc <- fit %>% 
  forecast(h = "2 year") %>% 
  filter(Date <= yearmonth("2015 Dec"))

fit_fc %>% 
  accuracy(train_df) %>% 
  arrange(RMSE)


# ETS using train df
fit_ets <- train_df %>% 
  model(ETS(Mean))

gg_tsresiduals(fit_ets)

# TSLM using train df
fit_tslm <- train_df %>% 
  model(TSLM(Mean))

gg_tsresiduals(fit_tslm)

# ARIMA using train df
fit_arima <- train_df %>% 
  model(ARIMA(Mean))

gg_tsresiduals(fit_arima)

# Drift_with_Snaive using train df
fit_drift_snaive <- train_df %>% 
  model(SNAIVE(Mean ~ drift()))

gg_tsresiduals(fit_drift_snaive)


# ARIMA is best performing because all points residuals have white noise and lowest RMSE. 

# ETS using Test Data
fit_fc <- fit_ets %>% 
  forecast(h = "2 year", times=0)

fit_fc %>% 
  accuracy(test_df) 

# TSLM using Test Data
fit_fc <- fit_tslm %>% 
  forecast(h = "2 year", times=0)

fit_fc %>% 
  accuracy(test_df) 

# ARIMA using Test Data
fit_fc <- fit_arima %>% 
  forecast(h = "2 year", times=0)

fit_fc %>% 
  accuracy(test_df) 

# Drift with Snaive using Test Data
fit_fc <- fit_drift_snaive %>% 
  forecast(h = "2 year", times=0)

fit_fc %>% 
  accuracy(test_df) 

# ARIMA also performs the best on the test data. 


# Final Models
gistemp <- gistemp %>% 
  mutate(Date = yearmonth(Date)) %>%
  select(Date, Mean) %>% 
  as_tsibble(index=Date)

fit <- gistemp %>%
  model(Drift_with_Snaive = SNAIVE(Mean ~ drift()),
        tslm = TSLM(Mean~ trend() + season()),
        ets = ETS(Mean),
        arima = ARIMA(Mean))

# ARIMA
fit_fc_arima <- fit %>% 
  select(.model = "arima") %>% 
  forecast(h = "24 months") 

final_arima <- tsibble(Date = fit_fc_arima$Date, Mean = fit_fc_arima$.mean) %>% 
  as_tsibble()

# arima plot
# autoplot(final_arima, facets = TRUE, color="blue") +
#   autolayer(gistemp, series = "GISTEMP", color = "black") +
#   labs(title = "Forecast GISTEMP Data using Arima",
#        y = "Temperature") +
#   theme_minimal()
# 




# ETS
fit_fc_ets <- fit %>% 
  select(.model = "ets") %>% 
  forecast(h = "24 months") 

final_ets <- tsibble(Date = fit_fc_ets$Date, Mean = fit_fc_ets$.mean) %>% 
  as_tsibble()

# ets plot
# autoplot(final_ets, facets = TRUE, color="blue") +
#   autolayer(gistemp, series = "GISTEMP", color = "black") +
#   labs(title = "Forecast GISTEMP Data using Exponential smoothing",
#        y = "Temperature") +
#   theme_minimal()



# TSLM
fit_fc_tslm <- fit %>% 
  select(.model = "tslm") %>% 
  forecast(h = "24 months") 

final_tslm <- tsibble(Date = fit_fc_tslm$Date, Mean = fit_fc_tslm$.mean) %>% 
  as_tsibble()

# TSLM plot
# autoplot(final_tslm, facets = TRUE, color="blue") +
#   autolayer(gistemp, series = "GISTEMP", color = "black") +
#   labs(title = "Forecast GISTEMP Data using TSLM",
#        y = "Temperature") +
#   theme_minimal()


# Drift with Snaive
fit_fc_drift_snaive <- fit %>% 
  select(.model = "Drift_with_Snaive") %>% 
  forecast(h = "24 months") 

final_drift_snaive <- tsibble(Date = fit_fc_drift_snaive$Date, Mean = fit_fc_drift_snaive$.mean) %>% 
  as_tsibble()

# Drift with Snaive plot
# autoplot(final_drift_snaive, facets = TRUE, color="blue") +
#   autolayer(gistemp, series = "GISTEMP", color = "black") +
#   labs(title = "Forecast GISTEMP Data using Drift with Snaive",
#        y = "Temperature") +
#   theme_minimal()
# 







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
           radioButtons("Choose_Plot", label = h3("Choose Plot:", style = "color: white; font-size: 24px; font-weight: bold;"),
                        choices = list("Seasonality Plot" = "Seasonality", 
                                       "Autocorrelation Plot" = "Autocorrelation", 
                                       "Classical Decomposition" = "Decomposition"), 
                        selected = "Seasonality", 
                        inline=TRUE),
           plotOutput("Plot_3"),
           fluidRow(column(width = 12, HTML("<br>"))), # Add a gap
           mainPanel(uiOutput("Plot_3_interpretation"))
  ),
  
  tabPanel("Additional Feature: Tesla Google Trends and Global Mean Temperature",
           plotOutput("Tesla"), 
           plotOutput("Global_temp_trend"),
           mainPanel(uiOutput("teslatext")),
           plotOutput("cross_corr")
  ),
  tabPanel("Forecast: GISTEMP",
           radioButtons("Choose_forecast", label = h3("Choose Forecast:", style = "color: white; font-size: 24px; font-weight: bold;"),
                        choices = list("ARIMA" = "arima", 
                                       "ETS" = "ets", 
                                       "Drift with Snaive" = "drift_and_snaive",
                                       "TSLM" = "tslm"), 
                        selected = "arima", 
                        inline=TRUE),
           plotOutput("Forecast_Plots"),
           fluidRow(column(width = 12, HTML("<br>"))), # Add a gap
           mainPanel(uiOutput("Forecast_interpretations")), 
           fluidRow(column(width = 12, HTML("<br>"))), # Add a gap
           fluidRow(
             column(8, plotOutput("Residuals")), # Adjust width here
           )
           
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
  
  
  output$Forecast_Plots <- renderPlot({
    if(input$Choose_forecast == "arima") {
      ggplot() +
        geom_line(data = as.data.frame(final_arima), aes(x = Date, y = Mean), color = "blue") +
        geom_line(data = as.data.frame(gistemp), aes(x = Date, y = Mean), color = "black") +
        labs(title = "Forecast GISTEMP Data using ARIMA",
             y = "Temperature") +
        theme_minimal()
      
    } else if(input$Choose_forecast == "ets") {
      ggplot() +
        geom_line(data = as.data.frame(final_ets), aes(x = Date, y = Mean), color = "blue") +
        geom_line(data = as.data.frame(gistemp), aes(x = Date, y = Mean), color = "black") +
        labs(title = "Forecast GISTEMP Data using ETS",
             y = "Temperature") +
        theme_minimal()
    } else if(input$Choose_forecast == "drift_and_snaive") {
      ggplot() +
        geom_line(data = as.data.frame(final_drift_snaive), aes(x = Date, y = Mean), color = "blue") +
        geom_line(data = as.data.frame(gistemp), aes(x = Date, y = Mean), color = "black") +
        labs(title = "Forecast GISTEMP Data using Drift with Snaive",
             y = "Temperature") +
        theme_minimal()
    } else if(input$Choose_forecast == "tslm") {
      ggplot() +
        geom_line(data = as.data.frame(final_tslm), aes(x = Date, y = Mean), color = "blue") +
        geom_line(data = as.data.frame(gistemp), aes(x = Date, y = Mean), color = "black") +
        labs(title = "Forecast GISTEMP Data using TSLM",
             y = "Temperature") +
        theme_minimal()
    }
  })
  
  
  output$Residuals <- renderPlot({
    if(input$Choose_forecast == "arima") {
      # ARIMA using train df
      fit_arima <- train_df %>% 
        model(ARIMA(Mean))
      
      gg_tsresiduals(fit_arima)
      
    } else if(input$Choose_forecast == "ets") {
      # ETS using train df
      fit_ets <- train_df %>% 
        model(ETS(Mean))
      
      gg_tsresiduals(fit_ets)
      
    } else if(input$Choose_forecast == "drift_and_snaive") {
      # Drift_with_Snaive using train df
      fit_drift_snaive <- train_df %>% 
        model(SNAIVE(Mean ~ drift()))
      
      gg_tsresiduals(fit_drift_snaive)
      
    } else if(input$Choose_forecast == "tslm") {
      # TSLM using train df
      fit_tslm <- train_df %>% 
        model(TSLM(Mean))
      
      gg_tsresiduals(fit_tslm)
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
  
  
  output$Forecast_interpretations <- renderUI({
    if(input$Choose_forecast == "arima") {
      tags$ul(
        tags$li("Arima is used to forecast the monthly GISTEMP for the year 2017 and 2018."),
        tags$li("Arima model is the best model to forecast the GISTEMP time-series."),
        tags$li("It has the lowest RMSE using Cross Validation."),
        tags$li("The below residual plot shows that it has white noise, centered at zero, and it misses only few patterns.")
      )
    } else if(input$Choose_forecast == "ets") {
      tags$ul(
        tags$li("ETS is used to forecast the monthly GISTEMP for the year 2017 and 2018."),
        tags$li("ETS model does not perform the best because it didn't capture seasonality"),
        tags$li("The below residual plot shows that it does not satisfy the requirements of residuals."),
        tags$p("- It has significant ACF, roughly centered around 0, and it misses significant patterns of time-series.", style = "margin-left: 25px;")
      )
    } else if(input$Choose_forecast == "drift_and_snaive") {
      tags$ul(
        tags$li("Drift with Snaive is used to forecast the monthly GISTEMP for the year 2017 and 2018."),
        tags$li("Drift with Snaive model is the best simple model here because it can capture the trend and seasonality well."),
        tags$li("This model is not realistic, because it is highly unlikely to see similar upward spikes for 2017 and 2018."),
        tags$li("The below residual plot shows that it does not satisfy the requirements of residuals."),
        tags$p("- It has significant ACF (misses seasonality), not centered at 0, and it misses significant patterns of time-series.", style = "margin-left: 25px;")
      )
    } 
    else if(input$Choose_forecast == "tslm") {
      tags$ul(
        tags$li("TSLM is used to forecast the monthly GISTEMP for the year 2017 and 2018."),
        tags$li("TSLM model is the second best model to predict the GISTEMP time-series."),
        tags$li("The below residual plot shows that it does not satisfy the requirements of residuals."),
        tags$p("- It has significant ACF, centered at 0, and it misses significant patterns of time-series.", style = "margin-left: 25px;")
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
