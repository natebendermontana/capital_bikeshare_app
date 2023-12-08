# setup & plotting
#library(here)
library(plotly)
library(ggplot2)
library(dplyr)
library(scales)
# modeling
library(caTools)
library(Metrics)
library(randomForest)
library(rpart)
# shiny
library(shiny)
library(shinydashboard)
library(rsconnect)

# Load clean data ###################################################
#setwd("/Users/natebender/Desktop/repo/r_learning_overall/capital_bikeshare_app/capital_bikeshare_project")
#bike_clean <- read.csv(here("test_bike_app/data_clean_copy", "bike_clean.csv"))
bike_clean <- read.csv("data_clean/bike_clean.csv")

# You can create custom labels for the hours
custom_labels <- c("12 AM", "1 AM", "2 AM", "3 AM", "4 AM", "5 AM", "6 AM", 
                   "7 AM", "8 AM", "9 AM", "10 AM", "11 AM", "12 PM", 
                   "1 PM", "2 PM", "3 PM", "4 PM", "5 PM", "6 PM", 
                   "7 PM", "8 PM", "9 PM", "10 PM", "11 PM")

bike_clean <- bike_clean %>%
    mutate(
        mnth = factor(mnth, levels = month.abb, ordered = TRUE),
        yr = factor(yr, levels = c("2011", "2012"), ordered = TRUE),
        season = factor(season, levels = c("Spring", "Summer", "Fall", "Winter"), ordered = TRUE),
        hr = factor(hr, ordered = TRUE),
        holiday = factor(holiday, levels = c("No", "Yes"), ordered = TRUE),
        weekday = factor(weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), ordered = TRUE),
#        workingday = factor(workingday, levels = c("No", "Yes"), ordered = TRUE),
        weathersit = factor(weathersit, levels = c("Very Bad", "Bad", "Fair", "Good"), ordered = TRUE),
    )

# Apply the custom labels to the ordered factor
bike_clean$hr <- ordered(bike_clean$hr, levels = levels(bike_clean$hr), labels = custom_labels)

# Modeling ###################################################
set.seed(11272023)

#Dropping columns
# drop season and workingday bc "mnth" and "weekday" provide the same info. 
# drop year bc the model takes into account weather and specific daily info.
# drop "causal" and "new" bc the purpose of the model is to predict total num of registrations, not type of registration
bike_formodel <- bike_clean[c(-1,-2,-7,-13,-14)] # drop season, yr, workingday, casual, new

#Splitting data
split = sample.split(bike_formodel$total, SplitRatio = 0.8)
train_set = subset(bike_formodel, split == TRUE)
test_set = subset(bike_formodel, split == FALSE)

#Write new files for the train and test sets
# write.csv(train_set, here("test_bike_app/data_clean_copy", "bike_train.csv"), row.names = FALSE)
# write.csv(test_set, here("test_bike_app/data_clean_copy", "bike_test.csv"), row.names = FALSE)

write.csv(train_set, "data_clean/bike_train.csv", row.names = FALSE)
write.csv(test_set, "data_clean/bike_test.csv", row.names = FALSE)

# Multilinear regression ###
multi = lm(formula = total ~ ., data = train_set)

#Predicting the test values
y_pred_m = predict(multi, newdata = test_set)

#Performance metrics
mae_m = mae(test_set$total, y_pred_m)
rmse_m = rmse(test_set$total, y_pred_m)

# Decision tree ###
dt = rpart(formula = total ~ ., data = train_set,
           # minsplit sets the minimum number of observations in a node
           control = rpart.control(minsplit = 3))

#Predicting the test values
y_pred_dt = predict(dt, newdata = test_set)

#Performance metrics
mae_dt = mae(test_set$total, y_pred_dt)
rmse_dt = rmse(test_set$total, y_pred_dt)

# Random forest ###
rf = randomForest(formula = total ~ ., data = train_set,
                  ntree = 100)

#Predicting the test values
y_pred_rf = predict(rf, newdata = test_set)

#Performance metrics
mae_rf = mae(test_set$total, y_pred_rf)
rmse_rf = rmse(test_set$total, y_pred_rf)

# Compare models, choose best ###################################################
metrics_df <- data.frame(Model = c("Multilinear Regression", "Decision Tree", "Random Forest"),
                         name = c("multi", "dt", "rf"),
                         MAE = c(mae_m, mae_dt, mae_rf),
                         RMSE = c(rmse_m, rmse_dt, rmse_rf))

# Find the index of the minimum MAE
best_model_idx <- which.min(metrics_df$MAE)
best_model <- get(metrics_df$name[best_model_idx])
print(metrics_df$name[best_model_idx])

# Accuracy stats for Shiny
best_model_mae <- metrics_df$MAE[best_model_idx]
best_model_rmse <- metrics_df$RMSE[best_model_idx]


# Shiny UI ###################################################
ui <- dashboardPage(
    
    #Dashboard title
    dashboardHeader(title = 'Bike Sharing Explorer', titleWidth = 290),
    
    #Sidebar layout
    dashboardSidebar(width = 220,
                     sidebarMenu(menuItem("Introduction", tabName = "intro", icon = icon('book-open')),
                                 menuItem("EDA Plots", tabName = "plots", icon = icon('poll')),
                                 menuItem("Dashboard", tabName = "dash", icon = icon('tachometer-alt')),
                                 menuItem("Prediction", tabName = 'pred', icon = icon('search')))),
    
    #Tabs layout
    dashboardBody(    
        tags$head(
            # Header
            tags$style(HTML('.skin-blue .main-header .logo:hover {font-weight: bold; background-color: #3d5a80; color: #e8e8e8}
                        .skin-blue .main-header .logo {font-weight: bold; background-color: #3d5a80; color: #e8e8e8}'),
                       # center title
                       HTML('.tab-pane {text-align: center;}'),
                       # rest of header 
                       HTML('.skin-blue .main-header .navbar {background-color: #3d5a80;}'),
                       # sidebar
                       HTML('.skin-blue .main-sidebar {background-color: #e8e8e8;}'),
                       # active selected tab in the sidebarmenu
                       HTML('.skin-blue .main-sidebar .sidebar .sidebar-menu a{background-color: #e8e8e8; color: #3c3c3c;}
                        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{background-color: #3c8dbc; color: #e8e8e8; border-left: 4px solid #3c8dbc;}
                        .skin-blue .main-sidebar .sidebar .sidebar-menu > li:hover > a {background-color: #dcdcdc; color: #3c3c3c;'),
                       # other links in the sidebarmenu
                       HTML('.skin-blue .background a{background-color: #e8e8e8}')
            ) # closes tags$style
        ), # closes tags$head
        
        # Intro tab item ##       
        #Plots tab content
        tabItems(tabItem('intro',
                         # intro box ##
                         box(status = 'primary', title = "Introduction", solidHeader = TRUE, 
                             width=6,
                             div(align = "left",
                                 helpText('This tool explores the Capital Bikeshare dataset, courtesy of the UC Irvine Machine Learning Repository. The bikeshare system consists of a fleet of bikes located at docking stations throughout Washington, D.C. that can be unlocked with an app.
                                                 After a ride, a user simply returns their bike to any available docking station.')
                             ),
                             div(align = "left",
                                 helpText('The dataset contains the hourly count of bike registrations between the years 2011 and 2012, taking into consideration several contextual variables such as weather condition, day of the week, and season. 
                                                In total the data includes 16 variables and 17,379 observations in which each row of the data represents a specific hour of the day. The data ranges from January 1, 2011 to December 31, 2012.')
                             ),
                         ), # closes box
                         
                         # numerical variables ##
                         box(status = 'primary', title = "Numerical Variables", solidHeader = T,
                             width=6,
                             div(align = "left",h5(strong('Temperature (F)'))),
                             div(align = "left",h5(strong('Feeling Temperature (F)'))),
                             div(align = "left",
                                 helpText('Subjective assessment of how the temperature feels, in Fahrenheit'),
                             ),
                             div(align = "left",h5(strong('Humidity (%)'))),
                             div(align = "left",h5(strong('Wind speed (mph)'))),
                             div(align = "left",h5(strong('Casual'))),
                             div(align = "left",
                                 helpText('Count of casual users (Single Trip, 24-Hour Pass, 3-Day Pass or 5-Day Pass users).'),
                             ),
                             div(align = "left",h5(strong('Registered'))),
                             div(align = "left",
                                 helpText('Count of registered users (Annual Member, 30-Day Member or Day Key Member users).'),
                             ),
                             div(align = "left",h5(strong('Total'))),
                             div(align = "left",
                                 helpText('Count of total users.'))
                         ),
                         
                         # tabs expl box ##
                         box(status = 'primary', title = 'Tabs Explanation', width = 6, solidHeader = T,
                             div(align = "left", h5(strong('EDA Plots')),
                                 helpText("Descriptive exploration of each variable in the dataset.")
                             ),
                             div(align = "left", h5(strong('Dashboard')),
                                 helpText("Exploration of year, user type, and weather type on number of users.")
                             ),
                             div(align = "left", h5(strong('Prediction')),
                                 helpText('Predictive analysis. The model will predict the total number of bikes rented (aka "users") based on specific contextual conditions present in the data.'),
                                 helpText(sprintf('The prediction is based on a random forest supervised machine learning model that has a mean absolute error (MAE) of %s users, and a root mean squared error (RMSE) of %s users. This uncertainty is noted in the model prediction dashboard.',
                                                  round(best_model_mae, digits = 0), round(best_model_rmse, digits = 0)))
                             ), 
                         ),
                         
                         # categorical vars box ##
                         box(status = 'primary', title = "Categorical Variables", solidHeader = T,
                             div(align = "left", h5(strong('Year'))),
                             div(align = "left", h5(strong('Season'))),
                             div(align = "left",
                                 helpText('Spring, Summer, Fall, Winter'),
                             ),                                       
                             div(align = "left",h5(strong('Month'))),
                             div(align = "left",h5(strong('Weekday'))),
                             div(align = "left",
                                 helpText('Monday - Sunday'),
                             ),                                        
                             div(align = "left",h5(strong('Hour'))),
                             div(align = "left",h5(strong('Holiday'))),
                             div(align = "left",
                                 helpText('Yes / No'),
                             ),
                             div(align = "left",h5(strong('Weather'))),
                             div(align = "left",
                                 helpText('Good: clear, few clouds, partly cloudy.'),
                                 helpText('Fair: mist, cloudy, broken clouds.'),
                                 helpText('Bad: light snow, light rain, thunderstorm, scattered clouds.'),
                                 helpText('Very Bad: heavy rain, ice pallets, thunderstorm, mist, snow, fog.'))
                         )
        ), # closes tabitem "Intro"
        
        # Plots tab item ##
        tabItem('plots', 
                #Histogram filter
                box(status = 'primary', title = 'Numerical Variables', solidHeader = TRUE,
                    # "" bc I don't want a title on the selection box
                    selectInput('num', "", c('Temperature (F)', 'Feeling temperature (F)', 'Humidity (%)', 'Wind speed (mph)', 'Casual', 'Registered', 'Total')),
                ), #footer = 'Histogram plot for numerical variables'),
                #Frequency plot filter
                box(status = 'primary', title = 'Categorical Variables', solidHeader = TRUE,
                    # "" bc I don't want a title on the selection box
                    selectInput('cat', '', c('Season', 'Year', 'Month', 'Hour', 'Holiday', 'Weekday', 'Weather')),
                ), #footer = 'Frequency plot for categorical variables'),
                #Boxes to display the plots
                box(plotlyOutput('histPlot')),
                box(plotlyOutput('freqPlot'))
        ),
        
        #Dashboard tab content
        tabItem('dash',
                #Dashboard filters
                box(title = 'Filters', status = 'primary',
                    solidHeader = TRUE,
                    width = 12, 
                    height = 150,
                    fluidPage(
                        tags$head(
                            tags$style(HTML("#my_btns, #my_div {display: flex; justify-content: center;}"
                            ))
                        ),
                        fluidRow(
                            column(3, align = "left", selectInput('year', 'Year', c('All', '2011', '2012'))),
                            column(3, align = "left", selectInput('regis', 'Users', c('Total', 'Registered', 'Casual'))),
                            column(3, align = "left", selectInput('weather', 'Weather', c('All', 'Good', 'Fair', 'Bad', 'Very Bad')))
                        ))),
                #Boxes to display the plots
                box(plotlyOutput('linePlot')),
                box(plotlyOutput('barPlot'))
        ),
        
        # Prediction tab item ##
        #Prediction tab content
        tabItem('pred',
                
                # new layout test
                box(
                    title = 'Customize Scenario',
                    status = 'primary',
                    solidHeader = TRUE,
                    width = 12,
                    height = 250,
                    fluidPage(
                        fluidRow(
                            column(2, selectInput('p_mnth', 'Month', c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))),
                            column(2, selectInput('p_hr', 'Hour', custom_labels)),
                            column(2, selectInput('p_weekd', 'Weekday', c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))),
                            column(2, selectInput('p_weather', 'Weather', c('Good', 'Fair', 'Bad', 'Very Bad'))),
                            column(2, selectInput('p_holid', 'Holiday', c('Yes', 'No')))
                        ),
                        fluidRow(
                            column(2, sliderInput('p_hum', 'Humidity (%)', min = min(test_set$humidity), max = max(test_set$humidity), value = min(test_set$humidity))),
                            column(2, sliderInput('p_temp', 'Temperature (F)', min = 0, max = round(max(test_set$temp),0), value = 0)),
                            column(2, sliderInput('p_ftemp', 'Feeling temperature (F)', min = 0, max = round(max(test_set$atemp),0), value = 0)),
                            column(2, sliderInput('p_wind', 'Wind speed (mph)', min = min(test_set$windspeed), max = round(max(test_set$windspeed),0), value = min(test_set$windspeed))),
                            column(2, actionButton('cal',
                                                   label = 'Calculate', 
                                                   icon = icon('calculator'),
                                                   # white text, orange accent background color. Slight margin on top to bump it down a bit.
                                                   style = 'color: #ffffff; background-color: #ee6c4d; border-color: #ee6c4d; 
                                                                font-size: 120%; margin-top: 20px;'))
                        )
                    )
                ),
                
                #Box to display the prediction results
                box(title = 'Prediction', status = 'primary', solidHeader = TRUE, width = 2, height = 280,
                    div(h5('Estimated users')),
                    verbatimTextOutput("value", placeholder = TRUE),
                    div(h5('Range of uncertainty')),
                    verbatimTextOutput("range", placeholder = TRUE)
                ),
                
                # Line chart for distribution of predictions
                box(title = 'Distribution of Possible Predictions', status = 'primary', solidHeader = TRUE, width = 10, height = 460,
                    plotOutput("predictionDistribution")
                ) # box
        ) # tabitem
        ) # closes all tabitems
    ) # closes dashboardbody
) # closes dashboardpage

# Shiny server ####
server <- shinyServer(function(input, output) {
    
    # Univariate analysis ##
    output$histPlot <- renderPlotly({
        
        #Column name variable
        num_val = ifelse(input$num == 'Temperature (F)', 'temp',
                         ifelse(input$num == 'Feeling temperature (F)', 'atemp',
                                ifelse(input$num == 'Humidity (%)', 'humidity',
                                       ifelse(input$num == 'Wind speed (mph)', 'windspeed',
                                              ifelse(input$num == 'Casual', 'casual',
                                                     ifelse(input$num == 'Registered', 'registered', 'total'))))))
        
        p_hist <- ggplot(data = bike_clean, aes(x = !!sym(num_val))) + 
            geom_histogram(stat = "bin", 
                           aes(fill = ifelse(..count.. == max(..count..), 'Highest', 'Other')),  # Set fill conditionally
                           color = 'lightgrey') +
            scale_fill_manual(values = c('Highest' = '#ee6c4d', 'Other' = '#20639b')) +  # Define fill colors
            theme(axis.text = element_text(size = 10),
                  axis.title = element_text(size = 12)) +
            labs(x = sprintf('%s', input$num), y = 'Count of Users') +
            stat_bin(geom = 'text', size = 3.5,
                     aes(label = ifelse(..count.. == max(..count..), comma(max(..count..)), '')),
                     position = position_stack(vjust = 1.03)) +
            
            scale_y_continuous(labels = scales::comma) +  # Add this line for thousands separators
            
            guides(fill = FALSE) # Suppress the fill legend
        
        ggplotly(p_hist, tooltip = c("x", "y", "label"))
        
    })
    
    output$freqPlot <- renderPlotly({
        
        #Column name variable
        cat_val = ifelse(input$cat == 'Season', 'season',
                         ifelse(input$cat == 'Year', 'yr',
                                ifelse(input$cat == 'Month', 'mnth',
                                       ifelse(input$cat == 'Hour', 'hr',
                                              ifelse(input$cat == 'Holiday', 'holiday',
                                                     ifelse(input$cat == 'Weekday', 'weekday',
                                                            #ifelse(input$cat == 'Working day', 'workingday',
                                                                   'weathersit'))))))#)
        
        #Frequency plot
        p_freq <- ggplot(data = bike_clean, aes(x = !!sym(cat_val))) +
            geom_bar(stat = 'count', 
                     aes(fill = ifelse(..count.. == max(..count..), 'Highest', 'Other')), 
                     width = 0.5) +
            scale_fill_manual(values = c('Highest' = '#ee6c4d', 'Other' = '#20639b')) +  # Define fill colors
            stat_count(geom = 'text', size = 3.5,
                       aes(label = ifelse(..count.. == max(..count..), comma(max(..count..)), "")), # display value for only the bar with the highest count
                       position = position_stack(vjust = 1.03)) +
            theme(#axis.text.y = element_blank(),
                #axis.ticks.y = element_blank(),
                axis.text = element_text(size = 10),
                axis.title = element_text(size = 12)) +
            labs(x = sprintf('%s', input$cat), y = 'Count of Users') +
            
            scale_y_continuous(labels = scales::comma) +  # Add this line for thousands separators
            
            theme(legend.position = "none")  # Suppress legend
        
        ggplotly(p_freq, tooltip = c("x", "y", "label"))
        
        
    })
    
    # Dashboard analysis ##
    output$linePlot <- renderPlotly({
        
        # original
        if(input$year != 'All'){
            
            #Creating a table filter by year for the line plot
            counts <- bike_clean %>% group_by(mnth) %>%
                filter(yr == input$year) %>%
                summarise(registered = sum(registered),
                          casual = sum(casual),
                          total = sum(total))
            
        } else{
            
            #Creating a table for the line plot
            counts <- bike_clean %>%
                group_by(mnth) %>%
                summarise(registered = sum(registered),
                          casual = sum(casual),
                          total = sum(total))
        }
        
        #Column name variable
        regis_val = ifelse(input$regis == 'Total', 'total',
                           ifelse(input$regis == 'Registered', 'registered','casual'))
        
        #Line plot
        p_line <- ggplot(counts, aes(x = mnth, y = counts[[regis_val]],
                                     group = 1))+
            geom_line(linewidth = 1.25, colour = "#20639b")+
            geom_point(size = 3.25,
                       color = ifelse(counts[[regis_val]] == max(counts[[regis_val]]), '#ee6c4d','#20639b'))+
            labs(title = sprintf('%s users by month', input$regis),
                 x = 'Month', y = 'Count of Users')+
            theme(axis.text = element_text(size = 10),
                  axis.title = element_text(size = 12),
                  plot.title = element_text(size = 12))+ #, face = 'bold'))+
            geom_text(aes(label = ifelse(counts[[regis_val]] == max(counts[[regis_val]]), comma(counts[[regis_val]]),'')),
                      col ='#293241',
                      size = 3.5,
                      nudge_x = 0.25, nudge_y = 17000) +
            
            scale_y_continuous(
                labels = function(x) paste0(format(x / 1000), "k"),
                breaks = seq(0, 375000, 50000),
                limits = c(0, 375000)
            )
        
        ggplotly(p_line, tooltip = c("x", "y", "label"))
    })
    
    output$barPlot <- renderPlotly({
        
        if(input$year != 'All'){
            
            if(input$weather != 'All'){
                
                #Creating a table filter by year and weathersit for the bar plot
                weather <- bike_clean %>% 
                    group_by(season, weathersit) %>% 
                    filter(yr == input$year) %>%  
                    summarise(registered = sum(registered), casual = sum(casual), total = sum(total))
                
                weather <- weather %>% 
                    filter(weathersit == input$weather)
                
            } else{
                
                #Creating a table filter by year for the bar plot
                weather <- bike_clean %>% 
                    group_by(season, weathersit) %>% 
                    filter(yr == input$year) %>%  
                    summarise(registered = sum(registered), casual = sum(casual), total = sum(total))
            }
            
        } else{
            
            if(input$weather != 'All'){
                
                #Creating a table filter by weathersit for the bar plot
                weather <- bike_clean %>% 
                    group_by(season, weathersit) %>% 
                    filter(weathersit == input$weather) %>%  
                    summarise(registered = sum(registered), casual = sum(casual), total = sum(total))
                
            } else{
                
                #Creating a table for the bar plot
                weather <- bike_clean %>% 
                    group_by(season, weathersit) %>%  
                    summarise(registered = sum(registered), casual = sum(casual), total = sum(total))
            }
        }
        
        #Column name variable
        regis_val = ifelse(input$regis == 'Total', 'total', 
                           ifelse(input$regis == 'Registered', 'registered','casual'))
        
        
        #Bar plot
        p_bar <- ggplot(weather, aes(x = season, y = !!sym(regis_val), fill = weathersit))+
            geom_bar(stat = 'identity', position=position_dodge())+
            geom_text(aes(label = ifelse(weather[[regis_val]] == max(weather[[regis_val]]), comma(weather[[regis_val]]),'')),
                      nudge_x = 0.225, nudge_y = 14000, 
                      size = 3.5) +
            theme(axis.text = element_text(size = 10),
                  axis.title = element_text(size = 12),
                  plot.title = element_text(size = 12),
                  legend.text = element_text(size = 10)) +
            labs(title = sprintf('%s users by season and weather', input$regis),
                 #subtitle = sprintf('Throughout the year %s', input$year),
                 x = 'Season', 
                 y = 'Count of Users')+
            scale_fill_manual(values = c('Good' = '#22a7f0', 'Fair' = '#a7d5ed', 'Bad' = '#e1a692', 'Very Bad' = '#e14b31'),
                              name = 'Weather') +
            scale_y_continuous(
                labels = function(x) paste0(format(x / 1000), "k"),
                breaks = seq(0, 800000, 100000),
                limits = c(0, 825000)
            )
        
        ggplotly(p_bar, tooltip = c("x", "y", "label"))
    })
    
    # Prediction model ##
    #React value when using the action button
    a <- reactiveValues(result = NULL)
    
    observeEvent(input$cal, {
        #Copy of the test data without the dependent variable
        test_pred <- test_set[, names(test_set) != "total"] # exclude response var
        
        #Dataframe for the single prediction
        values = data.frame(mnth = input$p_mnth, 
                            hr = input$p_hr,
                            weekday = input$p_weekd,
                            weathersit = input$p_weather,
                            holiday = input$p_holid,
                            humidity = as.integer(input$p_hum),
                            temp = input$p_temp, 
                            atemp = input$p_ftemp, 
                            windspeed = input$p_wind)
        
        #Include the values into the new data
        test_pred <- rbind(test_pred,values)
        
        #Single prediction using the randomforest model
        a$result <-  round(predict(best_model, 
                                   newdata = test_pred[nrow(test_pred),]), 
                           digits = 0)
    })
    #Display the prediction value
    output$value <- renderText({
        paste(a$result)
    })
    
    output$range <- renderText({
        #Display the range of prediction value using the MAE value
        input$cal
        isolate(sprintf('%s to %s', 
                        round(a$result - best_model_mae, digits = 0), 
                        round(a$result + best_model_mae, digits = 0)))
    })
    
    output$predictionDistribution <- renderPlot({
        #Copy of the test data without the dependent variable
        test_set_preds <- test_set[, names(test_set) != "total"]
        
        # Distribution of predictions
        test_set_preds <- test_set_preds %>%
            mutate(model_preds = as.numeric(predict(best_model, newdata = test_set_preds)))
        
        # Plotting the distribution
        ggplot(data = test_set_preds, aes(x = model_preds)) +
            geom_histogram(stat = "bin", fill = '#20639b', color = 'lightgrey') +
            labs(x = 'Predicted Users', y = 'Frequency') +
            geom_vline(xintercept = a$result, colour = "red", linewidth = 1.2) + # prediction line
            geom_vline(xintercept = c(a$result-best_model_mae, a$result+best_model_mae), colour = "red", alpha = .4, linetype = "dashed", linewidth = 1.1) + # confidence bounds
            theme(axis.text = element_text(size = 10),
                  axis.title = element_text(size = 12))
        
    })
    
})

# Run the app
shinyApp(ui = ui, server = server)

