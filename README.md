# capital_bikeshare_app
This Shiny app explores the Capital Bikeshare dataset, courtesy of the UC Irvine Machine Learning Repository. The bikeshare system consists of a fleet of bikes located at docking stations throughout Washington, D.C. that can be unlocked with an app. After a ride, a user simply returns their bike to any available docking station.
The dataset contains the hourly count of bike registrations between the years 2011 and 2012, taking into consideration several contextual variables such as weather condition, day of the week, and season. In total the data includes 16 variables and 17,379 observations in which each row of the data represents a specific hour of the day. The data ranges from January 1, 2011 to December 31, 2012.

*EDA Plots*
Descriptive exploration of each variable in the dataset.

*Dashboard*
Exploration of year, user type, and weather type on number of users.

*Prediction*
Predictive analysis. The model will predict the total number of bikes rented (aka "users") based on specific contextual conditions present in the data.
The prediction is based on a random forest supervised machine learning model that has a mean absolute error (MAE) of 45 users, and a root mean squared error (RMSE) of 67 users. This uncertainty is noted in the model prediction dashboard.

*Variables*
Temperature (F)
Feeling Temperature (F): Subjective assessment of how the temperature feels, in Fahrenheit
Humidity (%)
Wind speed (mph)
Casual: Count of casual users (Single Trip, 24-Hour Pass, 3-Day Pass or 5-Day Pass users).
Registered: Count of registered users (Annual Member, 30-Day Member or Day Key Member users).
Total: Count of total users.
Year
Season: Spring, Summer, Fall, Winter
Month
Weekday: Monday - Sunday
Hour
Holiday: Yes / No
Weather
Good: clear, few clouds, partly cloudy.
Fair: mist, cloudy, broken clouds.
Bad: light snow, light rain, thunderstorm, scattered clouds.
Very Bad: heavy rain, ice pallets, thunderstorm, mist, snow, fog.