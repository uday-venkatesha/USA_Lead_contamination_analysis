library(tidyverse)
library(tsibble)
library(forecast)

ts<-read.csv('Desktop/Time_Series.csv',header = TRUE)

ts$Year<-as.factor(ts$Year)

# Top 10 EBLL Counties
top_counties <- ts %>%
  group_by(County) %>%
  summarise(total_eblls = sum(Number.of.Children.with.EBLLs)) %>%
  arrange(desc(total_eblls)) %>%
  slice(1:10) %>%
  pull(County)

# Filter Top 10 EBLL Counties
df_top_10 <- ts %>%
  filter(County %in% top_counties)

# Plotly plot for Top 10 based on EBLL and Year
plot_ly(df_top_10, x = ~Year, y = ~Number.of.Children.with.EBLLs, color = ~County, type = "scatter", mode = "lines") %>%
  layout(title = "Top 10 Counties with Highest levels of EBLL",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Number of Children with EBLLs"))

# Plotly plot for Top 10 Counties with highest EBLL based on Income and Year
plot_ly(df_top_10, x = ~Year, y = ~Income, color = ~County, type = "scatter", mode = "lines") %>%
  layout(title = "Median Income of Top 10 Counties with highest EBLL",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Median Income"))

# Combining both
plot_ly() %>%
  add_trace(data = df_top_10, x = ~Year, y = ~Number.of.Children.with.EBLLs, color = ~County,
            type = "scatter", mode = "lines", name = "Number of Children with EBLLs",
            hoverinfo = "text", text = ~paste("County: ", County, "<br>Year: ", Year, "<br>EBLLs: ", Number.of.Children.with.EBLLs)) %>%
  add_trace(data = df_top_10, x = ~Year, y = ~Income, color = ~County,
            type = "scatter", mode = "lines", name = "Median Income",
            yaxis = "y2", hoverinfo = "text", text = ~paste("County: ", County, "<br>Year: ", Year, "<br>Income: ", Income)) %>%
  layout(title = "Top 10 Counties with Highest Levels of EBLL and their Median Income",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Number of Children with EBLLs", side = "left"),
         yaxis2 = list(title = "Median Income", side = "right", overlaying = "y", showgrid = FALSE),
         showlegend = FALSE,
         hovermode = "closest")


# Top 10 Elevation Rate Counties
top_elevation_counties <- ts %>%
  group_by(County) %>%
  summarise(total_elevation = sum(Rate.of.Elevation)) %>%
  arrange(desc(total_elevation)) %>%
  slice(1:10)

# Filter Top 10 Elevation Rate Counties
df_top_10_elevation <- ts %>%
  filter(County %in% top_elevation_counties$County)

plot_ly(df_top_10_elevation, x = ~Year, y = ~Rate.of.Elevation, color = ~County, type = "scatter", mode = "lines") %>%
  layout(title = "Top 10 Counties with highest Rate of Elevation",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Rate Of Elevation"))




# Forecast Blood Levels in 2020
ts_subset <- ts[ts$Year %in% c(2017, 2018, 2019), ]
counties <- unique(ts_subset$County)

forecast_df <- data.frame(County = character(), Forecasted_EBLLs_2020 = numeric(), stringsAsFactors = FALSE)


for (county in counties) {
  county_data <- ts_subset$Number.of.Children.with.EBLLs[ts_subset$County == county]
  arima_model <- auto.arima(county_data)
  

  forecast_result <- forecast(arima_model, h = 1)
  forecast_value <- forecast_result$mean[1]
  
  forecast_df <- rbind(forecast_df, data.frame(County = county, Forecasted_EBLLs_2020 = forecast_value))
}


sorted_forecast_df <- forecast_df[order(-forecast_df$Forecasted_EBLLs_2020), ]
top_10_counties_forecast <- head(sorted_forecast_df, 10)


library(ggplot2)

# Create a bar plot for the top 10 counties forecast 
ggplot(top_10_counties_forecast, aes(x = reorder(County, -Forecasted_EBLLs_2020), y = Forecasted_EBLLs_2020, fill = County)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(Forecasted_EBLLs_2020/sum(Forecasted_EBLLs_2020))), position = position_stack(vjust = 0.5)) +
  labs(title = "Top 10 Counties with Highest Forecasted Blood Levels in 2020", x = "County", y = "Forecasted Blood Levels") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))