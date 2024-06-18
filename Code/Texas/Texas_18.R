library(ggplot2)
library(corrplot)
library(magrittr)
library(dplyr)
texas_df_18<- read.csv('Desktop/Dpa project/Texas /Texas_2018_Final.csv',header = TRUE)
colnames(texas_df_18)
# Top 10 Counties with highest Percent of Children with Elevated Blood Levels
texas_df_18_top_10 <- texas_df_18[order(-texas_df_18$Number.of.Children.with.EBLLs), ]


top_10_counties <- head(texas_df_18_top_10, 10)

library(ggplot2)

ggplot(top_10_counties, aes(x = reorder(County, Number.of.Children.with.EBLLs), y = Number.of.Children.with.EBLLs, fill = County)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(Number.of.Children.with.EBLLs/sum(Number.of.Children.with.EBLLs))), position = position_stack(vjust = 0.5)) +
  labs(title = "Top 10 Counties with Highest Percent of Children with EBLLs", x = "County", y = "Number of Children with EBLLs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bottom 10 Counties (Not used just for reference)

texas_df_18_bottom_10 <- texas_df_18[order(texas_df_18$Number.of.Children.with.EBLLs), ]
bottom_10_counties <- head(texas_df_18_bottom_10, 10)
ggplot(bottom_10_counties, aes(x = reorder(County, desc(Number.of.Children.with.EBLLs)), y = Number.of.Children.with.EBLLs, fill = County)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Bottom 10 Counties with Lowest Number of Children with EBLLs", x = "County", y = "Number of Children with EBLLs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = Number.of.Children.with.EBLLs), vjust = -0.5, size = 3, color = "black")


#Median Income Comparison between Counties with Lowest and Highest EBLLs
income_county_data <- rbind(
  data.frame(Group = "Lowest EBLL", County = factor(bottom_10_counties$County, levels = unique(bottom_10_counties$County)), Income.2018 = bottom_10_counties$Income.2018),
  data.frame(Group = "Highest EBLL", County = factor(top_10_counties$County, levels = unique(top_10_counties$County)), Income.2018 = top_10_counties$Income.2018)
)

# Bar graph (Not Recommended)
ggplot(income_county_data, aes(x = County, y = Income.2018, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Median Income Comparison between Counties with Lowest and Highest EBLLs",
       x = "County", y = "Median Income") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Scatter Plot(Use this)
ggplot(income_county_data, aes(x = County, y = Income.2018, color = Group)) +
  geom_point(size = 4, alpha = 0.7, position = position_jitter(width = 0.3)) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
  labs(title = "Median Income Comparison between Counties with Lowest and Highest EBLLs",
       x = "County", y = "Median Income") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top") +
  scale_color_manual(values = c("Lowest EBLL" = "#4e79a7", "Highest EBLL" = "#e15759")) +
  scale_fill_manual(values = c("Lowest EBLL" = "#4e79a7", "Highest EBLL" = "#e15759"))



# Plot the data points and the regression line
ggplot(texas_df_18, aes(x = Number.of.Children.with.EBLLs, y = Income.2018)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Linear Regression",
       x = "Number of Children with EBLLs",
       y = "Income 2019")

model <- lm(Income.2018 ~ Number.of.Children.with.EBLLs, data = texas_df_18)
# Plot the data points and the regression line
ggplot(texas_df_18, aes(x = Number.of.Children.with.EBLLs, y = Income.2018)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Linear Regression",
       x = "Number of Children with EBLLs",
       y = "Income 2019")


# KMeans Clustering for EBLL and Income

set.seed(123) 
data_for_clustering <- texas_df_18[, c("Number.of.Children.with.EBLLs", "Income.2018")]
scaled_data <- scale(data_for_clustering)


k <- 3
kmeans_model <- kmeans(scaled_data, centers = k)
texas_df_18_copy<- texas_df_18
texas_df_18_copy$cluster <- as.factor(kmeans_model$cluster)



ggplot(texas_df_18_copy, aes(x = Number.of.Children.with.EBLLs, y = Income.2018, color = cluster)) +
  geom_point() +
  labs(title = "K-Means Clustering",
       x = "Number of Children with EBLLs",
       y = " Median Income ") +
  scale_color_discrete(name = "Cluster")

#rate of elev vs percent change 

# Children Tested vs Income and rate of elevation 
custom_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", color = "darkblue"),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )


ggplot(texas_df_18, aes(x = Number.of.Children.Tested, y = Income.2018, size = Number.of.Children.Tested, color = Rate.of.Elevation)) +
  geom_point(alpha = 0.7) +
  scale_color_gradient(low = "blue", high = "orange") +
  scale_size_continuous(range = c(3, 15)) +
  labs(title = "Children Tested vs Income",
       x = "Number of Children Tested",
       y = "Income",
       size = "Number of Children Tested",
       color = "Rate of Elevation in EBLLs") +
  custom_theme

# Children Tested vs Income Plot Zoomed in on Clustered Area(Axis Changed)
custom_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", color = "darkblue"),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

ggplot(texas_df_18, aes(x = Number.of.Children.Tested, y = Income.2018, size = Number.of.Children.Tested, color = Rate.of.Elevation)) +
  geom_point(alpha = 0.7) +
  scale_color_gradient(low = "blue", high = "orange") +
  scale_size_continuous(range = c(3, 15)) +
  labs(title = "Children Tested vs Income",
       x = "Number of Children Tested",
       y = "Income",
       size = "Number of Children Tested",
       color = "Rate of Elevation in EBLLs") +
  custom_theme +
  coord_cartesian(xlim = c(0, 5000), ylim = c(26000, 85000))
