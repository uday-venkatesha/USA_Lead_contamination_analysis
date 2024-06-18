library(ggplot2)
library(corrplot)
texas_df<- read.csv('Desktop/Dpa project/Texas /Texas_19_Final.csv',header = TRUE)

head(texas_df,2)

ggplot(texas_df, aes(x = County, y = Number.of.Children.Tested)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of Children Tested per County", x = "County", y = "Number of Children Tested") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(texas_df, aes(x = "", y = Rate.of.Elevation)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Box plot for Rate of Elevation", x = "", y = "Rate of Elevation")

ggplot(texas_df, aes(x = County, y = Rank.in.State.2019)) +
  geom_bar(stat = "identity", fill = "coral") +
  labs(title = "Rank in State 2019", x = "County", y = "Rank in State") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Correlation Matrix
correlation_matrix <- cor(texas_df[, c("Number.of.Children.Tested", "Rate.of.Testing", "Number.of.Children.with.EBLLs", "Rate.of.Elevation", "Income.2019", "Percent.change.2019", "Rank.in.State.2019")])
correlation_matrix


ggplot(texas_df, aes(x = "", y = Number.of.Children.with.EBLLs, fill = County)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Percent of Children with EBLLs by County", x = "", y = "")

# Assuming your data frame is named 'df'

# Sort the data frame by Rate of Elevation in descending order


