install.packages("ggplot2")
library(ggplot2)
install.packages("caret")
library(caret)
install.packages("lattice")
install.packages("data.table")
library(data.table)
install.packages("ggplot2")
library(ggplot2)



data <- read.csv("/Users/udayvenkatesha/Downloads/NJ1.csv")
head(data)

colnames(data)

str(data)
data$Income<- as.numeric(data$Income)
install.packages("corrplot")
hist(data$Income)


ggplot(data, aes(x = County., y = PercentageScreened)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  theme_minimal() +
  labs(title = "Total Children Screened by County",
       x = "County",
       y = "Percent Screened")


library(ggplot2)

ggplot(data, aes(x = Income, y = PercentageScreened)) +
  geom_point(color = "#4e79a7", size = 3, alpha = 0.7) +
  labs(
    x = "Per Capita Income",
    y = "% of People Affected",
    title = "Relationship Between Income and % of People Affected",
    fill = "Variable Legend"  # Corrected from color to fill
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "bottom"
  )


# Example: Larger bar chart with vertical county names
ggplot(data, aes(x = County., y = PercentageScreened)) +
  geom_bar(fill = "#4e79a7", alpha = 0.7, stat = "identity") +
  labs(
    x = "County",
    y = "% People Affected",
    title = " % People Affected by each County",
    fill = "Legend Title"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12),  
    legend.position = "bottom",  
    plot.margin = margin(1, 1, 2, 1, "cm")  
  )

# this can be reported 
library(ggplot2)

# Assuming 'Income' is a categorical variable
ggplot(data, aes(x = County., y = PercentageScreened, fill = Income)) +
  geom_bar(alpha = 0.7, stat = "identity") +
  labs(
    x = "County",
    y = "% People Affected",
    title = "Comparison of Per Capita Income and % People Affected",
    
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "bottom",
    plot.margin = margin(1, 1, 2, 1, "cm")
  )


ggplot(data,aes(x=Income,y=PercentageScreened),group=1)+
  geom_line(aes(y=PercentageScreened),color= "Green", size=2)+
  theme_minimal()+
  
  theme( axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
         plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
         axis.title = element_text(size = 14),
         axis.text = element_text(size = 12),
         plot.margin = margin(1, 1, 2, 1, "cm"))



numeric_data <- data[, sapply(data, is.numeric)]
pairs(numeric_data, cex = 1 )


# Install corrplot (if not already installed)
install.packages("corrplot")

# Load corrplot
library(corrplot)

corelation_matrix <- cor(numeric_data)
print(corelation_matrix)
corrplot(corelation_matrix)

ggplot(data, aes(x=Income,y=PercentageEBLL))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot of EBLL vs. Income",
       x = "Income",
       y = "EBLL")



# Data
counties <- data$County.
total_children <- data$Total_children
percent_affected <- data$PercentageScreened

# Bar chart
library(ggplot2)
ggplot(
  data = data.frame(counties, total_children, percent_affected),
  aes(x = countries, y = total_children, fill = "total_children")
) +
  geom_bar(stat = "identity") +
  aes(x = counties, y = percent_affected, fill = "percent_affected") +
  geom_bar(stat = "identity", alpha = 0.5) +
  labs(
    title = "Comparison of Total Children and % of People Affected",
    x = "Country",
    y = "Value"
  ) +
  theme(axis.text.x = rot = 45)



library(ggplot2)

ggplot(
  data = data.frame(counties, total_children, percent_affected),
  aes(x = counties)
) +
  geom_bar(aes(y = total_children, fill = "Total Children"), stat = "identity") +
  geom_bar(aes(y = percent_affected, fill = "Percent Affected"), stat = "identity", alpha = 0.5) +
  labs(
    title = "Comparison of Total Children and % of People Affected",
    x = "Country",
    y = "Value"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




library(ggplot2)

ggplot(
  data = data.frame(counties, total_children, percent_affected),
  aes(x = counties, y = total_children, size = percent_affected)
) +
  geom_point() +
  labs(
    title = "Bubble Chart of Total Children by County",
    x = "Country",
    y = "Total Children"
  )+theme_minimal()+
  
  theme( axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
         plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
         axis.title = element_text(size = 14),
         axis.text = element_text(size = 12),
         plot.margin = margin(1, 1, 2, 1, "cm"))


median(data$Income)

data[data$County.=="ESSEX", 'Income']
data[data$County.=="HUDSON", 'Income']
data[data$County.=="OCEAN", 'Income']
data[data$County.=="PASSAIC", 'Income']
data[data$County.=="UNION", 'Income']















