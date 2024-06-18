library(dplyr)

texas_2016<- read.csv('Desktop/Dpa project/Texas /Texas_2016.csv')
head(texas_2016)


str(texas_2016)
summary(texas_2016)
head(texas_2016,3)

texas_2016$Year <- 2016

head(texas_2016,2)

any(is.na(texas_2016))

nrow(texas_2016)
texas_2016 <- data.frame(lapply(texas_2016, function(x) gsub("~", "NA", x)))
texas_2016<-na.omit(texas_2016)
nrow(texas_2016)
hist(texas_2016$Rate.of.Testing, main = "Histogram of Testing Rates")
has_tilde <- sapply(texas_2016, function(x) any(grepl("~", as.character(x))))
has_tilde

class(texas_2016$Rate.of.Testing)
class(texas_2016)
str(texas_2016)
texas_2016$Rate.of.Testing

texas_2016 <- texas_2016[complete.cases(texas_2016$Rate.of.Testing), ]
texas_2016

texas_2016<-texas_2016[texas_2016$Rate.of.Elevation !='NA',]
str(texas_2016)


nrow(texas_2016)

texas_2016 <- texas_2016 %>% mutate_all(~ gsub('\\<5', '5', .))

head(texas_2016,20)



hist(texas_2016$Rate.of.Testing, main = "Histogram of Testing Rates")


texas_2016$Number.of.Children.Tested<-as.numeric(b$Number.of.Children.Tested)
texas_2016$Number.of.Children.with.EBLLs<-as.numeric(b$Number.of.Children.with.EBLLs)

write.csv(texas_2016,'Texas_2016_Formatted.csv',header=TRUE)

final_df<-read.csv('Desktop/Dpa project/Texas /Texas_2016_Formatted.csv',header =TRUE)
final_df

str(final_df)

