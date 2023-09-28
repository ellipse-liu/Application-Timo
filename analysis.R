# load csv data
data = read.csv("C:/Users/Timo_L/Desktop/Simmons Proj/results.csv")

# do analysis of age on politics
# binned 22-26 and 27-30

age_under_26 = strtoi(data$result[data$age <= 26])
age_over_26 = strtoi(data$result[data$age > 26])

age_under_26 = age_under_26[!is.na(as.numeric(age_under_26))]
age_over_26 = age_over_26[!is.na(as.numeric(age_over_26))]

mean_under_26 = mean(age_under_26)
mean_over_26 = mean(age_over_26)

shapiro.test(age_under_26) #ensure normality of data
shapiro.test(age_over_26) #ensure normality of data

t.test(age_under_26, age_over_26)

# plotting data
# x-axis age bins
# y-axis frequency
# columns greater or less than 5 for result

install.packages("ggplot2")
library(ggplot2)

x1 = length(age_under_26[age_under_26 <= 5])
x2 = length(age_over_26[age_over_26 <= 5])

x3 = length(age_under_26[age_under_26 > 5])
x4 = length(age_over_26[age_over_26 > 5])

data <- data.frame(
  x_category = c("age under 26", "age under 26", "age over 26", "age over 26"),
  y_category = c("more liberal", "more conservative", "more liberal", "more conservative"),
  frequency = c(x1, x3, x2, x4)
)

bar_chart <- ggplot(data, aes(x = x_category, y = frequency, fill = y_category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Stance on Economic Policy by Age",
       x = "X Categories",
       y = "Frequency") +
  scale_fill_manual(values = c("more liberal" = "blue", "more conservative" = "red")) + # Customize fill colors
  theme_minimal()

print(bar_chart)

# analysis of educational status on politics
masters = strtoi(data$result[data$degree == "master's"])
bachelors = strtoi(data$result[data$degree > "bachelor's"])

masters = masters[!is.na(as.numeric(masters))]
bachelors = bachelors[!is.na(as.numeric(bachelors))]

masters_mean = mean(masters)
bachelors_mean = mean(bachelors)

shapiro.test(masters)
shapiro.test(bachelors)

x1 = length(bachelors[bachelors <= 5])
x2 = length(masters[masters <= 5])

x3 = length(bachelors[bachelors > 5])
x4 = length(masters[masters > 5])

data <- data.frame(
  x_category = c("Bachelors", "Bachelors", "Masters", "Masters"),
  y_category = c("more liberal", "more conservative", "more liberal", "more conservative"),
  frequency = c(x1, x3, x2, x4)
)

bar_chart <- ggplot(data, aes(x = x_category, y = frequency, fill = y_category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Stance on Economic Policy by Degree",
       x = "X Categories",
       y = "Frequency") +
  scale_fill_manual(values = c("more liberal" = "blue", "more conservative" = "red")) + # Customize fill colors
  theme_minimal()

print(bar_chart)


# check for imbalance in data between conservative and liberal
data = read.csv("C:/Users/Timo_L/Desktop/Simmons Proj/results.csv")
print(length(data$result[data$result > 5]))
