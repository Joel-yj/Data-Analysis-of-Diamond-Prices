library(ggplot2)

#removing outliers
y_outliers <- which(diamonds$y > 20)
z_outliers <- which(diamonds$z > 15)
z_zero_outliers <- which(diamonds$z < 0.1)
del_indices <- c(y_outliers, z_outliers, z_zero_outliers)
del_indices <- c(del_indices, 52861)

#Dataframe cleaning & adding new log(price) and log(carat) as a column
new_diamonds <- diamonds[-del_indices, ]
new_diamonds$log_price <- log(new_diamonds$price)
new_diamonds$log_carat <- log(new_diamonds$carat)

#Summary Statistics of price,carat, (cut,color,clarity), (x,y,z), (table,depth)

#price
price = diamonds$price
boxplot(price, main = "Boxplot of price")
hist(price, xlim = c(0, 20000))
qqnorm(price)
qqline(price, col = 'red')
summary(price)

log_price = log(price)
hist(log_price, main = "Histogram of log(price)")
boxplot(log_price, main = "Boxplot of log(price)")
qqnorm(log_price)
qqline(log_price, col = 'red')
summary(log_price)

#carat
carat = diamonds$carat
hist(carat, ylim = c(0, 20000))
qqnorm(carat)
qqline(carat, col = 'red')
summary(carat)

log_carat = log(carat)
hist(log_carat, ylim = c(0, 10000), main = "Histogram of log(carat)")
boxplot(log_carat, main = "Boxplot of log(carat)")
qqnorm(log_carat)
qqline(log_carat, col = 'red')
summary(log_carat)

#Statistical analysis of log(carat) VS log(price)
print(paste(
  "The correlation coefficient between log(carat) and log(price):",
  cor(new_diamonds$log_carat, new_diamonds$log_price)
))
plot(
  new_diamonds$log_carat ~ new_diamonds$log_price,
  xlab = "log(carat)",
  ylab = "log(price)",
  main = "log(price) vs log(carat)"
)
#linear regression between log_carat and log_price
fit_log_carat = lm(new_diamonds$log_carat ~ new_diamonds$log_price, data = new_diamonds)
summary(fit_log_carat)
abline(fit_log_carat, col = "red")

#Cut, Color, Clarity Respectively
diamonds$cut = factor(diamonds$cut,
                      levels = c('Fair', 'Good', 'Very Good', 'Premium', 'Ideal'))
ggplot(data = diamonds, aes(x = cut)) + ggtitle('Count of diamonds with each cut rating') +
  geom_bar()

diamonds$color = factor(diamonds$color, levels = c('J', 'I', 'H', 'G', 'F', 'E', 'D'))
ggplot(data = diamonds, aes(x = color)) + ggtitle('Count of diamonds with each color rating') +
  geom_bar()

diamonds$clarity = factor(diamonds$clarity,
                          levels = c('I1', 'SI2', 'SI1', 'VS2', 'VS1', 'VVS2', 'VVS1', 'IF'))
ggplot(data = diamonds, aes(x = clarity)) + ggtitle('Count of diamonds with each clarity rating') +
  geom_bar()

##Statistical Analysis of Cut, Color, Clarity

#boxplot
boxplot(log_price ~ cut, data = new_diamonds, main = "log_price grouped by Cut")
boxplot(log_price ~ color, data = new_diamonds, main = "log_price grouped by Color")
boxplot(log_price ~ clarity, data = new_diamonds, main = "log_price grouped by Clarity")

#anova
aov_cut = aov(log_price ~ cut, data = new_diamonds)
summary(aov_cut)
aov_color = aov(log_price ~ color, data = new_diamonds)
summary(aov_color)
aov_clarity = aov(log_price ~ clarity, data = new_diamonds)
summary(aov_clarity)

# linear regression
new_diamonds$cut_num <- unclass(new_diamonds$cut)
new_diamonds$color_num <- unclass(new_diamonds$color)
new_diamonds$clarity_num <- unclass(new_diamonds$clarity)
lm_cut = lm(log_price ~ cut_num, data = new_diamonds)
summary(lm_cut)
lm_color = lm(log_price ~ color_num, data = new_diamonds)
summary(lm_color)
lm_clarity = lm(log_price ~ clarity_num, data = new_diamonds)
summary(lm_clarity)

#plotting regression plots
plot(
  new_diamonds$cut_num,
  new_diamonds$log_price,
  main = 'Regression for Cut and log(price)',
  xlab = 'Cut (numbered)',
  ylab = 'log(price)'
)
abline(lm_cut, col = 'red')
plot(
  new_diamonds$color_num,
  new_diamonds$log_price,
  main = 'Regression for Color and log(price)',
  xlab = 'Color (numbered)',
  ylab = 'log(price)'
)
abline(lm_color, col = 'red')
plot(
  new_diamonds$clarity_num,
  new_diamonds$log_price,
  main = 'Regression for Clarity and log(price)',
  xlab = 'Clarity (numbered)',
  ylab = 'log(price)'
)
abline(lm_clarity, col = 'red')

# for SUBSET where carat = 0.5
equal_carat = subset(new_diamonds, carat == 0.5)

#plotting boxplot
equal_carat$color = factor(equal_carat$color, levels = c('J', 'I', 'H', 'G', 'F', 'E', 'D'))
boxplot(log_price ~ cut, data = equal_carat, main = "log_price grouped by Cut")
boxplot(log_price ~ color, data = equal_carat, main = "log_price grouped by Color")
boxplot(log_price ~ clarity, data = equal_carat, main = "log_price grouped by Clarity")

#anova
aov_cut = aov(log_price ~ cut, data = equal_carat)
summary(aov_cut)
aov_color = aov(log_price ~ color, data = equal_carat)
summary(aov_color)
aov_clarity = aov(log_price ~ clarity, data = equal_carat)
summary(aov_clarity)

# linear regression
equal_carat$cut_num <- unclass(equal_carat$cut)
equal_carat$color_num <- unclass(equal_carat$color)
equal_carat$clarity_num <- unclass(equal_carat$clarity)
lm_cut = lm(log_price ~ cut_num, data = equal_carat)
summary(lm_cut)
lm_color = lm(log_price ~ color_num, data = equal_carat)
summary(lm_color)
lm_clarity = lm(log_price ~ clarity_num, data = equal_carat)
summary(lm_clarity)

#plotting regression plots
plot(
  equal_carat$cut_num,
  equal_carat$log_price,
  main = 'Regression for Cut and log(price)',
  xlab = 'Cut (numbered)',
  ylab = 'log(price)'
)
abline(lm_cut, col = 'red')
plot(
  equal_carat$color_num,
  equal_carat$log_price,
  main = 'Regression for Color and log(price)',
  xlab = 'Color (numbered)',
  ylab = 'log(price)'
)
abline(lm_color, col = 'red')
plot(
  equal_carat$clarity_num,
  equal_carat$log_price,
  main = 'Regression for Clarity and log(price)',
  xlab = 'Clarity (numbered)',
  ylab = 'log(price)'
)
abline(lm_clarity, col = 'red')


#x
x = diamonds$x
hist(x)
boxplot(x, main = "Boxplot of untrimmed x")
summary(x)

#y
y = diamonds$y
hist(y, breaks = 100, ylim = c(0, 10000))
boxplot(y, main = "Boxplot of untrimmed y")
summary(y)

#z
z = diamonds$z
hist(z, breaks = 100, ylim = c(0, 20000))
boxplot(z, main = "Boxplot of untrimmed z")
summary(z)

#z outlier
diamonds[48411, ]

#y outlier
diamonds[24068, ]
diamonds[49190, ]

#x outlier
diamonds[11183, ]


#new x
x = new_diamonds$x
hist(x)
boxplot(x, main = "Boxplot of x_trim")
summary(x)

#Statistical analysis of new x
plot(
  new_diamonds$log_price ~ new_diamonds$x,
  xlab = "x",
  ylab = "log(price)",
  main = "log(price) vs x"
)
print(paste(
  "The correlation coefficient between x_trim and log(price):",
  cor(new_diamonds$x, new_diamonds$log_price)
))
fit_x <- lm(log_price ~ x, data = new_diamonds)
abline(fit_x, col = "red")
summary(fit_x)

#new y
y = new_diamonds$y
hist(y, ylim = c(0, 10000))
boxplot(y, main = "Boxplot of y_trim")
summary(y)

#Statistical analysis of new y
plot(
  new_diamonds$log_price ~ new_diamonds$y,
  xlab = "y",
  ylab = "log(price)",
  main = "log(price) vs y"
)
print(paste(
  "The correlation coefficient between y_trim and log(price):",
  cor(new_diamonds$y, new_diamonds$log_price)
))
fit_y <- lm(log_price ~ y, data = new_diamonds)
abline(fit_y, col = "red")
summary(fit_y)

#new z
z = new_diamonds$z
hist(z, ylim = c(0, 20000))
boxplot(z, main = "Boxplot of z_trim")
summary(z)

#Statistical analysis of new z
plot(
  new_diamonds$log_price ~ new_diamonds$z,
  xlab = "z",
  ylab = "log(price)",
  main = "log(price) vs z"
)
print(paste(
  "The correlation coefficient between z_trim and log(price):",
  cor(new_diamonds$z, new_diamonds$log_price)
))
fit_z <- lm(log_price ~ z, data = new_diamonds)
abline(fit_z, col = "red")
summary(fit_z)

# Multivariate linear regression between log(price) and (x,y,z)
new_diamonds$x_norm <-
  (new_diamonds$x - mean(new_diamonds$x)) / sd(new_diamonds$x)
new_diamonds$y_norm <-
  (new_diamonds$y - mean(new_diamonds$y)) / sd(new_diamonds$y)
new_diamonds$z_norm <-
  (new_diamonds$z - mean(new_diamonds$z)) / sd(new_diamonds$z)

fit_xyz_plus_norm <-
  lm(log_price ~ x_norm + y_norm + z_norm, data = new_diamonds)
summary(fit_xyz_plus_norm) # show results

# Multivariate linear regression between log(price) and (x,y,z,log_carat) (section 4.6)
new_diamonds$log_carat_norm <-
  (new_diamonds$log_carat - mean(new_diamonds$log_carat)) / sd(new_diamonds$log_carat)
fit_multi <-
  lm(log_price ~ x_norm + y_norm + z_norm + log_carat_norm , data = new_diamonds)
summary(fit_multi) # show results

#table_percentage
table_percentage = new_diamonds$table
boxplot(price, main = "Boxplot of table_percentage")
hist(table_percentage)
summary(table_percentage)

#table_percentage statistical analysis [Linear Regression]
plot(
  new_diamonds$log_price ~ new_diamonds$table,
  main = "log(price) vs Table Percentage",
  xlab = "Table Percentage",
  ylab = "log(price)"
)
fit_table = lm(formula = log_price ~ table , data = new_diamonds)
summary(fit_table)
abline(fit_table, col = "red")
print(paste(
  "The correlation coefficient between table_percentage and logPrice:",
  cor(new_diamonds$table, new_diamonds$log_price)
))

cor.test(new_diamonds$table,new_diamonds$log_price)



#depth_percentage
depth_percentage = diamonds$depth
boxplot(price, main = "Boxplot of depth_percentage")
hist(depth_percentage)
summary(depth_percentage)

#depth_percentage statistical analysis [Linear Regression]
plot(
  new_diamonds$log_price ~ new_diamonds$depth,
  main = "log(price) vs Depth Percentange",
  xlab = "Depth Percentage" ,
  ylab = "log(price)"
)
fit_depth = lm(formula = log_price ~ depth, data = new_diamonds)
summary(fit_depth)
abline(fit_depth, col = "red")
print(paste(
  "The correlation coefficient between depth_percentage and logPrice:",
  cor(new_diamonds$depth, new_diamonds$log_price)
))

cor.test(new_diamonds$depth,new_diamonds$log_price)
