## Turtle Games’s sales department has historically preferred to use R when performing 
## sales analyses due to existing workflow systems. As you’re able to perform data analysis 
## in R, you will perform exploratory data analysis and present your findings by utilising 
## basic statistics and plots. You'll explore and prepare the data set to analyse sales per 
## product. The sales department is hoping to use the findings of this exploratory analysis 
## to inform changes and improvements in the team. 


# The R code below uses the cleaned 'reviews_renamed' csv file i created using python

# Set the working directory
setwd("C:/Users/omaru/LSE_DA301_assignment_files/LSE_DA301_assignment_files_new/Final Submission/Final")

# Import the necessary libraries.
library(tidyverse)
library(scales)
library(plotly) 

# Importing cleaned version of turtle review csv file
turtle <- read.csv('reviews_renamed.csv', header=T)

# View the data frame
head(turtle)
summary(turtle)
### Observation: There are 2000 rows and 9 columns in the data set. 


# Gender column has datatype character. Lets change it to factor (categorical variable)
turtle2 <- mutate(turtle, gender = as.factor(gender))

summary(turtle2)

# Check for N/A in the data set 
sum(is.na(turtle2))
### Observation: There are no N/A values


# Numbering the education categories for ease of sorting in tables and visualisations
turtle2$education[turtle2$education == "Basic"] <- "1. Basic"
turtle2$education[turtle2$education == "diploma"] <- "2. dip"
turtle2$education[turtle2$education == "graduate"] <- "3. grad"
turtle2$education[turtle2$education == "postgraduate"] <- "4. postgrad"
turtle2$education[turtle2$education == "PhD"] <- "5. PhD"


#      ***DISTRIBUTION of the loyalty points data***

# Pull summary statistics of loyalty points
summary(turtle2$loyalty_points)
sd(turtle2$loyalty_points)
## Observation: The mean of the data is 1751 points. The max value 6847 is quite 
## far away from the mean


# Boxplot of the loyalty points data. (It is easy to see outliers with this chart)
boxplot <- ggplot(turtle2, aes(y=loyalty_points)) +
  geom_boxplot() +
  labs(title="Box plot of loyalty points",
       y='Loyalty points') +  
  theme_minimal()

ggplotly(boxplot) 
## Observation: The plot show the upper whisker is longer or it is right tailed 
## with a lot of positive outliers 


# Histogram of the loyalty points. Similar to boxplot, easy to spot outliers here.
histogram <- ggplot(turtle2, aes(x=loyalty_points)) +
  geom_histogram() + 
  labs(x="Loyalty points",
       y="Count of customers",
       title="Histogram of loyalty points")

ggplotly(histogram)
# Observation: It shows the data has a long right tail and is positively skewed.
# Bulk of the observations are around 1436 points with fewer after 2000 points


# Determine normality of the loyalty points data
# Observation: The data has a heavy tail between 1 and 3 standard deviations away from the mean
#qqnorm(turtle2$loyalty_points)
#qqline(turtle2$loyalty_points)

# Conduct shapiro-wilk test to test the null hypothesis that the data is normally distributed
# Observation: The p-value is extremely small(2.2e-16), therefore, in line with 
# the qqplot above I would reject the null hypothesis 
#shapiro.test(turtle2$loyalty_points)

# Install the 'moments' package
# I did not restart R to install this package
install.packages("moments")
library(moments) 

# Measure skewness of the loyalty points 
# Observation: A value of zero would have meant a perfectly normal distribution.
# Or the value should falls between the range of -0.5 and 0.5, indicating a fairly 
# symmetrical distribution (This range is the 'rule of thumb' for symmetry)
# However the actual skewness value is 1.46 which means it is heavily positive skewed
#skewness(turtle2$loyalty_points)

# Measure the tailness of the distribution using kurtosis
# Observation: Ideally the kurtosis of a normal distribution should be 3. A light 
# tailed distribution would have had a value of less than 3
# The kutosis measured here is 4.70 which means the distribution is heavy tailed.
#kurtosis(turtle2$loyalty_points)


#      *** Exploratory data analysis ***

# Plot gender on bar plot, as it is easy to visualise categorical data here. 
ggplot(turtle2, aes(x=gender)) +
  geom_bar(fill='green') +
  labs(title="Count of female and male customers") +
  theme_classic()
## Observation: Females are the majority customers 


# Summary statistics, grouping by gender and education
turtle2_summary <- turtle2 %>% group_by(gender, education) %>%
  summarise(Customer_count = n(),
            sum_renumeration=comma(round(sum(remuneration),0)),
            sum_loyalty_points=comma(round(sum(loyalty_points),0)),
            min_renumeration=round(min(remuneration),0),
            min_loyalty_points=round(min(loyalty_points),0),
            max_renumeration=round(max(remuneration),0),
            max_loyalty_points=round(max(loyalty_points),0),
            mean_renumeration=round(mean(remuneration),0),
            mean_loyalty_points=round(mean(loyalty_points),0),
            sd_renumeration=round(sd(remuneration),0),
            sd_loyalty_points=round(sd(loyalty_points),0),
            .groups='drop')

View(turtle2_summary)
## Observation: Graduates are the most customers by female (530) and also male. 
## They have the highest remuneration for both females (24,349) 
## and males (18,223) and also have the most loyalty points, females (866,257), 
## males (633,195). The marketing team can target graduates with more offers.


# Box plot between education and loyalty points. Good for displaying categorical and numeric variables.
boxplot <- ggplot(turtle2, aes(x=education, y=loyalty_points)) +
  geom_boxplot(fill = 'red', outlier.color = 'red') +
  facet_wrap(~gender) +
  theme_bw()

ggplotly(boxplot)
## Observation: Females (20) and males (30) with basic education have a very large remuneration 
## spread even though there are not many customers in this group. 
## After this graduates and PHD educated customers have large spreads and a large 
## number of customers. The marketing team could target grads and PHDs more. 


# Scatter plot to indicate relation between remuneration, loyalty points and education
# Good for showing relationship between numeric variables, with education as the hue. 
scatterplot <- ggplot(turtle2,
       mapping=aes(x=remuneration, y=loyalty_points, color=education)) +
  geom_point(alpha = 0.5, size = 1.5) +
  # Add the line of best fit to the plot.
  geom_smooth(method = 'lm', se = FALSE, size = 1.5) +
  # Add a scale layer for x.
  scale_x_continuous(breaks = seq(0, 120, 20)) +  
  # Add a scale layer for y.
  scale_y_continuous(breaks = seq(0, 7000, 1000)) +
  # Add a facet layer.
  facet_wrap(~gender) +
  # Add labels for title, subtitle, and caption.
  labs(title = "Relationship between remuneration and loyalty points",
       subtitle = "Split by gender and education") +
  # Add a theme layer. 
  theme_bw()  

ggplotly(scatterplot)
## Observation: There is a positive trend between remuneration and loyalty points. 
## The trend lines are steeper for female customers and the outliers 
## are also farther for them. 
## For females basic, post grad and diploma educated ones have steeper loyalty points trendlines. 
## For males, graduates and then PHD educated customers have steeper trend lines. 
## The marketing team can perhaps target diploma and graduate female customers, 
## and graduate and PHD male customers.


# Scatter plot to indicate relation between remuneration, spending scores and education
scatter <- ggplot(turtle2,
       mapping=aes(x=remuneration, y=spending_score, color=gender)) +
  geom_point(alpha = 0.5, size = 1.5) +
  # Add a facet layer.
  facet_wrap(~gender) +
  # Add labels for title, subtitle, and caption.
  labs(title = "Relationship between remuneration and spending score",
       subtitle = "Split by gender") +
  # Add a theme layer. 
  theme_bw()  

ggplotly(scatter)
## Observation: This was observed in week 4 during segmentation / clustering. 
## Male and female customers who have high remuneration and spending scores should 
## be targeted more by the marketing team.


#      *** Exploratory data analysis observations ***

# There are more females in the dataset. The shape of the loyalty points 
# distribution is skewed with a heavy right tail. The outliers should be removed
# to increase chances of normality.  
# There appears to be a positive trend between remuneration and loyalty points.
# For males, graduate and PHD educated customers have a steep positive trend, 
# whereas for females diploma and graduate educated ones show the same characteristics.
# The marketing team can perhaps target these groups to maximise sales revenue.


## We need to investigate customer behaviour and the effectiveness of the current loyalty program. 
##  - Can we predict loyalty points given the existing features using a relatively simple MLR model?
##  - Where should the business focus their marketing efforts?
##  - How could the loyalty program be improved?
##  - How could the analysis be improved?

# PREPARE DATA FOR MULTIPLE LINEAR REGRESSION

# Remove non-numeric columns to prepare for multiple linear regression
str(turtle2)
turtle3 <- select(turtle2, -gender, -education, -review, -summary, -product)

head(turtle3)
summary(turtle3$loyalty_points)


# Plot the boxplot again
# Observation: There are not many outliers in the loyalty points now
#boxplot(turtle3$loyalty_points)

# Determine normality of the loyalty points data again
# Observation: There is still an upper tail but it is not far from the line as previously,as the outliers have been removed
#qqnorm(turtle3$loyalty_points)
#qqline(turtle3$loyalty_points)

# Conduct shapiro-wilk test again
# Observation: The p-value is extremely small(2.2e-16), so its not exactly a normally distributed. 
#shapiro.test(turtle3$loyalty_points)

# Measure skewness again
# Observation: It is better now at 0.6, much better than the 1.46 value earlier. 
# Its not within -0.5 and +0.5 range but its not far off 
#skewness(turtle3$loyalty_points)

# Perform kurtosis again
# The previous value was 4.70. Now it is 3.78. Its closer to 3 so it is light tailed.
# The data is not perfectly symmetric but it is better than before and we will use it for regression.
#kurtosis(turtle3$loyalty_points)

#    ****Creating the Regression Model****

# Determine correlation
cor(turtle3)
## Observation: Loyalty points has moderately strong correlations with remuneration 
## and spending score. There is weaker correlation with age


# split into train and test datasets
set.seed(123)

# Create an index selecting 70% of the rows
# sample() randomly picks 70% of row numbers
# train_index is just a vector of row positions
n <- nrow(turtle3)                     # total number of rows
train_index <- sample(1:n, size = 0.7 * n)

# Split the dataset
train <- turtle3[train_index, ]
test  <- turtle3[-train_index, ]

# Select only relevant columns for both data sets
train <-select(train, age, remuneration , spending_score, loyalty_points)
test <-select(test, age, remuneration , spending_score, loyalty_points)

# Check for duplicate records in both datasets. Multiple customers have identical records
#sum(duplicated(train))
#sum(duplicated(test))

# Remove duplicates
#train <- train[!duplicated(train), ]
#test <- test[!duplicated(test), ]

# Check for duplicate again
#sum(duplicated(train))
#sum(duplicated(test))


# Check the split
nrow(train)
nrow(test)

head(train)
head(test)

# Create a new model using all the numeric columns apart from product which is different for each customer
colnames(train)
modelB <- lm(loyalty_points~age+remuneration+spending_score, data=train)

summary(modelB)
## Observation: The adjusted R square is 0.83 which explains the variability in the
## loyalty points. The independent variables are highly significant (with three 
## asterisks against them) and p-values less than 5%.


# Check for multicollinearity (VIF)
library(car)
vif(modelB)
## Observation: the VIF values for each variable is 1 i.e. less than 5
## Therefore, there is no multicollinearity between the predictors.


# Look at residuals
res <- residuals(modelB)
head(res)


# Histogram of residuals
hist(res, main="Histogram of Residuals", col="lightblue", xlab="Residuals")


# QQplot
qqnorm(res)
qqline(res, col = "red")
## Observation: The QQ plot shows the residuals not falling completely on the red line


# Shapiro test
shapiro.test(res)   
## Observation: The value is less than 0.05 so data is not normally distributed


#   ***Make predictions***

# Add the values to the turtle3_Forecast data frame.
test$loyalty_points_pred <- predict(modelB,
                                           newdata=test, interval = 'confidence',
                                           level=0.80 )                                         


# View the turtle3_Forecast data frame.
View(test)

# Final observations
# The data set was split into train (70%) and test (30%)
# The independent attributes selected were age, remuneration and spending score. 
# Their P-value was less than 5% and the adjusted R square value was 83% 
# Therefore, 83% of the variability in the loyalty points were attributed to 
# these three independent variables. There was no multicollinearity between them. 

# There appears to be a positive trend between remuneration and loyalty points. 
# For males; graduate and PHD educated customers have a steep positive trend, whereas for females 
# diploma and graduate educated ones show the same characteristics. 
# The marketing team can perhaps target these groups to maximise sales revenue.


