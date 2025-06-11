## LSE Data Analytics Online Career Accelerator 
# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment 5 scenario
## Turtle Games’s sales department has historically preferred to use R when performing 
## sales analyses due to existing workflow systems. As you’re able to perform data analysis 
## in R, you will perform exploratory data analysis and present your findings by utilising 
## basic statistics and plots. You'll explore and prepare the data set to analyse sales per 
## product. The sales department is hoping to use the findings of this exploratory analysis 
## to inform changes and improvements in the team. (Note that you will use basic summary 
## statistics in Module 5 and will continue to go into more detail with descriptive 
## statistics in Module 6.)

################################################################################

## Assignment 5 objective
## Load and wrangle the data. Use summary statistics and groupings if required to sense-check
## and gain insights into the data. Make sure to use different visualisations such as scatterplots, 
## histograms, and boxplots to learn more about the data set. Explore the data and comment on the 
## insights gained from your exploratory data analysis. For example, outliers, missing values, 
## and distribution of data. Also make sure to comment on initial patterns and distributions or 
## behaviour that may be of interest to the business.

################################################################################

# Module 5 assignment: Load, clean and wrangle data using R

## It is strongly advised that you use the cleaned version of the data set that you created and 
##  saved in the Python section of the course. Should you choose to redo the data cleaning in R, 
##  make sure to apply the same transformations as you will have to potentially compare the results.
##  (Note: Manual steps included dropping and renaming the columns as per the instructions in module 1.
##  Drop ‘language’ and ‘platform’ and rename ‘remuneration’ and ‘spending_score’) 

## 1. Open your RStudio and start setting up your R environment. 
## 2. Open a new R script and import the turtle_review.csv data file, which you can download from 
##      Assignment: Predicting future outcomes. (Note: You can use the clean version of the data 
##      you saved as csv in module 1, or, can manually drop and rename the columns as per the instructions 
##      in module 1. Drop ‘language’ and ‘platform’ and rename ‘remuneration’ and ‘spending_score’) 
## 3. Import all the required libraries for the analysis and view the data. 
## 4. Load and explore the data.
##    - View the head the data.
##    - Create a summary of the new data frame.
## 5. Perform exploratory data analysis by creating tables and visualisations to better understand 
##      groupings and different perspectives into customer behaviour and specifically how loyalty 
##      points are accumulated. Example questions could include:
##    - Can you comment on distributions, patterns or outliers based on the visual exploration of the data?
##    - Are there any insights based on the basic observations that may require further investigation?
##    - Are there any groupings that may be useful in gaining deeper insights into customer behaviour?
##    - Are there any specific patterns that you want to investigate
## 6. Create
##    - Create scatterplots, histograms, and boxplots to visually explore the loyalty_points data.
##    - Select appropriate visualisations to communicate relevant findings and insights to the business.
## 7. Note your observations and recommendations to the technical and business users.

###############################################################################

# Install relevant packages
install.packages('tidyverse')
install.packages('DataExplorer')
install.packages("moments")
library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)
library(skimr)
library(DataExplorer)
library(moments)

# ---- Explore and wrangle the data ----

# Determine the working directory.
getwd() 

# Import the Turtle Games data
df <- read.csv('tr_clean_clusters_scores.csv', header=TRUE)

# Check data has imported correctly
View(df)

# Check the dimensions of the dataframe
dim(df)

# Check the internal structure
str(df)

# Check the tibble
as_tibble(df)

# Check for missing values
df[is.na(df)]

# Convert 'product' to categorical/factor
df2 <- mutate(df,
              product = as.factor(product))

# Convert 'gender' to categorical/factor
df3 <- mutate(df2,
              gender = as.factor(gender))

# Convert 'education' to categorical/factor
df4 <- mutate(df3,
              education = as.factor(education))

# Convert 'cluster' to categorical/factor
df4 <- mutate(df3,
              cluster = as.factor(cluster))

df4 <- df4 %>%
  mutate(cluster = recode(cluster,
                          "0" = "HP-HS",
                          "1" = "MP-MS",
                          "2" = "HP-LS",
                          "3" = "LP-HS",
                          "4" = "LP-LS"))

# Run Data Explorer on df4
DataExplorer::create_report(df4)

# Export the df4 to csv
write.csv(df4, 'tr_clean_R2.csv')

# ---- Create Exploratory Visualisations ----

# Histogram of spending_score
ggplot(df4, aes(x = spending_score))+
  geom_histogram(bins = 10,
                 fill = 'darkgreen',
                 color = 'black')
# Histogram of pay
ggplot(df4, aes(x = pay))+
  geom_histogram(bins = 10,
                 fill = 'darkgreen',
                 color = 'black')

# Histogram of loyalty_points
ggplot(df4, aes(x = loyalty_points))+
  geom_histogram(bins = 10,
                 fill = 'darkgreen',
                 color = 'black')
#

# Density Plot of spending_score
ggplot(df4, aes(x = spending_score))+
  geom_density()

# Histogram of pay
ggplot(df4, aes(x = pay))+
  geom_histogram(bins = 20)

# Density Plot of pay
ggplot(df4, aes(x = pay))+
  geom_density()

# Histogram of loyalty_points
ggplot(df4, aes(x = loyalty_points))+
  geom_histogram(bins = 10,
                 fill = "steelblue",
                 color = "black")

# You can see that most users have less than 2,000 loyalty points, 
# with the data highly skewed

# Density Plot of pay
ggplot(df4, aes(x = loyalty_points))+
  geom_density()

# ---- Explore relationships between variables -----

# Scatterplot of loyalty_points and review_polarity
ggplot(df4, aes(x = review_polarity, y = loyalty_points,))+
  geom_point()+
  geom_jitter(width = 0.1, height = 0.1)+
  geom_smooth(se = FALSE)

# There is no correlation between loyalty points and review_polarity

# Scatterplot of loyalty_points and review_sentiment
ggplot(df4, aes(x = review_sentiment, y = loyalty_points,))+
  geom_point()+
  geom_jitter(width = 0.1, height = 0.1)+
  geom_smooth(se = FALSE)

# There is no correlation between loyalty points and review_sentiment

# Scatterplot of loyalty_points and summary_polarity
ggplot(df4, aes(x = summary_polarity, y = loyalty_points,))+
  geom_point()+
  geom_jitter(width = 0.1, height = 0.1)+
  geom_smooth(se = FALSE)

# There is no correlation between loyalty points and summary_polarity

# Scatterplot of loyalty_points and summary_sentiment
ggplot(df4, aes(x = summary_sentiment, y = loyalty_points,))+
  geom_point()+
  geom_jitter(width = 0.1, height = 0.1)+
  geom_smooth(se = FALSE)

# There is no correlation between loyalty points and summary_sentiment

# Scatterplot with spline
ggplot(df4, aes(x = spending_score, y = loyalty_points, colour = education, ))+
  geom_point()+
  geom_jitter(width = 0.1, height = 0.1)+
  geom_smooth(se = FALSE)

# Customers with high spending score + loyalty points tend to have an educational
# level as graduate, basic and diploma

# Scatterplot with spline
ggplot(df4, aes(x = spending_score, y = pay, colour = education, ))+
  geom_point()+
  geom_smooth(se = FALSE)

# Scatterplot with spline
ggplot(df4, aes(x = spending_score, 
                y = loyalty_points, colour = education))+
  geom_smooth(lwd = 0.5, se = FALSE)

# Barchart for products
ggplot(df4, aes(x = gender, fill = education)) + 
  geom_bar(position = 'dodge') +
  ggtitle('Gender split by education')+
  theme_classic()

# Females have more postgraduate qualifications but otherwise the education
# split between genders is similar


# Compare Pay and Spending Score (with education)
ggplot(data = df4,
       mapping = aes(x = pay, y = spending_score,
                     color = education)) +
  geom_point(alpha = 0.5,
             size = 1.5) +
  geom_smooth(method = 'lm',
              se = FALSE,
              linewidth = 1)+
  labs(title = 'Spend vs. Pay')+
  theme_bw()

# Basic & Diploma
# Positive correlation between Pay & Spending Score

# Graduate
# Mild positive correlation between Pay & Spending Score

# PHD & PostGrad
# negative correlation between Pay & Spending Score

# Add gender as facet
ggplot(data = df4,
       mapping = aes(x = pay, y = spending_score,
                     color = education)) +
  geom_point(alpha = 0.5,
             size = 1.5) +
  geom_smooth(method = 'lm',
              se = FALSE,
              linewidth = 1)+
  facet_wrap(~gender)+
  labs(title = 'Spend vs. Pay')+
  theme_bw()

# The positive correlation between spending score and pay is most prominent in
# woman with a basic and diploma education


# ---- Filter data to look at subsets ----

# Loyalty points >2,000
# Create a new dataframe filtering loyalty points >2000
df_high_lp = filter(df4, loyalty_points>2000)

# Create a histogram of loyalty points on the filtered data
ggplot(df_high_lp, aes(x = loyalty_points))+
  geom_histogram(fill = 'steelblue',
                 color = 'black',
                 bins = 10)

# Create a histogram on education from the filtered data
ggplot(df_high_lp, aes(x = education))+
         geom_histogram(stat = 'count')

# Create a histogram on education from the filtered data
ggplot(df_high_lp, aes(x = gender))+
  geom_histogram(stat = 'count')

# Create a histogram on product from the filtered data
ggplot(df_high_lp, aes(x = product))+
  geom_histogram(fill = 'blue', stat = 'count')

# Create a histogram of ages from the filtered data
ggplot(df_high_lp, aes(x = age)) +
  geom_histogram(fill = 'steelblue',
                 color = 'black')

# Create a density plot of ages from filtered data
ggplot(df_high_lp, aes(x = age)) +
  geom_density(fill = 'blue')
       
# ---- Aggregate data to understand cohorts ----

# Use aggregate function to explore data
clusters <- aggregate(loyalty_points~cluster, df4, mean)

df4_cluster

# Find the mode for categorical data
# Define the function to calculate mode
get_mode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

# Adjust df4_cluster to include mode values
df4_cluster <- df4 %>% 
  group_by(cluster) %>% 
  summarize(
    count = n(),
    mean_loyalty = mean(loyalty_points),
    mean_age = mean(age),
    mean_review_polarity = mean(review_polarity),
    mode_education = get_mode(education),
    mode_product = get_mode(product)
  )

df4_cluster


# Filter High Pay / High Spend cluster
df_cluster_0 = filter(df4, cluster == 0)

# Scatterplot of loyalty_points and review_polarity
ggplot(df_cluster_0, aes(
  x = review_polarity, 
  y = loyalty_points,))+
  geom_point()+
  geom_jitter(width = 0.1, height = 0.1)+
  geom_smooth(se = FALSE)

# Use aggregate function on product + loyalty points
lp_x_product <- aggregate(loyalty_points~product, df4, mean)
lp_x_product[order(-lp_x_product$loyalty_points,review_polarity),]
  

# Use aggregate function on education + loyalty points
aggregate(loyalty_points~education, df4, mean)

# Use aggregate function on gender + loyalty points
aggregate(loyalty_points~gender, df4, mean)

# ---- Group data by product ----

# Use groupby function to manipulate the datafram
# Groupby product
df4_product <- df4 %>% group_by(product) %>% summarize(
  count = n(),
  mean_loyalty = mean(loyalty_points),
  mean_spending = mean(spending_score),
  mean_pay = mean(pay),
  mean_age = mean(age),
  mean_review_polarity = mean(review_polarity),
  mean_review_sentiment = mean(review_sentiment))

df4_product

# Compare review polarity and loyalty points by product
ggplot(df4_product, aes(x = mean_review_polarity, 
                        y = mean_loyalty,))+
  geom_point()+
  geom_smooth(se = FALSE,
              color = 'steelblue')

# Create histogram of mean_review_polarity by product
ggplot(df4_product, aes(x = mean_review_polarity))+
  geom_histogram(fill = 'steelblue',
                 color = 'black',
                 bins = 15)

# Compare review polarity and pay by product
ggplot(df4_product, aes(x = mean_review_polarity, 
                        y = mean_pay,))+
  geom_point()+
  geom_smooth(se = FALSE,
              color = 'steelblue')+
  labs(
    x = "Review Sentiment",
    y = "Pay")+
  theme_minimal()


# Compare review polarity and spending by product
ggplot(df4_product, aes(x = mean_review_polarity, 
                        y = mean_spending,))+
  geom_point()+
  geom_smooth(se = FALSE,
              color = 'steelblue')+
  labs(
    x = "Review Sentiment",
    y = "Spending")+
  theme_minimal()

# Filter dataframe to highest rated products
df_product_high = filter(df4_product, 
                         mean_review_polarity > 0.4) %>%
  arrange(desc(mean_review_polarity))

df_product_high

# Filter dataframe to lowest rated products
df_product_low = filter(df4_product, 
                         mean_review_polarity < 0.1) %>%
  arrange(mean_review_polarity)

df_product_low

# ---- Group data by education ----

# Group data by education 
df4_education <- df4 %>% 
  group_by(education) %>% 
  summarize(
    count = n(),
    mean_loyalty = mean(loyalty_points),
    mean_age = mean(age),
    mean_review_polarity = mean(review_polarity),
    mode_cluster = get_mode(cluster),
    mode_product = get_mode(product))

df4_education

###############################################################################
###############################################################################

# Assignment 6 scenario

## In Module 5, you were requested to redo components of the analysis using Turtle Games’s preferred 
## language, R, in order to make it easier for them to implement your analysis internally. As a final 
## task the team asked you to perform a statistical analysis and create a multiple linear regression 
## model using R to predict loyalty points using the available features in a multiple linear model. 
## They did not prescribe which features to use and you can therefore use insights from previous modules 
## as well as your statistical analysis to make recommendations regarding suitability of this model type,
## the specifics of the model you created and alternative solutions. As a final task they also requested 
## your observations and recommendations regarding the current loyalty programme and how this could be 
## improved. 

################################################################################

## Assignment 6 objective
## You need to investigate customer behaviour and the effectiveness of the current loyalty program based 
## on the work completed in modules 1-5 as well as the statistical analysis and modelling efforts of module 6.
##  - Can we predict loyalty points given the existing features using a relatively simple MLR model?
##  - Do you have confidence in the model results (Goodness of fit evaluation)
##  - Where should the business focus their marketing efforts?
##  - How could the loyalty program be improved?
##  - How could the analysis be improved?

################################################################################

## Assignment 6 assignment: Making recommendations to the business.

## 1. Continue with your R script in RStudio from Assignment Activity 5: Cleaning, manipulating, and 
##     visualising the data.
## 2. Load and explore the data, and continue to use the data frame you prepared in Module 5.
## 3. Perform a statistical analysis and comment on the descriptive statistics in the context of the 
##     review of how customers accumulate loyalty points.
##  - Comment on distributions and patterns observed in the data.
##  - Determine and justify the features to be used in a multiple linear regression model and potential
##.    concerns and corrective actions.
## 4. Create a Multiple linear regression model using your selected (numeric) features.
##  - Evaluate the goodness of fit and interpret the model summary statistics.
##  - Create a visual demonstration of the model
##  - Comment on the usefulness of the model, potential improvements and alternate suggestions that could 
##     be considered.
##  - Demonstrate how the model could be used to predict given specific scenarios. (You can create your own 
##     scenarios).
## 5. Perform exploratory data analysis by using statistical analysis methods and comment on the descriptive 
##     statistics in the context of the review of how customers accumulate loyalty points.
## 6. Document your observations, interpretations, and suggestions based on each of the models created in 
##     your notebook. (This will serve as input to your summary and final submission at the end of the course.)

################################################################################

# ---- Further explore data ahead of multi-linear regression ----

# Use aggregate function to explore data
# Use aggregate function on product + loyalty points
lp_x_product <- aggregate(loyalty_points~product, df4, mean)
lp_x_product[order(-lp_x_product$loyalty_points),]


# Use aggregate function on education + loyalty points
lp_x_education <- aggregate(loyalty_points~education, df4, mean)
lp_x_education[order(-lp_x_education$loyalty_points),]

# Use aggregate function on gender + loyalty points
lp_x_gender <- aggregate(loyalty_points~gender, df4, mean)
lp_x_gender[order(-lp_x_gender$loyalty_points),]

# Use groupby function to manipulate the dataframe
# Groupby product
df4_product <- df4 %>% group_by(product) %>% summarize(
  mean_loyalty = mean(loyalty_points),
  mean_spending = mean(spending_score),
  mean_pay = mean(pay),
  mean_review_polarity = mean(review_polarity),)
tail(df4_product[order(-df4_product$mean_review_polarity),])

tail(df4_product)

# ---- Explore multiple linear regressions ----

# Create a multi-linear regression
# Linear regression for loyalty points with pay and spending score
lm_lp_pay_ss = lm(loyalty_points~pay+spending_score, data=df4)

# Print the summary statistics.
summary(lm_lp_pay_ss)

# See if adding education changes anything
lm_lp_pay_ss_ed = lm(loyalty_points~pay+spending_score+education, data=df4)

# Print the summary statistics.
summary(lm_lp_pay_ss_ed)

### Looks like adding education has had a slight improvement on the regression model

# See if adding product changes anything
lm_lp_pay_ss_ed_pd = lm(loyalty_points~
                          pay+spending_score+
                          education+
                          product, 
                        data=df4)
# Print the summary statistics.
summary(lm_lp_pay_ss_ed_pd)

### Adding product has reduced the Adjusted R-Squared so not worth including

# See if adding age changes anything
lm_lp_pay_ss_ed_age = lm(loyalty_points~
                          pay+spending_score+
                          education+
                          age, 
                        data=df4)
# Print the summary statistics.
summary(lm_lp_pay_ss_ed_age)

### Adding age has improved the model, so will keep ###


# ---- Use descriptive statistics to assess the model ----

# Check for skewness that might impact results
# Histogram of loyalty_points
ggplot(df4, aes(x = loyalty_points))+
  geom_histogram(bins = 10,
                 fill = "steelblue",
                 color = "black")+
  labs(
    x = "Loyalty Points",
    y = "Count"
  )+
  theme_minimal()

### Looks like loyalty data is heavily skewed ###

# Check through descriptive Statistics for Loyalty Points
skewness(df4$loyalty_points)
kurtosis(df4$loyalty_points)

### Data is skewed so I will transform it and refit model ###

# ---- Transform the data and refit the model ----

# Transform the loyalty data
df4$loyalty_points_log <- log(df4$loyalty_points + 1)

# Rerun the model with transformed data
# Start with pay and spending score
lm_lp_log = lm(loyalty_points_log ~ pay + 
                 spending_score,
               data = df4)
summary(lm_lp_log)
### The Adjusted R-squared is now at .7997 ###

# Add age to model to assess impact
lm_lp_log = lm(loyalty_points_log ~ pay + 
                 spending_score +
                 age,
               data = df4)
summary(lm_lp_log)

### The Adjusted R-squared has improved to 0.8173 ###

# Add education to model to assess impact
lm_lp_log = lm(loyalty_points_log ~ pay + 
                 spending_score +
                 age +
                 education,
               data = df4)
summary(lm_lp_log)

### Minimal improvement with eduaction added 0.818 ###


# ---- Test the model works with dummy figures ----

# Test new customer with low pay and low spending_score
new_customer <- data.frame(
  pay = 20000,
  spending_score = 20,
  education = "graduate",
  age = 30
)

predict(lm_lp_log, newdata = new_customer)

### Customer would have predicted 472 loyalty points ###

# Test new customer with high pay and high spending_score
new_customer_2 <- data.frame(
  pay = 90000,
  spending_score = 80,
  education = "graduate",
  age = 30
)

predict(lm_lp_log, newdata = new_customer_2)

### Customer would have predicted 2112 loyalty points ###

# Test new customer with high pay and high spending_score but with diploma
new_customer_3 <- data.frame(
  pay = 90000,
  spending_score = 80,
  education = "diploma",
  age = 30
)

predict(lm_lp_log, newdata = new_customer_3)

### Customer would have predicted 2112 loyalty points - no change ###

# Test new customer with high pay and high spending_score but with postgraduate
new_customer_4 <- data.frame(
  pay = 90000,
  spending_score = 80,
  education = "postgraduate",
  age = 30
)

predict(lm_lp_log, newdata = new_customer_4)

### Customer would have predicted 2112 loyalty points - no change 

# Test new customer with high pay and high spending_score but with postgraduate
new_customer_5 <- data.frame(
  pay = 90000,
  spending_score = 80,
  education = "postgraduate",
  age = 30
)

predict(lm_lp_log, newdata = new_customer_5)

### Customer would have predicted 2112 loyalty points - no change 

# Test new customer with high pay and high spending_score but aged 80
new_customer_6 <- data.frame(
  pay = 90000,
  spending_score = 80,
  education = "postgraduate",
  age = 80
)

predict(lm_lp_log, newdata = new_customer_6)

### Customer would have predicted 2112 loyalty points - no change 

# Test new customer with low pay and high spending_score and aged 80
new_customer_7 <- data.frame(
  pay = 20000,
  spending_score = 80,
  education = "postgraduate",
  age = 80
)

predict(lm_lp_log, newdata = new_customer_7)

### Customer would have predicted 475 loyalty points - no change 

### Age and Education did not impact the predicted value of loyalty scores
### However, lower pay significantly reduced the amount of loyalty points
### Question to the business is whether this is ethical

# ---- Create Plots to include in presentation ----

# Histogram of loyalty_points
ggplot(df4, aes(x = loyalty_points))+
  geom_histogram(bins = 10,
                 fill = "steelblue",
                 color = "black")+
  labs(
    x = "Loyalty Points",
    y = "Count"
  )+
  theme_minimal()

# Descriptive Statistics for Loyalty Points
skewness(df4$loyalty_points)
kurtosis(df4$loyalty_points)

# Show relationship between loyalty points and spending score in scatterplot
ggplot(df4, aes(x = spending_score, y = loyalty_points)) +
  geom_point(color = 'coral') +
  geom_smooth(method = "lm", se = FALSE, color = 'black')+
  labs(
    x = "Spending Score",
    y = "Loyalty Points"
  )+
  theme_minimal()
ggsave("ss_lp.png", width = 10, height = 6, dpi = 300)

# Show relationship between loyalty points and pay in scatterplot
ggplot(df4, aes(x = pay, y = loyalty_points)) +
  geom_point(color = 'purple') +
  geom_smooth(method = "lm", se = FALSE, color = 'black')+
  labs(
    x = "pay",
    y = "Loyalty Points"
  )+
  theme_minimal()
ggsave("p_lp.png", width = 10, height = 6, dpi = 300)

# Show relationship between loyalty points, pay and spending score in scatterplot
ggplot(df4, aes(x = spending_score, y = loyalty_points, color = pay)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = 'black')+
  scale_color_gradient(low = 'lightgreen', high = 'darkgreen')+
  labs(
    x = "Spending Score",
    y = "Loyalty Points"
  )+
  theme_minimal()
  
ggsave("ml_lp_sc_p.png", width = 10, height = 6, dpi = 300)



# Pay and Spending Score in a scatterplot
ggplot(df4, aes(x = spending_score, y = pay)) +
  geom_point() +
  labs(
    x = "Spending Score",
    y = "Pay")+
  theme_minimal()

ggsave("Pay_Spend.png", width = 10, height = 6, dpi = 300)


# Show the clusters in a scatterplot
ggplot(df4, aes(x = pay, y = spending_score, color = cluster)) +
  geom_point() +
  labs(
    x = "Pay",
    y = "Spending Score")+
  theme_minimal()

ggsave("clusters.png", width = 10, height = 6, dpi = 300)

# Look at descriptive statistics for each cluster
df4_cluster

# Histogram of Comment Sentiment
ggplot(df4, aes(x = review_polarity))+
  geom_histogram(bins = 10,
                 fill = 'darkgreen',
                 color = 'black')+
  labs(
    x = "Review Sentiment",
    y = "Count")+
  theme_minimal()
ggsave("Comment_sentiment.png", width = 10, height = 6, dpi = 300)

# Mean of comment sentiment
mean(df4$review_polarity)

# Create histogram of mean_review_polarity by product
ggplot(df4_product, aes(x = mean_review_polarity))+
  geom_histogram(fill = 'steelblue',
                 color = 'black',
                 bins = 10)+
  labs(
    x = "Product Sentiment",
    y = "Count")+
  theme_minimal()
ggsave("Product_sentiment.png", width = 10, height = 6, dpi = 300)

# Compare review polarity and loyalty points by product
ggplot(df4_product, aes(x = mean_review_polarity, 
                        y = mean_loyalty,))+
  geom_point(color = "darkgreen")+
  geom_smooth(se = FALSE,
              color = 'steelblue')+
  labs(
    x = "Review Sentiment",
    y = "Loyalty Points")+
  theme_minimal()

ggsave("loyalty_sentiment.png", width = 10, height = 6, dpi = 300)

# Mean of comment sentiment
mean(df4_product$mean_review_polarity)

# Histogram of loyalty_points
ggplot(df4, aes(x = loyalty_points))+
  geom_histogram(bins = 10,
                 fill = "steelblue",
                 color = "black")+
  labs(
    x = "Loyalty Points",
    y = "Count"
  )+
  theme_minimal()

ggsave("hist_loyalty.png", width = 10, height = 6, dpi = 300)

# Descriptive Statistics for Loyalty Points
skewness(df4$loyalty_points)
kurtosis(df4$loyalty_points)

# Run model with ss, pay, age and education
lm_lp_log = lm(loyalty_points_log ~ pay + 
                 spending_score +
                 education +
                 age,
               data = df4)

summary(lm_lp_log)

# Create plot with transformed data
ggplot(df4, aes(x = spending_score, y = loyalty_points_log, color = pay)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = 'black') +
  scale_color_gradient(low = 'lightgreen', high = 'darkgreen') +
  labs(x = "Spending Score", y = "Log(Loyalty Points)") +
  theme_minimal()

ggsave("Log_loyalty.png", width = 10, height = 6, dpi = 300)

###############################################################################
###############################################################################



