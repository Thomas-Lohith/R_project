install.packages("dplyr")

#1. Import the database:
library(dplyr)
library(ggplot2)
kidney_data <- read.csv("/Users/thomas/Desktop/phd_unipv/courses/Summerschool_R/Codes/Assignment/Thomas Routhu - kidney_disease.csv", header = TRUE,sep=",", na.strings = c("", " ", ".", "NA", "nan"),dec = ".")

#1a.Create an object with the names of numerical variables.

numerical_varibles <- select_if(kidney_data, is.numeric)

#1b.Create an object with the names of categorical variables and reclassify them as factor variables.
catgeorical_variables <- select_if(kidney_data, is.character)

categorical_names <- colnames(catgeorical_variables)

catgeorical_variables <- catgeorical_variables %>% mutate(across(all_of(categorical_names), as.factor))


#2. For each of the two selected groups (factors and numerical):

#2a. Check the presence and number of missing data for each variable.

missing_num_data_counts <- sapply(numerical_varibles, function(x) sum(is.na(x)))

missing_categ_data_counts <- sapply(catgeorical_variables, function(x) sum(is.na(x)))

#2b. Impute missing data.

summary(kidney_data)

#first we analyse the missing data then we proceed to imputation methods or removing data

library(VIM)

aggr(kidney_data, numbers= TRUE)

# We will use median imputation for numerical variables and mode imputation for categorical variables

# Function to impute missing numerical data with median

impute_median <-function(x) {
  ifelse(is.na(x), median(x, na.rm = TRUE), x)
}

#Function to impute missing categorical data with mode

impute_mode <-function(x) {
  mode <- as.character(names(sort(table(x), decreasing = TRUE))[1])
  ifelse(is.na(x), mode, as.character(x))
}

#applying the imputation 

kidney_data <- kidney_data %>%
  mutate(across(where(is.numeric), impute_median)) %>%
  mutate(across(all_of(categorical_names), impute_mode))

#2c. Indicate the imputation method used.

#3.Outliers:

# 3a: Identify variables containing outliers
# Function to identify outliers using IQR
identify_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  x < lower_bound | x > upper_bound
}

# Identify outliers for numerical variables
outliers_summary <- sapply(numerical_varibles, identify_outliers)
outliers_summary_count <- colSums(outliers_summary, na.rm = TRUE)
outliers_summary_count

#3b Illustrate the method used to identify outliers.
ggplot(kidney_data, aes(x = "", y = sod)) +
  geom_boxplot() +
  labs(title = "Boxplot to Identify Outliers in Hemoglobin")

#3cRemove patients that contain at least one outlier value
outlier_rows <- apply(outliers_summary, 1, any)
cleaned_data <- kidney_data[!outlier_rows, ]

#4. Plot the relationship between the following variables and comment:
#a. Hemoglobin (hemo)
#b. Packed Cell Volume (pcv)

ggplot(kidney_data, aes(x = hemo, y= pcv)) +
  geom_point() + 
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Relationship between Hemoglobin and Packed Cell Volume", x = "Hemoglobin (hemo)", y = "Packed Cell Volume (pcv)")

#the relationship is linear btw hemo and pcv


#5. Quantitatively evaluate the relationship in question 4) with an appropriate metric.

#as the realtion is linear we can go with correlation
correlation <- cor(kidney_data$hemo, kidney_data$pcv, use = "complete.obs")

correlation

#the correlation shows the strength and the direction btw hemo and pcv

#6.Is there any variable that follows a Gaussian distribution?

#a. Comment on the answer.
# no, no variable is normally distributed

#b. Which test verifies the null hypothesis that a variable X follows a Gaussian distribution?
#shapiro test verifes the normal distributiuon
shapiro_test_results <- sapply(numerical_varibles, function(x) shapiro.test(x)$p.value)
print(shapiro_test_results)

# Interpretation
shapiro_test_results_interpretation <- sapply(shapiro_test_results, function(p) {
  if (p < 0.05) {
    return("Reject null hypothesis: Data is not normally distributed")
  } else {
    return("Fail to reject null hypothesis: Data may be normally distributed")
  }
})
print(shapiro_test_results_interpretation)


#7. What is the proportion of patients with "Chronic Kidney Disease" in the sample?

proportion_ckd <- sum(kidney_data$class == "ckd") / nrow(kidney_data)

proportion_ckd 


#8.8. Considering the "Hemoglobin" variable:
#a. Calculate its mean and median.

mean_hemo<- mean(kidney_data$hemo, na.rm = TRUE)
median_hemo<- median(kidney_data$hemo, na.rm = TRUE)

#b. Divide the samples into cases (classification: ckd = 1) and controls (classification: notckd = 0).

kidney_data$class_binary <- ifelse(kidney_data$class == "ckd", 1, 0)

cases <- kidney_data %>% filter(class_binary == 1)
controls <- kidney_data %>% filter(class_binary == 0)


#c. Is there a difference between the two means (cases vs controls)? What is the significance of this
#difference? Apply the correct statistical test and comment the result.

#we can apply the t TEST 

t_test_result <- t.test(cases$hemo, controls$hemo)
t_test_result

#d. Does it make sense to apply the same test as in point c, but on one tail? Comment on the answer.

t_test_result_one_tailed <- t.test(cases$hemo, controls$hemo, alternative = "greater")
t_test_result_one_tailed


#9Standardize the numerical variables (all except CKD) and reconstruct the dataset with only the numeric
#variables and CKD.

#9: Standardize numerical variables


# Reconstruct the dataset with standardized numerical variables and CKD
standardized_data <-numerical_varibles %>%
  mutate(across(everything(), scale)


