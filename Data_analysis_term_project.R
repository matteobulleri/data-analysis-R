# Script Assignment 

'Before importing the dataset, you will need to set the working directory
using the command     setwd("path where you have the file")

Also, make sure to have all the required packages I have used.
Those that have not been used in the course are
library(skimr)
library(stargazer)
library(reshape2)
library(lmtest)
library(car)
'
# Part 1

#1####
# Let us import the dataset which we will use for the first task.
library(readr)
kidney <- read_delim("dataKidneyReg.csv",
                            delim = ";",
                            escape_double = FALSE,
                            trim_ws = TRUE)

# Now, let us look at the basic features of the dataset
str(kidney, vec.len = 2)

# 'to change all column names making sure that are all consistent -- i.e. everything lower case
kidney <- rename_with(kidney, tolower)

# quickly see which variables are dummies and which are not
summary(kidney)

# dataset description
skim_without_charts(kidney)

#2####
library(tidyverse)
eda <- kidney[, c(1,2,4,7, 14)]
eda <- as.data.frame(eda)


# to nicely visualise the descriptive statistics
library(stargazer)

stargazer(eda, type = "text")

# visual

library(reshape2)
id <- c(1:156)

eda1 <- eda %>%
  mutate(age_group = case_when(
    age <= 35 ~ 1,
    age >35 & age <60 ~ 2,
    age >= 60 ~ 3
  ))

plot_eda <- cbind(eda1, id)
plot_eda <- plot_eda %>% rename(`blood pressure` = blood_pressure)
melt_plot_eda <- melt(plot_eda, id = c("id", "age_group"))
melt_plot_eda$age_group <- as.factor(melt_plot_eda$age_group)


ggplot(melt_plot_eda, aes(x = value, fill = variable))+
  geom_histogram()+
  facet_wrap(~variable, scales = "free")+
  labs(x = "Value ranges for each variable considered", y = "Frequency count",
       title = "Visualisation of the distribution of values for each variable",
       subtitle = "")+
  theme_bw()+
  theme(legend.position = "none")

# let us check outliers
'for categorical variables, the detection of outliers does not make sense'

par(mfrow = c(1, 3))
boxplot(eda$age, col = "tomato", xlab = "Age")
boxplot(eda$blood_pressure, col = "yellow3", xlab = "Blood pressure")
boxplot(eda$glucose, col = "steelblue1", xlab = "Glucose level")

# more detailed


melt_plot_eda %>%
  filter(variable != "age") %>%
  ggplot(aes(value, fill = age_group))+
  geom_histogram()+
  facet_grid(age_group~variable, scales = "free_x")+
  scale_fill_discrete(labels = c("<=35", "35 < x < 60", ">=60"))+
  guides(fill=guide_legend(title="Age group"))+
  labs(x = "", y = "", title = "Distribution of data by Age groups")+
  theme_bw()



#3####
library(corrplot)

alla <- cor(kidney, method = "pearson")

#4####

cormatrix <- structure(cor(kidney), .Dim = c(16L, 16L), 
                       .Dimnames = list(c("1. age", "2. blood pressure",
                                          "3. gravity", "4. sugar", "5. red blood",    
                                           "6. bacteria", "7. glucose", "8. sodium", "9. potassium",                                 
                                          "10. hemoglobin", "11. cell volume",
                                          "12. white bloodcount", "13. red bloodcount",
                                          "14. diabetes", "15. pedal edama", "16. anemia"), 
                                          c(1:16)))
corrplot.mixed(cormatrix, upper = "circle", lower = "number",
               tl.pos = "lt", tl.col = "black", tl.offset = 1,
               number.cex = 0.5, bg = "gray5")


#5####

# we can create a function to remove those variables with the highest average correlations.
dep_var <- kidney$glucose
expl_var <- kidney[, -c(7)]

cor_matrix <- abs(cor(expl_var))

diag(cor_matrix) = 0
while(max(cor_matrix)>=0.8) {
  # Find explanatory variables with highest correlation
  maxvar = which(cor_matrix == max(cor_matrix), arr.ind = TRUE)
  # select variable with the highest average correlation
  maxavg = which.max(rowMeans(cor_matrix[maxvar[,1],]))
  # removal
  expl_var = expl_var[,-maxvar[maxavg,1]]
  cor_matrix = cor_matrix[-maxvar[maxavg, 1], -maxvar[maxavg,1]]
}
# we have removed cell_volume

#additional, non-required visualisation
corrplot(cor(expl_var), "number", number.cex = 0.5)

# new dataframe after removing highly correlated explanatory variable
new_kidney <- cbind("glucose" = dep_var, expl_var)

#6####
lm1 <- lm(glucose ~ age + blood_pressure + gravity + sugar +
            red_blood + bacteria + sodium + potassium + hemoglobin +
            white_bloodcount + red_bloodcount + diabetes + 
            pedal_edema + anemia,
          data = new_kidney)
summary(lm1)

#7####

# it can be improved, so we remove the variable with the highest p-value (-hemoglobin)


lm2 <- lm(glucose ~ age + blood_pressure + gravity + sugar +
            red_blood + bacteria + sodium + potassium +
            white_bloodcount + red_bloodcount + diabetes + 
            pedal_edema + anemia,
          data = new_kidney)
summary(lm2)

# R-squared did not change from modification. We can still reiterate as long as
# all the variables in the model are statistically significant.
# (-white_bloodcount)

lm3 <- lm(glucose ~ age + blood_pressure + gravity + sugar +
            red_blood + bacteria + sodium + potassium +
             red_bloodcount + diabetes + 
            pedal_edema + anemia,
          data = new_kidney)
summary(lm3)

# (-sodium)

lm4 <- lm(glucose ~ age + blood_pressure + gravity + sugar +
            red_blood + bacteria + potassium +
            red_bloodcount + diabetes + 
            pedal_edema + anemia,
          data = new_kidney)
summary(lm4)

# (-red_bloodcount)

lm5 <- lm(glucose ~ age + blood_pressure + gravity + sugar +
            red_blood + bacteria + potassium +
            diabetes + 
            pedal_edema + anemia,
          data = new_kidney)
summary(lm5)

# (-bacteria)

lm6 <- lm(glucose ~ age + blood_pressure + gravity + sugar +
            red_blood + potassium +
            diabetes + 
            pedal_edema + anemia,
          data = new_kidney)
summary(lm6)

# (-anemia)

lm7 <- lm(glucose ~ age + blood_pressure + gravity + sugar +
            red_blood + potassium +
            diabetes + 
            pedal_edema,
          data = new_kidney)
summary(lm7)

# (-blood_pressure)

lm8 <- lm(glucose ~ age + gravity + sugar +
            red_blood + potassium +
            diabetes + 
            pedal_edema,
          data = new_kidney)
summary(lm8)

# (-age)

lm9 <- lm(glucose ~ gravity + sugar +
            red_blood + potassium +
            diabetes + 
            pedal_edema,
          data = new_kidney)
summary(lm9)

# all the variables (and intercept) are statistically significant at a 
# 95% confidence interval (or 5% significance level).
# R-squared slightly deteriorated and F-statistic dramatically improved.



# to nicely summarise the results and be presented in the report and in the appendix

stargazer(lm3, align = TRUE, 
          title = "Third regression model",
          single.row = TRUE, report = ('vc*sp'),
          table.placement = "!htb")

stargazer(lm4, align = TRUE, 
          title = "Fourth regression model",
          single.row = TRUE, report = ('vc*sp'),
          table.placement = "!htb")

stargazer(lm5, align = TRUE, 
          title = "Fifth regression model",
          single.row = TRUE, report = ('vc*sp'),
          table.placement = "!htb")

stargazer(lm6, align = TRUE, 
          title = "Sixth regression model",
          single.row = TRUE, report = ('vc*sp'),
          table.placement = "!htb")

stargazer(lm7, align = TRUE, 
          title = "Seventh regression model",
          single.row = TRUE, report = ('vc*sp'),
          table.placement = "!htb")

stargazer(lm8, align = TRUE, 
          title = "Eight regression model",
          single.row = TRUE, report = ('vc*sp'),
          table.placement = "!htb")

stargazer(lm9, align = TRUE, 
          title = "Ninth regression model",
          single.row = TRUE, report = ('vc*sp'),
          table.placement = "!htb")

stargazer(lm1, lm2, align = TRUE, 
          title = "Comparison of initial and second regression models",
          single.row = TRUE, report = ('vc*sp'),
          table.placement = "!htb")

stargazer(lm1, lm9, align = TRUE, 
          title = "Comparison of initial and final regression models",
          single.row = TRUE, report = ('vc*sp'),
          table.placement = "!htb")

# fitted vs original
library(tidyverse)
library(reshape2)

fit_lm9 <- fitted.values(lm9)
original_glucose <- new_kidney$glucose

comparison <- data.frame(id = c(1:156), Original = original_glucose, Fitted = fit_lm9)

comparison <- melt(comparison, id = "id")
ggplot(comparison, aes(id, value, color = variable))+
  geom_line(lwd = 1)+
  labs(y = "Glucose level", x = "Observations", title = "Fitted vs Original data",
       subtitle = "")+
  scale_fill_discrete(labels = c("Original", "Fitted"))+
  theme_bw()+
  theme(legend.position = "bottom", legend.title = element_blank())

#9####

# Determining property 1 of OLS - expected value of residuals equal to 0

mean(residuals(lm9))
# basically zero, then Ok


# Determining property 2 of OLS - The variance of residuals is constant and finite
res_lm9 <- residuals(lm9)
original_glucose <- new_kidney$glucose
comparison_two <- data.frame(original_glucose, res_lm9)
comparison_two <- comparison_two %>%
  arrange(original_glucose)

par(mfrow = c(1,2))

plot(residuals(lm9), type = "p", col = "gray", pch = 16,
     main = "Non-sorted observations",
     cex = 0.8, ylab = "Residuals")
abline(a = 3*sd(residuals(lm9)), b = 0, col = "red")
abline(a = -3*sd(residuals(lm9)), b = 0, col = "red")
abline(a = mean(residuals(lm9)), b = 0, col = "purple")

plot(comparison_two$res_lm9, type = "p", col = "black", pch = 16,
     main = "Arranged by Glucose level in ascending order",
     cex = 0.8, ylab = "Residuals")
abline(a = 3*sd(comparison_two$res_lm9), b = 0, col = "red")
abline(a = -3*sd(comparison_two$res_lm9), b = 0, col = "red")
abline(a = mean(comparison_two$res_lm9), b = 0, col = "purple")
# Visually, when sorting in ascending order the residuals based on glucose,
# it appears that variance is increasing, thus not constant --> heteroskedasticity.

# noneheless, we can compute the Breusch-Pagan test to assess whether
# heteroskedasticity is present or not.
library(lmtest)
bptest(lm9)
# it's not.

# Determining property 3 of OLS - residuals are not autocorrelated
library(car)
durbinWatsonTest(lm9)
# since p-value is 0, then we reject the null hypothesis
# that error terms are not autocorrelated.


# Determining property 4 of OLS - no relationship between the error term and explanatory variables

prop4_df <- new_kidney[, c(4,5,6,9,13,14)]
stargazer(cor(resid(lm9), prop4_df), title = "Correlation Matrix", align = TRUE)

# Determining property 5 of OLS - residuals are normally distributed
library(tsoutliers)

#mathematically
JarqueBera.test(residuals(lm9))

# visually

ggplot(comparison_two, aes(res_lm9))+
  geom_density(fill = alpha(rgb(0,0,1), alpha = 0.4))+
  labs(title = "Density distribution of residuals", subtitle = "",
       x = "Residuals' values")+
  theme_bw()



# small p-value and high X-squared, then we can reject
# the null hypothesis that residuals are normally distributed.

# way of summarising the results so that can be put in the report
Test <- c("Expected value of residuals", "Breusch-Pagan test",
          "Durbin-Watson test", "Jarque-Bera test")
Statistic <- c(mean(residuals(lm9)), bptest(lm9),
               durbinWatsonTest(lm9), JarqueBera.test(residuals(lm9)))

`Output statistic` <- c(as.numeric(round(Statistic[[1]], digits = 3)), 
                        as.numeric(round(Statistic[["statistic"]], digits = 3)), 
                        as.numeric(round(Statistic[["dw"]], digits = 3)),
                        as.numeric(round(Statistic[[11]][["statistic"]], digits = 3)))

`Output p-value` <- c(NA, 
                      as.numeric(round(Statistic[["p.value"]], digits = 4)), 
                      as.numeric(round(Statistic[["p"]], digits = 4)),
                      as.numeric(round(Statistic[[11]][["p.value"]], digits = 4)))

`Rejection rule` <- c("<> 0", "< 0.05", "< 0.05", "< 0.05")


properties <- cbind(Test, `Output statistic`, `Output p-value`, `Rejection rule`)
stargazer(properties, align = TRUE, title = "OLS properties assessment")

#Part 2####

#1####
library(readr)
data <- read_delim("dataShopping.csv", 
                   delim = ";", 
                   escape_double = FALSE, 
                   trim_ws = TRUE)
data <- as.data.frame(data)

# study the dataset
str(data)
'There are no missing values'

library(tidyverse)
data <- rename_with(data, tolower)
# we need to remove one variable because not part of the analysis
data <- data [, -7]


'3 dummies: specialday, newvisitor, revenue'


#2####

# broad numerical overview
library(skimr)
skim_without_charts(data)


library(reshape2)
id2 <- c(1:12330)
eda2 <- cbind(id2, data)
# to maintain the environment clean
remove(id2)

melt_eda2 <- melt(eda2, id = "id2")
names <- c("Administrative duration", 
           "Informational duration", 
           "Product-related duration", 
           "Bounce rates","Special Day", 
           "New Visitor", "Revenue")

# general overview

ggplot(melt_eda2, aes(x = value, fill = variable))+
  geom_histogram()+
  facet_wrap(~variable, scales = "free")+
  labs(x = "Value ranges for each variable considered", y = "Frequency count",
       title = "Visualisation of the distribution of values for each variable", subtitle = "")+
  theme_bw()+
  theme(legend.position = "none")

'This appears to be a little too confusing because of the excessive amount of zeros.
We should focus on values greater than zero'

# focus on values greater than zero
melt_eda2 %>%
  filter(value > 0 & variable != "revenue" & variable != "newvisitor") %>% 
  ggplot(aes(x = value, fill = variable))+
  geom_histogram()+
  facet_wrap(~variable, scales = "free")+
  labs(x = "Value ranges for each variable considered", y = "Frequency count",
       title = "Focused visualisation of the distribution of values for each variable", subtitle = "")+
  guides(fill=guide_legend(title="List of variables"))+
  scale_fill_discrete(labels = names)+
  theme_bw()


# boxplots
melt_eda2 %>%
  filter(value > 0 & variable != "revenue" & variable != "newvisitor") %>% 
  ggplot(aes(value))+
  geom_boxplot()+
  facet_wrap(~variable, scales = "free")+
  theme_bw()
'not useful these boxplots, so not included in the report'


#3####
library(corrplot)

cor(data)

# visualisation
cormatrix_2 <- structure(cor(data), .Dim = c(7L, 7L), 
                         .Dimnames = list(c("1. Administrative duration", 
                                            "2. Informational duration", 
                                            "3. Product-related duration", 
                                            "4. Bounce rates","5. Special Day", 
                                            "6. New Visitor", "7. Revenue"), 
                                          c(1:7)))
corrplot.mixed(cormatrix_2, upper = "circle", lower = "number",
               tl.pos = "lt", tl.col = "black", tl.offset = 1,
               number.cex = 0.5, bg = "gray5")

#4####
library(scales)
new_data <- apply(data, 2, rescale, to = c(0,1))
new_data <- as.data.frame(new_data)


#5####
new_data <- new_data[,c(1:6)]

library(purrr)
library(NbClust)

TWSS = map_dbl(1:12, function(k){
  model = kmeans(new_data, centers = k, nstart = 100)
  model$tot.withinss
})


# bear in mind that these three commands are computational heavy. It will take
# some time (approx. 5-8 minutes) to complete. 
silhouette_clustering <- NbClust(new_data, distance = "euclidean",
                                 min.nc = 2, max.nc = 12,
                                 method = "kmeans", index = "silhouette")

ch_clustering <- NbClust(new_data, distance = "euclidean",
                         min.nc = 2, max.nc = 12,
                         method = "kmeans", index = "ch")

gap_clustering <- NbClust(new_data, distance = "euclidean",
                          min.nc = 2, max.nc = 12,
                          method = "kmeans", index = "gap")

par(mfrow = c(1,4))
plot(1:12, TWSS, type = "o", col = "turquoise", 
     xlab = "number of clusters", ylab = "TWSS",
     main ="Elbow method")
plot(2:12, silhouette_clustering$All.index, type = "o", col = "blue",
     xlab = "number of clusters", ylab = "Silhouette score",
     main = "Silhouette method")
plot(2:12, gap_clustering$All.index, type = "o", col = "red",
     xlab = "number of clusters", ylab = "gap statistic",
     main = "Gap Statistic method")
plot(2:12, ch_clustering$All.index, type = "o", col = "purple",
     xlab = "number of clusters", ylab = "CH score", 
     main = "Calinski-Harabasz index")

# Do not run unless you really want to. It took me 76 minutes (yes!) to run it.
' 
res.nbclust <- NbClust(new_data, distance = "euclidean",
                       min.nc = 2, max.nc = 9, 
                       method = "complete", index ="all")

# this visualisation is quite fast instead, but you need the above nonetheless
library(factoextra)
fviz_nbclust(res.nbclust)+
  theme_bw()+
  ggtitle("NbClust''s optimal number of clusters")
'

#6####

# 3 centers
kmod <- kmeans(new_data, centers = 3, nstart = 100)

# let us put each (unnormalised) observation in a cluster
wc_data <- data %>% mutate(member = as.factor(kmod$cluster))

# either this 
stargazer(data_one, data_two, data_three, 
          align = TRUE, title = "Summary statistics, by Cluster",
          column.sep.width = "1pt", font.size = "small",
          digits = 2, covariate.labels = names,
          table.placement = "!htb")
# or this
wc_data %>%
  group_by(member) %>%
  skim_without_charts() %>%
  select(-n_missing, -complete_rate)

# correlation matrix by groups
one <- wc_data %>%
  filter(member == 1) %>%
  select(-member, -newvisitor)

corrplot(cor(one), "number")

two <- wc_data %>%
  filter(member == 2) %>%
  select(-member, -newvisitor)

corrplot(cor(two), "number")

three <- wc_data %>%
  filter(member == 3) %>%
  select(-member, -informational_duration)

corrplot(cor(three), "number")

'no risks of multicollinearity'


# regression models by group
# finding the model for C1
lmc1_1 <- lm(revenue ~ administrative_duration + informational_duration + 
               productrelated_duration + bouncerates + 
               specialday + newvisitor, data = data_one)
summary(lmc1_1)
lmc1_2 <- lm(revenue ~ administrative_duration + informational_duration + 
               productrelated_duration + bouncerates + 
               specialday, data = data_one)
summary(lmc1_2)
lmc1_3 <- lm(revenue ~ administrative_duration + informational_duration + 
               productrelated_duration + bouncerates, 
             data = data_one)
summary(lmc1_3)

# final C1
lmc1 <- lm(revenue ~ administrative_duration +
             productrelated_duration + bouncerates,
           data = data_one)
summary(lmc1)

# finding the model for C2
lmc2_1 <- lm(revenue ~ administrative_duration + informational_duration + 
               productrelated_duration + bouncerates + 
               specialday + newvisitor, data = data_two)
summary(lmc2_1)
lmc2_1 <- lm(revenue ~ administrative_duration + informational_duration + 
               productrelated_duration + bouncerates + 
               specialday, data = data_two)
summary(lmc2_1)

# final C2
lmc2 <- lm(revenue ~ administrative_duration +
             productrelated_duration + bouncerates +
             specialday, data = data_two)
summary(lmc2)


# finding the model for C3
lmc3_1 <- lm(revenue ~ administrative_duration + informational_duration +
               productrelated_duration + bouncerates +
               specialday + newvisitor, data = data_three)
summary(lmc3_1)
lmc3_2 <- lm(revenue ~ administrative_duration +
               productrelated_duration + bouncerates +
               specialday + newvisitor, data = data_three)
summary(lmc3_2)
lmc3_3 <- lm(revenue ~ administrative_duration +
               productrelated_duration + bouncerates +
               specialday, data = data_three)
summary(lmc3_3)
lmc3_4 <- lm(revenue ~ administrative_duration + bouncerates +
               specialday, data = data_three)
summary(lmc3_4)
lmc3_5 <- lm(revenue ~ administrative_duration + bouncerates,
             data = data_three)
summary(lmc3_5)

# final C3
lmc3 <- lm(revenue ~ administrative_duration,
           data = data_three)
summary(lmc3)

# summing up
stargazer(lmc1, lmc2, lmc3, digits = 2,
          align = TRUE, title = "Final regression models for each cluster",
          covariate.labels = c("Administrative duration", "Product-related duration",
                               "Bounce rates", "Special day"),
          column.sep.width = "1pt", font.size = "small",
          table.placement = "!htb")

# let's see the general behaviorus of the groups dividing them according whether they concluded a transaction or not

# C1
wc_data[,c(1,2,3,7,8,4,5)] %>%
  filter(member == 1) %>%
  group_by(revenue) %>%
  skim_without_charts() %>%
  select(-n_missing, -complete_rate)
'no substantial differences among people who buy and those who do not
If you think about it, it makes sense because we aimed at high intra-class similarity'

# prod.rel since it may be the only interesting difference
wc_data[,c(1,2,3,7,8,4,5)] %>%
  filter(member == 1) %>%
  ggplot(aes(x = productrelated_duration, fill = as.factor(revenue)))+
  geom_density(alpha = 0.5)+
  labs(title = "Time spent in product-related pages -- C1", subtitle = "Differentiating by buyers and non-buyers",
       x = "Duration", y = "")+
  xlim(c(0, 5000))+
  guides(fill=guide_legend(title="Type:"))+
  scale_fill_discrete(labels = c("Non-buyers", "Buyers"))+
  theme_bw()+
  theme(legend.position = "right")

# C2
wc_data[,c(1,2,3,7,8,4,5)] %>%
  filter(member == 2) %>%
  group_by(revenue) %>%
  skim_without_charts() %>%
  select(-n_missing, -complete_rate)
'interesting differences with respect to the prod.rel and admin.rel'

# prod.rel
wc_data[,c(1,2,3,7,8,4,5)] %>%
  filter(member == 2) %>%
  ggplot(aes(x = productrelated_duration, fill = as.factor(revenue)))+
  geom_density(alpha = 0.5)+
  labs(title = "Time spent in product-related pages -- C2", subtitle = "Differentiating by buyers and non-buyers",
       x = "Duration", y = "")+
  xlim(c(0, 12000))+
  guides(fill=guide_legend(title="Type:"))+
  scale_fill_discrete(labels = c("Non-buyers", "Buyers"))+
  theme_bw()+
  theme(legend.position = "right")

# admin.rel
wc_data[,c(1,2,3,7,8,4,5)] %>%
  filter(member == 2) %>%
  ggplot(aes(x = administrative_duration, fill = as.factor(revenue)))+
  geom_density(alpha = 0.5)+
  labs(title = "Time spent in administrative pages -- C2", subtitle = "Differentiating by buyers and non-buyers",
       x = "Duration", y = "")+
  xlim(c(0,600))+
  guides(fill=guide_legend(title="Type:"))+
  scale_fill_discrete(labels = c("Non-buyers", "Buyers"))+
  theme_bw()+
  theme(legend.position = "right")


# buyers of C2 vs C1
wc_data[,c(1,2,3,7,8,4,5)] %>%
  filter(revenue == 1) %>%
  filter(member != 3) %>%
  group_by(member) %>%
  skim_without_charts() %>%
  select(-n_missing, -complete_rate)

# prod.rel diff
wc_data[,c(1,2,3,7,8,4,5)] %>%
  filter(revenue == 1) %>%
  filter(member != 3) %>%
  ggplot(aes(x = productrelated_duration, fill = as.factor(member)))+
  geom_density(alpha = 0.5)+
  labs(title = "Time spent by buyers in product-related pages -- C1 vs C2", subtitle = "Differentiating by C1 and C2",
       x = "Duration", y = "")+
  xlim(c(0, 10000))+
  guides(fill=guide_legend(title="Membership:"))+
  scale_fill_discrete(labels = c("C1", "C2"))+
  theme_bw()+
  theme(legend.position = "right")

# adm.rel diff
wc_data[,c(1,2,3,7,8,4,5)] %>%
  filter(revenue == 1) %>%
  filter(member != 3) %>%
  ggplot(aes(x = administrative_duration, fill = as.factor(member)))+
  geom_density(alpha = 0.5)+
  labs(title = "Time spent by buyers in administrative pages -- C1 vs C2", subtitle = "Differentiating by C1 and C2",
       x = "Duration", y = "")+
  xlim(c(0, 1800))+
  guides(fill=guide_legend(title="Membership:"))+
  scale_fill_discrete(labels = c("C1", "C2"))+
  theme_bw()+
  theme(legend.position = "right")

# br diff
wc_data[,c(1,2,3,7,8,4,5)] %>%
  filter(revenue == 1) %>%
  filter(member != 3) %>%
  ggplot(aes(x = bouncerates, fill = as.factor(member)))+
  geom_density(alpha = 0.5)+
  labs(title = "Bounce rates of buyers -- C1 vs C2", subtitle = "Differentiating by C1 and C2",
       x = "Rate", y = "")+
  xlim(c(0,0.05))+
  guides(fill=guide_legend(title="Membership:"))+
  scale_fill_discrete(labels = c("C1", "C2"))+
  theme_bw()+
  theme(legend.position = "right")






































