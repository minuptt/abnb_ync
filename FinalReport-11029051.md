INFO 3010 - Final Report
================

## Fall 2019 by Lingzi Hong

## Time: Friday, Dec 13, 2019

### Instructions

1.  This is an R Markdown format used for publishing markdown documents
    to GitHub. When you click the **Knit** button, all R code chunks are
    run and a markdown file (.md) suitable for publishing to GitHub is
    generated.
2.  Fill in the code chunks for following question and submit this R
    markdown file to the assignment on Canvas. Make sure when you save
    that you have run all cells, so the outputs displace between the
    cells.
3.  make sure to replace the directoryID in the filename with your
    student ID.
4.  Solo work, open book & open notes - This part of the exam is open
    book, open notes. You may use the textbook, your own notes, and any
    pre-existing resources. You must not use communicate with others -
    in person, using online forums, social media, Slack, WeChat,
    GroupMe, etc. If you have concerns, contact me directly so I can
    advise you.
5.  Partial credit - If you can’t get your code to work 100% correctly,
    you can often earn partial credit. To get partial credit you need to
    show that you know there is a problem and describe it. The better
    you can describe the problem, the more partial credit you can earn,
    because it helps demonstrate your knowledge and skill.

### 

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
```

### Read the file ‘abnbnyc.csv’ to a dataframe. Check the first 5 lines of the data.

``` r
setwd('/Users/ThiPhan1/Documents/UNT/1FALL2019/INFO3010/FINAL')
abnb <- read.csv("abnbync.csv")
str(abnb)
```

    ## 'data.frame':    48895 obs. of  16 variables:
    ##  $ id                            : int  2539 2595 3647 3831 5022 5099 5121 5178 5203 5238 ...
    ##  $ name                          : Factor w/ 47906 levels ""," 1 Bed Apt in Utopic Williamsburg ",..: 12573 38016 45018 15591 19219 24849 8257 24896 15486 17573 ...
    ##  $ host_id                       : int  2787 2845 4632 4869 7192 7322 7356 8967 7490 7549 ...
    ##  $ host_name                     : Factor w/ 11453 levels "","​ Valéria",..: 4997 4791 2913 6210 5929 1938 3549 9649 6880 1235 ...
    ##  $ neighbourhood_group           : Factor w/ 5 levels "Bronx","Brooklyn",..: 2 3 3 2 3 3 2 3 3 3 ...
    ##  $ neighbourhood                 : Factor w/ 221 levels "Allerton","Arden Heights",..: 109 128 95 42 62 138 14 96 203 36 ...
    ##  $ latitude                      : num  40.6 40.8 40.8 40.7 40.8 ...
    ##  $ longitude                     : num  -74 -74 -73.9 -74 -73.9 ...
    ##  $ room_type                     : Factor w/ 3 levels "Entire home/apt",..: 2 1 2 1 1 1 2 2 2 1 ...
    ##  $ price                         : int  149 225 150 89 80 200 60 79 79 150 ...
    ##  $ minimum_nights                : int  1 1 3 1 10 3 45 2 2 1 ...
    ##  $ number_of_reviews             : int  9 45 0 270 9 74 49 430 118 160 ...
    ##  $ last_review                   : Factor w/ 1765 levels "","2011-03-28",..: 1503 1717 1 1762 1534 1749 1124 1751 1048 1736 ...
    ##  $ reviews_per_month             : num  0.21 0.38 NA 4.64 0.1 0.59 0.4 3.47 0.99 1.33 ...
    ##  $ calculated_host_listings_count: int  6 2 1 1 1 1 1 1 1 4 ...
    ##  $ availability_365              : int  365 355 365 194 0 129 0 220 0 188 ...

``` r
head(abnb, 5)
```

    ##     id                                             name host_id
    ## 1 2539               Clean & quiet apt home by the park    2787
    ## 2 2595                            Skylit Midtown Castle    2845
    ## 3 3647              THE VILLAGE OF HARLEM....NEW YORK !    4632
    ## 4 3831                  Cozy Entire Floor of Brownstone    4869
    ## 5 5022 Entire Apt: Spacious Studio/Loft by central park    7192
    ##     host_name neighbourhood_group neighbourhood latitude longitude
    ## 1        John            Brooklyn    Kensington 40.64749 -73.97237
    ## 2    Jennifer           Manhattan       Midtown 40.75362 -73.98377
    ## 3   Elisabeth           Manhattan        Harlem 40.80902 -73.94190
    ## 4 LisaRoxanne            Brooklyn  Clinton Hill 40.68514 -73.95976
    ## 5       Laura           Manhattan   East Harlem 40.79851 -73.94399
    ##         room_type price minimum_nights number_of_reviews last_review
    ## 1    Private room   149              1                 9  2018-10-19
    ## 2 Entire home/apt   225              1                45  2019-05-21
    ## 3    Private room   150              3                 0            
    ## 4 Entire home/apt    89              1               270  2019-07-05
    ## 5 Entire home/apt    80             10                 9  2018-11-19
    ##   reviews_per_month calculated_host_listings_count availability_365
    ## 1              0.21                              6              365
    ## 2              0.38                              2              355
    ## 3                NA                              1              365
    ## 4              4.64                              1              194
    ## 5              0.10                              1                0

### Find the missing value and then drop missing value if it has

``` r
#missing <- colSums(is.na(abnb))
#abnb <- na.omit(abnb)
```

### Find the lowest, the highest and median of the price

``` r
summary(abnb$price)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     0.0    69.0   106.0   152.7   175.0 10000.0

### Find how many neighborhood groups in the dataset

``` r
unique(abnb$neighbourhood_group)
```

    ## [1] Brooklyn      Manhattan     Queens        Staten Island Bronx        
    ## Levels: Bronx Brooklyn Manhattan Queens Staten Island

### Map of neighborhood group

``` r
ggplot(abnb, aes(x = longitude, y = latitude, color = neighbourhood_group)) + geom_point()
```

![](FinalReport-11029051_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### Distribution of price for each group

``` r
abnb_group <- abnb %>%
  group_by(neighbourhood_group) %>%
  summarize(price = mean(price))

abnb_group
```

    ## # A tibble: 5 x 2
    ##   neighbourhood_group price
    ##   <fct>               <dbl>
    ## 1 Bronx                87.5
    ## 2 Brooklyn            124. 
    ## 3 Manhattan           197. 
    ## 4 Queens               99.5
    ## 5 Staten Island       115.

``` r
ggplot(abnb, aes(price)) +
  geom_histogram(bins = 30, aes(y = ..density..), fill = "yellow") + 
  geom_density(alpha = 0.2, fill = "yellow") +
  geom_vline(data = abnb_group, aes(xintercept = price), size = 2, linetype = 3) +
  geom_text(data = abnb_group,y = 1.5, aes(x = price + 1400, label = paste("Mean  = ",price)), color = "darkgreen", size = 4) +
  facet_wrap(~neighbourhood_group) +
  scale_x_log10() 
```

    ## Warning: Transformation introduced infinite values in continuous x-axis
    
    ## Warning: Transformation introduced infinite values in continuous x-axis

    ## Warning: Removed 11 rows containing non-finite values (stat_bin).

    ## Warning: Removed 11 rows containing non-finite values (stat_density).

![](FinalReport-11029051_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

### Distribution of price for each room type

``` r
ggplot(abnb, aes(x = room_type, y=price, color = room_type)) + geom_bar(stat = "identity")
```

![](FinalReport-11029051_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

### The list of top 10 popular places that were rented mostly

``` r
popularplace <- abnb %>% group_by(neighbourhood, name) %>% summarise(count = n_distinct(id)) %>% arrange(desc(count))
head(popularplace,10)
```

    ## # A tibble: 10 x 3
    ## # Groups:   neighbourhood [9]
    ##    neighbourhood name                                               count
    ##    <fct>         <fct>                                              <int>
    ##  1 Jamaica       Hillside Hotel                                        12
    ##  2 Greenpoint    Loft Suite @ The Box House Hotel                      11
    ##  3 Fort Greene   Artsy Private BR in Fort Greene Cumberland            10
    ##  4 Williamsburg  Private room in Williamsburg                           7
    ##  5 Briarwood     Hillside Hotel                                         6
    ##  6 East Village  Cozy East Village Apartment                            6
    ##  7 Midtown       IN MINT CONDITION-STUDIOS EAST 44TH/UNITED NATIONS     6
    ##  8 West Village  West Village Apartment                                 6
    ##  9 East Village  East Village Apartment                                 5
    ## 10 Harlem        Harlem Gem                                             5

### Model - Data splitting

``` r
# Divide as training and testing: 20% test 80% train
# get the training data size
sample_size <- floor(0.8*nrow(abnb))

# check the training data size
#sample_size

# get the index of training samples
train_ind <- sample(seq_len(nrow(abnb)), size = sample_size)
#train_ind

# generate the train and test dataset
train <- abnb[train_ind,]
test <- abnb[-train_ind,]
```

### Linear Regression Model

``` r
linearmodel <- lm(price ~ latitude + longitude + room_type + minimum_nights  + availability_365 + neighbourhood_group, data = train)

# check the model
summary(linearmodel)
```

    ## 
    ## Call:
    ## lm(formula = price ~ latitude + longitude + room_type + minimum_nights + 
    ##     availability_365 + neighbourhood_group, data = train)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -248.7  -62.3  -24.8   14.6 9943.1 
    ## 
    ## Coefficients:
    ##                                    Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                      -3.044e+04  3.561e+03  -8.547  < 2e-16
    ## latitude                         -1.802e+02  3.451e+01  -5.222 1.78e-07
    ## longitude                        -5.134e+02  3.991e+01 -12.864  < 2e-16
    ## room_typePrivate room            -1.059e+02  2.388e+00 -44.329  < 2e-16
    ## room_typeShared room             -1.371e+02  7.616e+00 -18.000  < 2e-16
    ## minimum_nights                    2.697e-02  5.622e-02   0.480  0.63139
    ## availability_365                  1.601e-01  8.924e-03  17.936  < 2e-16
    ## neighbourhood_groupBrooklyn      -3.044e+01  9.769e+00  -3.117  0.00183
    ## neighbourhood_groupManhattan      2.813e+01  8.883e+00   3.166  0.00155
    ## neighbourhood_groupQueens        -2.252e+00  9.422e+00  -0.239  0.81109
    ## neighbourhood_groupStaten Island -1.392e+02  1.843e+01  -7.551 4.43e-14
    ##                                     
    ## (Intercept)                      ***
    ## latitude                         ***
    ## longitude                        ***
    ## room_typePrivate room            ***
    ## room_typeShared room             ***
    ## minimum_nights                      
    ## availability_365                 ***
    ## neighbourhood_groupBrooklyn      ** 
    ## neighbourhood_groupManhattan     ** 
    ## neighbourhood_groupQueens           
    ## neighbourhood_groupStaten Island ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 225.9 on 39105 degrees of freedom
    ## Multiple R-squared:  0.09651,    Adjusted R-squared:  0.09628 
    ## F-statistic: 417.7 on 10 and 39105 DF,  p-value: < 2.2e-16

``` r
# optional diagnosis graphs
layout(matrix(c(1,2,3,4),2,2))  
plot(linearmodel)
```

![](FinalReport-11029051_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
# evaluate the model on test data
prediction <-predict(linearmodel, newdata = test)

# check head of the prediction
summary(prediction)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -63.05   88.65  157.83  152.28  211.82  312.85

``` r
library('hydroGOF') 
```

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## Registered S3 method overwritten by 'xts':
    ##   method     from
    ##   as.zoo.xts zoo

``` r
rmse(prediction, test$price, na.rm = TRUE)
```

    ## [1] 238.6428

``` r
# test the correlation between predicted values and real values
cor.test(prediction,test$price,use= "complete")
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  prediction and test$price
    ## t = 30.87, df = 9777, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.2798462 0.3159676
    ## sample estimates:
    ##       cor 
    ## 0.2980136

``` r
# train linear models with regularization
# before training, remove samples with NA 
train <- train[complete.cases(train),]
#train

#set cross validation method: ## 5-fold CV, repeated two times 
library('caret')
```

    ## Loading required package: lattice

``` r
fitControl <- trainControl(method = "repeatedcv", 
                           number = 5,
                           repeats = 2)
#fitControl

#train the model
glmFit1 <- train(price ~ latitude + longitude + room_type + minimum_nights  + availability_365 + neighbourhood_group, 
                data = train, method = "penalized", 
                trControl= fitControl)
```

``` 
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
## # nonzero coefficients: 11

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          

# nonzero coefficients: 11          
```

``` r
# check the model
glmFit1
```

    ## Penalized Linear Regression 
    ## 
    ## 31053 samples
    ##     6 predictor
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold, repeated 2 times) 
    ## Summary of sample sizes: 24842, 24842, 24844, 24841, 24843, 24842, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   lambda1  lambda2  RMSE      Rsquared   MAE     
    ##   1        1        183.8973  0.1237163  61.44932
    ##   1        2        183.8994  0.1237050  61.41799
    ##   1        4        183.9073  0.1236431  61.37371
    ##   2        1        183.8973  0.1237163  61.44920
    ##   2        2        183.8994  0.1237049  61.41789
    ##   2        4        183.9073  0.1236430  61.37364
    ##   4        1        183.8973  0.1237162  61.44896
    ##   4        2        183.8994  0.1237048  61.41768
    ##   4        4        183.9073  0.1236427  61.37348
    ## 
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final values used for the model were lambda1 = 1 and lambda2 = 1.

``` r
layout(matrix(c(1,2,3,4),2,2))  
plot(glmFit1)
```

![](FinalReport-11029051_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

``` r
# apply model on test data
test <-test[complete.cases(test),]
prediction<-predict(glmFit1,newdata = test)

# evaluate prediction on test data
rmse(prediction, test$price)
```

    ## [1] 180.4596

``` r
cor.test(prediction, test$price)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  prediction and test$price
    ## t = 33.843, df = 7788, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.3385481 0.3772704
    ## sample estimates:
    ##       cor 
    ## 0.3580632

### Logistic Model
