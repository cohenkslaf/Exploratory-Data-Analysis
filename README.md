README
================
# Wine Quality Analysis Report

## Business Understanding

Good quality wine is typically characterized by a balance of several factors including acidity, tannins, alcohol content, sweetness, and the wine's overall structure. Wine quality is often judged by consumers based on taste preferences, which may vary by region, culture, and even personal preference. Generally, higher-quality wines tend to have well-balanced acidity and alcohol levels, appropriate residual sugar content, and fewer defects, such as high volatile acidity. Wine ratings, like those used in this dataset, are often influenced by expert panels and consumer reviews, which typically rate the wines on a scale (e.g., 1-10). Factors such as the vineyard’s terroir (environmental factors), the grape variety, and production techniques such as fermentation temperature and time, also play significant roles in determining the final quality.

## Data Understanding

The dataset used for this analysis comes from the UCI Wine Quality Database and includes chemical and sensory attributes of red and white wines, specifically:

- **Fixed Acidity**: Influences the taste of the wine, contributing to its tartness and aging potential.
- **Volatile Acidity**: A measure of wine spoilage, typically an undesirable trait at high levels.
- **Citric Acid**: Contributes to the wine's freshness and acidity.
- **Residual Sugar**: Impacts the sweetness of the wine.
- **Chlorides**: Higher levels can result in undesirable salty flavors.
- **Free Sulfur Dioxide**: Helps preserve wine but excessive levels can result in an unpleasant taste.
- **Total Sulfur Dioxide**: A form of sulfur used in wine preservation.
- **Density**: Affects the mouthfeel and body of the wine.
- **pH**: Influences the wine's acidity.
- **Sulphates**: Adds to the wine's preservation and stability.
- **Alcohol**: A key factor influencing the body and taste of wine.
- **Quality**: A subjective rating assigned by consumers and wine experts.

This dataset has no missing values and consists of 6,497 instances from both red and white wines, providing a sufficient sample for analysis.

## Purpose

> We needed to extract some “actionable” insights from the UCI Wine
> Quality Data Set . The purpose is that a wine producer has hired us to
> better understand what factors affect wine quality. The producer can
> also look at which production factors have an effect on rating scores
> by consumers. The reason why we utilized exploratory data analysis is
> show different distributions of certain production techniques and why
> consumers probably favor one type of wine over the other.


## Loading and Installing the Required Packages and Data Loading and Merging

``` r
required_packages <- c("tidyverse", "broom", "corrplot")
to_install <- setdiff(required_packages, installed.packages()[, "Package"])
if (length(to_install) > 0) install.packages(to_install)

# Core libraries for data wrangling, plotting, and correlation analysis
library(tidyverse)
library(dplyr)       # ensure dplyr functions like bind_rows are available
library(broom)
library(corrplot)
```

``` r
# URLs for the datasets and other sources
red_url   <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
white_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
volatile_acidity_url <- "https://www.awri.com.au/wp-content/uploads/2018/03/s1982.pdf"

# Read data
red   <- read.csv(red_url, sep = ";")
white <- read.csv(white_url, sep = ";")

# Add type column
red$type   <- "red"
white$type <- "white"

# Combine using dplyr::bind_rows to ensure the function is found
data <- dplyr::bind_rows(red, white)
```

## Assumptions Check for Statistical Tests

For the t-test used to compare the means of red and white wine quality, we must check the assumptions of normality and variance homogeneity:

### Normality:
We assume that the distribution of wine quality scores for both red and white wines is approximately normal. To verify this, we can conduct Shapiro-Wilk tests for normality or visually inspect histograms and Q-Q plots for both types of wine.

### Homogeneity of Variances:
The assumption of equal variances between the two groups (red and white wines) can be tested using Levene’s test. If the variances are unequal, we may need to use a Welch's t-test instead of a standard t-test.

For the correlation tests, we assume:

- Linearity between the variables (alcohol and quality).
- Normality of the residuals, which can be checked using Q-Q plots.
- Independence of the observations.

  
## Initial Data Exploration

``` r
# Sample sizes
glue::glue("Red: {nrow(red)}, White: {nrow(white)}, Total: {nrow(data)}")
```

    ## Red: 1599, White: 4898, Total: 6497

``` r
# Structure and summary
glimpse(data)
```

    ## Rows: 6,497
    ## Columns: 13
    ## $ fixed.acidity        <dbl> 7.4, 7.8, 7.8, 11.2, 7.4, 7.4, 7.9, 7.3, 7.8, 7.5…
    ## $ volatile.acidity     <dbl> 0.700, 0.880, 0.760, 0.280, 0.700, 0.660, 0.600, …
    ## $ citric.acid          <dbl> 0.00, 0.00, 0.04, 0.56, 0.00, 0.00, 0.06, 0.00, 0…
    ## $ residual.sugar       <dbl> 1.9, 2.6, 2.3, 1.9, 1.9, 1.8, 1.6, 1.2, 2.0, 6.1,…
    ## $ chlorides            <dbl> 0.076, 0.098, 0.092, 0.075, 0.076, 0.075, 0.069, …
    ## $ free.sulfur.dioxide  <dbl> 11, 25, 15, 17, 11, 13, 15, 15, 9, 17, 15, 17, 16…
    ## $ total.sulfur.dioxide <dbl> 34, 67, 54, 60, 34, 40, 59, 21, 18, 102, 65, 102,…
    ## $ density              <dbl> 0.9978, 0.9968, 0.9970, 0.9980, 0.9978, 0.9978, 0…
    ## $ pH                   <dbl> 3.51, 3.20, 3.26, 3.16, 3.51, 3.51, 3.30, 3.39, 3…
    ## $ sulphates            <dbl> 0.56, 0.68, 0.65, 0.58, 0.56, 0.56, 0.46, 0.47, 0…
    ## $ alcohol              <dbl> 9.4, 9.8, 9.8, 9.8, 9.4, 9.4, 9.4, 10.0, 9.5, 10.…
    ## $ quality              <int> 5, 5, 5, 6, 5, 5, 5, 7, 7, 5, 5, 5, 5, 5, 5, 5, 7…
    ## $ type                 <chr> "red", "red", "red", "red", "red", "red", "red", …

``` r
summary(data)
```

    ##  fixed.acidity    volatile.acidity  citric.acid     residual.sugar  
    ##  Min.   : 3.800   Min.   :0.0800   Min.   :0.0000   Min.   : 0.600  
    ##  1st Qu.: 6.400   1st Qu.:0.2300   1st Qu.:0.2500   1st Qu.: 1.800  
    ##  Median : 7.000   Median :0.2900   Median :0.3100   Median : 3.000  
    ##  Mean   : 7.215   Mean   :0.3397   Mean   :0.3186   Mean   : 5.443  
    ##  3rd Qu.: 7.700   3rd Qu.:0.4000   3rd Qu.:0.3900   3rd Qu.: 8.100  
    ##  Max.   :15.900   Max.   :1.5800   Max.   :1.6600   Max.   :65.800  
    ##    chlorides       free.sulfur.dioxide total.sulfur.dioxide    density      
    ##  Min.   :0.00900   Min.   :  1.00      Min.   :  6.0        Min.   :0.9871  
    ##  1st Qu.:0.03800   1st Qu.: 17.00      1st Qu.: 77.0        1st Qu.:0.9923  
    ##  Median :0.04700   Median : 29.00      Median :118.0        Median :0.9949  
    ##  Mean   :0.05603   Mean   : 30.53      Mean   :115.7        Mean   :0.9947  
    ##  3rd Qu.:0.06500   3rd Qu.: 41.00      3rd Qu.:156.0        3rd Qu.:0.9970  
    ##  Max.   :0.61100   Max.   :289.00      Max.   :440.0        Max.   :1.0390  
    ##        pH          sulphates         alcohol         quality    
    ##  Min.   :2.720   Min.   :0.2200   Min.   : 8.00   Min.   :3.000  
    ##  1st Qu.:3.110   1st Qu.:0.4300   1st Qu.: 9.50   1st Qu.:5.000  
    ##  Median :3.210   Median :0.5100   Median :10.30   Median :6.000  
    ##  Mean   :3.219   Mean   :0.5313   Mean   :10.49   Mean   :5.818  
    ##  3rd Qu.:3.320   3rd Qu.:0.6000   3rd Qu.:11.30   3rd Qu.:6.000  
    ##  Max.   :4.010   Max.   :2.0000   Max.   :14.90   Max.   :9.000  
    ##      type          
    ##  Length:6497      
    ##  Class :character  
    ##  Mode  :character  
    ##                    
    ##                    
    ##

> The main reason why we ran this code was to see summary statistics for
> red and white wine. While doing this, we see that there are more white
> wine samples than red wine.

## Missing Values

``` r
colSums(is.na(data))
```

    ##        fixed.acidity     volatile.acidity          citric.acid
    ##                    0                    0                    0
    ##       residual.sugar            chlorides  free.sulfur.dioxide
    ##                    0                    0                    0
    ## total.sulfur.dioxide              density                   pH
    ##                    0                    0                    0
    ##            sulphates              alcohol              quality
    ##                    0                    0                    0
    ##                 type
    ##                    0

> I ran this code to confirm that there is no missing data.

## Exploratory Data Analysis

### Quality Distribution

``` r
ggplot(data, aes(x = factor(quality), fill = type)) +
  geom_bar(position = "dodge") +
  labs(title = "Quality Score Distribution",
       x = "Quality Score", y = "Count") +
  theme_minimal()
```

![Distribution of Quality Scores by Wine
Type](4_25_Capstone_-Complete1-_.Rmd-_files/figure-gfm/quality-distribution-1.png)
\> Red wine ratings have had stayed mainly in the five to six ranges.
White wine ratings had higher ratings with six, seven, and eight.
Reveals whether one wine type generally receives higher ratings.
Additonally, white wine has an overall higher rating than red wine and
shows an overall wider spread than it. In conclusion this can mean that
white wine is more enjoyed than red wine to consumers. Another reason
could be that white wine also outnumbers red wine in the amount of
observations.

### Chemical Properties

``` r
features <- c("alcohol", "pH", "residual.sugar", "volatile.acidity")
plot_list <- lapply(features, function(feat) {
  ggplot(data, aes(x = type, y = .data[[feat]])) +
    geom_boxplot() +
    labs(title = feat, y = feat) +
    theme_minimal()
})

# Print all plots
a <- plot_list[[1]]
for(p in plot_list) print(p)
```

![Key Chemical Properties by Wine
Type](4_25_Capstone_-Complete1-_.Rmd-_files/figure-gfm/chemical-boxplots-1.png)![Key
Chemical Properties by Wine
Type](4_25_Capstone_-Complete1-_.Rmd-_files/figure-gfm/chemical-boxplots-2.png)![Key
Chemical Properties by Wine
Type](4_25_Capstone_-Complete1-_.Rmd-_files/figure-gfm/chemical-boxplots-3.png)![Key
Chemical Properties by Wine
Type](4_25_Capstone_-Complete1-_.Rmd-_files/figure-gfm/chemical-boxplots-4.png)
\> The first box-plot compares alcohol levels for red and white wine.
The white wine has a higher median than the red wine and most of the
white wine’s data points are clustered towards the top while the red
wine’s data points are clustered more towards the bottom. This means
that on average, white wine contains more alcohol than red wine. 



The
second box-plot represents ph levels of both types of wine. The higher
the ph level, the less acidic the, which we see with the white wine. Red
wine acoording to the box plots has a higher acidic level. The third
box-plot compares the levels of residual sugar. While looking at the
box-plot, one could infer that since white wine data points cluster
towards the top, it is safe to say that white wine tends to be sweeter
than red wine since the red wine data points are clustered more twoards
the bottom. 



Finally, the last box-plot compares the wines’ data based
off of volatile acidity. While studying the box-plot, it is shown that
the red wine quartile range has a higher value than the white wine’s
range, meaning that red wine tends to have a higher volatile acidity.
One question to ask would be “What is the difference between ph level
testing and volatile acidity?” The main difference is that the two
measure different ideas of acid. Ph levels are often the measure how
strong the acid feeling is while you consume something acidic. On the
contrary volatile acidity “is a measure of the low molecular weight (or
steam distillable) fatty acids in wine and is generally percieved as the
odour of vinegar” (Australian Wine Research Institute, 2018, p. 2).




Lastly, these are important chemical differences to look into becasue
most of the time it depends on what the consumer is preferring, whether
it is a less acidic feeling while drinking, the amount of alcohol, the
amount of sugar, or the amount of fatty acids in their wine.

### Correlation Heatmaps

``` r
cor_red   <- cor(filter(data, type == "red")   %>% select(-type))
cor_white <- cor(filter(data, type == "white") %>% select(-type))

par(mfrow = c(1,2))
corrplot(cor_red,   main = "Red Wine",   mar = c(0,0,1,0))
corrplot(cor_white, main = "White Wine", mar = c(0,0,1,0))
```

<figure>
<img
src="4_25_Capstone_-Complete1-_.Rmd-_files/figure-gfm/correlation-heatmaps-1.png"
alt="Correlation Matrices for Red and White Wines" />
<figcaption aria-hidden="true">Correlation Matrices for Red and White
Wines</figcaption>
</figure>

``` r
par(mfrow = c(1,1))
```

> Similarly to the box plots above, these correlational heatmaps only
> test the chemical compisitions in wine. However, the box-plots only
> compared the marginal distributions between the red and white wine.
> The correlational heatmaps test the relationship between quality and
> chemicals that are inside. The darer the circle, the strongher the
> correlation. The darker the blue, the more of a positve correlation
> while if it has a red shade, it is closer to a negative correlation.
> While looking at the some of the circles for red wine, it is clear
> that ph and alcohol do not have that strong of a coreelation with one
> another because the circle is light blue, meaning that its value is
> not close to one or negative one. One strong relationship that one
> could pull from the heatmap is that alcohol and quality have a strong
> correlational relationship. Compared to white wine, it has a slightly
> darker color menaing that the relationship between quality and alcohol
> is stronger in red.

## Statistical Testing

### T-Test for Mean Quality

``` r
ttest <- t.test(quality ~ type, data = data)
tidy(ttest)
```

    ## # A tibble: 1 × 10
    ##   estimate estimate1 estimate2 statistic  p.value parameter conf.low conf.high
    ##      <dbl>     <dbl>     <dbl>     <dbl>    <dbl>     <dbl>    <dbl>     <dbl>
    ## 1   -0.242      5.64      5.88     -10.1 8.17e-24     2951.   -0.289    -0.195
    ## # ℹ 2 more variables: method <chr>, alternative <chr>

> The purpose of this is to run a two-sample t-test comparing the means
> of white and red wine. Estimate one represents the mean quality red
> wine while Estimate 2 represents the mean quality of white wine. As
> shown, white wine has a higher mean quality proving that white wine is
> favored more by consumers.

### Alcohol vs Quality Correlation

``` r
cor_red_alc   <- cor.test(filter(data, type == "red")$alcohol,
                            filter(data, type == "red")$quality)
cor_white_alc <- cor.test(filter(data, type == "white")$alcohol,
                            filter(data, type == "white")$quality)

list(
  red   = tidy(cor_red_alc),
  white = tidy(cor_white_alc)
)
```

    ## $red
    ## # A tibble: 1 × 8
    ##   estimate statistic  p.value parameter conf.low conf.high method    alternative
    ##      <dbl>     <dbl>    <dbl>     <int>    <dbl>     <dbl> <chr>     <chr>      
    ## 1    0.476      21.6 2.83e-91      1597    0.437     0.513 Pearson'… two.sided  
    ##
    ## $white
    ## # A tibble: 1 × 8
    ##   estimate statistic   p.value parameter conf.low conf.high method   alternative
    ##      <dbl>     <dbl>     <dbl>     <int>    <dbl>     <dbl> <chr>    <chr>      
    ## 1    0.436      33.9 5.61e-226      4896    0.413     0.458 Pearson… two.sided

> For this table, we decided to run a pearson correlation between
> qualtiy and alcohol level. We already compared the two wines with
> these statistics in the heatmap above. Similarly, it is proven that
> both wines have a strong relationship when it comes quality and
> alcohol. Additionally, it is also proven that red wine has a stronger
> link between the two than white wine but only by a small marigin. Both
> of the p-vaulues being significant proves these tests are legit and
> the results we gathered during this study is reliable.

## Conclusion

> Overall, our exploratory analysis concludes that white wines are
> favored more than red wines. White wine has earned a score of 5.88,
> while red wine has earned a score of 5.64, with a statistically
> significant difference of 0.24. Alcohol remains one of the strongest
> determining factors for consumers with red wine having a higher score
> than white (.48 \> .44). Additionally, volotility acididty has a
> stringer affect on red wine with a score of 0.39, compared to white
> which had a -0.19, showing that less volotility acididty is favored.
> Sweetness and ph levels played less of a role in quality according to
> the heatmap, however white wine had a sweeter taste while red wine had
> a higher ph level. In conclusion the observation can be definite is
> that white wine producers should stick to sweeter wines while red wine
> producers should stick to a higher level ph method of making wine.



## Conclusion and Recommendations

From the exploratory data analysis, statistical tests, and visualizations, we conclude that white wine tends to have higher ratings and is preferred by consumers over red wine. The significant difference in average quality scores (white wine = 5.88, red wine = 5.64) supports this finding. Furthermore, alcohol content is a strong predictor of wine quality for both red and white wines. Red wine is characterized by a higher pH and volatile acidity, which consumers may perceive as less favorable.

### Recommendations:
We recommend that wine producers focus on the following to improve wine quality:

- **For Red Wine Producers**: Reducing volatile acidity and ensuring a balanced alcohol content may improve the overall quality perception. Additionally, adjusting pH levels to reduce acidity might appeal to consumers.
- **For White Wine Producers**: Since white wines tend to be sweeter and less acidic, focusing on maintaining moderate alcohol content while ensuring sweetness and a smooth, balanced acidity can further enhance consumer preference.

## Future Research

Future studies could incorporate additional variables such as the geographical region of production, vintage year, or consumer preferences through survey data. Moreover, examining the sensory attributes of wine (e.g., aroma, flavor complexity) in conjunction with chemical properties could offer a more comprehensive understanding of what drives consumer preferences.





