hw01-Zihao-Li
================

``` r
load("data/nba2017-salary-points.RData")
ls()
```

    ## [1] "experience" "player"     "points"     "points1"    "points2"   
    ## [6] "points3"    "position"   "salary"     "team"

#### Data preprocessing:

``` r
salary_million <- salary/1000000
salary_million <- round(salary_million, digits = 2)

experience_new <- replace(experience, experience == "R", 0)
experience_int <- as.integer(experience_new)

position_fac <- factor(position)
levels(position_fac) <- c("center", "power_fwd", "point_guard", "small_fwd", "shoot_guard")
table(position_fac)
```

    ## position_fac
    ##      center   power_fwd point_guard   small_fwd shoot_guard 
    ##          89          89          85          83          95

#### Scatterplot of points and salary:

``` r
plot(points, salary_million, xlab = "Points", ylab = "Salary (in millions)", 
     main = paste("Scatterplot of Points and Salary"), col = "red", pch = 1, cex = 0.8)
```

<img src="hw01-Zihao-Li_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

#### Correlation between points and salary:

``` r
n <- length(points)
x_mean <- sum(points)/n
y_mean <- sum(salary_million)/n
x_var <- sum((points - x_mean)^2)/(n-1)
y_var <- sum((salary_million - y_mean)^2)/(n-1)
x_sd <- sqrt(x_var)
y_sd <- sqrt(y_var)
covariance <- sum((points - x_mean)*(salary_million - y_mean))/(n-1)
correlation <- covariance/(x_sd*y_sd)
correlation
```

    ## [1] 0.6367296

#### Linear Regression:

``` r
b1 <- correlation*y_sd/x_sd
b0 <- y_mean - b1*x_mean
y_hat <- b0 + b1*points

summary(y_hat)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.509   2.844   5.206   6.187   8.184  23.400

The equation of the regression line is: $\\hat{Y} = 1.509 + 0.008558X$

-   The coefficient b1 can be interpreted as the expected increase in salary (in unit of millions), when the points scored increases by 1.
-   The intercept b0 can be interpreted as the salary of a player who scores zero points.
-   The predicted salaries (in millions) for players that score:
    -   0: 1.509 million
    -   100: 1.509 + 0.008558 \* 100 = 2.3648
    -   500: 1.509 + 0.008558 \* 500 = 5.788
    -   1000: 1.509 + 0.008558 \* 1000 = 10.067
    -   2000: 1.509 + 0.008558 \* 2000 = 18.625

#### Plotting the Regression Line

``` r
plot(points, salary_million, xlab = "Points", ylab = "Salary (in millions)", 
     main = paste("Scatterplot of Points and Salary"), col = "grey", pch = 1, cex = 0.8)
abline(1.509, 0.0088558, lwd = 2, col = "purple")
lines(lowess(points, salary_million, f = 2/3), col = "red", lwd = 2)
text(2400,20,labels = "regression", col = "purple")
text(2000,25,labels = "lowess", col = "red")
```

<img src="hw01-Zihao-Li_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

#### Regression Residuals and Coefficient of Determination

``` r
residual <- salary_million - (1.509 + 0.008558*points)
summary(residual)
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -14.19000  -2.79200  -1.09500  -0.00016   2.55600  18.81000

``` r
RSS <- sum(residual^2)
TSS <- sum((salary_million - y_mean)^2)
R2 <-  1 - RSS/TSS
R2
```

    ## [1] 0.4054246

#### Exploring Position and Experience

``` r
plot(experience_int, salary_million, xlab = "Years of Experience", 
     ylab = "Salary (in millions)", main = paste("Scatterplot with Lowess Smooth"),
     col = "grey", pch = 1, cex = 0.8)
lines(lowess(experience_int, salary_million, f = 2/3), col = "red", lwd = 2)
```

<img src="hw01-Zihao-Li_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

``` r
library("scatterplot3d")
scatterplot3d(points, experience_int, salary_million, xlab = "Points", 
              ylab = "Experience", zlab = "Salary (in millions)", color = "red")
```

<img src="hw01-Zihao-Li_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-2.png" style="display: block; margin: auto;" />

``` r
boxplot(salary_million ~ position_fac, xlab = "Position", ylab = "Salary (in millions)", main = paste("Boxplot for Salary and Position"), par(mar=c(6,4,4,4)))
```

<img src="hw01-Zihao-Li_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-3.png" style="display: block; margin: auto;" />

-   Brief descriptions for the plots:
    -   The first plot shows the relationship between salary and experience. It is evident that salary increases as experience increases in the first 7-8 years, as the player gets more mature and skilled. However, salary decreases afterwards, even though experience continues to increase. This is because as the play gets older, he is not as strong and flexible as he used to be, and thus his salary decreases accordingly. The standard deviation of salaries as experience gets larger also increases significantly.
    -   The second 3D plot shows how salary is a function of both points and experience. In general salary increases as points and experience increase, but points play a bigger role than experience.
    -   The boxplot compares salary of different positions. The mean salary of each position is approximately the same, but standard deviation varies a lot. More specifically, there is not outlier for "Center", but there are outliers for other positions. The third quantile of "Center" is also higher than other positions.
-   Experience does seem to be related to salary. It shows a positive relation when experience is less than 7 or 8 years, but shows a negative relation after that. However, this relation is not as strong as that between salary and points.

-   Salary does seem to be related to position. Even though the mean salary for each position is similar, the standard deviation varies. The third quantile salary for "Center" players is higher, which means that there is a higher proportion of "Center" players who earn more than the mean.

#### Reflections

-   Vector subsetting is still quite hard. I haven't seen some functions such as replace(), so it took some time to learn it.
-   Plotting is relatively easy. The parameters to specify can be figured out after a couple of trials.
-   This is the first time I use git. I find it very convenient, since it's easy to push a local file to github and save it there.
-   I like github. It's very convenient to keep projects and data, and it carries the idea of open source.
-   I didn't really get help from anyone, and so it cost me some time to figure some parts out.
-   It took me around two hours to complete the homework.
-   The most time consuming part is calculating the correlation, since there are a lot of things to calculate in the process.
-   Even though plotting is easy(ish), there are still some parameters that I do not understand. I hope we could go over plotting a little bit in class.
-   The y-axis label of the boxplot didn't show up at first, and it took me a while to fix that problem.
-   I'm somehow proud of making the plot, since it's quite fun to change the parameters and experiment with those.
