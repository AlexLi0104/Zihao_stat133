---
title: "Principal Component Analysis: Theory, Visualization, and Application"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

### 1. Introduction

Principal Component Analysis (PCA), a statistical method based on orthogonal transformation and singular value decomposition, is a powerful tool to reduce the dimension of the data while capturing the most of its variabilities. We have learned some general mathematical ideas behind PCA decomposition so far in class, and we also did a PCA ranking of the NBA teams in one of our homework assignments. 

The purpose of this post is to explain in greater detail the theory behind PCA decomposition, as well as to present better ways to visualize the results, including screeplots and biplots. This post also gives some examples of what can be done after PCA decomposition. 

<br/>

### 2. Mathematical Theory behind PCA Decomposition

The general idea behind PCA is simple. It is equivalent to a change of basis problem in linear algebra, where we select a new coordinate system (a new set of basis vectors) such that the greatest variability of the original data is projected onto the first coordinate axis (PC1), the second greatest variability onto the second coordinate axis (PC2), and so on. We therefore want a new coordinate system that better represents our data. 

Mathematically speaking, suppose that we have a matrix $X$ with $m$ objects and $n$ variables, and we have a linear transformation $T$ that acts on $M_{m \times n (F)}$ for some field F. Define $Y = TX$ in the standard basis $e$. Then

\begin{equation}
  y_i = \sum_i a_iT(x_i)
\end{equation}

for some coefficients $a_i$, where $y_i$ represents the $i^{th}$ column of $Y$. In other words, each column of $Y$ is a linear combination of the columns of $X$ for some coefficients. 

<br/>

Next, we want to choose an appropriate $X$. Since we are most interested in capturing the variabilities of our data, a natural choice would be to use a covariance matrix. In order to convert $x$ into a square matrix, we define 

\begin{equation}
  S_X = \dfrac{1}{n-1} XX^T
\end{equation}

where each entry of $S_X$ corresponds to the correlation between an object and a variable. 

</br>

Finally, we want to choose a new basis for $T$ that best represents $S_X$. We do so simply by diagonalizing $S_X$. The eigenvectors that we get after diagonalization will constitute the new basis $\beta$ such that $[S_X]_\beta$ is a diagonal matrix. In other words, Vectors in $\beta$ are the principal components that we seek. The corresponding eigenvalues are the contributions of each variable to the principal components (the weights $a_i$ in the first equation).

</br>

### 3. Visualization of PCA Results

we have seen in class that it is easy to generate a histogram based on the rank of PC1. However, that doesn't tell us much information since it only includes one principal component. I present some other ways to visualize PCA results in this post.

Let's use the NBA team example in homework-3:
```{r echo = F}
ggbiplot <- function(pcobj, choices = 1:2, scale = 1, pc.biplot = TRUE, 
                      obs.scale = 1 - scale, var.scale = scale, 
                      groups = NULL, ellipse = FALSE, ellipse.prob = 0.68, 
                      labels = NULL, labels.size = 3, alpha = 1, 
                      var.axes = TRUE, 
                      circle = FALSE, circle.prob = 0.69, 
                      varname.size = 3, varname.adjust = 1.5, 
                      varname.abbrev = FALSE, ...)
{
  library(ggplot2)
  library(plyr)
  library(scales)
  library(grid)

  stopifnot(length(choices) == 2)

  # Recover the SVD
 if(inherits(pcobj, 'prcomp')){
    nobs.factor <- sqrt(nrow(pcobj$x) - 1)
    d <- pcobj$sdev
    u <- sweep(pcobj$x, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- pcobj$rotation
  } else if(inherits(pcobj, 'princomp')) {
    nobs.factor <- sqrt(pcobj$n.obs)
    d <- pcobj$sdev
    u <- sweep(pcobj$scores, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- pcobj$loadings
  } else if(inherits(pcobj, 'PCA')) {
    nobs.factor <- sqrt(nrow(pcobj$call$X))
    d <- unlist(sqrt(pcobj$eig)[1])
    u <- sweep(pcobj$ind$coord, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- sweep(pcobj$var$coord,2,sqrt(pcobj$eig[1:ncol(pcobj$var$coord),1]),FUN="/")
  } else if(inherits(pcobj, "lda")) {
      nobs.factor <- sqrt(pcobj$N)
      d <- pcobj$svd
      u <- predict(pcobj)$x/nobs.factor
      v <- pcobj$scaling
      d.total <- sum(d^2)
  } else {
    stop('Expected a object of class prcomp, princomp, PCA, or lda')
  }

  # Scores
  choices <- pmin(choices, ncol(u))
  df.u <- as.data.frame(sweep(u[,choices], 2, d[choices]^obs.scale, FUN='*'))

  # Directions
  v <- sweep(v, 2, d^var.scale, FUN='*')
  df.v <- as.data.frame(v[, choices])

  names(df.u) <- c('xvar', 'yvar')
  names(df.v) <- names(df.u)

  if(pc.biplot) {
    df.u <- df.u * nobs.factor
  }

  # Scale the radius of the correlation circle so that it corresponds to 
  # a data ellipse for the standardized PC scores
  r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4)

  # Scale directions
  v.scale <- rowSums(v^2)
  df.v <- r * df.v / sqrt(max(v.scale))

  # Change the labels for the axes
  if(obs.scale == 0) {
    u.axis.labs <- paste('Standardized PC', choices, sep='')
  } else {
    u.axis.labs <- paste('PC', choices, sep='')
  }

  # Append the proportion of explained variance to the axis labels
  u.axis.labs <- paste(u.axis.labs, 
                       sprintf('(%0.1f%% explained var.)', 
                               100 * pcobj$sdev[choices]^2/sum(pcobj$sdev^2)))

  # Score Labels
  if(!is.null(labels)) {
    df.u$labels <- labels
  }

  # Grouping variable
  if(!is.null(groups)) {
    df.u$groups <- groups
  }

  # Variable Names
  if(varname.abbrev) {
    df.v$varname <- abbreviate(rownames(v))
  } else {
    df.v$varname <- rownames(v)
  }

  # Variables for text label placement
  df.v$angle <- with(df.v, (180/pi) * atan(yvar / xvar))
  df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar)) / 2)

  # Base plot
  g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + 
          xlab(u.axis.labs[1]) + ylab(u.axis.labs[2]) + coord_equal() + ggtitle("Biplot of NBA Teams")

  if(var.axes) {
    # Draw circle
    if(circle) 
    {
      theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
      circle <- data.frame(xvar = r * cos(theta), yvar = r * sin(theta))
      g <- g + geom_path(data = circle, color = muted('white'), 
                         size = 1/2, alpha = 1/3)
    }

    # Draw directions
    g <- g +
      geom_segment(data = df.v,
                   aes(x = 0, y = 0, xend = xvar, yend = yvar),
                   arrow = arrow(length = unit(1/2, 'picas')), 
                   color = muted('red'))
  }

  # Draw either labels or points
  if(!is.null(df.u$labels)) {
    if(!is.null(df.u$groups)) {
      g <- g + geom_text(aes(label = labels, color = groups), 
                         size = labels.size)
    } else {
      g <- g + geom_text(aes(label = labels), size = labels.size)      
    }
  } else {
    if(!is.null(df.u$groups)) {
      g <- g + geom_point(aes(color = groups), alpha = alpha)
    } else {
      g <- g + geom_point(alpha = alpha)      
    }
  }

  # Overlay a concentration ellipse if there are groups
  if(!is.null(df.u$groups) && ellipse) {
    theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
    circle <- cbind(cos(theta), sin(theta))

    ell <- ddply(df.u, 'groups', function(x) {
      if(nrow(x) <= 2) {
        return(NULL)
      }
      sigma <- var(cbind(x$xvar, x$yvar))
      mu <- c(mean(x$xvar), mean(x$yvar))
      ed <- sqrt(qchisq(ellipse.prob, df = 2))
      data.frame(sweep(circle %*% chol(sigma) * ed, 2, mu, FUN = '+'), 
                 groups = x$groups[1])
    })
    names(ell)[1:2] <- c('xvar', 'yvar')
    g <- g + geom_path(data = ell, aes(color = groups, group = groups))
  }

  # Label the variable axes
  if(var.axes) {
    g <- g + 
    geom_text(data = df.v, 
              aes(label = varname, x = xvar, y = yvar, 
                  angle = angle, hjust = hjust), 
              color = 'darkred', size = varname.size)
  }
  # Change the name of the legend for groups
  # if(!is.null(groups)) {
  #   g <- g + scale_color_brewer(name = deparse(substitute(groups)), 
  #                               palette = 'Dark2')
  # }

  # TODO: Add a second set of axes

  return(g)
}
```

```{r message = F}
library(readr)
library(dplyr)
library(ggplot2)
teams <- read.csv(file = "data/nba2017-teams.csv")  #load data file of teams
```

<br/>

#### Screeplot

The first thing that we would like to know is how much variations each principal component explains. As a rule of thumb, we want to consider all principal components up to which 90% of variations are captured. The easiest way to do this is by making a screeplot:

```{r}
# Creates a function screeplot() whose input is a prcomp() result
# This function is taken from
# <http://rstudio-pubs-static.s3.amazonaws.com/27823_dbc155ba66444eae9eb0a6bacb36824f.html> 
# with some modifications

screeplot <- function(x) {
    
    # calculate the variation based on standard deviation
    x.var <- x$sdev ^ 2
    
    # the proportion of variation each PC explains
    x.pvar <- x.var/sum(x.var)
    
    print("proportions of variance:")
    print(x.pvar)
    
    # creates a matrix that combines the plots 
    par(mfrow = c(1,2))   
    
    # plot the proportion of variation each PC explains
    plot(x.pvar, xlab = "Principal component", ylab = "Proportion of variation", 
         main = "Screeplot of NBA Teams", ylim = c(0,1), type = 'b', las = 1)
    
    # plot the cumulative variation 
    plot(cumsum(x.pvar), xlab = "Principal component", ylab = "Cumulative Proportion of variation", 
         ylim = c(0,1), type = 'b', las = 1)
}

# use prcomp() to perform the PCA analysis
teams_select <- teams %>% 
  select(points3, points2, free_throws, off_rebounds, def_rebounds, 
         assists, steals, blocks, turnovers, fouls)
pca_team <- prcomp(teams_select, scale. = T)

screeplot(pca_team)
```


Based on these two plots, it is clear that the proportion of variations explained decreases significantly at PC2. The cumulative variation reaches the 90% mark at PC6. This is generally to be avoided because we need to take into account of six PC's. In most cases, we want at most three PC's to explain over 90% of the variations. 

<br/>

#### Biplot

In addition to making a screetplot, we would also like to visualize how the objects and variables are related. A good way to do this is by making a biplot, which plots the position of objects relative to the PC's, as well as the correlations of variables. The function that does this is called "ggbiplot", and is shown in <https://github.com/vqv/ggbiplot/blob/master/R/ggbiplot.r>. Since this function is quite long, I've decided to hide this code chunk in my final knitted document. If you are interested, you are welcomed to go on this website and see how the function works. 

```{r message = F}
ggbiplot(pca_team, labels = teams$team, circle = T)
```

One way to think about such a biplot is to treat each arrow (variable) as a vector. Then each object (team) is represented as the linear combination of those vectors. Since the variables are all standardized, the origin represents the average of all statistics. For instance, Houston Rockets (HOU) has the lowest y-value, which means that it makes a lot of three-point shots each game. Golden State Warriors occupies the south-east cornor, which means that it makes many assists. In this way, we can easily identify the relationship between each object and all the variables.

Moreover, the rank of PC1 that we see in class is simply the projection of this biplot onto the x-axis. The circle in the graph, along with the length of the arrows, shows how well a variable is explained. For example, the tip of the points3 arrow is very close to the circle, which means that it is well-explained by the first two PCs. On the contrary, variables such as blocks and free throws are not quite well-explained. 

<br/>

#### Cluster plotting

To better appreciate the power of biplots, we can group the teams by some criteria and plot them in clusters using different colors. For instance, I can group the teams by total salary and see how that affects the distribution. Just for the sake of illustration, I arbitrarily group salary into 3 groups: less than 80 million, greater than 80 but less than 95 million, and greater than 95 million. 

```{r message = F}
# replicates the data frame
teams_salary = teams  

# loops over all 30 teams
for (i in 1:30) { 
  if (teams_salary[i,4] > 95) {   # salary is at the 4th column
    teams_salary[i,4] = "1"   # change salary > 80 to group 1
  }
  else if (teams_salary[i,4] < 95 & teams_salary[i,4] > 80) {
    teams_salary[i,4] = "2"   # cahnge 60 < salary < 80 to group 2
  }
  else if (teams_salary[i,4] < 80) {
    teams_salary[i,4] = "3"   # change salary < 60 to group 3
  }
}
```

<br/>

Next, we would like to generate a biplot of the teams grouped by salary. 

```{r}
# The parameter ellipse generates ellipses based on groups
# The parameter var.axes determines whether include arrows for variables.

ggbiplot(pca_team, groups = teams_salary$salary, ellipse = T, var.axes = F)

```

This biplot may seem a bit weird since the ellipses overlap extensively with each other. This means that salary does not really impact the distribution of teams based on the variables we've chosen. 

However, if the data set is large enough, it is possible that we get ellipses with little or no overlap. This means that each group has distinct properties and that we can analyze the objects based on which groups they are in. 

<br/>

#### 3D plotting

The above plotting techniques can also be extended to three dimensions, where we take into account three PCs instead of two. This is needed when the first two PC's do not explain most of the variations, as in our case. To do this, we can simply use the scatter3D() function in the "plot3D" package:

```{r}
library(plot3D)
scatter3D(x = pca_team$x[,1], y = pca_team$x[,2], z = pca_team$x[,3], colvar = NULL, col = "blue", 
  pch = 19, xlab = "PC1", ylab = "PC2", zlab = "PC3", main = "3D Plot of First three PC's")
```

We can further modify the plot so that it looks more informative. 
```{r}
# The parameters phi and theta determines the angle from which you view the plot
# type= "h"" generates a projection of each point onto the xy-plane
# bty = "b2" makes grid lines on each plane.

scatter3D(x = pca_team$x[,1], y = pca_team$x[,2], z = pca_team$x[,3], colvar = NULL, col = "blue", 
  pch = 19, xlab = "PC1", ylab = "PC2", zlab = "PC3", main = "3D Plot of First three PC's", phi = 20, bty = "b2", theta = 25, type = "h")
```

<br/>

### 4. Applications

Here I would like to briefly talk about some potential applications of PCA in other disciplines. As I've mentioned in the first section, the primary goal of PCA is to reduce the dimension of data, and discard those dimensions that do not contribute to the variations. This is most useful when the dimension of data is extremely large. 

For instance, in the field of biological sciences, suppose that we are interested in some property of cells. Most likely there are many variables that might influence that property: temperature, moisture, nutrition, density, etc. Sometimes it is very hard to control for all those variables, and sometimes we might be interested in the cross-effects of them. In these cases, PCA decomposition can be handy because it tells us what variables have significant influences and what variables don't. In addition, we might want to group the cells based on their types, plot the results, and see if the groups differ significantly. Hence PCA can provide helpful insight in this scenario.

PCA decomposition can also be built into physical models. One article published in Nature discusses the possibility of quantum PCA. The spectural measurements can be decomposed in a similar manner, and the results provide useful insight to the quantum states. However, this topic is much beyond my knowledge, and I mention it to show that PCA can be used in many advanced settings.

<br/>

### 5. Take Home Message

PCA is a technique to reduce the dimension of data while capturing most of its variations. The results can be easily visualized and can be applied to many advanced fields of science.

<br/>

### 6. References

<https://github.com/vqv/ggbiplot/blob/master/R/ggbiplot.r>

<http://rstudio-pubs-static.s3.amazonaws.com/27823_dbc155ba66444eae9eb0a6bacb36824f.html>

<https://www.cs.princeton.edu/picasso/mats/PCA-Tutorial-Intuition_jp.pdf>

<https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html>

<http://www.sthda.com/english/wiki/ggfortify-extension-to-ggplot2-to-handle-some-popular-packages-r-software-and-data-visualization#plotting-cluster-package>

<https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3298499/>

<http://www.nature.com/nphys/journal/v10/n9/full/nphys3029.html>