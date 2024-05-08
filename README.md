---
title: "IMDb Movie Rating Predictions"
author: "Full EDA & Multiple Linear Regression"
output:
  prettydoc::html_pretty:
    theme: hpstr
    css: imdb_mlr_style.css
    toc: true
---

# Introduction

This is a Multiple Linear Regression Analysis with the R programming language that attempts to create a linear regression model to predict [IMDb](https://www.imdb.com/) Ratings for movies using data from [Rotten Tomatoes](https://www.rottentomatoes.com/) and [The Academy for Motion Picture Arts & Sciences](https://www.oscars.org/). 


### Load packages

```{r load-packages, message = FALSE}
library(statsr)
library(tidyverse)
library(GGally)
library(ggcorrplot)
```


### Load data

We are using a dataset from 2016 with 651 rows. Its codebook can be [viewed here.](moviedata_codebook.html)


```{r load-data, message=F, error=F}
movies <- read_csv("moviedata.csv")
```

# Research Question

Can we predict IMDb Movie Ratings using Rotten Tomatoes Ratings, movie genre, runtime, MPAA rating, theater debut year, Oscar awards/nominations, whether the film appear in BoxOfficeMojo's Top 200 list?


* * *


# Exploratory Data Analysis

We begin by verifying the information from our codebook by viewing the structure of the data then separating the variables by type.

```{r}
str(movies)
```




**Character (no categories)**:
`title`
`director`
`actor1`
`actor2`
`actor3`
`actor4`
`actor5`
`imdb_url`
`rt_url`



**Categorical (factor)**:
`title_type` (nominal)
`genre` (nominal)
`mpaa_rating` (nominal)
`studio` (nominal)
`thtr_rel_year` (nominal)
`thtr_rel_month` (nominal)
`thtr_rel_day` (nominal)
`dvd_rel_year` (nominal)
`dvd_rel_month` (nominal)
`dvd_rel_day` (nominal)
`critics_rating` (ordinal)
`audience_rating` (ordinal)



**Binary**:
`best_pic_nom`
`best_pic_win`
`best_actor_win`
`best_actress `
`best_dir_win`
`top200_box`


**Numeric**:
`runtime`
`imdb_rating`
`imdb_num_votes`
`critics_score`
`audience_score`


*****


There are a number of changes we need to make to the data types to not only match the codebook, but to make the data more applicable for the model to be created.

### Character (no categories) Variables

We start by addressing the character variables that represent no categories. We know that `title`, `imdb_url`, and `rt_url` should be unique to every row so we can get rid of them first.


Next, if we want any chance at including `director` or `actor` into our model, we need to see how many unique values there are first.

```{r}
# Select all actor columns, unlist df to vector, count distinct actors
movies %>% select(actor1, actor2, actor3, actor4, actor5) %>% unlist() %>% n_distinct()
```
With 2,363 different actors in the data, we aren't even going to attempt to classify actors, and choose to exclude them.


```{r}
# Number of distinct directors
n_distinct(movies$director)
```
And with 533 different directors in the data we are going to exclude this variable.

For the initial changes, we create a new data frame to leave the original dataset unchanged.

```{r}
# New df without actors, directors, & URLs
movies_df <- select(movies, -actor1, -actor2, -actor3, -actor4, -actor5, -director, -imdb_url, -rt_url)
```


******


### Categorical Variables

#### Nominal

The first variable of this type to examine is `studio` to see if the number of unique studios will succumb it to the same fate of our Character Variables above, or if we can reduce the number of factor dimensions to something more appropriate for the model.


```{r}
n_distinct(movies_df$studio)
```

With 212 unique studios in the data we have the choice of leaving `studio` as a string that we can not include in the model, or we can attempt to bin the studios together to reduce the dimensionality that will be added to the model from incorporating it as a nominal factor. 

Let's look at the top 30.

```{r}
# Top 30 studios
movies_df %>% count(studio) %>% arrange(desc(n)) %>% .[1:30,]
```



First we'll try to identify subsidiaries to all of the `studio` parent organizations. 

```{r}
# Find string pattern in studio then show distinct
movies_df %>% 
  filter(grepl("paramount", studio, ignore.case = TRUE)) %>% distinct(studio) 
```

```{r}
movies_df %>% 
  filter(grepl("universal", studio, ignore.case = TRUE)) %>% distinct(studio) 
```

```{r}
movies_df %>% 
  filter(grepl("warner", studio, ignore.case = TRUE)) %>% distinct(studio) 
```

```{r}
movies_df %>% 
  filter(grepl("sony", studio, ignore.case = TRUE)) %>% distinct(studio) 
```

```{r}
movies_df %>% 
  filter(grepl("fox", studio, ignore.case = TRUE)) %>% distinct(studio) 
```

```{r}
movies_df %>% 
  filter(grepl("mgm", studio, ignore.case = TRUE)) %>% distinct(studio) 
```

```{r}
movies_df %>% 
  filter(grepl("miramax", studio, ignore.case = TRUE)) %>% distinct(studio) 
```

```{r}
movies_df %>% 
  filter(grepl("disney|buena vista", studio, ignore.case = TRUE)) %>% distinct(studio) 
```


Now we will group these together with their respective parent company.

```{r}
# Find patterns for studio values and replace with parent studio
movies_df <- movies_df %>% 
  mutate(studio = case_when(grepl("Paramount", studio, ignore.case = TRUE) ~ "Paramount",
                            grepl("Universal", studio, ignore.case = TRUE) ~ "Universal",
                            grepl("Fox", studio, ignore.case = TRUE) ~ "Fox",
                            grepl("Warner", studio, ignore.case = TRUE) ~ "Warner",
                            grepl("Sony", studio, ignore.case = TRUE) ~ "Sony",
                            grepl("MGM", studio, ignore.case = TRUE) ~ "MGM",
                            grepl("Miramax", studio, ignore.case = TRUE) ~ "Miramax",
                            grepl("Disney|Buena Vista", studio, ignore.case = TRUE) ~ "Disney",
                            TRUE ~ studio)
  )

movies_df %>% count(studio) %>% arrange(desc(n)) %>% .[1:30,]
```


Now, with these studios binned together with their parent organizations, we can address the 8 `NA` values for `studio.`

```{r}
# NA studio values
movies_df %>% filter(is.na(studio))
```

With a quick internet search for the 8 titles, we've replaced the `NA` values.

```{r}
# Insert studio values found on web search
movies_df <- mutate(movies_df, 
                    studio = case_when(title == "Dirty Sanchez: The Movie" ~ "MTV",
                                       title == "Caveman" ~ "Turman-Foster Company",
                                       title == "Attack of the 50 Foot Woman" ~ "Woolner Brothers",
                                       title == "Oliver & Company" ~ "Disney",
                                       title == "The Man Who Sued God" ~ "Disney",
                                       title == "Inserts" ~ "United Artists",
                                       title == "Inbred" ~ "New Flesh Films",
                                       title == "Death Line (Raw Meat)" ~ "American International Pictures",
                                       TRUE ~ studio
                                       )
                    )
```

One last look at the top 20.

```{r}
# Top 20 studios
movies_df %>% count(studio) %>% arrange(desc(n)) %>% .[1:20,]
```

We will put the top 8 into a vector named `major_studios` and then change all other `studio` values to "Other". 

```{r}
# Put top 8 studios into a character vector
major_studios <- movies_df %>% count(studio) %>% arrange(desc(n)) %>% .[1:8,1] %>% pull()

# Change studio value to "Other" if not in the top 8 studios
movies_df <- mutate(movies_df, studio = if_else(grepl(paste0(major_studios, collapse="|"), 
                                                      studio, ignore.case = TRUE), studio, "Other"))
```

We can consider the `studio` variable ready to change to a nominal factor with 9 levels into the model.

```{r}
movies_df <- movies_df %>% mutate(studio = as.factor(studio))
```


***

The next categorical variables we want to assess for categorical data typing is `thtr_rel_year` and `dvd_rel_year.` This is a question of whether there is a linear relationship between `thtr_rel_year`/`dvd_rel_year` and `imdb_rating` (the response variable). If we find a relationship over many years that `imdb_rating` increases/decreases linearly, then we would change our data type to numeric. On the other hand if we find that not to be the case (which we suspect it won't), then these yearly variables could be considered for nominal categories by each year, or levels with multiple year spans. 

First, we check for any `NA` values present in these 6 time variables. To get more clarity on the rows with `NA` values, we add title and studio from the original data.

```{r}
movies %>% select(title, studio, thtr_rel_year, thtr_rel_month, thtr_rel_day, 
                     dvd_rel_year, dvd_rel_month, dvd_rel_day) %>% 
  filter(rowSums(is.na(select(., -studio))) > 0)
```

These 8 titles appear to be TV productions or distributed by Home Video studios. We are going to use our discretion to copy the time values from the `thtr_rel_` variables to the `dvd_rel_` variables for these 8 specific titles.

```{r}
# The titles with DVD release times to be updated
titles <- c("Charlie: The Life and Art of Charles Chaplin", 
            "Streets of Gold",
            "The Squeeze",
            "Electric Dreams",
            "Porky's Revenge",
            "Teen Wolf Too",
            "The Last Remake of Beau Geste",
            "Let It Be")

# Replace dvd_rel values with thtr_rel values for titles in titles vector
movies_df <- movies_df %>%
  mutate(dvd_rel_year = if_else(title %in% titles, thtr_rel_year, dvd_rel_year),
         dvd_rel_month = if_else(title %in% titles, thtr_rel_month, dvd_rel_month),
         dvd_rel_day = if_else(title %in% titles, thtr_rel_day, dvd_rel_day))
```
  
Verify that there are no more NA values in these columns.

```{r}
# Select 8 time columns and filter for any rows with an NA
movies_df %>% select(thtr_rel_year, thtr_rel_month, thtr_rel_day, 
                     dvd_rel_year, dvd_rel_month, dvd_rel_day) %>% 
  filter(rowSums(is.na(.)) > 0)
```

Assessing linear associations for time variables.

```{r, warning=F, message=F}
movies_df %>% ggplot(aes(x = thtr_rel_year, y = imdb_rating)) +
  geom_point() + 
  geom_smooth(method = "lm", color = "red") +
  theme_bw()
```

```{r, warning=F, message=F}
movies_df %>% ggplot(aes(x = dvd_rel_year, y = imdb_rating)) +
  geom_point() + 
  geom_smooth(method = "lm", color = "red") +
  theme_bw()
```

We see no linear relationship from year values and the response variable, therefore if we wish to leave these variables in the model, they must be typed to nominal factors. Note that ordinal factors should be typed to numeric variables, not factors. We also see higher variations in lower scores indicating that our response variable is skewed to the left. 

We will now perform the same test for months and days.

`imdb_rating ~ thtr_rel_month`

```{r, warning=F, message=F}
movies_df %>% ggplot(aes(x = thtr_rel_month, y = imdb_rating)) +
  geom_point() + 
  geom_smooth(method = "lm", color = "red") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  theme_bw()
```

`imdb_rating ~ dvd_rel_month`

```{r, warning=F, message=F}
movies_df %>% ggplot(aes(x = dvd_rel_month, y = imdb_rating)) +
  geom_point() + 
  geom_smooth(method = "lm", color = "red") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  theme_bw()
```

`imdb_rating ~ thtr_rel_day`

```{r, warning=F, message=F}
movies_df %>% ggplot(aes(x = thtr_rel_day, y = imdb_rating)) +
  geom_point() + 
  geom_smooth(method = "lm", color = "red") +
  scale_x_continuous(breaks = 1:31) +
  theme_bw()
```

`imdb_rating ~ dvd_rel_day`

```{r, warning=F, message=F}
movies_df %>% ggplot(aes(x = dvd_rel_day, y = imdb_rating)) +
  geom_point() + 
  geom_smooth(method = "lm", color = "red") +
  scale_x_continuous(breaks = 1:31) +
  theme_bw()
```


We conclude that all 6 of these time variables must be nominal factors and not numeric variables, if we wish to include them in the model.


We wish to convert the yearly variables into decade categories to reduce dimensionality.

```{r}
movies_df <- movies_df %>%
  mutate(thtr_rel_decade = paste0((thtr_rel_year %/% 10) * 10, "'s"),
         .before = thtr_rel_year) %>%
  mutate(dvd_rel_decade = paste0((dvd_rel_year %/% 10) * 10, "'s"),
         .before = dvd_rel_year) %>% 
  select(-thtr_rel_year, -dvd_rel_year)

```

Now we'll convert all 6 to factors.

```{r}
# Mutate variables with as.factor
movies_df <- movies_df %>% mutate(thtr_rel_decade = as.factor(thtr_rel_decade),
                                  thtr_rel_month = as.factor(thtr_rel_month),
                                  thtr_rel_day = as.factor(thtr_rel_day),
                                  dvd_rel_decade = as.factor(dvd_rel_decade),
                                  dvd_rel_month = as.factor(dvd_rel_month),
                                  dvd_rel_day = as.factor(dvd_rel_day)
                                  )
```



* * * * *



The next group of variables: `title_type`, `genre`, and `mpaa_rating` we are going to type as nominal categories by using a summary table. 

This custom function, `unique_with_counts()` will return a summary table that shows us the counts for each distinct value, including `NA` values. 
```{r unique_with_counts function}
unique_with_counts <- function(data) {
  # Getting the unique values and counts for each column
  list_columns <- lapply(data, function(col) {
    tbl <- table(col, useNA = "ifany")
    tibble(value = as.vector(names(tbl)), count = as.vector(tbl))
  })
  
  # Getting the maximum number of unique values (including NA) in any column
  max_length <- max(sapply(list_columns, nrow))
  
  # Making all tibbles the same length (with NA for missing)
  list_columns <- lapply(list_columns, function(tbl) {
    add_rows <- max_length - nrow(tbl)
    if (add_rows > 0) {
      tbl <- rbind(tbl, tibble(value = rep(NA, add_rows), count = rep(NA, add_rows)))
    }
    arrange(tbl, desc(tbl[,2]))
  })
  
  # Binding all the tibbles column-wise
  output <- do.call(cbind, list_columns)
  
  # Creating new column names
  new_col_names <- c(rbind(paste0(names(data), "_value"), paste0(names(data), "_count")))
  colnames(output) <- as.vector(new_col_names)
  
  # Returning the final data frame
  return(output)
}
```

```{r}
movies_df %>% select(title_type, genre, mpaa_rating) %>% unique_with_counts()

```

We can see there are no `NA` values present here. `title_type` looks good, `genre` doesn't appear to have any similar or repeating levels, and `mpaa_rating` has 2 "NC-17" observations which we will change to "R". 

```{r}
# Change "NC-17" values to "R"
movies_df <- movies_df %>% mutate(mpaa_rating = if_else(mpaa_rating == "NC-17", "R", mpaa_rating))


# Mutate variables with as.factor
movies_df <- movies_df %>% mutate(title_type = as.factor(title_type),
                                  genre = as.factor(genre),
                                  mpaa_rating = as.factor(mpaa_rating)
                                  )
```


* * * *

#### Ordinal Categories

Our final 2 categorical variables represent ordinal categories.

Let's view their summary.

```{r}
movies_df %>% select(critics_rating, audience_rating) %>% unique_with_counts()
```

These look ready to convert with ordinary/integer encoding.




Let's look at how `critics_rating` is derived.

| Icon        | Score         | Description                                                                                                                                                                           |
|-------------|---------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| ![Certified Fresh](https://upload.wikimedia.org/wikipedia/en/thumb/b/b2/Certified_Fresh_2018.svg/30px-Certified_Fresh_2018.svg.png) | 100-75%   | **Certified Fresh**: Wide-release films with a score of 75% or higher that are reviewed by at least 80 critics, of whom 5 are "Top Critics", are given this seal. The "Certified Fresh" seal remains until the score drops below 70%. Films with limited releases require only 40 reviews (including 5 from "Top Critics") to qualify for this seal. For TV shows, only individual seasons are eligible for consideration, and each must have at least 20 critic reviews. |
| ![Fresh](https://upload.wikimedia.org/wikipedia/commons/thumb/5/5b/Rotten_Tomatoes.svg/30px-Rotten_Tomatoes.svg.png)               | 100-60%   | **Fresh**: Films or TV shows with a score of 60% or higher that do not meet the requirements for the "Certified Fresh" seal.                                                            |
| ![Rotten](https://upload.wikimedia.org/wikipedia/commons/thumb/5/52/Rotten_Tomatoes_rotten.svg/30px-Rotten_Tomatoes_rotten.svg.png) | 59-0%     | **Rotten**: Films or TV shows with a score of 59% or lower receive this seal.                                                                                                          |



Since the 3 levels of `critics_rating` are not equidistant and are weighted by types of critics it would not be appropriate to convert these to numerical values with ordinary/integer encoding. Instead we want to leave them as a ordinal categorical factors in order for them each to get their own reference level in the model. Since I was not able to find information on how `audience_rating` was derived (other than it being calculated from `audience_score`) it too will be converted to an ordinal factor.

```{r}
movies_df <- movies_df %>% 
  mutate(critics_rating = factor(critics_rating, levels = c("Rotten", "Fresh", "Certified Fresh")),
         audience_rating = as.factor(audience_rating)
         )
```



* * * * * * * *

## Binary Variables

We use our summary table to show the 6 "yes/no" value variables.

```{r}
select(movies_df, best_pic_nom, best_pic_win, best_actor_win, best_actress_win, best_dir_win, top200_box) %>%
  unique_with_counts()
```

We wish to convert from categories to numeric values (0,1) with ordinary/integer encoding.
```{r}
movies_df <- movies_df %>% 
  mutate(best_pic_nom = if_else(best_pic_nom == "yes", 1, 0),
         best_pic_win = if_else(best_pic_win == "yes", 1, 0),
         best_actor_win = if_else(best_actor_win == "yes", 1, 0),
         best_actress_win = if_else(best_actress_win == "yes", 1, 0),
         best_dir_win = if_else(best_dir_win == "yes", 1, 0),
         top200_box = if_else(top200_box == "yes", 1, 0)
         )
```


* * * * * * *


## Numeric Variables

Since our summary table is not practical for numeric variables, we will filter for `NA` values manually.

```{r}
# Filter any rows with an NA numeric column
movies_df %>% select(title, runtime, imdb_rating, imdb_num_votes, critics_score, audience_score) %>% 
  filter(rowSums(is.na(.)) > 0)
```

The runtime of 74 minutes for the movie, "The End of America", can be found [here](https://www.rottentomatoes.com/m/end_of_america).

```{r}
movies_df <- movies_df %>% mutate(runtime = if_else(is.na(runtime), 74, runtime))
```

We will view the distributoin of each numeric variable with a historgram.

`runtime`
`imdb_rating`
`imdb_num_votes`
`critics_score`
`audience_score`

```{r}
ggplot(movies_df, aes(x = runtime)) +
  geom_histogram(binwidth = 1, 
                 color = "red", alpha = 0.7) +
  labs(title = paste("Histogram of runtime")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 
```

Let's look closer at the outliers on both sides.

```{r}
# runtime outliers in original data to retrieve rt_url again
movies %>% select(title, runtime, rt_url) %>% filter(runtime < 60 | runtime > 180)
```

All of these `runtime` values are accurate.

```{r}
ggplot(movies_df, aes(x = imdb_rating)) +
  geom_histogram(binwidth = 0.1, fill = "red", color = "black", alpha = 0.7, 
                 bins=length(unique(movies_df$imdb_rating))) +
  labs(title = "Histogram of imdb_rating") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
```


Let's verify the outliers again.

```{r}
# imdb_rating outliers in original data to retrieve imdb_url again
movies %>% filter(imdb_rating < 4 | imdb_rating > 8.8) %>% select(title, imdb_rating, imdb_url) %>% 
  arrange(imdb_rating)
```

While not exact, due to the fluidity of this variable in real time and when this data was collected, but they are all in very comparable ranges. 

We wish to remove the ratings lower than 4.0 from the data to lower the skew.

```{r}
movies_df <- movies_df %>% filter(imdb_rating > 4)
```


```{r}
ggplot(movies_df, aes(x = imdb_num_votes)) +
  geom_histogram(binwidth = 100, fill = "red", color = "red", alpha = 0.7) +
  labs(title = "Histogram of imdb_num_votes") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
```

This amount of skew requires a closer look.

```{r}
ggplot(movies_df %>% filter(imdb_num_votes < 50000), aes(x = imdb_num_votes)) +
  geom_histogram(binwidth = 100, fill = "red", alpha = 0.7) +
  labs(title = "Histogram of imdb_num_votes") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
```

```{r}
movies_df %>% filter(imdb_num_votes > 500000)
```

We can verify this skew by these popular films and apply a few log transformations to `imdb_num_votes` to reduce the skew.

```{r}
ggplot(movies_df, aes(x = log(imdb_num_votes))) +
  geom_histogram(binwidth = .1, fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Histogram of imdb_num_votes") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
```
```{r}
ggplot(movies_df, aes(x = log2(imdb_num_votes))) +
  geom_histogram(binwidth = .2, fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Histogram of imdb_num_votes") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

```
```{r}
ggplot(movies_df, aes(x = log10(imdb_num_votes))) +
  geom_histogram(binwidth = .05, fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Histogram of imdb_num_votes") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
```

The base log transformation appears to depict the most normal distribution.

```{r}
movies_df$imdb_num_votes <- log(movies_df$imdb_num_votes)
```


```{r}
ggplot(movies_df, aes(x = critics_score)) +
  geom_histogram(binwidth = 1, fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Histogram of critics_score") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
```

`critics_score` is not skewed.

```{r}
ggplot(movies_df, aes(x = audience_score)) +
  geom_histogram(binwidth = 1, fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Histogram of audience_score") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
```

The skew in `audience_score` is bearable for our purposes.


#### Final Variable Assessment

Are there any rows with an NA value left?

```{r}
movies_df %>% filter(rowSums(is.na(.)) > 0)
```

Are there any duplicated rows?

```{r}
movies_df[duplicated(movies_df), ]
```
Drop the 1 duplicated row.

```{r}
movies_df <- movies_df %>% distinct()
```


Final review of all variable types for the model.

```{r}
str(movies_df)
```

This concludes our EDA and Data Wrangling of all variables in the data.

* * * * * * *


# Multicollinearity of Independent Variables & Linear Relations to Y


We're going to start with 2 custom functions to create a correlation heat map for all variables (numeric or factor).


`calculate_association` takes any 2 variables and performs a series of `else if` statements to calculate association/correlation.

1. If both variables are identical, returns 1.

2. If both variables are numeric, returns Pearson correlation.

3. If variables are 1 factor & 1 numeric,

    3.1. If factor = 2 levels, returns point-biserial correlation.
    
    3.2. If factor > 2 levels, returns eta-squared of ANOVA.
      
3. If both variables are factors (any levels), returns Cramer's V correlation.


```{r}
calculate_association <- function(x, y) {
  if (identical(x, y)) { # TEST 1
    return(c("identity" = 1))
    
  } else if (is.numeric(x) && is.numeric(y)) {       # TEST 2
    return(c("pearson" = cor(x, y, use = "pairwise.complete.obs"))) 
    
  } else if ((is.numeric(x) && is.factor(y)) || (is.factor(x) && is.numeric(y))) { # TEST 3
    # Swap if necessary to ensure x is factor and y is numeric
    if (is.numeric(x)) {
      temp <- x
      x <- y
      y <- temp
    }
    
    if (length(levels(x)) == 2) { # TEST 3.1
      # Point-Biserial Correlation for binary factor
      return(c("point-biserial" = cor(y, as.integer(x) - 1)))
    } else {                      # TEST 3.2
      # Use eta-squared for factors with more than two levels
      anova_result <- summary(aov(y ~ x))  
      sum_sq <- anova_result[[1]][["Sum Sq"]]
      eta_squared <- sum_sq[1] / sum(sum_sq)
      return(c("eta-squared" = eta_squared))
    }
    
  } else {                        # TEST 4
    # Assuming both variables are factors
    tbl <- table(x, y)
    chi_squared <- chisq.test(tbl)$statistic
    n <- sum(tbl)
    k <- ncol(tbl)
    r <- nrow(tbl)
    cramers_v <- sqrt((chi_squared / n) / min(k - 1, r - 1))
    return(c("cramer's-v" = cramers_v))
  }
}
```

`custom_corr_plot` takes a data frame and iterates each pair of variables through the `calculate_association` function and then melts the matrix to a long format suitable for `ggplot`, and plotted with `geom_tile`. Text labels of correlation values are removed if = 1, black if > 0.5 or < -0.5, else gray.

```{r}
custom_corr_plot <- function(data) {
  var_names <- names(data) 
  n <- length(var_names)
  assoc_matrix <- matrix(NA, ncol=n, nrow=n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      assoc_matrix[i, j] <- calculate_association(data[[var_names[i]]], data[[var_names[j]]])
    }
  }
  
  colnames(assoc_matrix) <- var_names
  rownames(assoc_matrix) <- var_names
  
  # Melt the matrix
  melted <- reshape2::melt(assoc_matrix)
  
  # Plot the heatmap
  p <- ggplot(data = melted, aes(x = Var2, y = Var1, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Association") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 10, hjust = 1),
          axis.text.y = element_text(size = 10),
          axis.title = element_blank()) +
    coord_fixed()
  
  # Add text labels for associations >= 0.5 or <= -0.5, but not equal to 1, in black
  p <- p + geom_text(data = subset(melted, (value >= 0.5 | value <= -0.5) & value != 1), 
                    aes(x = Var2, y = Var1, label = round(value, 2)), 
                    size = 4, color = "black")
  
  # Add text labels for associations < 0.5 and > -0.5 and not equal to 1, in gray
  p <- p + geom_text(data = subset(melted, value < 0.5 & value > -0.5 & value != 1), 
                    aes(x = Var2, y = Var1, label = round(value, 2)), 
                    size = 4, color = "darkgray")
  
return(p)

}
```

```{r, warning=F, fig.width=14, fig.height=12}
movies_df %>% select(-title) %>% custom_corr_plot()
```

The custom correlation plot reveals the release date variables have the lowest association with the response variable, followed by studio. 

There is also very high coliniarity between (`critics_rating` & `audience_rating`), (`critics_score` & `critics_rating`), as well as (`audience_score` & `critics_score`).

We know the `audience_rating` and `critics_rating` are derived from `audience_score` and `critics_score` respectively. And that `audience_rating` is a 2 level factor, while `critics_score` is a 3 level factor. 

Since `critics_score` has 3 levels, and we actually have some clarity on how it is calculated, unlike `audience_rating`, we will omit `audience_rating` and `critics_score` from the model.

```{r}
movies_df <- movies_df %>% select(-thtr_rel_decade, -thtr_rel_month, -thtr_rel_day, 
                                  -dvd_rel_decade, -dvd_rel_month, -dvd_rel_day,
                                  -audience_rating, -critics_score)
```




* * *

# Modeling

We are going to build a Multiple Linear Regression model using the backwardation method to optimize the p-values of the independent variables. At the end we will pick the model with the highest \( R^{2}_{\text{adj}} \).

**Dependent Variable**

1.  `imdb_rating` (numeric: 1.0 - 10.0)


**Independent Variables**

1.  `title_type` (categorical: 3 levels)

2.  `genre` (categorical: 11 levels)

3.  `runtime` (numeric)

4.  `mpaa_rating` (categorical: 5 levels)

5.  `critics_rating` (categorical: 3 levels)

6.  `audience_score` (numeric)

7.  `studio` (categorical: 9 levels)

8.  `imdb_num_votes` (numeric (log))

9.  `best_pic_nom` (binary)

10. `best_pic_win` (binary)

11. `best_actor_win` (binary)

12. `best_actress_win` (binary)

13. `best_dir_win` (binary)

14. `top200_box` (binary)


We are going to split the training and test sets at a 70/30 ratio.

```{r data-splitting, warning=F}
df <- movies_df %>% select(-title)

set.seed(321)
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train  <- df[sample, ]
test   <- df[!sample, ]
```


### Full Model

```{r}
# Full model has 14 independent variables
full_model <- lm(imdb_rating ~ ., data = train)

# Model summary
summary(full_model)

# Predictions
predictions <- predict(full_model, newdata = test)

# Residuals
residuals <- test$imdb_rating - predictions

# RMSE
rmse <- sqrt(mean(residuals^2))
print(paste("Root Mean Squared Error: ", rmse))

# RMSE Percentage from Mean
mean <- mean(train$imdb_rating)
pct_rmse <- (rmse / mean) * 100

# R-Squared
ssr <- sum((test$imdb_rating - predictions)^2)
sst <- sum((test$imdb_rating - mean(test$imdb_rating))^2)
rsq <- 1 - (ssr / sst)
print(paste("R-Squared: ", rsq))

# Adjusted R-Squared
n <- length(test$imdb_rating) # Number of observations
p <- length(full_model$model) - 1 # Number of predictors (excluding the constant term)
adj_rsq <- 1 - (1 - rsq) * (n - 1) / (n - p - 1)
print(paste("Adjusted R-Squared: ", adj_rsq))

# Table to store results
results <- tibble("model" = NA, "rmse" = NA, "pct_rmse" = NA, "rsq" = NA, "adj_rsq" = NA) 

# Store results
results <- rbind(results, c("Full Model", rmse, pct_rmse, rsq, adj_rsq)) %>% drop_na()
```

The full model has a RMSE of 0.41, meaning our mean error for predictions of `imdb_rating` are 0.41, the Adjusted R-Squared for this model was 0.80, meaning 80% of the variations in the data can be explained by this model.

`studio` and `mpaa_rating` have no significant levels in the model and will be removed for the next model.


### Model 2

```{r}
# Model 2 has 12 independent variables
model2 <- lm(imdb_rating ~ . , 
                 data = train %>% select(-studio, -mpaa_rating))

# Model summary
summary(model2)

# Predictions
predictions <- predict(model2, newdata = test)

# Residuals
residuals <- test$imdb_rating - predictions

# RMSE
rmse <- sqrt(mean(residuals^2))
print(paste("Root Mean Squared Error: ", rmse))

# RMSE Percentage from Mean
mean <- mean(train$imdb_rating)
pct_rmse <- (rmse / mean) * 100

# R-Squared
ssr <- sum((test$imdb_rating - predictions)^2)
sst <- sum((test$imdb_rating - mean(test$imdb_rating))^2)
rsq <- 1 - (ssr / sst)
print(paste("R-Squared: ", rsq))

# Adjusted R-Squared
n <- length(test$imdb_rating) # Number of observations
p <- length(model2$model) - 1 # Number of predictors (excluding the constant term)
adj_rsq <- 1 - (1 - rsq) * (n - 1) / (n - p - 1)
print(paste("Adjusted R-Squared: ", adj_rsq))

# Store results
results <- rbind(results, c("Model 2", rmse, pct_rmse, rsq, adj_rsq)) 
```

Since the p-value for `top200_box`, `best_pic_nom`, and `best_pic_win` are all extraordinarily high, we remove all 3 on the next iteration.


### Model 3

```{r}
# Model 3 has 9 independent variables
model3 <- lm(imdb_rating ~ . , 
                 data = train %>% select(-studio, -mpaa_rating,
                                         -top200_box, -best_pic_nom, -best_pic_win))

# Model summary
summary(model3)

# Predictions
predictions <- predict(model3, newdata = test)

# Residuals
residuals <- test$imdb_rating - predictions

# RMSE
rmse <- sqrt(mean(residuals^2))
print(paste("Root Mean Squared Error: ", rmse))

# RMSE Percentage from Mean
mean <- mean(train$imdb_rating)
pct_rmse <- (rmse / mean) * 100

# R-Squared
ssr <- sum((test$imdb_rating - predictions)^2)
sst <- sum((test$imdb_rating - mean(test$imdb_rating))^2)
rsq <- 1 - (ssr / sst)
print(paste("R-Squared: ", rsq))

# Adjusted R-Squared
n <- length(test$imdb_rating) # Number of observations
p <- length(model3$model) - 1 # Number of predictors (excluding the constant term)
adj_rsq <- 1 - (1 - rsq) * (n - 1) / (n - p - 1)
print(paste("Adjusted R-Squared: ", adj_rsq))

# Store results
results <- rbind(results, c("Model 3", rmse, pct_rmse, rsq, adj_rsq)) 
```

The last 3 binary variables are dropped for the next iteration.


### Model 4

```{r}
# Model 4 has 6 independent variables
model4 <- lm(imdb_rating ~ . , 
                 data = train %>% select(-studio, -mpaa_rating,
                                         -top200_box, -best_pic_nom, -best_pic_win,
                                         -best_actor_win, -best_actress_win, -best_dir_win))

# Model summary
summary(model4)

# Predictions
predictions <- predict(model4, newdata = test)

# Residuals
residuals <- test$imdb_rating - predictions

# RMSE
rmse <- sqrt(mean(residuals^2))
print(paste("Root Mean Squared Error: ", rmse))

# RMSE Percentage from Mean
mean <- mean(train$imdb_rating)
pct_rmse <- (rmse / mean) * 100

# R-Squared
ssr <- sum((test$imdb_rating - predictions)^2)
sst <- sum((test$imdb_rating - mean(test$imdb_rating))^2)
rsq <- 1 - (ssr / sst)
print(paste("R-Squared: ", rsq))

# Adjusted R-Squared
n <- length(test$imdb_rating) # Number of observations
p <- length(model4$model) - 1 # Number of predictors (excluding the constant term)
adj_rsq <- 1 - (1 - rsq) * (n - 1) / (n - p - 1)
print(paste("Adjusted R-Squared: ", adj_rsq))

# Store results
results <- rbind(results, c("Model 4", rmse, pct_rmse, rsq, adj_rsq)) 
```

Let's see how each model compares to the full.

```{r}
# Change results columns to numeric and round
results %>% mutate_at(vars(2:5), as.numeric) %>% 
  mutate_if(is.numeric, round, digits = 4)
```
 
While the full model provided the lowest RMSE, indicating the smallest average prediction error, it also had the lowest adjusted R-squared value. This suggests that, despite its lower prediction error, the full model may be overfitting to the training data and may not generalize as well to new data. On the other hand, Model 4, despite having a slightly higher RMSE, had the highest adjusted R-squared value, suggesting that it may provide a better balance between fit and generalizability. 


*****


# Predictions

Our final test of predictions is based upon 3 films released in 2016 that were not included in the original data.

| Title          | URLs            |
|----------------|---------------------|
| Doctor Strange | [Rotten Tomatoes](https://www.rottentomatoes.com/m/doctor_strange_2016) <br> [IMDb](https://www.imdb.com/title/tt1211837/?ref_=nv_sr_srsg_0_tt_8_nm_0_q_doctor%2520str) |
| Zoolander 2    | [Rotten Tomatoes](https://www.rottentomatoes.com/m/zoolander_2) <br> [IMDb](https://www.imdb.com/title/tt1608290/?ref_=nv_sr_srsg_0_tt_8_nm_0_q_Zoolander%25202) |
| Deadpool       | [Rotten Tomatoes](https://www.rottentomatoes.com/m/deadpool) <br> [IMDb](https://www.imdb.com/title/tt1431045/?ref_=ttls_li_tt) |


```{r}
# New data frame for outside data

new_test <- tibble(title = c("Doctor Strange","Zoolander 2", "Deadpool"),
              title_type = c("Feature Film", "Feature Film", "Feature Film"),
                   genre = c("Action & Adventure", "Comedy", "Action & Adventure"),
                 runtime = c(115, 102, 108),
             mpaa_rating = c("PG-13", "PG-13", "R"),
                  studio = c("Disney", "Paramount", "Fox"),
           thtr_rel_year = c(2016, 2016, 2016),
          thtr_rel_month = c(NA, NA, NA),
            thtr_rel_day = c(NA, NA, NA),
            dvd_rel_year = c(NA, NA, NA),
           dvd_rel_month = c(NA, NA, NA),
             dvd_rel_day = c(NA, NA, NA),
             imdb_rating = c(7.5, 4.7, 8),
          imdb_num_votes = log(c(769000, 73000, 1100000)),
          critics_rating = factor(c("Certified Fresh", "Rotten", "Certified Fresh"), 
                                  levels = c("Rotten", "Fresh", "Certified Fresh")),
           critics_score = c(NA, NA, NA),
         audience_rating = c(NA, NA, NA),
          audience_score = c(85, 20, 90),
            best_pic_nom = c(0, 0, 0),
            best_pic_win = c(0, 0, 0),
          best_actor_win = c(0, 0, 1),
        best_actress_win = c(1, 0, 1),
            best_dir_win = c(0, 0, 0),
              top200_box = c(0, 1, 0),
                director = c(NA, NA, NA),
                  actor1 = c(NA, NA, NA),
                  actor2 = c(NA, NA, NA),
                  actor3 = c(NA, NA, NA),
                  actor4 = c(NA, NA, NA),
                  actor5 = c(NA, NA, NA),
                imdb_url = c(NA, NA, NA),
                  rt_url = c(NA, NA, NA))


# Predictions for new data with all 4 models

new_test$full_model_y <- predict(full_model, newdata = new_test) 
new_test$full_model_res <- new_test$imdb_rating - new_test$full_model_y

new_test$model2_y <- predict(model2, newdata = new_test)
new_test$model2_res <- new_test$imdb_rating - new_test$model2_y

new_test$model3_y <- predict(model3, newdata = new_test)
new_test$model3_res <- new_test$imdb_rating - new_test$model3_y

new_test$model4_y <- predict(model4, newdata = new_test)
new_test$model4_res <- new_test$imdb_rating - new_test$model4_y
```


How did each model perform on new data?

```{r}
# All model predictions
new_test %>% select(title, imdb_rating, 
                    full_model_y, 
                    model2_y, 
                    model3_y, 
                    model4_y)  %>% 
  mutate_if(is.numeric, round, digits = 3)

# All models residuals
new_test %>% select(title, imdb_rating, 
                    full_model_res,
                    model2_res,
                    model3_res,
                    model4_res)  %>% 
  mutate_if(is.numeric, round, digits = 3)

# Average residuals per model on new data
tibble(full_model = abs(new_test$full_model_res),
       model2     = abs(new_test$model2_res),
       model3     = abs(new_test$model3_res),
       model4     = abs(new_test$model4_res)) %>% 
  colSums() / 3
```

The Full Model appears to outperform on our test data and our outside data despite its lower \( R^{2}_{\text{adj}} \). The RMSE for all 4 models in the outside data is significantly lower than the 0.41 error in the test data. Therefore, Model 4 will be chosen as the final parsimonious model. 


*****


# Diagnostics

We put the model data in its own data frame to make plotting the diagnostics easier.

```{r}
# Put residuals & fitted values into data frame for ggplot 

model_data <- data.frame(residuals = model4$residuals, 
                         fitted_values = model4$fitted.values, 
                         index = 1:length(model4$residuals))
```


### Linear Relelationships Between Y & X (numeric)

```{r}
# plot(full_model$residuals ~ train$runtime)

ggplot(train, aes(x = runtime, y = model_data$residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color="red", linetype="dashed") +
  labs(y="Residuals") +
  theme_bw()
```



```{r}
# plot(full_model$residuals ~ train$imdb_num_votes)

ggplot(train, aes(x = imdb_num_votes, y = model_data$residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(y = "Residuals") +
  theme_bw()
```

```{r}
# plot(full_model$residuals ~ train$audience_score)

ggplot(train, aes(x = audience_score, y = model_data$residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(y = "Residuals") +
  theme_bw()
```

All 3 numeric variables are randomly dispersed when plotted against the model residuals. This implies linearity in the numeric predictors and the response variable, and homoscedasticity which implies a constant variance accross all 3 numeric variables.

The residuals of the model, when plotted against each of the three numeric predictors, are randomly dispersed. This pattern suggests 

1. Linearity - each predictor has a linear relationship with the response variable.  

2. Homoscedasticity - the variance of the errors is constant across all levels of these predictors.


*****

### Nearly Normal Residuals

```{r}
# hist(full_model$residuals)

ggplot(model_data, aes(x = residuals)) +
  geom_histogram(binwidth = 0.1, fill = "red", color = "black", alpha = 0.7) +
  labs(title = paste("Histogram of Residuals")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 
```

A histogram of residuals reveals nearly normalization with a slight left skew and a mean = 0.

```{r}
ggplot(model_data, aes(sample = residuals)) +
  geom_qq_line(color = "red", linetype = "dashed") +
  geom_qq(size = 2, color = "black", alpha = 0.5) +
  labs(title = "Normal Q-Q Plot of Residuals",
           x = "Theoretical Quantiles",
           y = "Sample Quantiles") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
```

The Normal Q-Q Plot suggests normality with a majority of points along the line with a slight skew still visible on the left.


*****

### Constant Variability of Reisudals

```{r}
# plot(full_model$residuals ~ full_model$fitted.values)

ggplot(model_data, aes(x = fitted_values, y = residuals)) +
  geom_point(color = "black", fill = "darkgray", size = 2) +
  geom_hline(yintercept = 0, color="red", linetype = "dashed") +
  labs(x = "Fitted Values", y = "Residuals") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



# plot(abs(full_model$residuals) ~ full_model$fitted.values)

ggplot(model_data, aes(x = fitted_values, y = abs(residuals))) +
  geom_point(color = "black", fill = "darkgray", size = 2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Fitted Values", y = "Residuals") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

```

A check for constant variability of residuals along the predicted values suggests that the model does not perform any better or worse in certain ranges of the predicted values.


*****


### Independence of Residuals

```{r}
# plot(full_model$residuals)

ggplot(model_data, aes(x = index, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Index", y = "Residuals") +
  theme_bw()
```

The random scatter around 0 implies that the residuals in the model are independent of themselves.


* * *


# Conclusion

The primary objective of this analysis was to develop a Multiple Linear Regression model capable of predicting IMDb movie ratings using various independent variables related to the movie's characteristics and critics. 

The full model, which included all 14 independent variables, achieved a Root Mean Squared Error (RMSE) of 0.4109 and an Adjusted R-squared value of 0.802. This model performed relatively well in terms of prediction accuracy, as evidenced by its low RMSE. 

Through a series of iterations, we eliminated variables with high p-values, multicollinearity issues, and minimal contributions to the model's performance. This process led to the development of Model 4, which included six independent variables: `title_type`, `genre`, `runtime`, `critics_rating`, `audience_score`, and `imdb_num_votes.` Despite having a slightly higher RMSE of 0.418, Model 4 achieved the highest Adjusted R-squared value of 0.806, indicating a better balance between fit and generalizability.

When tested on a small set of new movies released in 2016, the full model outperformed the other models in terms of prediction accuracy, suggesting that it may be better suited for predicting ratings for new movies not included in the original dataset.

Overall, the analysis successfully developed multiple regression models capable of predicting IMDb movie ratings with reasonable accuracy. The choice between the full model and Model 4 would depend on the specific requirements and constraints of the use case. If the primary objective is to achieve the highest possible prediction accuracy, the full model may be preferred, despite the risk of overfitting. On the other hand, if generalization and interpretability are more important considerations, Model 4 could be the better choice due to its higher Adjusted R-squared value and simpler structure.

Future attempts at model construction could involve exploring alternative modeling techniques, such as ensemble methods or machine learning algorithms, to potentially improve prediction accuracy further. Additionally, incorporating more diverse and up-to-date data sources, as well as considering additional movie-related features, could enhance the models' predictive capabilities.
