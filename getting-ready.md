---
title: "Loading data"
teaching: 0
exercises: 0

---

:::::::::::::::::::::::::::::::::::::: questions 


- Which packages are needed?
- How is the dataset loaded?
- How is a dataset inspected?

::::::::::::::::::::::::::::::::::::::::::::::::

::::::::::::::::::::::::::::::::::::: objectives

- Knowledge of the relevant packages
- Ability to to load the dataset
- Ability to inspect the dataset

::::::::::::::::::::::::::::::::::::::::::::::::




## Getting started
When performing text analysis in R, the built-in functions in R are not sufficient. It is therefore necessary to install some additional packages. In this course we will be using the packages `tidyverse`, `tidytext` and `tm`.





``` r
install.packages("tidyverse")
install.packages("tidytext")
install.packages("tm")

library(tidyverse)
library(tidytext)
library(tm)
```


:::: callout
### Documentation for each package
If you would like to know more about the different packages, please click on the links below.

* [tidyverse](https://www.tidyverse.org/packages/){target="_blank"}
* [tidytext](https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html){target="_blank"}
* [tm](https://cran.r-project.org/web/packages/tm/tm.pdf){target="_blank"}

::::::

## Getting data
Begin by downloading the dataset called `articles.csv`. Place the downloaded file in the data/ folder. You can do this directly from R by copying and pasting this into your console. (The console is the panel under your script).


``` r
download.file("https://raw.githubusercontent.com/KUBDatalab/R-textmining_new/main/episodes/data/articles.csv", "data/articles.csv", mode = "wb")
```

After downloading the data you need to load the data into R's memory using the function `read_csv()`.


``` r
articles <- read_csv("data/articles.csv", na = c("NA", "NULL", ""))
```

## Data description
The dataset contains newspaper articles from the Guardian newspaper. The harvested articles were published on the first inauguration day of US presidents Barack Obama and Donald Trump. Inclusion criteria were that the articles contained the name of the relevant president, the word "inauguration" and a publication dates 20th of January 2009 and 2017.

The original dataset contained lots of variables considered irrelevant within the parameters of this course. The following variables were kept:

* __id__ - a unique number identifying each article
* __president__ - the president mentioned in the article
* __text__ - the full text from the article
* __web_publication_date__ - the date of publication
* __pillar_name__ - the section in the newspaper

::::::::::::::::::::::::::::::::::::::: discussion

### Taking a quick look at the data
The `tidyverse`-package has some functions that allow you to inspect the dataset. Below, you can see some of these functions and what they do.

:::::::::::::::::::::::::::::::::::::::

:::::::::::::::: solution

### How to show the first / last rows


``` r
head(articles)
```

``` output
# A tibble: 6 × 5
     id president text                          web_publication_date pillar_name
  <dbl> <chr>     <chr>                         <dttm>               <chr>      
1     1 obama     "Obama inauguration: We will… 2009-01-20 19:16:38  News       
2     2 obama     "Obama from outer space Whet… 2009-01-20 22:00:00  Opinion    
3     3 obama     "Obama inauguration: today's… 2009-01-20 10:17:27  News       
4     4 obama     "Obama inauguration: Countdo… 2009-01-19 23:01:00  News       
5     5 obama     "Inaugural address of Presid… 2009-01-20 16:07:44  News       
6     6 obama     "Liveblogging the inaugurati… 2009-01-20 13:56:40  News       
```

``` r
tail(articles)
```

``` output
# A tibble: 6 × 5
     id president text                          web_publication_date pillar_name
  <dbl> <chr>     <chr>                         <dttm>               <chr>      
1   132 trump     Buy, George? World's largest… 2017-01-20 15:53:41  News       
2   133 trump     Gove’s ‘snowflake’ tweet is … 2017-01-20 12:44:10  Opinion    
3   134 trump     Monet, Renoir and a £44.2m M… 2017-01-20 04:00:22  News       
4   135 trump     El Chapo is not a Robin Hood… 2017-01-20 17:09:54  News       
5   136 trump     They call it fun, but the di… 2017-01-20 16:19:50  Opinion    
6   137 trump     Totes annoying: words that s… 2017-01-20 12:00:06  News       
```


:::::::::::::::: 

:::::::::::::::: solution
### How to show information about the columns


``` r
glimpse(articles)
```

``` output
Rows: 137
Columns: 5
$ id                   <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15…
$ president            <chr> "obama", "obama", "obama", "obama", "obama", "oba…
$ text                 <chr> "Obama inauguration: We will remake America, vows…
$ web_publication_date <dttm> 2009-01-20 19:16:38, 2009-01-20 22:00:00, 2009-0…
$ pillar_name          <chr> "News", "Opinion", "News", "News", "News", "News"…
```
::::::::::::::::

:::::::::::::::: solution
### Get the names of the variables / columns

``` r
names(articles)
```

``` output
[1] "id"                   "president"            "text"                
[4] "web_publication_date" "pillar_name"         
```
 
:::::::::::::::: 

:::::::::::::::: solution
### Get the dimension of the dataset (number of rows and coloumns)


``` r
dim(articles)
```

``` output
[1] 137   5
```

::::::::::::::::


::::::::::::::::::::::::::::::::::::: keypoints 

- Packages must be installed and loaded
- The dataset needs to be loaded
- The dataset can be inspected by means of different functions

::::::::::::::::::::::::::::::::::::::::::::::::
