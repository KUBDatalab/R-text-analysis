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
When performing text analysis in R, the built-in functions in R are not sufficient. It is therefore necessary to install some additional packages. In this course we will be using the packages `tidyverse` and `tidytext`.





``` r
install.packages("tidyverse")
install.packages("tidytext")

library(tidyverse)
library(tidytext)
```


:::: callout
### Documentation for each package
If you would like to know more about the different packages, please click on the links below.

* [tidyverse](https://www.tidyverse.org/packages/){target="_blank"}
* [tidytext](https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html){target="_blank"}

::::::

## Getting data
Begin by downloading the dataset called `articles.csv`. Place the downloaded file in the data/ folder. You can do this directly from R by copying and pasting this into your console. (The console is the panel under your script).


``` r
download.file("https://raw.githubusercontent.com/KUBDatalab/R-textmining_new/main/episodes/data/guardianArticles.csv", "data/guardianArticles.csv", mode = "wb")
```

After downloading the data you need to load the data into R's memory using the function `read_csv()`.


``` r
articles <- read_csv("data/guardianArticles.csv", na = c("NA", "NULL", ""))
```

## Data description
The dataset contains newspaper articles from the Guardian newspaper. The harvested articles were published between June 2025 and May 2026 and contain the word "technology".

The original dataset contained lots of variables considered irrelevant within the parameters of this course. The following variables were kept:

* __id__ - unique number identifying each article
* __date__ - month and publication year
* __text__ - full text from the article
* __section__ - Guardian news section
* __region__ - production region
* __author__ - name(s) of journalist(s)
* __wordcount__ - number of words

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
# A tibble: 6 × 7
     id date    text                             section region author wordcount
  <dbl> <chr>   <chr>                            <chr>   <chr>  <chr>      <dbl>
1     1 2026-05 Britain’s biometrics watchdogs … News    UK     Jessi…      1328
2     2 2026-02 Iran’s architecture of internet… News    UK     Aisha…       657
3     3 2026-01 TikTok will begin to roll out n… News    UK     Mark …       623
4     4 2026-05 The parent company of Donald Tr… News    US     Edwar…       348
5     5 2026-05 It is a familiar story. Extrava… Opinion UK     Edito…       585
6     6 2026-04 Sonia Bompastor, the Chelsea he… Sport   UK     Tom G…       468
```

``` r
tail(articles)
```

``` output
# A tibble: 6 × 7
     id date    text                             section region author wordcount
  <dbl> <chr>   <chr>                            <chr>   <chr>  <chr>      <dbl>
1  4970 2025-07 "[Where’s the game at?] Definit… Sport   UK     James…      8817
2  4975 2025-06 "Tim Henman, who was disqualifi… Sport   UK     Katy …      7537
3  4981 2025-06 "Ali Martin’s report will be wi… Sport   UK     Rob S…      8874
4  4994 2025-06 "An epic tie-break to end an ep… Sport   UK     John …      9208
5  5003 2025-11 "Don’t you just love Christmas … Lifest… UK     Hanna…     13325
6  5018 2025-07 "Here’s Ewan Murray’s report an… Sport   UK     David…     14350
```


:::::::::::::::: 

:::::::::::::::: solution
### How to show information about the columns


``` r
glimpse(articles)
```

``` output
Rows: 1,898
Columns: 7
$ id        <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 1…
$ date      <chr> "2026-05", "2026-02", "2026-01", "2026-05", "2026-05", "2026…
$ text      <chr> "Britain’s biometrics watchdogs have warned that national ov…
$ section   <chr> "News", "News", "News", "News", "Opinion", "Sport", "Arts", …
$ region    <chr> "UK", "UK", "UK", "US", "UK", "UK", "UK", "AUS", "US", "UK",…
$ author    <chr> "Jessica Murray and Robert Booth", "Aisha Down", "Mark Swene…
$ wordcount <dbl> 1328, 657, 623, 348, 585, 468, 917, 794, 915, 4213, 1304, 66…
```
::::::::::::::::

:::::::::::::::: solution
### Get the names of the variables / columns

``` r
names(articles)
```

``` output
[1] "id"        "date"      "text"      "section"   "region"    "author"   
[7] "wordcount"
```
 
:::::::::::::::: 

:::::::::::::::: solution
### Get the dimension of the dataset (number of rows and coloumns)


``` r
dim(articles)
```

``` output
[1] 1898    7
```

::::::::::::::::


::::::::::::::::::::::::::::::::::::: keypoints 

- Packages must be installed and loaded
- The dataset needs to be loaded
- The dataset can be inspected by means of different functions

::::::::::::::::::::::::::::::::::::::::::::::::
