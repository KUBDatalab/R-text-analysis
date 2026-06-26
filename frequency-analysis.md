---
title: "Word frequency analysis"
teaching: 0
exercises: 0
---


:::::::::::::::::::::::::::::::::::::: questions 

- How is a frequency analysis conducted?

::::::::::::::::::::::::::::::::::::::::::::::::

::::::::::::::::::::::::::::::::::::: objectives

- Learn how to find frequent words
- Learn how to analyse and visualise it


::::::::::::::::::::::::::::::::::::::::::::::::





## Frequency analysis

A word frequency is a relatively simple analysis. It measures how often words occur in a text. 



``` r
articles_anti_join |> 
  count(word, sort = TRUE)
```

``` output
# A tibble: 68,899 × 2
   word           n
   <chr>      <int>
 1 it’s        6584
 2 ai          5778
 3 people      4422
 4 time        4277
 5 technology  3946
 6 world       2758
 7 don’t       2131
 8 day         1803
 9 life        1694
10 uk          1690
# ℹ 68,889 more rows
```

The previous code chunk resulted in a list containing the most frequent words. The words are from articles about both presidents, and they are sorted based on frequency with the highest number on top.

A closer look at the list may reveal that some words are irrelevant. Given that the articles in the dataset are about the two presidents' respective inaugurations, we consider the words below irrelevant for our analysis. Therefore, we make a new dataset without these words.


``` r
articles_filtered <- articles_anti_join |> 
  filter(!word %in% c("trump", "trump’s", 
                      "obama", "obama's", 
                      "inauguration", "president"))

articles_filtered |> 
  count(word, sort = TRUE)
```

``` output
# A tibble: 68,894 × 2
   word           n
   <chr>      <int>
 1 it’s        6584
 2 ai          5778
 3 people      4422
 4 time        4277
 5 technology  3946
 6 world       2758
 7 don’t       2131
 8 day         1803
 9 life        1694
10 uk          1690
# ℹ 68,884 more rows
```
The words deemed irrelevant are no longer on the list above.

Instead of a general list it may be more interesting to focus on the most frequent words belonging to articles about the two presidents respectively.


``` r
articles_filtered |> 
  count(president, word, sort = TRUE)
```

``` error
Error in `count()`:
! Must group by variables found in `.data`.
✖ Column `president` is not found.
```
Keeping an overview of the words associated with each president can be a bit tricky. For instance, the word "people" is associated with both presidents. This is easy to see, as the two words are right next to each other. The two occurrences of the word America, however, are further apart, although this word is also associated with both presidents. A visualisation may solve this problem.



``` r
articles_filtered |> 
  count(president, word, sort = TRUE) |> 
  group_by(president) |> 
  slice(1:10) |> 
  ggplot(mapping = aes(x = n, y = word, colour = president, shape = president)) +
  geom_point() 
```

``` error
Error in `count()`:
! Must group by variables found in `.data`.
✖ Column `president` is not found.
```
The plot above shows the top-ten words associated with Obama and Trump respectively. If a word features on both presidents' top-ten list, it only occurs once in the plot. This is why the plot doesn't contain 20 words in total.

Another interesting aspect to look at would be the most frequent words used in relation to each president. In this analysis the president is the guiding principle.


``` r
articles_filtered |> 
  count(president, word, sort = TRUE) |> 
  pivot_wider(
    names_from = president,
    values_from = n
  )
```

``` error
Error in `count()`:
! Must group by variables found in `.data`.
✖ Column `president` is not found.
```


``` r
articles_filtered |> 
  group_by(president) |> 
  count(word, sort = TRUE) |> 
  top_n(10) |> 
  ungroup() |> 
  mutate(word = reorder_within(word, n, president)) |> 
  ggplot(aes(n, word, fill = president)) +
  geom_col() +
  facet_wrap(~president, scales = "free") +
  scale_y_reordered() + 
  labs(x = "word occurrences")
```

``` error
Error in `group_by()`:
! Must group by variables found in `.data`.
✖ Column `president` is not found.
```
The analyses just made can easily be adjusted. For instance, if we want look at the words by `pillar_name` instead of by `president`, we simply replace `president` with `pillar_name` in the code.


``` r
articles_filtered |> 
  count(pillar_name, word, sort = TRUE) |> 
  group_by(pillar_name) |> 
  slice(1:10) |> 
  ggplot(mapping = aes(x = n, y = word, colour = pillar_name, shape = pillar_name)) +
  geom_point() 
```

``` error
Error in `count()`:
! Must group by variables found in `.data`.
✖ Column `pillar_name` is not found.
```





::::::::::::::::::::::::::::::::::::: keypoints 

- Making a frequency analysis
- Visualising the results


::::::::::::::::::::::::::::::::::::::::::::::::
