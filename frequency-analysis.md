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






``` r
articles_anti_join
```

``` output
# A tibble: 1,118,028 × 7
      id date    section region author                          wordcount word  
   <dbl> <chr>   <chr>   <chr>  <chr>                               <dbl> <chr> 
 1     1 2026-05 News    UK     Jessica Murray and Robert Booth      1328 brita…
 2     1 2026-05 News    UK     Jessica Murray and Robert Booth      1328 biome…
 3     1 2026-05 News    UK     Jessica Murray and Robert Booth      1328 watch…
 4     1 2026-05 News    UK     Jessica Murray and Robert Booth      1328 warned
 5     1 2026-05 News    UK     Jessica Murray and Robert Booth      1328 natio…
 6     1 2026-05 News    UK     Jessica Murray and Robert Booth      1328 overs…
 7     1 2026-05 News    UK     Jessica Murray and Robert Booth      1328 ai    
 8     1 2026-05 News    UK     Jessica Murray and Robert Booth      1328 power…
 9     1 2026-05 News    UK     Jessica Murray and Robert Booth      1328 scann…
10     1 2026-05 News    UK     Jessica Murray and Robert Booth      1328 catch 
# ℹ 1,118,018 more rows
```

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
  filter(!word %in% c("it’s", "don’t"))

articles_filtered |> 
  count(word, sort = TRUE)
```

``` output
# A tibble: 68,897 × 2
   word           n
   <chr>      <int>
 1 ai          5778
 2 people      4422
 3 time        4277
 4 technology  3946
 5 world       2758
 6 day         1803
 7 life        1694
 8 uk          1690
 9 that’s      1667
10 government  1644
# ℹ 68,887 more rows
```
The words deemed irrelevant are no longer on the list above.

Instead of a general list it may be more interesting to focus on the most frequent words belonging to articles about the two presidents respectively.


``` r
articles_filtered |> 
  count(section, word, sort = TRUE)
```

``` output
# A tibble: 138,065 × 3
   section   word           n
   <chr>     <chr>      <int>
 1 News      ai          3131
 2 News      technology  1993
 3 Sport     england     1371
 4 Sport     ball        1287
 5 Opinion   ai          1250
 6 Sport     1           1239
 7 News      people      1196
 8 Lifestyle time        1117
 9 Lifestyle people      1062
10 Sport     time        1050
# ℹ 138,055 more rows
```
Keeping an overview of the words associated with each section can be a bit tricky. For instance, the word "people" is associated with both presidents. This is easy to see, as the two words are right next to each other. The two occurrences of the word America, however, are further apart, although this word is also associated with both presidents. A visualisation may solve this problem.



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
