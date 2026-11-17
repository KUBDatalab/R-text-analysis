---
title: How did we get the data?
---

library(tidyverse)
library(readr)

install.packages("guardianapi")
library(guardianapi)

guardianapi::gu_api_key()
Vi skal bruge en api-nøgle - den kan du få fra.


obama <- guardianapi::gu_content(query = "Obama", from_date = "2009-01-20", to_date = "2009-01-20")

trump <- guardianapi::gu_content(query = "Trump", from_date = "2017-01-20", to_date = "2017-01-20")

trump %>% write_csv("episodes/data/trump.csv")
view(trump)

