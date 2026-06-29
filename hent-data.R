library(tidyverse)
library(readr)
library(guardianapi)
library(tidytext)
# kør og giv den api-nøglen i console
guardianapi::gu_api_key()
 
articles <- guardianapi::gu_content(query = "technology", from_date = "2025-06-01", to_date = "2026-05-31")
 
articles |> group_by(production_office) |> count()
 
 
tempArticlesTechnology <- articles %>%
  select(c(web_publication_date, pillar_name, byline, wordcount, body_text, production_office)) %>%
  rename(section = pillar_name) |>
  mutate(id = row_number(), .before = "web_publication_date") |>
  filter(!section == "NA" ) |>
  separate_wider_delim(web_publication_date, delim = " ", names = c("date", "time")) |>
  select(-time) |>
  separate_wider_delim(date, delim = "-", names = c("year", "month", "day")) |>
  select(-day) |>
  unite(date, year:month, sep='-') |>
  rename(region = production_office, text = body_text, author = byline) |>
  relocate(text, .after = date) |>
  relocate(region, .after = section)
 
anti_news <- tempArticlesTechnology |>
  filter(section == "News") |>
  group_by(date) |>
  slice(-c(1:50))
 
articlesTechnology <-
  tempArticlesTechnology |>
  anti_join(anti_news)
 
articlesTechnology |> group_by(section) |> count()
 
write_csv(articlesTechnology, "episodes/data/guardianArticles.csv")  
 
guardianArticles <- read_csv("episodes/data/guardianArticles.csv")

guardianArticles |> group_by(section) |>  count(date) |> 
  pivot_wider(names_from = section, values_from = n)
 
# articles_tokenized <- articlesTechnology %>%
#   unnest_tokens(word, text) %>%
#   relocate(word)
 
# # tæller stopord og kan vise dem at der er en masse som er 'unødvendige'
# # skal fjernes - stopord
# articles_tokenized %>% count(word, sort = TRUE)
 
# articles_tokenized %>%
#   group_by(section) %>%
#   count(word, sort = TRUE) %>%
#   top_n(10) %>%
#   ggplot(aes(n, word, fill = section)) +
#   geom_col() +
#   facet_wrap(~section, scales = "free") +
#   scale_y_reordered() +
#   labs(x = "Word occurrences")
 
 
 
# #trump
 
# trump <- guardianapi::gu_content(query = "Trump", from_date = "2017-01-20", to_date = "2017-01-20")
 
# trump %>%
#   filter(!(type %in% c("gallery", "audio", "video", "liveblog"))) %>%
#   filter(!(section_id %in% c("tv-and-radio",
#                              "film",
#                              "football",
#                              "stage",
#                              "theguardian",
#                              "lifeandstyle",
#                              "sport",
#                              "media",
#                              "travel",
#                              "music",
#                              "artanddesign",
#                              "books",
#                              "fashion"))) %>%
#    select(c(id, web_publication_date, pillar_name, headline,
#    standfirst, body_text)) %>%
#   mutate(id = row_number()) %>%
#   write_csv("episodes/data/trump.csv")
 
# # read in files -----------------------------------------------------------
 
# trumpOrg <- read_csv("episodes/data/trump.csv")
# obamaOrg <- read_csv("episodes/data/obama.csv")
 
 
# # tilrette datasæt --------------------------------------------------------
 
# trump <- trumpOrg %>%
#   mutate(president = "trump", .after = id)
 
# obama <- obamaOrg %>%
#   mutate(president = "obama", .after = id)
 
# obamaTrump <- obama %>%
#   rbind(trump) %>%
#   mutate(id = row_number()) %>%
#   mutate(standfirst = str_replace_na(standfirst, replacement = "")) %>%
#   mutate(text = str_c(headline, standfirst, body_text, sep = " "), .after = president) %>%
#   select(-c(headline, standfirst, body_text))
 
# write_csv(obamaTrump, "episodes/data/obamaTrump.csv")
 
# articles <- read_csv("episodes/data/obamaTrump.csv")
 
# articles_tidy <- articles %>%
#   unnest_tokens(word, text)