library(tidytext)
library(tidyverse)

paygap <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-28/paygap.csv')
uk_sic_codes <- read_csv("analysis/data/SIC07_CH_condensed_list_en.csv") %>%
  janitor::clean_names()

paygap_joined <- paygap %>%
  select(employer_name, diff_median_hourly_percent, sic_codes) %>%
  separate_rows(sic_codes, sep = ":") %>%
  left_join(uk_sic_codes, by = c("sic_codes" = "sic_code"))

paygap_token <- paygap_joined %>%
  unnest_tokens(word, description) %>%
  anti_join(get_stopwords()) %>%
  na.omit()

top_words <- 
  paygap_token %>%
  count(word) %>%
  filter(!word %in% c("activities", "n.e.c", "general", "non")) %>%
  slice_max(n, n = 50) %>%
  pull(word)

paygap_final <- paygap_token %>%
  filter(word %in% top_words) %>%
  transmute(
    diff_wage = diff_median_hourly_percent / 100, 
    word) %>%
  group_by(word) %>%
  summarise(diff_wage = round(mean(diff_wage), digits = 4)) %>%
  arrange(desc(diff_wage)) %>%
  rename(sector = word, percent_increase_in_mens_wages_compared_to_womens = diff_wage)

paygap_final

write_csv(paygap_final %>% slice_head(n = 10), "analysis/data/paygap_sector_averages.csv")
