library(tidyverse)

load("data/WVS_TimeSeries_1981_2020_R_v2_0.rdata")

countries_to_clust <- c("HUN", "CZE", "SVK", "SVN", "RUS", "UKR", "EST", "LTU", "LVA", "MDA", "SRB", "POL", "ROU", "BGR", "HRV", "BIH", "KAZ", "KGZ", "AZE", "GEO", "ARM", "BLR", "TKM", "TJK", "UZB", "MKD", "XKX", "ALB", "MNG")

countries_to_compare <- c("CHN", "USA", "TUR", "RUS", "DEU")

codenames_df <- readxl::read_excel("data/F00003844-WVS_Time_Series_List_of_Variables_and_equivalences.xlsx") %>% 
  select(code = 2, q = 3, values = 5) %>% 
  slice(-1) %>% 
  # slice(4) %>% 
  mutate(
    values = map(values, str_split, pattern = "\\n|\\r")
  ) %>% 
  unnest(values) %>% 
  filter(map_dbl(values, ~ sum(!is.na(.))) != 0) %>% 
  mutate(
    values = map(values, str_remove_all, "\\r|\\n"),
    values = map(values, keep, ~ . != ""),
    values = map(values, ~ {
      value_code <- gsub(" .*", "", .)
      label <- str_remove(., code)
      df <- cbind(value_code, label) %>% 
        data.frame() %>% 
        mutate(label = ifelse(str_starts(value_code, "-"), NA, str_trim(label)))
    })
    # label = values,
    # values = map(values, ~ gsub(" .*", "", .))
  ) %>% 
  unnest(values) %>% 
  distinct()


wv_df <- WVS_TimeSeries_1981_2020_v2_0. %>% 
  tibble() %>% 
  select(geo = COW_ALPHA, time = S002VS,  A035, C001, D057, D059, D060, F118, A098:A105, E025, E026, E027, E069_17, E069_04, A124_02, A124_06, A124_09, E114, E117, E226, E035, E036, E037, E039, A032, A042, E012, G006, G022C, G022E, G022I, F115, F116, F117) %>% 
  filter(geo %in% countries_to_clust | geo %in% countries_to_compare) %>% 
  select(geo, time, everything()) %>% 
  mutate_if(is.numeric, ~ ifelse(. < 0, NA, .))


ZA4804_v3_1_0 <- haven::read_dta("data/ZA4804_v3-1-0.dta")

evs_df <- ZA4804_v3_1_0 %>% 
  rename_all(str_to_upper) %>% 
  rename(geo = S009, time = S002VS) %>% 
  select(names(wv_df)) %>% 
  mutate_if(is.numeric, as.numeric) %>% 
  mutate_if(is.numeric, ~ ifelse(. < 0, NA, .)) %>% 
  mutate(geo = countrycode::countrycode(geo, "iso2c", "iso3c")) %>%
  group_by(geo, time) %>% 
  nest() %>% 
  ungroup() %>% 
  filter(geo %in% countries_to_clust | geo %in% countries_to_compare) %>% 
  anti_join(wv_df[c("geo", "time")])

save(wv_df, evs_df, countries_to_compare, countries_to_clust, codenames_df, file = "data/wf_df.Rdata")
