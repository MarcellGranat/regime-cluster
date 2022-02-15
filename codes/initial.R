library(tidyverse)
load("data/wf_df.Rdata")

wv_df %>% 
  count(geo, time) %>% 
  ggplot(aes(time, geo, fill = n)) +
  geom_tile(color = "black")

sumnonna <- function(x) {
  sum(!is.na(x))
}

wv_df %>% 
  group_by(geo, time) %>% 
  summarise_all(sumnonna) %>% 
  mutate_at(-c(1:2), ~ . != 0) %>% 
  pivot_longer(-c(1:2)) %>% 
  group_by(time, name) %>% 
  summarise(value = sum(value)) %>% 
  ggplot(aes(time, name, fill = value)) +
  
  # ggplot(aes(time, geo, fill = n)) +
  geom_tile(color = "black")

wv_df %>% 
  mutate(
    time = case_when(
      time == 3 ~ "1990",
      time == 4 | time == 5 ~ "2000", 
      time == 6 | time == 7 ~ "2010"
    )
  ) %>% 
  select(geo, !starts_with("G")) %>% 
  count(geo, time) %>% 
  na.omit() %>% 
  ggplot(aes(time, geo, fill = n)) + 
  geom_tile(color = "black")

3: 90
4-5: 2000
6-7: 2010


