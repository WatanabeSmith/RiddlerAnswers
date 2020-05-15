---
title: "Advantage_disadvantage"
output: html_document
---

```{r}
library(tidyverse)
rolld20 <- function(n=1){
  sample(1:20, n, replace = TRUE)
}
```

```{r}
single_die <- vector(mode = "numeric", length = 100000)
adv_vec <- vector(mode = "numeric", length = 100000)
dis_vec <- vector(mode = "numeric", length = 100000)
adv_of_dis_vec <- vector(mode = "numeric", length = 100000)
dis_of_adv_vec <- vector(mode = "numeric", length = 100000)
for(i in 1:100000){
  single_die[i] <- rolld20(1)
  #advantage
  adv_vec[i] <- max(rolld20(2))
  #disadvantage
  dis_vec[i] <- min(rolld20(2))
  #advantage of disadvantage
  adv_of_dis_vec[i] <- max(
    c(min(rolld20(2)),
      min(rolld20(2)))
  )
  #disadvantage of advantage
  dis_of_adv_vec[i] <- min(
    c(max(rolld20(2)),
      max(rolld20(2)))
  )
}

dfroll <- data.frame(single_die,
                     advantage = adv_vec,
                     disadvantage = dis_vec,
                     adv_on_dis = adv_of_dis_vec,
                     dis_on_adv = dis_of_adv_vec) %>% 
  select(advantage, dis_on_adv, single_die, adv_on_dis, disadvantage)

dfroll_long <- dfroll %>%
  mutate(id = row_number()) %>% 
  pivot_longer(cols = -id,
               names_to = "roll_type",
               values_to = "result") %>% 
  mutate(roll_type = factor(roll_type, levels = c("advantage",
                                                  "dis_on_adv",
                                                  "single_die",
                                                  "adv_on_dis",
                                                  "disadvantage"
                                                  )))
```

```{r}
summary(dfroll)

ggplot(dfroll_long, aes(x = roll_type, y = result)) +
  geom_boxplot() +
  labs(title = "Distribution of results by roll type",
       subtitle = "Ordered by highest average result",
       y = "Final die result",
       x = "Roll type")

ggsave("avg_result.jpg")

by_num_df <- data.frame()
for(i in 1:20){
  temp_df <- dfroll_long %>% 
   group_by(roll_type) %>% 
    summarise(pct_eq_or_over = mean(result >= i)) %>% 
    mutate(num = i)
  
  by_num_df <- bind_rows(by_num_df, temp_df)
}


ggplot(by_num_df, aes(x = num, y = pct_eq_or_over, group = roll_type,
                      color = roll_type)) +
  geom_line() +
  geom_point() +
  labs(title = "Odds to roll at least X by roll type",
       y = "Odds",
       x = "Result equal to or greater than X")


ggsave("odds_of_x.jpg")
```

