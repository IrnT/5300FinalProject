library(tidyverse)

bigPapa <- read.csv("./data/finalset.csv")

head(bigPapa)

bigPapa <- bigPapa[1:191702,] #dropping section that doesn't have weather data

tail(bigPapa)

#bigPapa$UniqueID <- rep(1:(nrow(bigPapa) / 2), each = 2)
bigPapa <- bigPapa %>% 
  mutate(UniqueID = rep(1:(n() / 2), each = 2))

head(bigPapa)

summarizedPapa <- bigPapa %>%
  group_by(UniqueID) %>%
  summarize(ConsumptionTarget = sum(target[is_consumption == 1]),
            ProductionTarget = sum(target[is_consumption == 0]))

#drop redundant cells from bigPapa
#grab distinct
#merge along uniqueID

uniquePapa <- bigPapa %>% 
  select(-c(X, row_id, target, is_consumption)) %>%
  distinct()

head(uniquePapa)

biggestPapa <- uniquePapa %>%
  left_join(summarizedPapa)

head(biggestPapa)

#bigPapa %>%
#  mutate(activity = ifelse(is_consumption == 1, "consumption", "production")) %>% 
#  select(UniqueID, activity, target) %>% 
#  pivot_wider(id_cols = UniqueID, names_from = activity, values_from = target)

#bigPapa %>% select(-c(row_id, target, is_consumption)) %>% distinct()

write.csv(biggestPapa, './data/tidyset.csv')


