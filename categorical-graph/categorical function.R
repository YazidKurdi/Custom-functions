library(tidyverse)
library(drlib)

categorical_graph_working <- function(dataframe,categorical_columns,max_categories = 10){
  dataframe %>%
  gather(category, value, categorical_columns) %>%
  count(category, value) %>%
  group_by(category) %>%
  top_n(max_categories, value) %>%
  ungroup() %>%
  mutate(value = reorder_within(value, n, category)) %>%
  ggplot(aes(value, n)) +
  geom_col() +
  facet_wrap(~ category, scales = "free_y") +
  scale_x_reordered() +
  coord_flip() +
  labs(title = "Categorical predictors",
       y = "Count")
  
}

categorical_graph(mtcars,categorical_columns = c("vs","am","mpg"),max_categories = 10)


