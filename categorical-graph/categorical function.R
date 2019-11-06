library(tidyverse)
library(drlib)

cat_graph <- function(dataframe,max_categories = 10){
  
    if(dataframe %>% map_lgl(~!is.factor(.))%>% all() == TRUE){
      stop("dataframe contains no factor columns")}
    else{dataframe %>%
        gather(category, value, which(map_lgl(., is.factor))) %>%
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
             y = "Count",
             x = "")}
  }

mtcars%>%
  mutate_at(vars(vs,am,carb,gear),~as.factor(.))%>%
  cat_graph()



