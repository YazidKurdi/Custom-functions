library(tidyverse)
library(ggbeeswarm)
library(ggtext)
library(grid)
library(cowplot)

df_lifts_raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-08/ipf_lifts.csv")

take_median <- function(x) {
  map_dbl(strsplit(x, "-"), ~median(as.numeric(.x)))
}


df_lifts_filter <- 
  df_lifts_raw %>% 
  filter(event != "SB", 
         age > 10, 
         age_class != "5-12",
         !place %in% c("DQ", "DD"), 
         equipment != "Wraps") %>% 
  mutate(age = ifelse(is.na(age) && !is.na(age_class), 
                      take_median(age_class), age))

df_lifts_beeswarm <- 
  df_lifts_filter %>% 
  select(sex, age_class, contains("kg"), equipment, date) %>% 
  gather(perform, max_weight_kg, best3squat_kg:best3deadlift_kg) %>% 
  mutate(perform = str_extract(perform, "squat|bench|deadlift")) %>% 
  drop_na() %>% 
  mutate(age_class = as.numeric(factor(age_class)))

df_lifts_smooth <- 
  df_lifts_beeswarm %>% 
  group_by(sex, age_class, equipment, perform) %>% 
  summarize(max_wt_kg = max(max_weight_kg)) %>% 
  ungroup() %>% 
  mutate(age_class = as.numeric(factor(age_class)))

xlbl <- df_lifts_filter %>% 
  drop_na() %>% 
  pull(age_class) %>% 
  unique() %>% 
  sort()

title_beeswarm <- "Gear up for better performance!"
sub_beeswarm <- "The following graph shows how age and equipment affect atheletes' performances on 
                benches, squats and deadlifts. Athletes equipped with single-plys are likely to lift 
                heavier weights than those compete with bare hands."

annotation_custom2 <- 
  function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data) 
  {
    layer(data = data, stat = StatIdentity, position = PositionIdentity, 
          geom = ggplot2:::GeomCustomAnn,
          inherit.aes = TRUE, params = list(grob = grob, 
                                            xmin = xmin, xmax = xmax, 
                                            ymin = ymin, ymax = ymax))
  }

legend_txt <- "The smooth lines fitted maximum weights \never lifted merely for displaying trends"
legend_caption <- textGrob(legend_txt, hjust = 0,x=0.62,  y=0.73, gp = gpar(fontsize = 7.5, fontfamily = "italic"))


p <- ggplot() +
  geom_quasirandom(data = df_lifts_beeswarm, 
                   aes(age_class, max_weight_kg, color = sex),
                   alpha = .5, size = 2) + 
  geom_smooth(data = df_lifts_smooth, 
              aes(x = age_class, y = max_wt_kg, color = sex, group = sex),
              method = "loess", se = F) + 
  scale_color_manual(
    name = "", values = c("#AB82FF", "#4876FF"), 
    labels = c("Women", "Men"),
    guide = guide_legend(reverse = TRUE)
  ) + 
  scale_x_continuous(
    breaks = seq_len(length(xlbl)),
    labels = xlbl, 
    limits = c(.5, 15.5)
  ) +
  labs(
    x = "Age Class", y = "Maximum Weights Lifted", 
    title = 
  ) +
  facet_grid(perform ~ equipment, switch = "y") + 
  theme_minimal(base_size = 16, base_family = "Impact") + 
  theme(
    strip.placement = "outside", 
    strip.text.y = element_text(size = 12), 
    legend.position = c(.9, .65), 
    legend.text = element_text(size = 14), 
    axis.text.x = element_text(size = 12, angle = 30)
  )+
  annotation_custom2(data = df_lifts_beeswarm%>%
                       filter(equipment == "Single-ply", perform == "deadlift"),grob = legend_caption)


  ggsave(plot = p,filename = "/Users/USER10/Desktop/10-09-2019 Powerlifting/Chuck.png", 
         height = 20, width = 30.9, units = "cm")
