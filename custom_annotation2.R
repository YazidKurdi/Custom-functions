
library(tidyverse)
library(grid)



annotation_custom2 <- 
  function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data) 
  {
    layer(data = data, stat = StatIdentity, position = PositionIdentity, 
          geom = ggplot2:::GeomCustomAnn,
          inherit.aes = TRUE, params = list(grob = grob, 
                                            xmin = xmin, xmax = xmax, 
                                            ymin = ymin, ymax = ymax))
  }


grob <- grobTree(textGrob("Scatter plot", x=0.1,  y=0.95, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))

mtcars%>%
  ggplot(aes(mpg,disp))+
  geom_point()+
  facet_grid(vs~am)+
  #annotation_custom(grob)
  annotation_custom2(grob,data = mtcars %>% filter(vs == 1, am == 1))
