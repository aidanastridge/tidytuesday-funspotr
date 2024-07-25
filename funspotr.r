#tools
library(ggplot2)
library(packcircles)
library(patchwork)
library(paletteer)
library(dplyr)
#read data
raw <- "https://raw.githubusercontent.com/brshallo/funspotr-examples/main/data/funs/drob-tidy-tuesdays-funs-20220114.csv"
raw_df<- read.csv(raw)

#filter data
filtered_df <- raw_df %>%
  filter(pkgs != '(unknown)')

df <- filtered_df %>%
  group_by(pkgs) %>%
  summarise(n=n())

#circle plot
circle <- df %>%
  mutate(pkgs = ifelse(!pkgs %in% c('ggplot','dplyr','base'), 'Other', pkgs)) %>%
  group_by(pkgs) %>%
  summarize(n = sum(n)) %>%
  mutate(proportion = round((n / sum(n) * 100),0))

size <- circleProgressiveLayout(circle$proportion, sizetype='area')
size$radius <- 0.95*size$radius
circle <- cbind(circle,size)
vertices <- circleLayoutVertices(size, npoints=100)

circle_plot <- ggplot() + 
  
  # Make the bubbles
  geom_polygon(data = vertices, aes(x, y, group = id, fill=as.factor(id)), colour = "black") +
  
  # Add text in the center of each bubble + control its size
  geom_text(data = circle, aes(x, y, label = paste0("", pkgs,": ", proportion, "%", ""))) +
  scale_size_continuous(range = c(3,4)) +
  
  # General theme:
  theme_void() + 
  theme(legend.position="none") +
  coord_equal() + paletteer::scale_fill_paletteer_d("LaCroixColoR::Pamplemousse")

#bar plot  
bar <- df %>%
  group_by(pkgs) %>%
  filter(!pkgs %in% c('ggplot','dplyr','base')) %>%
  summarize(n = sum(n)) %>%
  mutate(proportion = round((n / sum(n) * 100),2)) %>%
  filter(proportion > 1)

  
bar_plot<- ggplot(bar, aes(x=pkgs, y=proportion)) +
  geom_point() + 
  geom_segment(aes(x=pkgs, y=0, yend=proportion)) +
  theme_light() +
  coord_flip() + labs(x="",y="%",title = "R Packages by Contribution", subtitle = "Packages within Other", caption = '47 packages omitted due to each contributing too less than 1% of the total; 12% in sum. ') + theme(plot.title = element_text(face="bold"))

layout <- c(
  area(t = 1, l = 1, b = 5, r = 6),
  area(t = 1, l = 3, b = 4, r = 6)
)

bar_plot + circle_plot + 
  plot_layout(design = layout) + plot_annotation(caption = 'Source: TidyTuesday. Aidan Astridge, 2024.')
