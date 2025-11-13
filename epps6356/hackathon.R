library(tidyverse)
library(ggplot2)
data(iris)

#1

plot_data <- iris %>%
  mutate(
   sepal_length_group = cut(
      Sepal.Length,
      breaks = c(4, 5.5, 7.0, 8.0),
      labels = c("Small (4.0-5.5)", "Medium (5.6-7.0)", "Large (7.1-8.0)"),
      include.lowest = TRUE
    )
  ) %>%
  group_by(sepal_length_group) %>%
  summarise(
    count = n(), 
    avg_petal_length = mean(Petal.Length) 
  ) %>%
  mutate(
    xmax = cumsum(count),
    xmin = xmax - count,
    x_label_pos = (xmin + xmax) / 2
  )

ggplot(plot_data, aes(ymin = 0)) +
  geom_rect(
    aes(
      xmin = xmin,
      xmax = xmax,
      ymax = avg_petal_length,
      fill = sepal_length_group
    ),
    color = "white" 
  ) +
  scale_x_continuous(
    breaks = plot_data$x_label_pos, 
    labels = plot_data$count 
  ) +
  scale_fill_brewer(palette = "viridis", direction = -1) +
  labs(
    title = "Average Petal Length by Sepal Length Group",
    subtitle = "Column width is proportional to the number of flowers in each group",
    x = "Count of Flowers in Group",
    y = "Average Petal Length (cm)",
    fill = "Sepal Length Group"
  ) +
  # Apply a clean theme
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(), # Remove vertical grid lines
    panel.grid.minor.x = element_blank()
  )

library('gridExtra')
'2. I dont know what to put inside the grids so i just skipped it'





'3. Extract setona and versicolor from species.
Then create df_2 and df_3. Draw a bar plot using petal.width: p1 p2.
Finally, use gridExtra to combine the plots.'

df_2 <- subset(iris, Species %in% "setosa")
df_3 <- subset(iris, Species %in% "versicolor")
df_2$id <- 1:nrow(df_2)
df_3$id <- 1:nrow(df_3)



p1 = ggplot(df_2, aes(x = factor(id), y = Petal.Width)) + 
  geom_bar(stat = "identity", fill = 'red', color = "black") +
  coord_flip() +
  labs(title = "setosa") +
  theme(
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank(), 
    axis.text.x = element_blank(),  
    axis.ticks.x = element_blank()  #this was by GPT
  )
  

p2 = ggplot(df_3, aes(x = factor(id), y = Petal.Width)) + 
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  coord_flip() +
  labs(title = "versicolor")+
  theme(
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank(), 
    axis.text.x = element_blank(),  
    axis.ticks.x = element_blank()  #this was by GPT
  )
  


gridExtra::grid.arrange(p1, p2, ncol = 2)