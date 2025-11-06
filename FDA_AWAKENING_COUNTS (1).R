#helloworldnew
library(ggplot2)
library(dplyr) 
awakening_data <- read.csv("FDA_Awakening_Counts - FDA_Awakening_Counts.csv")
awakening_data
awakening_data <- awakening_data %>%
  mutate(
    Subject = as.factor(Subject),
    Nights = as.numeric(Nights),
    AwakeningCount = as.numeric(AwakeningCount)
  ) #upload data, separate columns for each nights
#making the regression plot
ggplot(awakening_data, aes(x= Nights, y= AwakeningCount, color= Subject))+
  geom_point()+
  geom_smooth(method = "lm", se = TRUE, linewidth = 1, linetype = "solid", fill= "grey")+
  facet_wrap(~Subject)+
  scale_x_continuous(
    limits = c(1, max(awakening_data$Night)),
    breaks = seq(1, max(awakening_data$Night), 1)
  ) +
  scale_color_brewer(palette = "Dark2")+
  labs(
    title= "Awakening Count Trends Over 2 Weeks By Subject",
    x= "Nights",
    y= "Awakening Count",
    color= "Subject"
  )+
  theme_minimal() +
  theme(
    axis.line = element_line(color = "grey40", linewidth = 0.8),  # bold axis lines
    axis.ticks = element_line(color = "grey40", linewidth = 0.8),   # bold tick marks
    theme(panel.grid.major.x = element_blank()),
    strip.background = element_blank(),
    panel.spacing = unit(1, "lines"), 
    panel.border = element_rect(color = "grey40", fill = NA),
    text = element_text(size = 15),
    plot.title = element_text(hjust = 0.5, face= "bold"),
    legend.position = "right"
  )
