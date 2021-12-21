library(tidyverse)

f1_cols <- c("mercedes" = rgb(86, 206, 190, maxColorValue = 255),
             "red_bull" = rgb(0, 26, 232, maxColorValue = 255),
             "alfa_romeo" = rgb(134, 28, 10, maxColorValue = 255),
             "ferrari" = rgb(205, 48, 21, maxColorValue = 255),
             "mclaren" = rgb(244, 157, 47, maxColorValue = 255),
             "alpine" = rgb(50, 143, 249, maxColorValue = 255),
             "alphatauri" = rgb(47, 69, 96, maxColorValue = 255),
             "aston_martin" = rgb(42, 109, 98, maxColorValue = 255),
             "williams" = rgb(15, 93, 248, maxColorValue = 255),
             "haas" = rgb(150, 150, 150, maxColorValue = 255))

dats <- read_rds("f1dats.rds")

#Example ggplot2
ggplot(dats %>% 
         filter(constructor %in% c("McLaren", "Ferrari", "Mercedes", "Red Bull", "Aston Martin", "AlphaTauri", "Alfa Romeo", "Haas F1 Team", "Williams")) %>% 
         filter(year >= 1980) %>% 
         group_by(year, constructor) %>% 
         distinct(pointsSeasonConstructorFinal, positionSeasonConstructorFinal, pointsYearTotal) %>% 
         mutate(points_pct = pointsSeasonConstructorFinal / pointsYearTotal), 
       aes(year, points_pct, fill = constructor, label = positionSeasonConstructorFinal)) + 
  geom_area(position = position_dodge(width = 1), show.legend = FALSE) +
  geom_text(size = 3, show.legend = FALSE, vjust = -1, position = position_dodge(width = 1), col = "black") +
  facet_grid(constructor~.) 



#Legacy work from first day we worked on this
c_dat <- list()
for(i in 1958:2021) {
c_dat[[i]] <- getFinalF1Standings(i, type = "constructor") %>% mutate(year = i)
}
c_dats <- bind_rows(c_dat) %>% 
  group_by(year) %>% 
  mutate(max_position = max(position),
         total_points = sum(points)) %>% 
  ungroup() %>% 
  mutate(points_pct = points / total_points,
         position_size = max_position + 1 - position) %>% 
  mutate(team = case_when(constructorId %in% c("alfa", "bmw_sauber", "sauber") ~ "alfa_romeo",
                          constructorId %in% c("alphatauri", "toro_rosso", "minardi") ~ "alphatauri",
                          constructorId %in% c("alpine", "benetton", "renault", "lotus_f1") ~ "alpine",
                          constructorId %in% c("aston_martin", "jordan", "mf1", "spyker", "force_india", "racing_point") ~ "aston_martin",
                          constructorId %in% c("mercedes", "tyrrell", "bar", "honda", "brawn") ~ "mercedes",
                          constructorId %in% c("red_bull", "stewart", "jaguar") ~ "red_bull",
                          TRUE ~ constructorId))




c_order <- c_dats %>% group_by(team) %>% summarise(points = sum(points)) %>% arrange(desc(points))
c_dats$team <- factor(c_dats$team, levels = rev(c_order$team))

team_filter <- c("ferrari", 
                 "mclaren",
                 "mercedes", 
                 "williams", 
                 "haas", 
                 "aston_martin", 
                 "alphatauri", 
                 "alpine", 
                 "alfa_romeo", 
                 "red_bull"
                 )

c_plot <- c_dats %>% filter(team %in% team_filter)

ggplot(c_plot, 
       aes(year, points_pct, fill = team, label = position)) + 
  geom_bar(stat = "identity", position = "stack") +
  geom_label(size = 3, position = position_stack(vjust = 0.5), show.legend = FALSE) +
  scale_fill_manual(values = f1_cols) +
  scale_x_continuous(labels = paste(c_dats %>% filter(position == 1) %>% pull(team), c(1958:2021)), breaks = c(1958:2021)) +
  scale_y_continuous(name = "% of Points", breaks = seq(0, 1.1, by = 0.1), labels = c(seq(0, 1, by = 0.1)*100, "Winner"), 
                     limits = c(0, max(c_plot %>% group_by(year) %>% summarise(points = sum(points_pct)) %>% pull(points)))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.x = element_blank())

ggplot(c_plot %>% filter(year >= 1980), 
       aes(year, points_pct, fill = team, label = position)) + 
  geom_area() +
  facet_grid(team~.) +
  geom_text(size = 2.5, show.legend = FALSE, vjust = -1) +
  scale_fill_manual(values = f1_cols) +
  scale_x_continuous(labels = c(1958:2021), breaks = c(1958:2021)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.x = element_blank(),
        panel.grid = element_blank())

c_dats$team <- factor(c_dats$team, levels = c_order$team)
c_plot <- c_dats %>% filter(team %in% team_filter)
c_plot$year <- factor(c_plot$year, levels = rev(unique(c_plot$year)))

ggplot(c_plot %>% filter(as.numeric(as.character(c_plot$year)) >= 1980), 
       aes(year, max_position + 1 - position, fill = team, label = position)) + 
  geom_col(position = position_dodge(width = 1)) +
  facet_wrap(~year, scales = "free") +
  geom_text(size = 3, show.legend = FALSE, vjust = 1, position = position_dodge(width = 1), col = "white") +
  scale_fill_manual(values = f1_cols) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "top")


