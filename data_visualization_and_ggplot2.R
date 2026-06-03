library(tidyverse)
library(Lahman)

#goals of data visualization
#represent the data in a visual way
#deliver the info to audience

theme_set(theme_bw())
theme_update(# your personalized changes
  legend.position = "bottom")

yearly_batting <- Batting |>
  filter(lgID %in% c("AL", "NL")) |>
  group_by(yearID) |>
  summarize(total_h = sum(H, na.rm = TRUE),
            total_hr = sum(HR, na.rm = TRUE),
            total_so = sum(SO, na.rm = TRUE),
            total_bb = sum(BB, na.rm = TRUE),
            total_ab = sum(AB, na.rm = TRUE)) |>
  mutate(batting_avg = total_h / total_ab)

#ggplot2 provides an implementation of grammar graphics
#specify plotting "layers" and combine them to produce a graphic
# 1. data - one or more data sets
# 2. geom - geometric objects to represent the data 
# 3. aes - mappings of variables to visual properties of the geometric objects
# 4. scale - one scale for each variable displayed
# 5. facet - similar subplots for subsets of the same data using a conditioning variable
# 6. stat - statistical transformation or summaries (i.e. identity, count, smooth, quantile)
# 7. coord - one or more coordinate system (i.e. cartesian, polar)
# 8. labs - labels/guides for each variable and other parts of the plot
#9. theme - customization of the plot

# walkthrough of ggplot2

#yearly_batting |>
#  ggplot() +
#. geom_point(aes(x = yearID, y = total_hr))


#ggplot(data = yearly_batting, aes(x = yearID, y = total_hr)) +
#  geom_point() + +
#  geom_line()

yearly_batting |>
  ggplot() +
  geom_line(aes(x = yearID, y = total_hr, color = "darkred", linetype = "dashed")) +
  geom_point(aes(x = yearID, y = total_hr, color = total_hr, size = total_bb)) 
  #scale_x_continuous(limits = c(2000, 2015))
  #scale_x_reverse() +
  #scale_y_log10()
  
  
  
  
  
  scale_x_continuous(breaks = seq(1880, 2020, 20)) +
  scale_y_continuous(breaks = seq(0, 6000, 1000))



yearly_batting |>
  ggplot(aes(x = yearID, y = total_hr)) +
  geom_line(color = "darkred", linetype = "dashed") +
  geom_point(aes(color = total_so, 
                 size = total_bb)) +
  scale_color_gradient(low = "darkblue", high = "gold") +
  scale_size_continuous(breaks = seq(0, 2000, 2500)) +
  labs(x = "Year", 
       y = "Total Homeruns",
       color = "Strikeouts", 
       size = "Walks",
       title = "The rise of the three true outcomes in baseball",
       caption = "Data courtesy by Lahman") +
  theme_bw() +
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5,
                                  face = "bold"))
#scale_x_continuous(limits = c(2000, 2015))
#scale_x_reverse() +
#scale_y_log10()

#simpler is better
#pivoting
#pivot_longer: casts/gathers information spread out across variables or 
# basically just moves a certain column to a cell inside another column. 
# then the data goes into its own column. data spread out from multiple column
# now goes into one with the type of data in a different column


yearly_batting |>
  select(yearID, HRs = total_hr, Strikeouts = total_so, Walks = total_bb) |>
  pivot_longer(cols = HRs:Walks, names_to = "stat",
               values_to = "value") |>
  #to undo it
  #pivot_wider(id_cols = yearID, names_from = stat, values_from = value)
  ggplot(aes(x = yearID, y = value)) +
  geom_line(color = "darkblue") +
  geom_point(color = "darkblue", size = 0.6) +
  facet_wrap(~stat, scales = "free_y", ncol = 1) + #our pivot longer (
# because the HR SO and W are in the same column, this allows us to 
# facet wrap by the stat)
  labs(x = "Year",
       y = "Total of statistics",
       title = "The rise of the three true outcomes in baseball",
       caption = "Data courtesy of Lahman") +
  theme_bw() +
  theme(strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5,
                                  face = "bold"))


library(babynames)

babynames <- babynames

babynames |>
  filter(name %in% c("Kendall", "Mary", "Stephanie") & sex == "F") |>
  ggplot(aes(x = year, y = n, color = name)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = 2007, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 1980, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = 2010, linetype = "dashed", color = "green") +
  labs(x = "Year", y = "Number of babies")

babynames |>
  filter(name == "Alex") |>
  group_by(year) |>
  summarise(total = sum(n)) |>
  ggplot(aes(x = year, y = n)) +
  geom_point() +
  geom_line()

 









