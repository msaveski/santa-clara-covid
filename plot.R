
library(tidyverse)
library(lubridate)
library(ggrepel)
library(fs)


setwd("~/code/santa-clara-covid")


# read all csv into a data frame and parse the dates
df <- dir_ls(path = "data", regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "date", col_types = "cddd") %>%
  mutate(
    date = str_extract(
      date,
      "[:digit:]{4}[:punct:][:digit:]{2}[:punct:][:digit:]{2}"
    ),
    date = ymd(date)
  )

# plot
highlighted_zips <- c(
  # stanford's zip
  "94301", 
  "94302",
  "94303",
  "94304",
  "94305",
  "94306"   # campus
)

# color the highlighted zips
df <- df %>% mutate(highlight = zipcode %in% highlighted_zips)

# find the last dates in the highlighted zips
data_ends <- df %>% 
  group_by(zipcode) %>% 
  top_n(1, date) %>% 
  filter(zipcode %in% highlighted_zips)

# ggplot it
plt <- df %>%
  ggplot(aes(x = date, y = Rate, color = highlight)) +
  geom_line(aes(group = zipcode)) +
  geom_text_repel(
    aes(label = zipcode),
    size = 3,
    nudge_x = 1.5,
    segment.color = "grey50",
    segment.size = 0.2,
    data = data_ends
  ) +
  scale_x_date(limits = c(min(df$date), max(df$date) + 3)) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = c("grey70", "black")) +
  labs(
    title = "Santa Clara COVID cases",
    subtitle = str_c("Data as of ", today()),
    x = "Date",
    y = "Rate (per 100k)"
  ) +
  theme_bw() + 
  theme(legend.position = "none")

print(plt)

ggsave("plot.png", width = 8, height = 6)

# END