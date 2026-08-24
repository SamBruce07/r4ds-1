#
#
#
#
#
#
#
#
#
#| message: false
library(tidyverse)
#
#
#
billboard |>
  select(artist, track, date.entered, wk1:wk4)

billboard_long <- billboard |>
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    names_prefix = "wk",
    values_to = "rank"
  ) |>
  mutate(week = as.integer(week))

  top_10_songs <- billboard_long |>
    group_by(artist, track, date.entered) |>
    filter(any(rank <= 10, na.rm = TRUE)) |>
    ungroup() |>
    mutate(
      highlight = case_when(
        artist == "Madonna" & track == "Music" ~ "Madonna - Music",
        artist == "Lonestar" & track == "Amazed" ~ "Lonestar - Amazed",
        artist == "Creed" & track == "Higher" ~ "Creed - Higher",
        TRUE ~ "Other top-10 songs"
      )
    )

  ggplot(top_10_songs, aes(x = week, y = rank, group = interaction(artist, track))) +
    geom_line(
      data = filter(top_10_songs, highlight == "Other top-10 songs"),
      color = "gray70",
      alpha = 0.4
    ) +
    geom_line(
      data = filter(top_10_songs, highlight != "Other top-10 songs"),
      aes(color = highlight),
      linewidth = 1
    ) +
    scale_color_manual(
      values = c(
        "Madonna - Music" = "#D55E00",
        "Lonestar - Amazed" = "#0072B2",
        "Creed - Higher" = "#009E73"
      ),
      name = "Notable songs"
    ) +
  scale_y_reverse() +
  labs(x = "Week", y = "Ranking")
#
#
#
summary(billboard$date.entered)
#
#
#
ranking_columns <- c("wk1", "wk4", "wk10", "wk20", "wk40", "wk76")

billboard |>
  summarise(
    across(
      all_of(ranking_columns),
      list(missing = ~ sum(is.na(.x)), present = ~ sum(!is.na(.x))),
      .names = "{.col}_{.fn}"
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = c("week", ".value"),
    names_sep = "_"
  )
#
#
#
rank_comparison <- billboard |>
  mutate(
    result = case_when(
      is.na(wk6) ~ "Missing by week 6",
      wk6 < wk1 ~ "Improved",
      wk6 == wk1 ~ "Stayed the same",
      wk6 > wk1 ~ "Got worse"
    )
  )

rank_comparison |>
  count(result, name = "songs")

median_change <- rank_comparison |>
  filter(!is.na(wk1), !is.na(wk6)) |>
  summarise(median_rank_change = median(wk1 - wk6))

median_change
#
#
#
billboard_long |>
  group_by(artist, track, date.entered) |>
  summarise(
    reentered = any(diff(which(!is.na(rank))) > 1),
    .groups = "drop"
  ) |>
  count(reentered, name = "songs")
#
#
#
song_summary <- billboard_long |>
  group_by(artist, track, date.entered) |>
  summarise(
    first_rank = first(rank, na_rm = TRUE),
    best_rank = min(rank, na.rm = TRUE),
    first_week_at_best = min(
      week[!is.na(rank) & rank == min(rank, na.rm = TRUE)]
    ),
    total_weeks = sum(!is.na(rank)),
    .groups = "drop"
  )

song_summary

notable_songs <- bind_rows(
  song_summary |>
    filter(best_rank == 1) |>
    slice_min(first_week_at_best, n = 1, with_ties = FALSE) |>
    mutate(notable = "Fastest to number one"),
  song_summary |>
    filter(best_rank == 1) |>
    slice_max(first_week_at_best, n = 1, with_ties = FALSE) |>
    mutate(notable = "Slowest to number one"),
  song_summary |>
    filter(best_rank <= 10) |>
    slice_max(total_weeks, n = 1, with_ties = FALSE) |>
    mutate(notable = "Longest top-10 chart run")
) |>
  select(notable, artist, track, best_rank, first_week_at_best, total_weeks)

notable_songs
#
#
#
#| cache: true
music <- read_csv("data/music.csv", show_col_types = FALSE)
#
#
#
names(music)
#
#
#
music |>
  select(starts_with("artist.")) |>
  glimpse()
#
#
#
music |>
  select(
    artist.name,
    artist.location,
    artist.latitude,
    artist.longitude,
    artist.terms,
    artist.familiarity,
    artist.hotttnesss,
    song.title,
    song.year
  ) |>
  slice_head(n = 8)
#
#
#
#
#
#
#
music |>
  summarise(
    across(
      c(artist.familiarity, artist.hotttnesss, song.year, song.tempo),
      list(
        minimum = ~ min(.x, na.rm = TRUE),
        lower_quartile = ~ quantile(.x, 0.25, na.rm = TRUE, names = FALSE),
        median = ~ median(.x, na.rm = TRUE),
        upper_quartile = ~ quantile(.x, 0.75, na.rm = TRUE, names = FALSE),
        maximum = ~ max(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = c("variable", "statistic"),
    names_sep = "_",
    values_to = "value"
  )
#
#
#
song_year_data <- music |>
  filter(song.year != 0, !is.na(song.year))

ggplot(song_year_data, aes(x = song.year)) +
  geom_histogram(binwidth = 5, boundary = 0) +
  labs(
    x = "Song year",
    y = "Number of songs",
    subtitle = paste("Number of songs used:", nrow(song_year_data))
  )
#
#
#
music |>
  summarise(
    across(
      c(
        artist.location,
        release.name,
        song.title,
        song.year,
        artist.familiarity,
        artist.hotttnesss
      ),
      ~ sum(is.na(.x) | .x == 0)
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = "column",
    values_to = "placeholder_rows"
  )
#
#
#
#
