library(tidyverse)
library(arrow)
library(grid)
library(gridExtra)
library(gtable)
library(gridtext)

st_fips <- sprintf("%02d", c(1, 3:14, 16:56)) # no AK, HI

flight_df_raw <- read_parquet("Year=2017/data_0.parquet") |>
  filter(Reporting_Airline %in% c("AA", "DL", "UA", "WN"),
         OriginStateFips %in% st_fips,
         DestStateFips %in% st_fips)

airports <- c('AUS', 'DFW')
block_size <- 10

assign_time_blocks <- function(time_vector, block_size = 10) {
  start_time <- min(time_vector)
  block_start <- start_time +
    floor(as.numeric(difftime(time_vector, start_time, units = "mins")) / block_size) * block_size * 60
  return(block_start)
}

two_df <- flight_df_raw |>
  filter(Reporting_Airline == "AA", Year == 2017,
         (Origin %in% c("AUS", "DFW") | Dest %in% c("AUS", "DFW"))) |>
  filter(!is.na(DepTime), !is.na(ArrTime)) |>
  mutate(DepTime = as_datetime(paste0("2017-01-01", "-", DepTime, "-00")),
         ArrTime = as_datetime(paste0("2017-01-01", "-", ArrTime, "-00"))) |>
  select(Reporting_Airline, FlightDate, DepTime, ArrTime, Origin, Dest) |>
  rename(dep_time = DepTime, arr_time = ArrTime, airline = Reporting_Airline,
         dep_airport = Origin, arr_airport = Dest) |>
  pivot_longer(cols = -c(FlightDate, airline),
               names_to = c("type", ".value"), names_sep = "_") |>
  filter(airport %in% airports) |>
  mutate(block = assign_time_blocks(time, block_size)) |>
  count(airline, airport, type, block) |>
  mutate(airline_airport = paste(airline, airport, sep = "/ ")) |>
  mutate(n = ifelse(type == "dep", n, -n)) |>
  mutate(type = factor(type, levels = c('dep', 'arr'), labels = c("dep" = 'Departure', "arr" = 'Arrival'))) |>
   mutate(airline_airport = factor(airline_airport,
                                  labels = c("American / Austin-Bergstrom International Airport (AUS)",
                                             "American / Dallas/Fort Worth International Airport (DFW)")))

write_csv(two_df, file = here::here("data/two_df.csv"))

# Plot aesthetics
color_list <- c("Arrival" = "#00a9b7", "Departure" = "#353F47")
xlabels <- paste0(seq(0, 24, 4), ':00')
xbreaks <- ymd_hm(paste0('2017-01-01 ', xlabels))
ausbreaks <- seq(-200, 600, 200)
auslabels <- abs(ausbreaks)
austitle <- 'American / Austin-Bergstrom International Airport (AUS)'
dfwbreaks <- seq(-4000, 4000, 2000)
dfwlabels <- abs(dfwbreaks)
dfwtitle <- 'American / Dallas/Fort Worth International Airport (DFW)'
xlab <- "Binned time (10 minute intervals)"
ylab <- "Annual Flight Count"

# Function to color y axis labels in new plots
color_y_axis_labels <- function(p,
                                breaks,
                                neg = "#00a9b7",
                                pos = "#353F47",
                                zero = "black") {

  gt <- ggplotGrob(p)

  axis_id <- which(gt$layout$name == "axis-l")

  axis_grob <- gt$grobs[[axis_id]]

  find_text_grob <- function(g) {

    if (inherits(g, "text")) return(g)

    if (!is.null(g$children)) {
      for (child in g$children) {
        out <- find_text_grob(child)
        if (!is.null(out)) return(out)
      }
    }

    if (!is.null(g$grobs)) {
      for (child in g$grobs) {
        out <- find_text_grob(child)
        if (!is.null(out)) return(out)
      }
    }

    NULL
  }

  replace_text_grob <- function(g, new_text) {

    if (inherits(g, "text")) {
      return(new_text)
    }

    if (!is.null(g$children)) {
      for (i in seq_along(g$children)) {
        g$children[[i]] <-
          replace_text_grob(g$children[[i]], new_text)
      }
    }

    if (!is.null(g$grobs)) {
      for (i in seq_along(g$grobs)) {
        g$grobs[[i]] <-
          replace_text_grob(g$grobs[[i]], new_text)
      }
    }

    g
  }

  txt <- find_text_grob(axis_grob)

  if (length(txt$label) != length(breaks)) {
    stop("Number of breaks does not match number of axis labels.")
  }

  txt$gp$col <- ifelse(
    breaks < 0, neg,
    ifelse(breaks > 0, pos, zero)
  )

  axis_grob <- replace_text_grob(axis_grob, txt)

  gt$grobs[[axis_id]] <- axis_grob

  gt
}

# The new plot (reviewer comments)
aus <- two_df |>
  filter(airport == 'AUS') |>
  ggplot(aes(x = block, y = n, color = type, fill = type)) +
  geom_col() +
  scale_x_datetime(labels = xlabels, breaks = xbreaks) +
  scale_y_continuous(labels = auslabels, breaks = ausbreaks) +
  scale_fill_manual(name = "Flight type", values = color_list) +
  scale_color_manual(name = "Flight type", values = color_list) +
  labs(title = austitle) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 8))

dfw <- two_df |>
  filter(airport == 'DFW') |>
  ggplot(aes(x = block, y = n, color = type, fill = type)) +
  geom_col() +
  scale_x_datetime(labels = xlabels, breaks = xbreaks) +
  scale_y_continuous(labels = dfwlabels, breaks = dfwbreaks) +
  scale_fill_manual(name = "Flight type", values = color_list) +
  scale_color_manual(name = "Flight type", values = color_list) +
  labs(title = dfwtitle) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 8)) +
  coord_cartesian(ylim = c(-4000, 4000))

dfw_legend <- cowplot::get_legend(dfw)

aus_col <- color_y_axis_labels(aus, ausbreaks)
dfw_col <- color_y_axis_labels((dfw + theme(legend.position = 'none')), dfwbreaks)

plots <- gridExtra::arrangeGrob(aus_col, dfw_col, ncol = 1)

# Create shared x/y axis title
x_title <- gridtext::richtext_grob(xlab, gp = gpar(fontsize = 10))
y_title <- gridtext::richtext_grob(ylab, rot = 90, gp = gpar(fontsize = 10))

# Add labels around the combined plots
combined <- gridExtra::arrangeGrob(plots, left = y_title, bottom = x_title)
final <- gridExtra::arrangeGrob(combined, bottom = dfw_legend)

# Finally save this!
ggsave(plot = final, filename = "figures/fig2.png",
       height = 10, width = 18, unit = "cm", bg = "white")

# Get the total number of arrivals and departure counts (for main text)
arr_dep_sum <- two_df |>
  mutate(n = abs(n)) |>
  group_by(airline, airport) |>
  summarise(total_flights = sum(n), .groups = 'drop')
