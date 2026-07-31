library(tidyverse)
library(arrow)
library(cowplot)

st_fips <- sprintf("%02d", c(1, 3:14, 16:56)) # no AK, HI

flight_df_raw <- read_parquet("Year=2017/data_0.parquet") |>
  filter(Reporting_Airline %in% c("AA", "DL", "UA", "WN"),
         OriginStateFips %in% st_fips,
         DestStateFips %in% st_fips)

aa_hubs <- c("DFW", "CLT", "ORD", "PHL", "LGA", "LAX", "DCA", "AUS", "ATL", "PDX", "EGE", "ALB", "OAK", "AMA")
airport_df <- tibble(
  aa_hubs=aa_hubs,
  airline_airport = c(
    "American / Dallas/Fort Worth International Airport (DFW)",
    "American / Charlotte Douglas International Airport (CLT)",
    "American / Chicago O'Hare International Airport (ORD)",
    "American / Philadelphia International Airport (PHL)",
    "American / LaGuardia Airport (LGA)",
    "American / Los Angeles International Airport (LAX)",
    "American / Ronald Reagan Washington National Airport (DCA)",
    "American / Austin-Bergstrom International Airport (AUS)",
    "American / Hartsfield-Jackson Atlanta International Airport (ATL)",
    "American / Portland International Airport (PDX)",
    "American / Eagle County Regional Airport (EGE)",
    "American / Albany International Airport (ALB)",
    "American / Oakland International Airport (OAK)",
     "American / Rick Husband Amarillo International Airport (AMA)"
  ))

summarize_count <- function(data, block_size = 10, airports){
  data |>
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
    mutate(n = ifelse(type == "dep", n, -n))
}

assign_time_blocks <- function(time_vector, block_size = 10) {
  start_time <- min(time_vector)
  block_start <- start_time +
    floor(as.numeric(difftime(time_vector, start_time, units = "mins")) / block_size) * block_size * 60
  return(block_start)
}

aa_df <- flight_df_raw |>
  filter(Reporting_Airline == "AA", Year == 2017, (Origin %in% aa_hubs | Dest %in% aa_hubs)) |>
  summarize_count(airports = aa_hubs, block_size = 10) |>
  mutate(type = factor(type, levels = c('dep', 'arr'), labels = c("dep" = 'Departure', "arr" = 'Arrival'))) |>
  select(-airline_airport) |>
  left_join(airport_df, by = c("airport" = "aa_hubs")) |>
  mutate(airline_airport = factor(airline_airport, levels = airport_df$airline_airport))

write_csv(aa_df, file = here::here("data/aa_df.csv"))

color_list <- c("Arrival" = "#00a9b7", "Departure" = "#353F47")

fig3 <- aa_df |>
  ggplot(aes(x = block, y = n, color = type, fill = type)) +
  geom_col() +
  scale_x_datetime(date_labels =  "%H:%M", date_breaks = "4 hour") +
  theme_minimal() +
  xlab("Time of the date") +
  ylab("Count") +
  facet_wrap(vars(airline_airport), scales = "free_y", ncol = 2, dir = "v") +
  scale_fill_manual(name = "Flight type", values = color_list) +
  scale_color_manual(name = "Flight type", values = color_list) +
  xlab("Binned time (10 minute intervals)") +
  ylab("Annual Flight Count") +
  theme(legend.position = "bottom", text = element_text(colour = "black", size = 10))

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

plot_formatted <- function(df, air){

  xlabels <- paste0(seq(0, 24, 4), ':00')
  xbreaks <- ymd_hm(paste0('2017-01-01 ', xlabels))

  df_filtered <- df |>
    filter(airport == air)

  ymax <- df_filtered |>
    pull(n) |>
    abs() |>
    max()

  if (ymax > 1800){
    yupper <- ceiling(ymax/1000)*1000
  } else if (ymax > 1000){
    yupper <- ceiling(ymax/200)*200
  } else if (ymax > 200){
    yupper <- ceiling(ymax/100)*100
  } else {
    yupper <- ceiling(ymax/20)*20
  }

  ybreaks <- seq(-yupper, yupper, length.out = 5)
  ylabels <- abs(ybreaks)

  title <- df_filtered |>
    pull(airline_airport) |>
    unique()

  p <- df_filtered |>
    ggplot(aes(x = block, y = n, color = type, fill = type)) +
    geom_col() +
    scale_x_datetime(labels = xlabels, breaks = xbreaks) +
    scale_y_continuous(labels = ylabels, breaks = ybreaks) +
    scale_fill_manual(name = "Flight type", values = color_list) +
    scale_color_manual(name = "Flight type", values = color_list) +
    labs(title = title) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 8)) +
    coord_cartesian(ylim = c(-yupper, yupper))
  return(color_y_axis_labels(p, breaks = ybreaks))
}

aa_hubs <- c("DFW", "CLT", "ORD", "PHL", "LGA", "LAX", "DCA", "AUS", "ATL", "PDX", "EGE", "ALB", "OAK", "AMA")

xlab <- "Binned time (10 minute intervals)"
ylab <- "Annual Flight Count"

# Create shared x/y axis title
x_title <- gridtext::richtext_grob(xlab, gp = gpar(fontsize = 10))
y_title <- gridtext::richtext_grob(ylab, rot = 90, gp = gpar(fontsize = 10))

plots_list <- lapply(aa_hubs, function(hub){
  plot_formatted(aa_df, hub)
})

combined <- gridExtra::arrangeGrob(
  grobs = plots_list,
  as.table = FALSE,
  ncol = 2,
  nrow = 7,
  left = y_title,
  bottom = x_title)

legend <- cowplot::get_legend(fig3)

final <- gridExtra::arrangeGrob(
  combined,
  legend,
  ncol = 1,
  heights = c(1, 0.08)
)

grid::grid.newpage()
grid::grid.draw(final)

ggsave(plot = final,
       filename = "figures/fig3.png",
       height = 18, width = 18, unit = "cm", bg = "white")
