############# Producing patient ward movement timelines with vistime

### Set working directory
# setwd("~/Documents/science/covid19/analysis/ward_timelines")

### Load packages
library("vistime")
library("tidyverse")
# library("RColorBrewer")
library("scales")
# library("cowplot")

### Generate input data


# timeline <- tribble(
#   ~cohort,            ~stage,             ~start,       ~end,         ~dataColDate,
#   "Cohort 1\n23/24",  "First year",       "2023-09-15", "2024-09-15", "2024-01-15", # "2023-10-01" 
#   "Cohort 1\n23/24",  "Middle year(s)",   "2024-09-15", "2025-09-15", NA,
#   "Cohort 1\n23/24",  "Final year",       "2025-09-15", "2026-09-15", "2025-10-01",
#   "Cohort 1\n23/24",  "After graduation", "2026-09-15", "2027-11-15", "2027-10-01"
# )


### Frame

c <- data.frame(matrix(NA, nrow = 4, ncol = 5))
colnames(c) <- c("cohort", "stage", "start", "end", "dataColDate")

c["stage"] <- c("First\nyear",
                "Middle\nyear(s)",
                "Final\nyear",
                "Post-\ngrad")
c$start <- as.Date(c$start)
c$end <- as.Date(c$end)
c$dataColDate <- as.Date(c$dataColDate)

### Cohorts
cP1 <- c
cP1["cohort"] <- "Cohort P1\n19/20"
startDate <- as.Date("2019-09-15")
# cP1[cP1$stage == "First year", "start"] <- "2023-09-15"

# 3-year course: start
for (x in 1:4)      {cP1[x,3] <- startDate + years(x-1)}
for (x in 1:4)      {cP1[x,4] <- cP1[x,3] + years(1)}
cP1[4,4] <- cP1[4,4] + months(1)

cP1$dataColDate[1] <- startDate + months(8)


cP2 <- c
cP2["cohort"] <- "Cohort P2\n20/21"
startDate <- as.Date("2020-09-15")

# 4-year course: start
for (x in 1:2)      {cP2[x,3] <- startDate + years(x-1)}
for (x in 3:4)      {cP2[x,3] <- startDate + years(x)}
for (x in c(1,3:4)) {cP2[x,4] <- cP2[x,3] + years(1)}
for (x in 2)        {cP2[x,4] <- cP2[x,3] + years(2)}
cP2[4,4] <- cP2[4,4] + months(1)

cP2$dataColDate[1] <- startDate + months(1)
cP2$dataColDate[3] <- cP2[3,3] + months(1)
cP2$dataColDate[4] <- cP2[4,4]


c1 <- c
c1["cohort"] <- "Cohort P3\nCohort 1\n23/24"
startDate <- as.Date("2023-09-15")

# 4-year course: start
for (x in 1:2)      {c1[x,3] <- startDate + years(x-1)}
for (x in 3:4)      {c1[x,3] <- startDate + years(x)}
for (x in c(1,3:4)) {c1[x,4] <- c1[x,3] + years(1)}
for (x in 2)        {c1[x,4] <- c1[x,3] + years(2)}
c1[4,4] <- c1[4,4] + months(1)

c1$dataColDate[1] <- startDate + months(4)
c1$dataColDate[3] <- c1[3,3] + months(1)
c1$dataColDate[4] <- c1[4,4]



### Data

timeline <- rbind(cP1, cP2, c1)


# write.csv(timeline, "timeline.csv", row.names = FALSE)

### Preparing for plot


timeline <- timeline |> 
  mutate(colour = case_when(stage == "First\nyear" & cohort != "Cohort P3\nCohort 1\n23/24" ~ "#8DD3C790",
                            stage == "Middle\nyear(s)" & cohort != "Cohort P3\nCohort 1\n23/24" ~ "#e3e3e380",
                            stage == "Final\nyear" & cohort != "Cohort P3\nCohort 1\n23/24" ~ "#6d6dab90",
                            stage == "Post-\ngrad" & cohort != "Cohort P3\nCohort 1\n23/24" ~ "#FB807290",
                            stage == "First\nyear" & cohort == "Cohort P3\nCohort 1\n23/24" ~ "#8DD3C790",
                            stage == "Middle\nyear(s)" & cohort == "Cohort P3\nCohort 1\n23/24" ~ "#e3e3e390",
                            stage == "Final\nyear" & cohort == "Cohort P3\nCohort 1\n23/24" ~ "#6d6dab90",
                            stage == "Post-\ngrad" & cohort == "Cohort P3\nCohort 1\n23/24" ~ "#FB807290"),
         shape = case_when(stage == "First\nyear" ~ 19,
                           stage == "Middle\nyear(s)" ~ NA,
                           stage == "Final\nyear" ~ 15,
                           stage == "Post-\ngrad" ~ 17)
         
         )

# inserting empty row at end for graphing space
timeline <- add_row(timeline, cohort = "")


### for testing
# timeline2 <- rename(timeline,
#                     group = cohort,
#                     event = stage,
#                     color = colour
# )


## Define date formats

format_x_1 <- function(x) {
  if_else(is.na(lag(x)) | !year(lag(x)) == year(x),
          paste(month(x, label = TRUE), "\n", year(x)),
          paste(month(x, label = TRUE)))
}

format_x_2 <- function(x) {
  months <- strftime(x, format = "%b")              # Abbreviated name of the month.
  years <- lubridate::year(x)                       # Year as a 4-digit number.
  if_else(is.na(lag(years)) | lag(years) != years,  # Conditions for pasting.
          true = paste(months, years, sep = "\n"), 
          false = months)
}

### Plotting

# Produce the basic plot
plot_timeline <- gg_vistime(data = timeline,
                        col.group = "cohort", # Each row will be a cohort
                        col.event = "stage", # Rows will be coloured by stage
                        col.color = "colour",
                        show_labels = TRUE, # Remove y labels; T = def
                        linewidth = 26,
                        # title = "Stage",
                        optimize_y = TRUE # T = def
                        )
plot_timeline


## Tweak the plot

plot_timeline2 <- plot_timeline + 

  ggplot2::theme(
    panel.background = element_rect(colour = "transparent"),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(size = 10, color = "black"),
    plot.margin = unit(c(0, 0.2, 0.2, 0.2), "inches"),
    axis.text.x = element_text(size = 9, color = "black", angle = 0, vjust = 1, hjust = 0)
    ) +

  geom_hline(yintercept = c(setdiff(seq_len(max(plot_timeline$data$y)),
                                  plot_timeline$data$y)), colour = "#FFFFFF") +       # white line over the grey line defined in vistime:::plot_ggplot
  
  # geom_text(aes(colour = "black", label = timeline$stage), size=3) +
  # ggrepel::geom_text_repel(aes(colour = "black", label = timeline$stage), direction = "y", segment.alpha = 0, point.padding = grid::unit(0.75, "lines")) +
  
  scale_x_datetime(
    breaks= c(as.POSIXct(min(timeline$start)) + days(30),
                             # as.POSIXct(timeline$dataColDate[1]) + days(15),
                             as.POSIXct(min(timeline$start)) + years(1),
                             as.POSIXct(min(timeline$start)) + years(3),
                             as.POSIXct(timeline$dataColDate[1]),
                             as.POSIXct(timeline$dataColDate[5])+days(0),
                             # as.POSIXct(timeline$dataColDate[7])+days(10),
                             as.POSIXct(timeline$dataColDate[7])+months(3),
                             as.POSIXct(timeline$dataColDate[8])+days(15),
                             as.POSIXct(timeline$dataColDate[11])+days(15)
                             # as.POSIXct(timeline$dataColDate[12])+days(15)
              ),
    labels = format_x_2,
    # expand = c(0.04,0),
    # breaks = breaks_width("360 days"),
    # labels = date_format("%b %Y")
    # limits = c(as.POSIXct("2023-08-15"), as.POSIXct("2028-01-01")),
    # labels = function(x) paste(month(x, label = TRUE), "\n", year(x))
    ) + 
  annotate("text", x = as.POSIXct(timeline$dataColDate[7]), y = 0.15, label = format_x_2(timeline$dataColDate[7]), size = 3.2, lineheight = 0.9, hjust = 1)  + 
  annotate("text", x = as.POSIXct(timeline$dataColDate[12]), y = 0.15, label = format_x_2(timeline$dataColDate[12]), size = 3.2, lineheight = 0.9, hjust = 1)  + 
  coord_cartesian(ylim = c(1,8.2), clip = "off") +
  annotate(
  "pointrange", x = as.POSIXct(timeline$dataColDate)[c(1)], y = 6.2, ymin = 6.2, ymax = 0.6, size = 0.7, colour = "#606060", shape = 19, stroke = 1.6) +
  annotate(
  "pointrange", x = as.POSIXct(timeline$dataColDate)[c(5,7,8)], y = 4.2, ymin = 4.2, ymax = 0.6, size = 0.7, colour = c("#606060", "#606060", "#FF000050"), shape = c(19, 15, 2), stroke = 1.6) +
  annotate(
  "pointrange", x = as.POSIXct(timeline$dataColDate)[c(9)]-months(3), y = 2.2, ymin = 2.2, ymax = 0.6, size = 0.7, colour = "#606060", shape = c(19), stroke = 1.6) +
  annotate(
  "pointrange", x = as.POSIXct(timeline$dataColDate)[c(9)], y = 2.2, ymin = 2.2, ymax = 0.6, size = 1, linewidth = 1.3, colour = c("black"), shape = c(19), stroke = 1.2) +
  annotate(
  "pointrange", x = as.POSIXct(timeline$dataColDate)[c(11, 12)], y = 2.2, ymin = 2.2, ymax = 0.6, size = 0.7, colour = c("#FF000050", "#FF000050"), shape = c(0, 2), stroke = 1.6) + 
  annotate("rect", xmin = as.POSIXct(timeline$dataColDate)[c(9)]-months(2), xmax = as.POSIXct(timeline$dataColDate)[c(9)]+months(8), ymin = -0.5, ymax = 3.93, alpha = .4) 

plot_timeline2


# ### Create a legend
# data_legend <- timeline_2 %>%
#   distinct(stage, .keep_all=T) %>%
#   arrange(stage)
# data_legend$start <- as.Date("2028-01-01")
# data_legend$end <- as.Date("2028-01-02")
# data_legend$Patient <- "Key"
# data_legend
# plot_legend <- gg_vistime(data = data_legend,
#                           col.group = "cohort",
#                           col.event = "stage",
#                           show_labels = TRUE,
#                           linewidth = 20,
#                           title = "Legend")
# plot_legend
# 
# # Tweak the legend plot
# plot_legend <- plot_legend + theme_void() +
#   ggplot2::theme(
#     plot.title = element_text(size=11),
#     axis.title.x=element_blank(),
#     axis.text.x=element_blank(),
#     axis.ticks.x=element_blank(),
#     axis.title.y=element_blank(),
#     axis.text.y=element_blank(),
#     axis.ticks.y=element_blank())
# plot_legend
# 
# 
# ### Combine the main plot and legend into a single figure
# plot_combined <- plot_grid(plot_data, plot_legend,
#                            rel_widths = c(1, 0.15))
# plot_combined

### Save plot
# ggplot2::ggsave(plot_combined, file = "timeline_plot_mock_data.pdf", dpi=300, height=4, width=7, units="in")


## Changing labels
g.d <- ggplot_build(plot_timeline2)
g.d$data[[4]]$size <- 3.5
rebuilt_timeline <- ggplot_gtable(g.d)

plot(rebuilt_timeline)
