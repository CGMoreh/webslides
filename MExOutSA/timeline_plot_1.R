############# Producing patient ward movement timelines with vistime

### Set working directory
# setwd("~/Documents/science/covid19/analysis/ward_timelines")

### Load packages
library("vistime")
library("tidyverse")
library("RColorBrewer")
library("scales")
library("cowplot")

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

c["stage"] <- c("First \nyear",
                "Middle \nyear(s)",
                "Final \nyear",
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

cP1$dataColDate[1] <- startDate + months(8)


cP2 <- c
cP2["cohort"] <- "Cohort P2\n20/21"
startDate <- as.Date("2020-09-15")

# 4-year course: start
for (x in 1:2)      {cP2[x,3] <- startDate + years(x-1)}
for (x in 3:4)      {cP2[x,3] <- startDate + years(x)}
for (x in c(1,3:4)) {cP2[x,4] <- cP2[x,3] + years(1)}
for (x in 2)        {cP2[x,4] <- cP2[x,3] + years(2)}

cP2$dataColDate[1] <- startDate + months(1)
cP2$dataColDate[3] <- cP2[3,3] + months(1)
cP2$dataColDate[4] <- cP2[4,4]


c1 <- c
c1["cohort"] <- "Cohort 1\n23/24"
startDate <- as.Date("2023-09-15")

# 4-year course: start
for (x in 1:2)      {c1[x,3] <- startDate + years(x-1)}
for (x in 3:4)      {c1[x,3] <- startDate + years(x)}
for (x in c(1,3:4)) {c1[x,4] <- c1[x,3] + years(1)}
for (x in 2)        {c1[x,4] <- c1[x,3] + years(2)}

c1$dataColDate[1] <- startDate + months(4)
c1$dataColDate[3] <- c1[3,3] + months(1)
c1$dataColDate[4] <- c1[4,4]



### Timeline

timeline <- rbind(cP1, cP2, c1)

# inserting row at end
timeline[13,] <- c("", NA, NA, NA, NA, NA, NA)


# write.csv(timeline, "timeline.csv", row.names = FALSE)

### Preparing for plot
## Colours

# # Define number of colours needed
# cols_n <- as.numeric(n_distinct(timeline$stage))
# 
# # Check we have enough colours
# ifelse(cols_n>12, 
#        "More than 12 wards - not enough colours!",
#        "12 wards or fewer - using Set3 colours")
# 
# # Select the colours from Set3 palette in RColorBrewer
# cols_to_use <- brewer.pal(n = cols_n, name = "Set3")
# 
# # Create mapping of colours to wards
# col_stage_mapping <- data.frame(stage=unique(c(as.character(timeline$stage))), color=cols_to_use)
# 
# 
# 
# # merge in the mapping to the df
# timeline_2 <- merge(timeline,
#                       col_stage_mapping,
#                       by="stage",
#                       all.x=T,all.y=T) %>%
#   select(cohort, dataColDate, stage, start, end, color) %>%
#   # mutate(start = as.POSIXlt(start),
#   #        end = as.POSIXlt(end),
#   #        dataColDate = as.POSIXlt(dataColDate)) |> 
#   arrange(dataColDate, cohort, start)
# timeline_2
# 
# ## Extract swab dates
# dataColDates <- timeline_2 %>%
#   select(stage, dataColDate) %>%
#   distinct(stage, .keep_all=TRUE) %>%
#   arrange(dataColDate)


### Plotting

timeline <- timeline |> 
  mutate(colour = case_when(stage == "First \nyear" ~ "#8DD3C7",
                            stage == "Middle \nyear(s)" ~ "#e3e3e3",
                            stage == "Final \nyear" ~ "#6d6dab80",
                            stage == "Post-\ngrad" ~ "#FB807280"),
         shape = case_when(stage == "First \nyear" ~ 19,
                           stage == "Middle \nyear(s)" ~ NA,
                           stage == "Final \nyear" ~ 15,
                           stage == "Post-\ngrad" ~ 17))

# Produce the basic plot
plot_timeline <- gg_vistime(data = timeline,
                        col.group = "cohort", # Each row will be a cohort
                        col.event = "stage", # Rows will be coloured by stage
                        col.color = "colour",
                        show_labels = TRUE, # Remove y labels 
                        linewidth = 25,
                        # title = "Stage"
                        )
plot_timeline

plot_timeline



# Tweak the plot

## Date formats

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




plot_timeline2 <- plot_timeline + theme_void() +

  ggplot2::theme(
    # panel.grid = element_blank(),
    # panel.border = element_blank(),
    # plot.title = element_text(size=14),
    axis.text.y = element_text(size = 10, color = "black"),
    # axis.line.y = element_blank(), 
    # axis.ticks.y = element_blank(), 
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    # panel.background = element_blank(),
    # axis.text.x = element_blank())
    # scale_x_continuous(breaks=seq(2023,2028,5)),
    axis.text.x = element_text(size = 9, color = "black", angle = 0, vjust = 1, hjust = 0)) +
    # scale_x_datetime(date_breaks = c(as.POSIXct("2023-10-15")),
    #                  expand = c(0.04,0),
    #               # breaks = breaks_width("360 days"),
    #                # labels = date_format("%b"),
    #                # limits = c(as.POSIXct("2023-08-15"), as.POSIXct("2028-01-01")),
    #                labels = 
# 
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
     # date_labels="%b-%y"
     expand = c(0.03,0),
     # labels = function(x) paste(month(x, label = TRUE), "\n", year(x))
     labels = format_x_2,
     # labels = date_format("%b %Y")
     ) + 
  annotate("text", x = as.POSIXct(timeline$dataColDate[7]), y = 0.35, label = format_x_2(timeline$dataColDate[7]), size = 3, lineheight = 1, hjust = 1)  + 
  annotate("text", x = as.POSIXct(timeline$dataColDate[12]), y = 0.35, label = format_x_2(timeline$dataColDate[12]), size = 3, lineheight = 1, hjust = 1)  + 
  coord_cartesian(ylim = c(1,8.2), clip = "off")

plot_timeline2



plot_timeline2 + 
  annotate(
  "pointrange", x = as.POSIXct(timeline$dataColDate)[c(1)], y = 6.3, ymin = 6.3, ymax = 0.7, size = 0.7, colour = "#606060", shape = 19, stroke = 1.6) +
  annotate(
  "pointrange", x = as.POSIXct(timeline$dataColDate)[c(5,7,8)], y = 4.3, ymin = 4.3, ymax = 0.7, size = 0.7, colour = c("#606060", "#606060", "#FF000050"), shape = c(19, 15, 2), stroke = 1.6) +
  annotate(
  "pointrange", x = as.POSIXct(timeline$dataColDate)[c(9)]-months(3), y = 2.3, ymin = 2.3, ymax = 0.7, size = 0.7, colour = "#606060", shape = c(19), stroke = 1.6) +
  annotate(
  "pointrange", x = as.POSIXct(timeline$dataColDate)[c(9)], y = 2.3, ymin = 2.3, ymax = 0.7, size = 1, linewidth = 1.3, colour = c("black"), shape = c(19), stroke = 1.2) +
  annotate(
  "pointrange", x = as.POSIXct(timeline$dataColDate)[c(11, 12)], y = 2.3, ymin = 2.3, ymax = 0.7, size = 0.7, colour = c("#FF000050", "#FF000050"), shape = c(0, 2), stroke = 1.6) + 
  annotate("rect", xmin = as.POSIXct(timeline$dataColDate)[c(9)]-months(2), xmax = as.POSIXct(timeline$dataColDate)[c(9)]+months(8), ymin = 0, ymax = 3.93, alpha = .4) 




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

