#===============Tidy Tuesday R Ladies Chapter Events 2023-11-21===============#

#=============================================================================#
# Library Load-in--------------------------------------------------------------
#=============================================================================#
library(tidyverse) # For Everything Data
library(tidytuesdayR) # To get the Data
library(sysfonts) # Because I'm not fighting with extra fonts today
library(glue) # For easier image importing
library(ggtext) # For easier glue-capable textboxes
library(showtext) # To get otf fonts to show

#=============================================================================#
# Options----------------------------------------------------------------------
#=============================================================================#
# Font Initialization #
showtext_auto()

# Font Options-----------------------------------------------------------------
font_add("popart", regular = "custom_fonts/PopArt.ttf")
title_font <- "popart" #Didn't use this as I decided to cheat with Canva#

font_add("mabook", regular = "custom_fonts/Mabook.otf")
label_font <- "mabook"

font_add("dominique", regular = "custom_fonts/Dominique.otf")
year_font <- "dominique"

# Color Options----------------------------------------------------------------
background_color <- "#B98BFA"
polka_color <- "#7759A1"
online_color <- "#2F2340"
inperson_color <- "#7C4B8F"
label_color <- "#3F95A1"
label_highlight_color <- "#ffffff"
shadow_color <- "#181524"

# Image Options----------------------------------------------------------------
width <- 1920
height <- 1009

#=============================================================================#
# Data Load-in-----------------------------------------------------------------
#=============================================================================#
tt_data <- tt_load('2023-11-21')

#=============================================================================#
# Data Wrangling---------------------------------------------------------------
#=============================================================================#
df_meetups <-
  tt_data |>
  pluck("rladies_chapters") |>
  select(location, year) |>
  group_by(year, location) |>
  count(name = "total") |>
  mutate(
    label = total,
    total = case_when(
      location == "online" ~ -total - 50,
      .default = total + 50
    ),
    baseline = if_else(total < 0, -50, 50),
    bar_fill = case_match(location,
                          "online" ~ online_color,
                          "inperson" ~ inperson_color
    ),
    callout_fill = if_else(total < 0, inperson_color, online_color),
    label_nudge = if_else(total < 0, -140, 140)
  )

df_years <-
  df_meetups |>
  ungroup() |>
  select(year) |>
  distinct() |>
  mutate(
    year_fill = shadow_color,
    year_color = label_color,
    baseline = 0
  )

#=============================================================================#
# Art Data Creation------------------------------------------------------------
#=============================================================================#
df_polka <-
  tibble(
    expand_grid(
      x = seq(min(df_years$year) - 1, max(df_years$year) + 1, l = 45),
      y = seq(min(df_meetups$total) - 250, max(df_meetups$total) + 250, l = 45)
    )
  ) |>
  mutate(row_num = row_number()) |>
  filter(row_num %% 2 == 0)

ggplot(df_polka,
       aes(x, y)) +
  geom_point()
#=============================================================================#
# Custom Text Data Imports-----------------------------------------------------
#=============================================================================#
plot_title <-
  glue("<img src='images/title.png' width='300' height='300'>")

inperson_tag <-
  glue("<img src='images/inperson.png' width='300' height='100'>")

online_tag <-
  glue("<img src='images/online.png' width='300' height='100'>")

contact_tag <-
  glue("<img src='images/contact.png' width='350' height='150'>")

#=============================================================================#
# Data Visualization-----------------------------------------------------------
#=============================================================================#
df_meetups |> 
  ggplot(
    aes(x = year, y = baseline, xend = year, yend = total, group = location)
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = background_color),
    legend.position = 'none'
  ) +
  geom_point(
    data = df_polka,
    aes(x,y),
    color = polka_color,
    size = 1,
    alpha = .2,
    inherit.aes = FALSE
  ) +
  geom_segment(
    aes(yend = total + (label_nudge * .8), 
        xend = year + .1, 
        x = year + .1),
    linewidth = 32,
    color = shadow_color,
    lineend = "round",
    alpha = .7
  ) +
  geom_hline(yintercept = 0, linewidth = 5) +
  geom_hline(yintercept = 0, linewidth = 3, color = label_highlight_color) +
  geom_segment(
    aes(yend = total + (label_nudge * .7)),
    linewidth = 32,
    color = "#000000",
    lineend = "round"
  ) +
  geom_segment(
    aes(yend = total + (label_nudge * .7)),
    linewidth = 30,
    color = df_meetups$callout_fill,
    lineend = "round"
  ) +
  geom_segment(
    color = "#000000",
    linewidth = 32,
    lineend = "round"
  ) +
  geom_segment(
    color = df_meetups$bar_fill,
    linewidth = 30,
    lineend = "round"
  ) +
  geom_text(
    aes(x = year + .02, y = total - .02, label = label),
    inherit.aes = FALSE,
    family = label_font,
    color = label_color,
    size = 10,
    nudge_y = df_meetups$label_nudge
  ) +
  geom_text(
    aes(x = year, y = total, label = label),
    inherit.aes = FALSE,
    family = label_font,
    color = label_highlight_color,
    size = 10,
    nudge_y = df_meetups$label_nudge
  ) +
  geom_label(
    data = df_years,
    aes(x = year, y = baseline, label = year),
    inherit.aes = FALSE,
    family = year_font,
    color = label_highlight_color,
    fill = "#000000",
    label.size = 0,
    label.padding = unit(.9, "lines"),
    size = 10,
  ) +
  geom_label(
    data = df_years,
    aes(x = year, y = baseline, label = year),
    inherit.aes = FALSE,
    family = year_font,
    color = label_highlight_color,
    fill = label_color,
    label.size = 0,
    label.padding = unit(.7, "lines"),
    size = 10,
  ) +
  geom_textbox(aes(x = 2012.5, y = max(df_meetups$total) - 100, 
                   label = plot_title), 
               inherit.aes = FALSE, 
               fill = NA, 
               color = NA) +
  geom_textbox(aes(x = 2021, 
                   y = max(df_meetups$total) - 100, 
                   label = inperson_tag), 
               inherit.aes = FALSE, 
               fill = NA, 
               color = NA) +
  geom_textbox(aes(x = 2013, 
                   y = min(df_meetups$total) + 100, 
                   label = online_tag), 
               inherit.aes = FALSE, 
               fill = NA, 
               color = NA) +
  geom_textbox(aes(x = 2016.5, 
                   y = min(df_meetups$total) - 100, 
                   label = contact_tag), 
               inherit.aes = FALSE, 
               fill = NA, 
               color = NA) +
  coord_cartesian(xlim = c(min(df_years$year), 
                           max(df_years$year))) 

#=============================================================================#
# Image Saving-----------------------------------------------------------------
#=============================================================================#
ggsave(
  filename = "images/final_image.png",
  dev = "png",
  dpi = 100,
  width = width,
  height = height,
  units = "px"
)

