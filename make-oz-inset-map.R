# geom_strayr
#----------------------------------------------#
# ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀ ⣤⣤⣤⡄[ ]⠀ ⢀⣧  ⠀⠀⠀⠀⠀
# ⠀⠀⠀⠀⠀⠀⠀⠀⠀⣰⣶⣄⣸⣿⣿⣿⣿⣄⠀⠀⠀ ⣼⣿⡆⠀⠀⠀⠀⠀⠀
# ⠀⠀⠀⠀⠀⠀⠀⢀⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣦⣠⣿⣿⣿⡀⠀⠀⠀⠀⠀
# ⠀⠀⠀⠀⠀⢀⣴⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣧⡀⠀[ ]⠀⠀⠀
# ⠀⢀⣠⣴⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣆⠀⠀⠀
# ⠀⠸⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⠀⠀
# ⠀⠀⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡇⠀
# ⠀⠀⠸⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠇[ ]
# ⠀⠀⠀⣿⣿⣿⣿⣿⣿⠟⠋⠁⠀⠙⠻⣿⣿⡿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠏⠀⠀
# [ ]⠘⠿⠿⠛⠉⠀⠀⠀⠀⠀⠀⠀⠀⠙⠀⠸⣿⣿⣿⣿⣿⣿⣿⠏⠀⠀⠀
# ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀ ⠹⠿⣿⣿⣿⣿⡟⠀[ ]⠀⠀⠀
# ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⠀⠀⠀⠀⠀
# ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀  ⠀⠀⠈⣿⡿⠀[ ]⠀⠀⠀⠀⠀
#----------------------------------------------#
## Ben Cochrane; built on R 4.3.1; 14/11/23

# Code to be changed for use is surrounded by '#-#-#-#-#-#-#-#-#-'

# Packages ----

if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,
  sf,
  rmapshaper,
  cowplot,
  RColorBrewer
  strayr
)

# where to save the map picture
output_path <- ""

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Settings:
# set keep = 1 and dpi to high for final maps
keep_value <- 1
dpi_value <- 500
colour_palette <- brewer.pal(9,'RdYlGn')
sa_level <- "sa3"     # one of: "sa1","sa2","sa3","sa4"
sa_year  <- 2021      # 2016 or 2021 (whatever your build uses)
sa_colname <- paste0(sa_level,'_code_',sa_year)

map_title <- paste0(toupper(sa_level)," zoomed capitals inset map with random values")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

sa_geo    <- read_absmap(paste0(sa_level, sa_year))
gcc_geo   <- read_absmap(paste0("gcc", sa_year))
state_geo <- read_absmap(paste0("state", sa_year))

sa_geo <- rmapshaper::ms_simplify(sa_geo, keep = keep_value)
state_geo <- rmapshaper::ms_simplify(state_geo, keep = keep_value)
gcc_geo <- rmapshaper::ms_simplify(gcc_geo, keep = keep_value)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
## Insert your data here
#  - Uncomment and replace `map_data` with your data
#  - You may need to check your column names are appropriate for a left join
#  - Currently generates random data for examples sake (line 81)

map_stats <- sa_geo %>%
  st_drop_geometry() %>%
  #   left_join(map_data) %>% ### insert data
  #   mutate(your_data = map_data_value) %>%
  mutate(your_data = sample(100, size = nrow(sa_geo), replace = TRUE)) %>%
  select(!!sa_colname, your_data) ### must have a geographic identifier
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# merge data with geometry
sa_aus_map <- merge(sa_geo, map_stats, by = sa_colname)

# check to see if data merged properly (e.g. different spacing, dashes etc. in sa3 names)
map_check <- sa_aus_map %>%
  select(!!sa_colname, your_data) %>%
  st_drop_geometry()

if (!nrow(map_check) == nrow(map_stats)) {
  message("*** Check map_check to see if map has fully matched during merge")
}

# Mapping ----
# create main map
map_aus_orig <-
  ggplot() +
  geom_sf(
    data = sa_aus_map, aes(fill = your_data),
    color = 'grey70', lwd = 0.2
  ) +
  theme_void() +
  xlim(108, 166) + # create spacing
  ylim(-46, -7) +
  coord_sf(expand = FALSE) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.35),
    panel.grid.major = element_line(colour = "transparent"),
    legend.direction = "horizontal",
    legend.position = c(0.4, 0.95)
  ) +
  labs(title = map_title, fill = "") +
  geom_sf(data = gcc_geo, aes(), lwd = 0.1, color = NA, fill = NA) + # add capital and state borders
  geom_sf(data = state_geo, aes(), lwd = 0.1, color = "grey25", fill = NA) +
  scale_fill_gradientn(colours = colour_palette)

## Insets ----
### Create capital city insets

sf_use_s2(FALSE) # multiple insets seems to error without this
map_aus <- map_aus_orig

# some bounding box info if you want to include all of the GCC:
# "1GSYD" xmin: 150.3567 ymin: -34.00578 xmax: 151.6305 ymax: -32.99607
# "2GMEL" xmin: 144.458 ymin: -38.085 xmax: 145.8784 ymax: -37.17629
# "3GBRI" xmin: 152.134 ymin: -28.33877 xmax: 153.5433 ymax: -26.99119
# "4GADE" xmin: 138.4356 ymin: -35.3503 xmax: 139.044 ymax: -34.50023
# "5GPER" xmin: 115.45 ymin: -32.80187 xmax: 116.4151 ymax: -31.45513
# "6GHOB xmin: 147.0277 ymin: -43.1213 xmax: 147.9369 ymax: -42.65538
# "7GDAR":  xmin: 130.8169 ymin: -12.86189 xmax: 131.3967 ymax: -12.00096
# "8ACTE" xmin: 148.7628 ymin: -35.92041 xmax: 149.3973 ymax: -35.12442

### Tribble for inset plotting

gcc_insets <- tibble::tribble(
  ~gcc_code, ~gcc_name,   ~city_x,   ~city_y,   ~gcc_xmin, ~gcc_xmax, ~gcc_ymin, ~gcc_ymax, ~inset_xpos, ~inset_ypos, ~inset_scale,
  "1GSYD",   "Sydney",     151.2093, -33.8688,   150.5,     151.5,     -34.4,     -33.4,      0.7200,      0.4100,      0.25,
  "2GMEL",   "Melbourne",  144.9631, -37.8136,   144.35,    145.9,     -38.6,     -37.25,     0.7039,      0.0100,      0.20,
  "3GBRI",   "Brisbane",   153.0260, -27.4705,   152.6,     153.73,    -28.2,     -26.7,      0.7305,      0.6650,      0.20,
  "4GADE",   "Adelaide",   138.6007, -34.9285,   138.4,     139.1,     -35.3,     -34.5,      0.2150,      0.0100,      0.35,
  "5GPER",   "Perth",      115.8613, -31.9523,   115.45,    116.5,     -32.8,     -31.4,      0.0700,      0.0100,      0.20,
  "6GHOB",   "Hobart",     147.3257, -42.8826,   147.0,     148.0,     -43.2,     -42.6,      0.3610,      0.0100,      0.30,
  "7GDAR",   "Darwin",     130.8444, -12.4637,   130.8,     131.4,     -12.9,     -12.0,      0.6352,      0.7395,      0.25,
  "8ACTE",   "Canberra",   149.1310, -35.2802,   148.85,    149.43,    -35.5,     -35.05,     0.7360,      0.2950,      0.25
)

make_inset <- function(gcc_filter) {
  gcc <- gcc_insets %>%
    filter(gcc_name == gcc_filter)

  gcc_name <- gcc$gcc_name

  # draw box and capital city dot on main map - both carry over to the inset
  map_aus <<-
    map_aus +
    # geom_segment(
    #   aes(
    #     xend = gcc$city_x,
    #     x = 108 + gcc$inset_xpos * (166 - 108),
    #     yend = gcc$city_y,
    #     y = -46 + gcc$inset_ypos * (-7 - (-46))
    #   ),
    #   alpha = 0.8, colour = "grey"
    # ) +
    annotate("rect",
             xmin = gcc$gcc_xmin,
             xmax = gcc$gcc_xmax,
             ymin = gcc$gcc_ymin,
             ymax = gcc$gcc_ymax,
             fill = NA,
             color = "grey25",
             linewidth = 0.6
    )

  # create inset
  map_layer <-
    draw_plot(
      {
        map_aus +
          coord_sf(
            xlim = c(gcc$gcc_xmin, gcc$gcc_xmax),
            ylim = c(gcc$gcc_ymin, gcc$gcc_ymax),
            expand = FALSE
          ) +
          theme(legend.position = "none", plot.subtitle = element_text(
            margin = margin(0, 0, 0, 0),
            face = "bold"
          ),
          plot.title = element_blank()) +
          labs(subtitle = gcc$gcc_name)
      },
      x = gcc$inset_xpos,
      y = gcc$inset_ypos,
      width = (gcc$gcc_xmax - gcc$gcc_xmin) * gcc$inset_scale,
      height = (gcc$gcc_ymax - gcc$gcc_ymin) * gcc$inset_scale
    )

  assign(paste0("inset_", str_sub(tolower(gcc_name), 1, 3)),
         map_layer,
         envir = .GlobalEnv
  )
}

walk(gcc_insets$gcc_name, make_inset)

list_inset <- ls(pattern = "inset_") %>%
  purrr::map(get, envir = .GlobalEnv)

# Export ----
### final map with insets
# join them all together
map_out <- ggdraw(map_aus) +
  list_inset

# map_out # note: how it shows in the plot is not necessarily how it looks when saved!

ggsave(paste0(output_path,paste0("map_export_", sa_level, ".jpg")),
       map_out,
       type = "cairo",
       width = 23, height = 18, dpi = dpi_value, units = "cm"
)
message(" **** Map complete! Check output folder ****")

