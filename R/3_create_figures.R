# Code to reproduce figures

# Check if the required packages are installed -----

# If not, install and load them
{list.of.packages <- 
  c("tidyverse", "sf", "ggmap", "gridExtra", "cowplot", "ggsn")
new.packages <- 
  list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
rm(list.of.packages, new.packages)}


# Load data -----

# Data to work with
dat <- 
  # Get the list of frogs with genetic samples and their NewHybrids IDs
  read_csv("Data/Litoria_ind_metrics.csv") %>% 
  left_join(read_csv("Data/NewHybrids_results/hybrid_status.csv") %>% 
              select(id, likely_h_status)  %>%  
              distinct()) %>% 
  rename(newhybrids_id = likely_h_status) %>%  
  # rename where alternative IDs have been given
  mutate(id = gsub("GN5BT11", "GN5BT06-T11", id), 
         id = gsub("SK3B01","SK3L02-B01", id), 
         id = gsub("CK5BT01", "CK5BT05-T01", id)) %>% 
  # Add the morphological ID results - 
  # match the actual frog IDs to the random frog IDs (and their results)
  left_join(
    read_csv("Data/Morphological_analyses/photo_folder_list.csv") %>% 
      select(code, rand_id) %>% 
      rename(id = code) %>% 
      distinct() %>% 
      left_join(
        read_csv("Data/Morphological_analyses/morphological_id_results.csv") %>% 
          rename(rand_id = frog_number, morph_id = species_code)
      ) %>% 
      mutate(morph_id = gsub("BB", "F1xboo", morph_id),
             morph_id = gsub("BW", "F1xwil", morph_id),
             morph_id = gsub("B", "boo", morph_id),
             morph_id = gsub("W", "wil", morph_id))
  ) %>% 
  filter(id != "CK5BXX") # remove this individual as no photos taken

# Alternate data with id method as one col (newhybrid and each persons' IDs)
dat_long <-
  dat %>% 
  pivot_wider(names_from = "validator", values_from = "morph_id") %>% 
  pivot_longer(cols = c(newhybrids_id, V1, V2, V3), 
               names_to = "id_method", 
               values_to = "species") %>% 
  filter(!is.na(species)) %>% 
  distinct()


# Figure 1 (distribution map and comparison of IDs at each site) -----
# But note that due to the sensitive nature of threatened species locations, 
# we have buffered the locations, so the map produced here may appear different
# to the map in the paper.

# load Aus outline
aus_coastline <- st_read("Data/Shapefiles_for_figures/aust_cd66states.shp")

# set crs
st_crs(aus_coastline) <- "+proj=longlat +ellps=GRS80 +no_defs"

# load species distributions
range_boo <- 
  st_read("Data/Shapefiles_for_figures/Litoria_booroolongensis.shp")
range_wilc <- 
  st_read("Data/Shapefiles_for_figures/Litoria_wilcoxii.shp")

## intersect the ranges of L. boo and L. wil so we can plot it in a diff colour
int <- st_intersection(range_boo, range_wilc)

# define the boundaries of zoomed map (NSW)
inset_bounds <- 
  c(min(dat$lon) - 3.5 , 
    min(dat$lat) - 3 , 
    max(dat$lon) + 4.5 , 
    max(dat$lat) + 6)

# the box that we will plot
box_zoom <- 
  st_sfc(
    st_polygon(list(cbind(
      c(inset_bounds[1], inset_bounds[1], inset_bounds[3], 
        inset_bounds[3], inset_bounds[1]),
      c(inset_bounds[4], inset_bounds[2], inset_bounds[2], 
        inset_bounds[4], inset_bounds[4]))))
  )
st_crs(box_zoom) <- "+proj=longlat +ellps=GRS80 +no_defs"

# overall Aus map with a box showing the location of zoomed in area
map_aus <-
  ggplot() +
  geom_sf(data = aus_coastline, color="grey50", fill="grey90", size = 0.25) +
  geom_sf(data = range_wilc, 
          colour = "transparent", fill = "firebrick2", 
          alpha = 0.9) + 
  geom_sf(data = range_boo, 
          colour = "transparent", fill = "dodgerblue4", 
          alpha = 0.9) + 
  geom_sf(data = int, 
          colour = "transparent", fill = "#472047", 
          alpha = 0.9) + 
  geom_sf(data = box_zoom, colour = "#880015", fill = NA, size = 0.65) +
  theme(panel.background = element_rect(colour="black", fill="lightblue"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_sf(xlim = c(113, 154))

# zoomed in map (NSW)
## define the boundaries of the study sites that we will plot as a box
site_bounds <- 
  c(min(dat$lon) - 0.15 , 
    min(dat$lat) - 0.05 , 
    max(dat$lon) + 0.15 , 
    max(dat$lat) + 0.05)

study_site_zoom <- 
  st_sfc(
    st_polygon(list(cbind(
      c(site_bounds[1], site_bounds[1], site_bounds[3], 
        site_bounds[3], site_bounds[1]),
      c(site_bounds[4], site_bounds[2], site_bounds[2], 
        site_bounds[4], site_bounds[4]))))
  )
st_crs(study_site_zoom) <- "+proj=longlat +ellps=GRS80 +no_defs"

## now plot
nsw_inset <-
  ggplot() +
  geom_sf(data = aus_coastline, color = "grey50", fill = "grey90") +
  geom_sf(data = range_wilc, 
          colour = "transparent", fill = "firebrick2", 
          alpha = 0.9) + 
  geom_sf(data = range_boo, 
          colour = "transparent", fill = "dodgerblue4", 
          alpha = 0.9) + 
  geom_sf(data = int, 
          colour = "transparent", fill = "#472047", 
          alpha = 0.9) + 
  geom_sf(data = box_zoom, colour = "black", fill = "transparent", size = 1) +
  # update the zoomed in map to include a box around the study site
  geom_sf(data = study_site_zoom, 
          colour = "black", size = 1, 
          fill = "#DFDFB9", alpha = 0.7) +
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"),
        panel.grid = element_blank(),
        panel.border = element_rect(colour="black", 
                                    fill = "transparent", 
                                    size = 1.1)) +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(inset_bounds[1], inset_bounds[3]), 
           ylim = c(inset_bounds[2], inset_bounds[4]),
           expand = F) +
  # add aus map inset
  annotation_custom(
    ggplotGrob(
      map_aus + 
        theme_void() + 
        theme(legend.position = "none", 
              plot.background = element_rect(fill = "white"),
              panel.border = element_rect(colour = "black", 
                                          fill = NA, 
                                          linewidth = 0.5))), 
    xmin = inset_bounds[3] - 3.1, 
    xmax = inset_bounds[3], 
    ymin = inset_bounds[2],
    ymax = inset_bounds[2] + 2.48
  ) 

# create zoomed in map of sites (collection locations)

register_google(key = "ENTER_YOUR_KEY")
register_stadiamaps(key = "ENTER_YOUR_KEY")

# provide centre lat and lon of sites instead of bounding box (which is glitchy)
site_overview <- 
  c(lon = mean(c(max(dat$lon), min(dat$lon))), 
    lat = mean(c(max(dat$lat), min(dat$lat))))

mysites <- 
  dat %>% 
  group_by(location) %>% 
  summarise(
    lon = mean(c(max(lon), min(lon))), 
    lat = mean(c(max(lat), min(lat)))
  )

# load river data to add to map later
rivers <- 
  sf::read_sf("Data/Shapefiles_for_figures/Named Watercourse (large scale).shp")
rivers <- 
  sf::st_transform(rivers, "+proj=longlat +datum=WGS84 +ellps=WGS84")

# define the boundaries of the site
site_bounds <- 
  c(min(dat$lon) - 0.15 , 
    min(dat$lat) - 0.05 , 
    max(dat$lon) + 0.15 , 
    max(dat$lat) + 0.05)

# this is the zoomed in map of sites (collection locations)
map_sites <-
  get_stadiamap(
    bbox = site_bounds,
    maptype = "stamen_terrain_background",
    crop = FALSE
    ) %>% 
  ggmap() +
  # plot rivers
  geom_sf(data = rivers %>% mutate(relevance = as.numeric(relevance)),  
          aes(size = relevance), 
          color = "blue", alpha = 0.25,
          inherit.aes = F) + # do not inherit the aesthetics
  scale_size(range = c(0.5, 0.1), 
             guide = "none") + # do not show the legend
  # plot sites
  geom_point(data = mysites, aes(x = lon, y = lat), 
             color = "black", size = 2) + 
  labs(x = "Longitude", y = "Latitude") +
  theme(panel.border = element_rect(colour = "black", fill = "transparent",
                                    size = 1.1)) +
  scale_x_continuous(limits = c(site_bounds[1], site_bounds[3])) + 
  scale_y_continuous(limits = c(site_bounds[2], site_bounds[4])) 

# Add bar plots (comparing IDs at each site) to the site map 

## function to get each counts and column bar for each site
#' @param site Name of the site (e,g, "Abercrombie_Beach") 
#' @param hyb_outline_col Draw a box around the hybrids? If so, specify colour.
plot_m_vs_g <- function(site = NULL, hyb_outline_col = "transparent"){
  
  # Sample size at each site
  site_N <- 
    dat %>% 
    select(id, location) %>% 
    distinct() %>% 
    group_by(location) %>% 
    count(name = "N")
  
  # Define the hybrid groups (collapse the hybrid categories)
  # so that we can plot them over the top of the main data plot later
  
  dat_hybrid_group_to_plot <-
    ## if site is specified, filter data to sites
    if(!is.null(site)){
      dat_long %>% 
        filter(location == site)
    } 
  ## otherwise, do not filter the data
  else(dat_long)
  
  dat_hybrid_group_to_plot <-
    dat_hybrid_group_to_plot %>% 
    group_by(location, id_method, species) %>% 
    count(name = "count") %>% 
    group_by(location, id_method) %>% 
    mutate(species2 = species, 
           species2 = if_else(condition = (species == "boo" | species == "wil"),  
                              true = species, 
                              false = "hybrid")) %>%  
    group_by(location, id_method, species2) %>% 
    summarise(count2 = sum(count)) %>% 
    ungroup() %>% 
    ## add new columns for plotting variables
    ## (see plot code below for details)
    mutate(width2 = if_else(condition = id_method == "newhybrids_id", 
                            true = 0.8, 
                            false = 0.4)) %>% 
    mutate(id_method2 = if_else(condition = id_method == "newhybrids_id", 
                                true = "newhybrids", 
                                false = "morphology")) %>% 
    mutate(x_axis_spacing = case_when(id_method == "newhybrids_id" ~ 0,
                                      id_method == "V1" ~ 0.75,
                                      id_method == "V2" ~ 1.3,
                                      id_method == "V3" ~ 1.85)) %>% 
    left_join(site_N, by = "location") 
  
  dat_hybrid_group_to_plot <-
    ## if site is specified, filter data to sites
    if(!is.null(site)){
      dat_hybrid_group_to_plot %>% 
        mutate(location = paste("N =", N))
    } 
  ## otherwise, do not filter the data
  else(dat_hybrid_group_to_plot)
  
  
  # Now, steps to plot the data...
  
  to_plot <-
    ## if site is specified, filter data to sites
    if(!is.null(site)){
      dat_long %>%
        filter(location == site)} 
  ## otherwise, do not filter the data
  else(dat_long)
  
  to_plot <-
    to_plot %>% 
    group_by(location, id_method, species) %>% 
    count(name = "count") %>% 
    # add new columns for plotting variables:
    ## width2 specifies the width of each column 
    ## newhyb ID column is 0.8 in width; morphological ID column is 0.4 in width
    mutate(width2 = if_else(condition = id_method == "newhybrids_id", 
                            true = 0.8, 
                            false = 0.4)) %>% 
    ## id_method2 is the variable that specifies newhyb vs morphology
    mutate(id_method2 = if_else(condition = id_method == "newhybrids_id", 
                                true = "newhybrids", 
                                false = "morphology")) %>% 
    ## x_axis_spacing specifies where on the x-axis each column will be positioned
    ## so we can have even spacing b/w columns when we use diff col widths
    mutate(x_axis_spacing = case_when(id_method == "newhybrids_id" ~ 0,
                                      id_method == "V1" ~ 0.75,
                                      id_method == "V2" ~ 1.3,
                                      id_method == "V3" ~ 1.85)) %>% 
    ## add sample size for each site
    left_join(site_N, by = "location")
  
  to_plot <- 
    ## if site is specified, change the site names to the sample size
    if(!is.null(site)){
      to_plot %>% 
        mutate(location = paste("N =", N))
    }
  ## otherwise, leave the site names as they are
  else(to_plot)
  
  # now to the plotting part
  plot <- 
    to_plot %>% 
    ggplot() + 
    # plot the main data with all the hybrid categories
    geom_col(mapping = aes(y = count, 
                           # reorder the x variable (based on spacing; see above) 
                           x = x_axis_spacing, 
                           fill = species, width=width2), 
             colour = "black", size = 0.4) + 
    scale_fill_manual(
      name = "Class",
      values = c("dodgerblue4", "cadetblue3", "gold", 
                 "papayawhip", "darkorange", "firebrick2"),   
      breaks = c("boo", "F1xboo", "F1", "F2", "F1xwil", "wil"),
      labels = c(substitute(paste(italic("L.boo"))), 
                 substitute(paste("F1 x ", italic("L.boo"))), 
                 "F1", "F2", 
                 substitute(paste("F1 x ", italic("L.wil"))), 
                 substitute(paste(italic("L.wil"))))
    ) +  
    # plot the grouped hybrid data over the top 
    # (i.e. a coloured box around the hybrids)
    geom_col(data = dat_hybrid_group_to_plot, 
             mapping = aes(y = count2, 
                           x = x_axis_spacing, 
                           colour = species2, 
                           width = width2), 
             fill = NA, size = 0.8, inherit.aes = F) +
    # additional aesthetics
    ## this draws a box around the hybrids 
    ## (default is transparent, i.e. no outline)
    scale_colour_manual(
      values = c("transparent", hyb_outline_col, "transparent"), 
      labels = c("boo", "hybrid", "wil"),  
      breaks = c("boo", "hybrid", "wil"),
      guide = "none") +
    theme(axis.text = element_text(angle = 45, hjust = 1)) + 
    facet_wrap(~location, strip.position = "bottom") + 
    scale_y_continuous(limits = c(0,35)) +
    # rename the x-axis levels
    scale_x_continuous(name = "id method", 
                       breaks = c(0, 0.75, 1.3, 1.85), 
                       labels = c("NewHyb","V1","V2", "V3"))
  
  return(plot)
  
}

## give the sites a site code
mysites <- 
  mysites %>%
  mutate(site_code = location,
         site_code = gsub("Abercrombie_Beach", "BH", site_code),
         site_code = gsub("Abercrombie_Bummaroo_Ford", "BM", site_code),
         site_code = gsub("Abercrombie_Glen", "GN", site_code),
         site_code = gsub("Abercrombie_Silent_Ck", "SC", site_code),
         site_code = gsub("Abercrombie_Sink", "SK", site_code),
         site_code = gsub("Essington_SF_Captain_Kings_Ck", "CK", site_code),
         site_code = gsub("Locksley_Fish_Rr", "FH", site_code)) 

## function to plot the bars on the map (at the defined x and y coordinates)
map_with_bars <- 
  function(my_map, 
           my_site_code, 
           N_text_size = 11,
           shift_l = 0.02, 
           # offset the bars by how many degrees? 
           # +ve shifts left; -ve shifts right
           shift_up = -0.02, 
           # offset the bars by how many degrees? 
           # +ve shifts up; -ve shifts down
           bar_width = 0.07, 
           bar_height = 0.12
  ){
    my_map + 
      annotation_custom(
        ggplotGrob(
          plot_m_vs_g(site = 
                        mysites %>% 
                        filter(site_code == my_site_code) %>% 
                        .$location) + 
            theme_void() + 
            theme(legend.position = "none",
                  strip.text = element_text(size = N_text_size))
        ), 
        xmin = mysites %>% 
          filter(site_code == my_site_code) %>% 
          .$lon + shift_l, 
        xmax = mysites %>% 
          filter(site_code == my_site_code) %>% 
          .$lon + shift_l + bar_width, 
        ymin = mysites %>% 
          filter(site_code == my_site_code) %>% 
          .$lat + shift_up, 
        ymax = mysites %>% 
          filter(site_code == my_site_code) %>% 
          .$lat + shift_up + bar_height
      )
  }

## function to add the site codes as annotations 
ann_map <- function(my_map, 
                    my_site_code, 
                    shift_l = 0, 
                    # offset the text by how many degrees? 
                    # +ve shifts left; -ve shifts right
                    shift_up = 0.02
                    # offset the text by how many degrees? 
                    # +ve shifts up; -ve shifts down
){
  site_to_label <-
    mysites %>% 
    filter(site_code == my_site_code)
  
  new_map <- 
    my_map + 
    annotate("text", label = my_site_code,
             x = site_to_label$lon + shift_l, 
             y = site_to_label$lat + shift_up, 
             size = 4.3, colour = "black")
  
  return(new_map)
}


## Putting it all together
main_map_panel <-
  map_with_bars(map_sites, "FH", 
                shift_l = 0.0275, shift_up = -0.05,
                bar_width = 0.09, bar_height = 0.21) %>% 
  map_with_bars(., "CK", 
                shift_l = 0.025, shift_up = -0.06,
                bar_width = 0.09, bar_height = 0.21) %>% 
  map_with_bars(., "GN", 
                shift_l = 0.025,
                bar_width = 0.09, bar_height = 0.21) %>% 
  map_with_bars(., "SK", 
                shift_l = -0.11, shift_up = 0.0115, 
                bar_width = 0.09, bar_height = 0.21) %>% 
  map_with_bars(., "BH", 
                shift_l = -0.12, shift_up = -0.158, 
                bar_width = 0.09, bar_height = 0.21) %>% 
  map_with_bars(., "SC", 
                shift_l = 0.02, shift_up = -0.01, 
                bar_width = 0.09, bar_height = 0.21) %>% 
  map_with_bars(., "BM", 
                shift_up = -0.08, 
                bar_width = 0.09, bar_height = 0.21) %>% 
  ann_map(., "FH") %>% 
  ann_map(., "CK") %>% 
  ann_map(., "GN") %>% 
  ann_map(., "SK") %>% 
  ann_map(., "BH", shift_up = -0.02) %>% 
  ann_map(., "SC") %>% 
  ann_map(., "BM", shift_up = -0.02) +
  # add scale bar
  scalebar(data = rivers %>% 
             # need to crop the data to avoid strange placement of scale bar
             st_crop(., 
                     xmin = site_bounds[1],
                     ymin = site_bounds[2], 
                     xmax = site_bounds[3], 
                     ymax = site_bounds[4]),
           dist = 4, dist_unit = "km",
           transform = T, st.bottom = F,
           model="WGS84", st.size = 3,
           anchor = c(x = site_bounds[3],  
                      y = site_bounds[2])) 

main_map_panel

# arrange the plot using plot_grid
Fig1 <-
  plot_grid(nsw_inset, NULL, 
            main_map_panel, 
            get_legend(
              plot_m_vs_g("Abercrombie_Sink") + 
                theme(legend.justification = c(0,1))), 
            ncol = 4, 
            rel_widths = c(1.2, 0.05, 1, 0.25)) 

Fig1 <- 
  Fig1 +
  annotate("text", label = "(a)",
           x = 0.075, y = 0.95, 
           size = 5, colour = "black", fontface = "bold") +
  annotate("text", label = "(b)",
           x = 0.595, y = 0.95, 
           size = 5, colour = "black", fontface = "bold") +
  annotate("text", label = "b",
           x = 0.238, y = 0.385, 
           size = 5, colour = "black", fontface = "bold")

Fig1 

# Note that the fill for wilcoxii doesn't appear 
# in the plot preview in R, 
# but a workaround is to save using 'cairo'
#ggsave(Fig1, filename = "Fig_1.pdf", 
#       width = 280, height = 180, units="mm", dpi=750,
#       device = cairo_pdf)


# Fig 2 (genetic similarity) -----

pc_dat <- 
  read_csv("Data/result_PC_scores.csv") %>% 
  left_join(read_csv("Data/NewHybrids_results/hybrid_status.csv"))

pc_plot <- 
  pc_dat %>% 
  ggplot(aes(x = PC1, y = PC2, fill = likely_h_status, shape = likely_h_status)) + 
  geom_hline(mapping = aes(yintercept = 0), linewidth = 0.25) + 
  geom_vline(mapping = aes(xintercept = 0), linewidth = 0.25) +
  geom_point(size = 1.5) +
  labs(x = "PC1 (79.5%)", y = "PC2 (1.3%)") +
  theme(
    axis.title.x = element_text(color = "black", size=14, face="bold"),
    axis.title.y = element_text(color = "black", size=14, face="bold"),
    axis.text.x = element_text(face = "plain", color="black", size=12),
    axis.text.y = element_text(face = "plain", color="black", size=12),
    panel.background = element_rect(fill = "transparent",
                                    colour = "black",
                                    linewidth = 1), 
    plot.background = element_rect(fill = "transparent", 
                                   color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "transparent", 
                                     colour = NA),
    legend.position = "bottom",
    legend.key = element_rect(fill = "transparent", colour = NA)) +
  scale_x_continuous(breaks = seq(-50,100,25)) +
  scale_fill_manual(name = "Class",
                    values = c("dodgerblue4", "cadetblue3", "gold", 
                               "papayawhip", "darkorange", "firebrick2"),  
                    breaks = c("boo", "F1xboo", "F1", "F2", "F1xwil", "wil"),
                    labels = c(substitute(paste(italic("L.boo"))), 
                               substitute(paste("F1 x ", italic("L.boo"))), 
                               "F1", "F2", 
                               substitute(paste("F1 x ", italic("L.wil"))), 
                               substitute(paste(italic("L.wil"))))) +
  scale_shape_manual(name = "Class",
                     values = c(21, 22, 24, 25, 22, 21),
                     breaks = c("boo", "F1xboo", "F1", "F2", "F1xwil", "wil"),
                     labels = c(substitute(paste(italic("L.boo"))), 
                                substitute(paste("F1 x ", italic("L.boo"))), 
                                "F1", "F2", 
                                substitute(paste("F1 x ", italic("L.wil"))), 
                                substitute(paste(italic("L.wil")))))

pc_plot

#ggsave(pc_plot, filename = "Fig_2.png", bg = "white", width = 12, height = 12, units = "cm", dpi = 600)


# Figure 3 (compare IDs of each individual) -----

# define the plotting order: we want to order the frogs by NewHybrids ID
# (boo/hybrid/wil, in that order)
frog_order <- 
  dat_long %>% 
  mutate(species2 = species, 
         species2 = if_else(condition = (species == "boo" | species == "wil"),  
                            true = species, 
                            false = "hybrid")) %>% 
  filter(id_method == "newhybrids_id") %>% 
  mutate(plotting_order = case_when(species == "boo" ~ 1,
                                    species == "F1xboo" ~ 2,
                                    species == "F1" ~ 3,
                                    species == "F2" ~ 4,
                                    species == "F1xwil" ~ 5,
                                    species == "wil" ~ 6)) %>% 
  arrange(plotting_order) %>% 
  mutate(plotting_order = 1:nrow(.)) %>% 
  select(id, plotting_order)


# newhybrids id data
dat_hy <- 
  read_csv("Data/NewHybrids_results/aa-pofZ.csv")  %>% 
  # rename where alternative IDs have been given
  mutate(id = gsub("GN5BT11", "GN5BT06-T11", id), 
         id = gsub("SK3B01","SK3L02-B01", id), 
         id = gsub("CK5BT01", "CK5BT05-T01", id)) %>% 
  filter(id != "CK5BXX")

# plot it 
nhyb_plot <-
  ggplot(dat_hy %>% 
           pivot_longer(cols = c(P0:F1xP1), 
                        names_to = "species", 
                        values_to = "probability") %>% 
           mutate(species = gsub("P0", "boo", species),
                  species = gsub("P1", "wil", species)) %>% 
           left_join(frog_order, by = "id") %>% 
           mutate(id = fct_reorder(id, plotting_order)), 
         aes(x = id, y = probability,  fill = species)) + 
  geom_col(width = 1) + 
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5, size = 7)) + 
  labs(y = "Probability", x = "ID") +
  scale_y_continuous(expand = c(0,0)) + 
  scale_fill_manual(
    name = "Class", 
    values = c("dodgerblue4", "cadetblue3", "gold", 
               "papayawhip", "darkorange", "firebrick2"),   
    breaks = c("boo", "F1xboo", "F1", "F2", "F1xwil", "wil"),
    labels = c(substitute(paste(italic("L.boo"))), 
               substitute(paste("F1 x ", italic("L.boo"))), 
               "F1", "F2", 
               substitute(paste("F1 x ", italic("L.wil"))), 
               substitute(paste(italic("L.wil"))))
  ) 


morph_plot <- 
  dat_long %>% 
  filter(id_method != "newhybrids_id") %>% 
  mutate(value = 1) %>% 
  left_join(frog_order, by = "id") %>% 
  mutate(id = fct_reorder(id, plotting_order)) %>% 
  ggplot(aes(x = id, y = value, fill = species)) + 
  geom_col(width = 1) + 
  labs(y = "Morphological \nclassification", x = "ID") +
  scale_y_continuous(expand=c(0,0)) +
  facet_wrap(~id_method, ncol = 1)  + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5, size = 7), 
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(), 
        #axis.title.y = element_blank(),
        strip.background = element_rect(fill=NA),
        strip.text = element_blank()) +
  scale_fill_manual(
    name = "Class", 
    values = c("dodgerblue4", "cadetblue3", "gold", 
               "papayawhip", "darkorange", "firebrick2"),  
    breaks = c("boo", "F1xboo", "F1", "F2", "F1xwil", "wil"),
    labels = c(substitute(paste(italic("L.boo"))), 
               substitute(paste("F1 x ", italic("L.boo"))), 
               "F1", "F2", 
               substitute(paste("F1 x ", italic("L.wil"))), 
               substitute(paste(italic("wil"))))
  )

nhyb_vs_morph <- 
  plot_grid(nhyb_plot + theme(axis.ticks.x = element_blank(), 
                              axis.text.x = element_blank(),
                              axis.title.x = element_blank(),
                              legend.position = "none"), 
            morph_plot + theme(legend.position = "none"), 
            align = "v", 
            ncol = 1, rel_heights = c(1, 1), labels = c("(a)", "(b)"))

Fig3 <- 
  plot_grid(nhyb_vs_morph, 
            get_legend(nhyb_plot + 
                         theme(legend.justification = c(0,1))), 
            rel_widths = c(1,0.1))

Fig3

#ggsave(Fig3, filename = "Fig_3.png", 
#       width = 290, height = 145, units="mm", dpi=750, bg = "white")


# Figure 4 (images of parentals and hybrids) -----

# function to load image 
load.myimage <- function(filename){
  ggdraw() + 
    draw_image(filename, scale = 1) + 
    theme(plot.margin = margin(8,2,0,2,"pt")) #margins are trbl
}

# create each panel
{ pa <- load.myimage("A_boo_Frog_092.jpg")
  pb <- load.myimage("B_F1xboo_Frog_149.jpg")
  pc <- load.myimage("C_F1_Frog_119.jpg")
  pd <- load.myimage("D_F2_Frog_164.jpg")
  pe <- load.myimage("E_F1xwil_Frog_199.jpg")
  pf <- load.myimage("F_wil_Frog_198.jpg")
  pg <- load.myimage("G_boo_Frog_092.jpg")
  ph <- load.myimage("H_F1xboo_Frog_149.jpg")
  pi <- load.myimage("I__F1_Frog_119.jpg")
  pj <- load.myimage("J__F2_Frog_164.jpg")
  pk <- load.myimage("K_F1xwil_Frog_199.jpg")
  pl <- load.myimage("L_wil_Frog_198.jpg")
  pm <- load.myimage("M_boo_Frog_092.jpg")
  pn <- load.myimage("N_F1xboo_Frog_149.jpg")
  po <- load.myimage("O_F1_Frog_119.jpg")
  pp <- load.myimage("P_F2_Frog_164.jpg")
  pq <- load.myimage("Q_F1xwil_Frog_199.jpg")
  pr <- load.myimage("R_wil_Frog_198.jpg")
  ps <- load.myimage("S_boo_Frog_092.jpg")
  pt <- load.myimage("T_F1xboo_Frog_149.jpg")
  pu <- load.myimage("U_F1_Frog_119.jpg")
  pv <- load.myimage("V_F2_Frog_164.jpg")
  pw <- load.myimage("W_F1xwil_Frog_199.jpg")
  px <- load.myimage("X_wil_Frog_198.jpg")
}

# Combine panels 
Fig4 <- plot_grid(pa, pb, pc, pd, pe, pf, 
                  pg, ph, pi, pj, pk, pl, 
                  pm, pn, po, pp, pq, pr, 
                  ps, pt, pu, pv, pw, px, 
                  ncol = 6, nrow = 4,
                  labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)",
                             "(g)", "(h)", "(i)", "(j)", "(k)", "(l)",
                             "(m)", "(n)", "(o)", "(p)", "(q)", "(r)",
                             "(s)", "(t)", "(u)", "(v)", "(w)", "(x)"),
                  vjust = 2.2, hjust = -0.45,
                  label_colour = "white")

# Extra label annotations at the top of the plot 
Fig4 <-
  plot_grid(NULL, Fig4, NULL, 
            ncol = 1, nrow = 3, 
            rel_heights = c(0.03, 1, 0.01)) + 
  annotate("text", label = "L. boo",
           x = 1/12, y = 0.977, 
           size = 5, colour = "black", fontface = "italic") +
  annotate("text", label = "F1 ~~ x ~~ italic(L. ~~ boo)", parse=T,
           x = 3/12, y = 0.977, 
           size = 5, colour = "black") +
  annotate("text", label = "F1",
           x = 5/12, y = 0.977, 
           size = 5, colour = "black") +
  annotate("text", label = "F2",
           x = 7/12, y = 0.977, 
           size = 5, colour = "black") +
  annotate("text", label = "F1 ~~ x ~~ italic(L. ~~ wil)", parse=T,
           x = 9/12, y = 0.977, 
           size = 5, colour = "black") +
  annotate("text", label = "L. wil",
           x = 11/12, y = 0.977, 
           size = 5, colour = "black", fontface = "italic") 

#ggsave(plot = Fig4, filename = "Fig_4.png", width = 287, height = 200, units = "mm",  dpi = 700, bg = "white")


# Fig S1 (boxplots comparing size) -----

{ # boxplot of mass (males only)
  mass_boxplot <-
    dat %>% 
    left_join(
      read_csv("Data/Morphological_analyses/frog_list.csv")
    ) %>% 
    filter(sex == "M") %>% 
    group_by(newhybrids_id) %>% 
    mutate(mean = mean(mass_g)) %>% 
    ungroup() %>% 
    ggplot() +
    geom_boxplot(aes(y = mass_g, 
                     x = fct_relevel(newhybrids_id, 
                                     "boo","F1xboo","F1","F2","F1xwil", "wil"), 
                     fill = newhybrids_id)) +
    geom_point(aes(x = newhybrids_id, 
                   y = mean), 
               colour="black", fill = "grey", shape=23, size=2, stroke=0.75) +
    scale_y_continuous(limits = c(0,20), breaks = seq(0,20,2)) +
    labs(y = "Mass (g)", x = "Class") +
    theme_classic(base_size = 11) + 
    theme(panel.border = element_rect(fill=NA)) +
    scale_fill_manual(name = "Class",
                      values = c("dodgerblue4", "cadetblue3", "gold", 
                                 "papayawhip", "darkorange", "firebrick2"),  
                      breaks = c("boo", "F1xboo", "F1", "F2", "F1xwil", "wil"),
                      labels = c(substitute(paste(italic("L.boo"))), 
                                 substitute(paste("F1 x ", italic("L.boo"))), 
                                 "F1", "F2", 
                                 substitute(paste("F1 x ", italic("L.wil"))), 
                                 substitute(paste(italic("L. wil"))))) +
    scale_x_discrete(breaks = c("boo", "F1xboo", "F1", "F2", "F1xwil", "wil"),
                     labels = c(substitute(paste(italic("L.boo"))), 
                                substitute(paste("F1 x ", italic("L.boo"))), 
                                "F1", "F2", 
                                substitute(paste("F1 x ", italic("L.wil"))), 
                                substitute(paste(italic("L. wil")))))
  
  # boxplot of SVL (males only)
  svl_boxplot <- 
    dat %>% 
    left_join(
      read_csv("Data/Morphological_analyses/frog_list.csv")
    ) %>% 
    filter(sex == "M") %>% 
    group_by(newhybrids_id) %>% 
    mutate(mean = mean(SVL_mm)) %>% 
    ungroup() %>% 
    ggplot() +
    geom_boxplot(aes(y = SVL_mm, 
                     x = fct_relevel(newhybrids_id, 
                                     "boo","F1xboo","F1","F2","F1xwil", "wil"), 
                     fill = newhybrids_id)) +
    geom_point(aes(x = newhybrids_id, 
                   y = mean), 
               colour="black", fill = "grey", shape=23, size=2, stroke=0.75) +
    scale_y_continuous(limits = c(20,55), breaks = seq(20,55,5)) +
    labs(y = "SVL (mm)", x = "Class") +
    theme_classic(base_size = 11) + 
    theme(panel.border = element_rect(fill=NA)) +
    scale_fill_manual(name = "Class",
                      values = c("dodgerblue4", "cadetblue3", "gold", 
                                 "papayawhip", "darkorange", "firebrick2"),  
                      breaks = c("boo", "F1xboo", "F1", "F2", "F1xwil", "wil"),
                      labels = c(substitute(paste(italic("L.boo"))), 
                                 substitute(paste("F1 x ", italic("L.boo"))), 
                                 "F1", "F2", 
                                 substitute(paste("F1 x ", italic("L.wil"))), 
                                 substitute(paste(italic("L. wil"))))) +
    scale_x_discrete(breaks = c("boo", "F1xboo", "F1", "F2", "F1xwil", "wil"),
                     labels = c(substitute(paste(italic("L.boo"))), 
                                substitute(paste("F1 x ", italic("L.boo"))), 
                                "F1", "F2", 
                                substitute(paste("F1 x ", italic("L.wil"))), 
                                substitute(paste(italic("L. wil")))))
  
  x.label <- grid::textGrob("Class", gp = grid::gpar(fontsize = 12))
  
  Fig_mass_svl <-
    grid.arrange(
      arrangeGrob(
        plot_grid(
          mass_boxplot + theme(legend.position = "none", 
                               axis.title.x = element_blank()), 
          svl_boxplot + theme(legend.position = "none", 
                              axis.title.x = element_blank()),
          labels = c("(a)", "(b)"), 
          label_size = 12, hjust = -0.1), 
        bottom = x.label)
    )
  
}

#ggsave(Fig_mass_svl, filename = "Fig_S1.png", 
#       width = 200, height = 80, units = "mm", dpi = 750, bg = "white")


# Fig S2 (photos of cryptic hybrids) -----

# function to load image 
load.myimage <- function(filename){
  ggdraw() + 
    draw_image(filename, scale = 1) + 
    theme(plot.margin = margin(8,2,0,2,"pt")) #margins are trbl
}

# create each panel
{ pa <- load.myimage("A_boo_Frog_092.jpg")
  pb <- load.myimage("B_F1xL.boo_SK2B06.jpg")
  pc <- load.myimage("C_wil_Frog_198.jpg")
  pd <- load.myimage("D_FxL.wil_Beach_LW15.jpg")
  pe <- load.myimage("E_boo_Frog_092.jpg")
  pf <- load.myimage("F_F1xL.boo_SK2B06.jpg")
  pg <- load.myimage("G_wil_Frog_198.jpg")
  ph <- load.myimage("H_F1xL.wil_BH2L15.jpg")
  pi <- load.myimage("I_boo_Frog_092.jpg")
  pj <- load.myimage("J_F1xL.boo_SK2B06.jpg")
  pk <- load.myimage("K_wil_Frog_198.jpg")
  pl <- load.myimage("L_F1xL.wil_BH2L15.jpg")
  pm <- load.myimage("M_boo_Frog_092.jpg")
  pn <- load.myimage("N_F1xL.boo_SK2B06.jpg")
  po <- load.myimage("O_wil_Frog_198.jpg")
  pp <- load.myimage("P_F1xL.wil_BH2L15.jpg")
}

# Combine panels 
FigS2 <- plot_grid(pa, pb, pc, pd, 
                   pe, pf, pg, ph, 
                   pi, pj, pk, pl,
                   pm, pn, po, pp,
                   ncol = 4, nrow = 4,
                   labels = c("(a)", "(b)", "(c)", "(d)", 
                              "(e)", "(f)", "(g)", "(h)", 
                              "(i)", "(j)", "(k)", "(l)",
                              "(m)", "(n)", "(o)", "(p)"),
                   vjust = 2.2, hjust = -0.45,
                   label_colour = "white")

# Extra label annotations at the top of the plot 
FigS2 <-
  plot_grid(NULL, FigS3, NULL, 
            ncol = 1, nrow = 3, 
            rel_heights = c(0.03, 1, 0.01)) + 
  annotate("text", label = "L. boo",
           x = 1/8, y = 0.977, 
           size = 5, colour = "black", fontface = "italic") +
  annotate("text", label = "F1 ~~ x ~~ italic(L. ~~ boo)", parse=T,
           x = 3/8, y = 0.977, 
           size = 5, colour = "black") +
  annotate("text", label = "L. wil",
           x = 5/8, y = 0.977, 
           size = 5, colour = "black", fontface = "italic") +
  annotate("text", label = "F1 ~~ x ~~ italic(L. ~~ wil)", parse=T,
           x = 7/8, y = 0.977, 
           size = 5, colour = "black") 

#ggsave(plot = FigS2, filename = "Fig_S2.png", width = 190, height = 200, units = "mm",  dpi = 700, bg = "white")
