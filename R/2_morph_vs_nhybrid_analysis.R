# Analysis comparing morphological IDs to NewHybrids IDs

# Check if the required packages are installed -----

# If not, install and load them
{list.of.packages <- 
  c("tidyverse", "sf")
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
      select(code, code_a, rand_id) %>% 
      rename(id = code,
             site = code_a) %>% 
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


# Summarise the no. of frogs of each spp/hybrid (NewHybrids vs morphology) -----

## count of each species/hybrid using NewHyb vs each person
id_count <- 
  dat_long %>% 
  count(id_method, species) 

## to assess morphology, we want to calculate the mean and sd count of each 
## spp/hybrid (of each person's IDs), but if someone didn't select certain 
## categories (e.g. F1, F2), then we need to set the count to 0 to calculate sd

all_combos <- 
  id_count %>% 
  expand(id_method, species)

id_count <- 
  id_count %>% 
  right_join(all_combos) %>% 
  mutate_all(~replace(., is.na(.), 0))

# function to round numbers to one digit
round_1_dig <- function(x){
  round(x, digits=1)
}

# morphology results
Table1_m <-
  id_count %>% 
  group_by(id_method) %>% 
  mutate(percent = n/sum(n) * 100, 
         N = sum(n)) %>% 
  ungroup() %>% 
  #select only morphological ids
  filter(id_method != "newhybrids_id") %>% 
  group_by(species) %>% 
  summarise(morph_n = mean(n),
            morph_n_sd = sd(n),
            morph_percent = mean(percent),
            morph_percent_sd = sd(percent)) %>% 
  ungroup() %>% 
  #round to 1 d.p.
  mutate(across(morph_n:morph_percent_sd, round_1_dig))

# all results
Table1 <-
  # summarise NewHyb results
  id_count %>% 
  group_by(id_method) %>% 
  mutate(gene_percent = n/sum(n) * 100,
         gene_percent = round(gene_percent, digits = 1)) %>% 
  ungroup() %>% 
  filter(id_method == "newhybrids_id") %>% 
  rename(gene_n = n) %>% 
  select(-id_method) %>% 
  # add morphology results
  left_join(Table1_m, by = "species") %>% 
  # merge columns so it is n (+/- sd), plus percent in brackets
  unite(col = "morph_n", morph_n:morph_n_sd, sep = " +/- ") %>% 
  unite(col = "morph_percent", morph_percent:morph_percent_sd, sep = " +/- ") %>% 
  unite(col = "gene", gene_n:gene_percent, sep = " (" ) %>% 
  unite(col = "morph", morph_n:morph_percent, sep = " (" ) %>% 
  mutate(gene = paste0(gene, ")"),
         morph = paste0(morph, ")"))


# mean number (and percentage) of hybrids based on morphology for 3 validators
id_count %>% 
  #select only morphological ids
  filter(id_method != "newhybrids_id") %>% 
  mutate(h_or_not = if_else(species == "boo" | species == "wil", species, "hybrid")) %>% 
  group_by(id_method, h_or_not) %>% summarise(n = sum(n)) %>% 
  group_by(id_method) %>% 
  mutate(percent = n/sum(n) * 100, 
         N = sum(n)) %>% 
  ungroup() %>% 
  #select only morphological ids
  filter(id_method != "newhybrids_id") %>% 
  group_by(h_or_not) %>% 
  summarise(morph_n = mean(n),
            morph_n_sd = sd(n),
            morph_percent = mean(percent),
            morph_percent_sd = sd(percent)) %>% 
  ungroup() %>% 
  #round to 1 d.p.
  mutate(across(morph_n:morph_percent_sd, round_1_dig))


# Chi-square test
chisq.test(x = c(73, 35.7, 3, 0.7, 9.3, 4.3), 
           p = c(72/126, 30/126, 2/126, 4/126, 10/126, 8/126))


# examine number/percentage of hybrids at each site (Table S1)
dat_long %>% 
  count(site, id_method, species) %>% 
  group_by(site, id_method) %>% 
  mutate(gene_percent = n/sum(n) * 100,
         gene_percent = round(gene_percent, digits = 1)) %>% 
  ungroup() %>% 
  filter(id_method == "newhybrids_id") %>% 
  rename(gene_n = n) %>% 
  select(-id_method) %>% 
  mutate(class = if_else(species == "boo" | species == "wil", 
                         species, 
                         "hybrid")) %>% 
  group_by(site, class) %>% 
  summarise(site_n = sum(gene_n),
            site_perc = sum(gene_percent)) %>% 
  distinct() %>% 
  filter(class == "hybrid") %>% 
  arrange(site_perc)

site_combo_hyb <- 
  dat_long %>% 
  count(site, id_method, species) %>% 
  group_by(site, id_method) %>% 
  mutate(gene_percent = n/sum(n) * 100,
         gene_percent = round(gene_percent, digits = 1)) %>% 
  ungroup() %>% 
  filter(id_method == "newhybrids_id") %>% 
  rename(gene_n = n) %>% 
  select(-id_method) %>% 
  mutate(class = if_else(species == "boo" | species == "wil", 
                         species, 
                         "hybrid")) %>% 
  group_by(site, class) %>% 
  summarise(site_n = sum(gene_n),
            site_perc = sum(gene_percent)) %>% 
  distinct() %>% 
  pivot_wider(names_from = class, values_from = c("site_n", "site_perc")) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  unite(col = "L.boo", c("site_n_boo", "site_perc_boo"), sep = " (") %>% 
  unite(col = "hybrid", c("site_n_hybrid", "site_perc_hybrid"), sep = " (") %>% 
  unite(col = "L.wil", c("site_n_wil", "site_perc_wil"), sep = " (") %>% 
  summarise("L.booroolongensis" = paste0(L.boo, ")"),
            "hybrids" = paste0(hybrid, ")"),
            "L.wilcoxii" = paste0(L.wil, ")"))

site_each_hyb <- 
  dat_long %>% 
  count(site, id_method, species) %>% 
  group_by(site, id_method) %>% 
  mutate(gene_percent = n/sum(n) * 100,
         gene_percent = round(gene_percent, digits = 1)) %>% 
  ungroup() %>% 
  filter(id_method == "newhybrids_id") %>% 
  rename(gene_n = n) %>% 
  select(-id_method) %>% 
  mutate(class = if_else(species == "boo" | species == "wil", 
                         species, 
                         "hybrid")) %>% 
  group_by(site, species) %>% 
  summarise(site_n = sum(gene_n),
            site_perc = sum(gene_percent)) %>% 
  distinct() %>% 
  pivot_wider(names_from = species, values_from = c("site_n", "site_perc")) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  unite(col = "L.boo", c("site_n_boo", "site_perc_boo"), sep = " (") %>% 
  unite(col = "F1xboo", c("site_n_F1xboo", "site_perc_F1xboo"), sep = " (") %>% 
  unite(col = "F1", c("site_n_F1", "site_perc_F1"), sep = " (") %>% 
  unite(col = "F2", c("site_n_F2", "site_perc_F2"), sep = " (") %>% 
  unite(col = "F1xwil", c("site_n_F1xwil", "site_perc_F1xwil"), sep = " (") %>% 
  unite(col = "L.wil", c("site_n_wil", "site_perc_wil"), sep = " (") %>% 
  summarise("L.booroolongensis" = paste0(L.boo, ")"),
            "F1xboo" = paste0(F1xboo, ")"),
            "F1" = paste0(F1, ")"),
            "F2" = paste0(F2, ")"),
            "F1xwil" = paste0(F1xwil, ")"),
            "L.wilcoxii" = paste0(L.wil, ")")) 

site_table <- 
  left_join(site_combo_hyb, site_each_hyb) %>% 
  select(site, "L.booroolongensis", "L.wilcoxii", "hybrids", 
         "F1", "F2", "F1xboo", "F1xwil") %>% 
  rename("All_hybrids" = "hybrids")

site_table[site_table == "0 (0)"] <- "-"
site_table


# 'Correct' vs 'incorrect' IDs (compare morphology to NewHybrids results) -----

# Function to calculate the number (and %) of 'correct' IDs in each class
# (assuming the NewHybrids ID is the correct ID)
# i.e. of all the individuals that were genotyped as boo/wil/hybrids, 
# how many did we get right/wrong?

##' @param group_hybs Whether to combine individual hybrid categories 
##' (i.e. backcrosses, F1, F2) into 1 "hybrid" category (binary: TRUE or FALSE) 
##' @param sp_category Which species/hybrid category to summarise

calc_correct_hyb <- function(group_hybs, sp_category){
  
  dat <- 
    # if group_hybs = TRUE, collapse all hybrid categories into a 
    # single hybrid category before further calculations
    if(isTRUE(group_hybs)){
      dat %>% 
        mutate(nhyb_id_hgroup = 
                 ifelse(newhybrids_id == "boo" | newhybrids_id == "wil",
                        newhybrids_id, 
                        "hybrid"),
               morph_id_hgroup = 
                 ifelse(morph_id == "boo" | morph_id == "wil", 
                        morph_id, 
                        "hybrid")) %>% 
        filter(nhyb_id_hgroup == sp_category) %>% 
        mutate(result = if_else(morph_id_hgroup == nhyb_id_hgroup, 
                                "correct", 
                                "incorrect"))
    } 
  # if group_hybs = FALSE, leave all hybrid categories as is  
  # (i.e. do not collapse into a single hybrid category 
  # before further calculations)
  else if(isFALSE(group_hybs)){
    dat %>% 
      filter(newhybrids_id == sp_category) %>% 
      mutate(result = if_else(morph_id == newhybrids_id, 
                              "correct", 
                              "incorrect"))
  }
  
  # get counts and percentages
  df <-
    dat %>% 
    group_by(validator, result) %>% 
    count() %>% 
    group_by(validator) %>% 
    mutate(sum = sum(n),
           percent = n/sum(n) * 100,
           percent = round(percent, digits = 1)) %>% 
    ungroup()
  
  df %>%
    # if a validator did not identify any frogs in a certain category 
    # (e.g. F1, F2), then add a row (and make n = 0)
    right_join(df %>% 
                 expand(validator, result),
               by = c("validator", "result")) %>% 
    filter(result == "correct") %>%  #remove incorrect rows as they are redundant
    mutate_all(~replace(., is.na(.), 0)) %>% 
    summarise(validator = c(validator, 'mean'),
              across(where(is.numeric), ~ c(., mean(.)))
    ) %>% 
    mutate(n = round(n, digits = 2),
           percent = round(percent, digits = 1)) %>% 
    # replace zeros in the sum column (no. of individuals) with the actual no.
    mutate(sum = if_else(sum != max(sum), max(sum), sum)) %>% 
    # combine count and percentages into 1 column (% in brackets)
    unite(col = n, c(n, sum), sep = "/") %>% 
    unite(col = n_, c(n, percent), sep = " (" ) %>% 
    mutate(n_ = paste0(n_, ")"),
           group = sp_category)
  
}

# results for each hybrid category (plus boo and wil)
correct_each_hybs <- 
  c("boo", "F1xboo", "F1", "F2", "F1xwil" ,"wil") %>% 
  map_df(~ calc_correct_hyb(group_hybs = F, sp_category = .)) %>% 
  pivot_wider(names_from = group, values_from = c(n_)) 

# results when hybrids are combined (plus boo and wil)
correct_combo_hybs <- 
  c("boo", "hybrid", "wil") %>% 
  map_df(~ calc_correct_hyb(group_hybs = T, sp_category = .)) %>% 
  pivot_wider(names_from = group, values_from = c(n_)) 

# results for all individuals combined
correct_all_indiv <- 
  dat %>% 
  mutate(result = if_else(morph_id == newhybrids_id, 
                          "correct", 
                          "incorrect")) %>% 
  count(validator, result)  %>% 
  group_by(validator) %>% 
  mutate(sum = sum(n),
         percent = n/sum(n) * 100,
         percent = round(percent, digits = 1)) %>% 
  ungroup() %>% 
  filter(result == "correct") %>%  #remove incorrect rows as they are redundant
  # get mean for all validators
  summarise(validator = c(validator, 'mean'),
            across(where(is.numeric), ~ c(., mean(.)))
  ) %>%  
  mutate(n = round(n, digits = 2),
         percent = round(percent, digits = 1)) %>% 
  # combine count and percentages into 1 column (% in brackets)
  unite(col = n, c(n, sum), sep = "/") %>% 
  unite(col = n_, c(n, percent), sep = " (" ) %>% 
  mutate(all = paste0(n_, ")")) %>% 
  select(validator, all)

Table2 <- 
  left_join(correct_combo_hybs, correct_each_hybs) %>% 
  left_join(correct_all_indiv) %>% 
  select(validator, all, boo, wil, hybrid, F1, F2, F1xboo, F1xwil)

Table2


# Function to calculate the number (and %) of 'correct' morphological IDs -----
# (assuming the NewHybrids ID is the correct ID)
# i.e. of all the individuals we called boo/wil/hybrids, 
# how many did we get right/wrong?

##' @param group_hybs Whether to combine individual hybrid categories 
##' (i.e. backcrosses, F1, F2) into 1 "hybrid" category (binary: TRUE or FALSE) 
##' @param sp_category Which species/hybrid category to summarise

calc_correct_by_morph <- function(group_hybs, sp_category){
  
  dat <- 
    # if group_hybs = TRUE, collapse all hybrid categories into a 
    # single hybrid category before further calculations
    if(isTRUE(group_hybs)){
      dat %>% 
        mutate(nhyb_id_hgroup = 
                 ifelse(newhybrids_id == "boo" | newhybrids_id == "wil",
                        newhybrids_id, 
                        "hybrid"),
               morph_id_hgroup = 
                 ifelse(morph_id == "boo" | morph_id == "wil", 
                        morph_id, 
                        "hybrid")) %>% 
        filter(morph_id_hgroup == sp_category) %>% 
        mutate(result = if_else(morph_id_hgroup == nhyb_id_hgroup, 
                                "correct", 
                                "incorrect"))
    } 
  # if group_hybs = FALSE, leave all hybrid categories as is  
  # (i.e. do not collapse into a single hybrid category 
  # before further calculations)
  else if(isFALSE(group_hybs)){
    dat %>% 
      filter(morph_id == sp_category) %>% 
      mutate(result = if_else(morph_id == newhybrids_id, 
                              "correct", 
                              "incorrect"))
  }
  
  # get counts and percentages
  df <-
    dat %>% 
    group_by(validator, result) %>% 
    count() %>% 
    group_by(validator) %>% 
    mutate(sum = sum(n),
           percent = n/sum(n) * 100,
           percent = round(percent, digits = 1)) %>% 
    ungroup()
  
  df %>%
    # if a validator did not identify any frogs in a certain category 
    # (e.g. F1, F2), then add a row (and make n = 0)
    right_join(df %>% 
                 expand(validator, result),
               by = c("validator", "result")) %>% 
    mutate_all(~replace(., is.na(.), 0)) %>% 
    group_by(validator) %>% 
    # replace zeros in the sum column (no. of individuals) with the no. in the class
    mutate(sum = if_else(sum != max(sum), max(sum), sum)) %>% 
    ungroup() %>%  
    filter(result == "correct") %>%  #remove incorrect rows as they are redundant
    summarise(validator = c(validator, 'mean'),
              across(where(is.numeric), ~ c(., mean(.))) 
    ) %>% 
    mutate(n = round(n, digits = 2),
           sum = round(sum, digits = 2),
           percent = round(percent, digits = 1)) %>% 
    # combine count and percentages into 1 column (% in brackets)
    unite(col = n, c(n, sum), sep = "/") %>% 
    unite(col = n_, c(n, percent), sep = " (" ) %>% 
    mutate(n_ = paste0(n_, ")"),
           group = sp_category)
  
}

# results for each hybrid class (plus boo and wil)
morph_correct_each_hy_class <- 
  c("boo", "F1xboo", "F1", "F2", "F1xwil" ,"wil") %>% 
  map_df(~ calc_correct_by_morph(group_hybs = F, sp_category = .)) %>% 
  pivot_wider(names_from = group, values_from = c(n_)) 

# results when hybrids are combined (plus boo and wil)
morph_correct_combo_hyb <- 
  c("boo", "hybrid", "wil") %>% 
  map_df(~ calc_correct_by_morph(group_hybs = T, sp_category = .)) %>% 
  pivot_wider(names_from = group, values_from = c(n_)) 

Table3 <- 
  left_join(morph_correct_combo_hyb, morph_correct_each_hy_class) %>% 
  left_join(correct_all_indiv) %>% 
  select(validator, all, boo, wil, hybrid, F1, F2, F1xboo, F1xwil)

Table3


# Compare SVL and mass -----

# mean +/- SD and SE
TableS2 <- 
  dat %>% 
  left_join(
    read_csv("Data/Morphological_analyses/frog_list.csv") 
    ) %>% 
  select(id, newhybrids_id, sex, SVL_mm, mass_g) %>% 
  distinct() %>%  
  group_by(newhybrids_id, sex) %>% 
  summarise(mean_SVL = mean(SVL_mm), 
            sd_SVL = sd(SVL_mm), 
            se_SVL = sd(SVL_mm)/length(SVL_mm),
            mean_mass = mean(mass_g), 
            sd_mass = sd(mass_g),
            se_mass = sd(mass_g)/length(mass_g),
            n = length(mass_g)) %>% 
  arrange(newhybrids_id)

TableS2


# Calculate area of geographic ranges -----

# load species distributions
range_boo <- 
  st_read("Data/Shapefiles_for_figures/Litoria_booroolongensis.shp")

range_wilc <- 
  st_read("Data/Shapefiles_for_figures/Litoria_wilcoxii.shp")

# L. wilcoxii in km2
st_area(range_wilc)/1000^2

# L. booroolongensis in km2
st_area(range_boo)/1000^2

# area of intersection of L. boo and L. wilcoxii ranges in km2
int <- st_intersection(range_boo, range_wilc)
st_area(int)/1000^2
