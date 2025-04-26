# Calculating abundance of functional groups
# BH 22/03/25
# TRIASSIC SCRIPT

library(dplyr)

# calculating relative abundance
temp_region_one_fg_abundance[[i]] <- region.one.data %>%
  count(FG_number) %>%
  mutate(relative_abundance = n / sum(n))

temp_region_two_fg_abundance[[i]] <- region.two.data %>%
  count(FG_number) %>%
  mutate(relative_abundance = n / sum(n))

temp_region_three_fg_abundance[[i]] <- region.three.data %>%
  count(FG_number) %>%
  mutate(relative_abundance = n / sum(n))

temp_region_four_fg_abundance[[i]] <- region.four.data %>%
  count(FG_number) %>%
  mutate(relative_abundance = n / sum(n))

}

# calculate mean relative abundance for each functional group across iterations

calculate_mean_abundance <- function(temp_region_data) {
  mean_abundance <- sapply(fg_numbers, function(fg) {
    # extract relative abundance values for the current FG across iterations
    fg_values <- sapply(temp_region_data, function(x) {
      fg_row <- x[x$FG_number == fg,]
      if (nrow(fg_row) > 0) {
        return(fg_row$relative_abundance)
      } else {
        return(0)  # if there are no occurences of the functional group, return 0
      }
    })
    mean(fg_values, na.rm = TRUE)
  })
  
  return(mean_abundance)
}

# calculate mean relative abundance for each region
mean_region_one_abundance <- calculate_mean_abundance(temp_region_one_fg_abundance)
mean_region_two_abundance <- calculate_mean_abundance(temp_region_two_fg_abundance)
mean_region_three_abundance <- calculate_mean_abundance(temp_region_three_fg_abundance)
mean_region_four_abundance <- calculate_mean_abundance(temp_region_four_fg_abundance)

# calculate the sd of relative abundance for region
calculate_sd_abundance <- function(temp_region_data) {
  sd_abundance <- sapply(fg_numbers, function(fg) {
    # extract the relative abundance values for the current FG across iterations
    fg_values <- sapply(temp_region_data, function(x) {
      fg_row <- x[x$FG_number == fg,]
      if (nrow(fg_row) > 0) {
        return(fg_row$relative_abundance)
      } else {
        return(0)  # if there are no occurences of the functional group, return 0
      }
    })
    sd(fg_values, na.rm = TRUE)  # calculate sd
  })
  
  return(sd_abundance)
}

# calculate the sd of relative abundance for each region
sd_region_one_abundance <- calculate_sd_abundance(temp_region_one_fg_abundance)
sd_region_two_abundance <- calculate_sd_abundance(temp_region_two_fg_abundance)
sd_region_three_abundance <- calculate_sd_abundance(temp_region_three_fg_abundance)
sd_region_four_abundance <- calculate_sd_abundance(temp_region_four_fg_abundance)

# add mean values to empty regional dataframes
t_region_1_df$relative_abundance <- mean_region_one_abundance
t_region_2_df$relative_abundance <- mean_region_two_abundance
t_region_3_df$relative_abundance <- mean_region_three_abundance
t_region_4_df$relative_abundance <- mean_region_four_abundance

# add sd values to empty regional dataframes
t_region_1_df$relative_abundance_sd <- sd_region_one_abundance
t_region_2_df$relative_abundance_sd <- sd_region_two_abundance
t_region_3_df$relative_abundance_sd <- sd_region_three_abundance
t_region_4_df$relative_abundance_sd <- sd_region_four_abundance

# check thats all good
print(t_region_1_df)
print(t_region_2_df)
print(t_region_3_df)
print(t_region_4_df)

###### Bray Curtis

library(vegan)

# data frame where rows are regions and columns are functional groups
bray_matrix <- data.frame(
  mean_region_one_abundance,
  mean_region_two_abundance,
  mean_region_three_abundance,
  mean_region_four_abundance
)

# rows represent regions and columns represent FG
bray_matrix <- t(bray_matrix)

# bray-Curtis dissimilarity
bray_dist <- vegdist(bray_matrix, method = "bray")

# print
print(bray_dist)
