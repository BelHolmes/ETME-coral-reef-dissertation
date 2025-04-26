# Analysis 2 - calculating Shannon's Diversity (H) for coral-associated taxa each region and averaged across each formation.
# 11/02/25
# BH
# TRIASSIC SCRIPT
## Corrected errors with support from AI

##### 18/03
library(vegan)

## For Region 1
associated_taxa_one <- region.one.data[region.one.data$order != 'Scleractinia', ]
taxa_table_one <- table(associated_taxa_one$formation, associated_taxa_one$genus)
summed_abundance_one <- colSums(taxa_table_one)
shannon_diversity_region_one <- diversity(summed_abundance_one, index = "shannon")
shannon_diversity_formations_one <- apply(taxa_table_one, 1, function(x) diversity(x, index = "shannon"))
avg_shannon_diversity_across_formations_one <- mean(shannon_diversity_formations_one, na.rm = TRUE)

# Store results in the temporary vectors
temp_region_one_total_shannons[i] <- shannon_diversity_region_one
temp_region_one_average_shannons[i] <- avg_shannon_diversity_across_formations_one

## For Region 2
associated_taxa_two <- region.two.data[region.two.data$order != 'Scleractinia', ]
taxa_table_two <- table(associated_taxa_two$formation, associated_taxa_two$genus)
summed_abundance_two <- colSums(taxa_table_two)
shannon_diversity_region_two <- diversity(summed_abundance_two, index = "shannon")
shannon_diversity_formations_two <- apply(taxa_table_two, 1, function(x) diversity(x, index = "shannon"))
avg_shannon_diversity_across_formations_two <- mean(shannon_diversity_formations_two, na.rm = TRUE)

# Store results in the temporary vectors
temp_region_two_total_shannons[i] <- shannon_diversity_region_two
temp_region_two_average_shannons[i] <- avg_shannon_diversity_across_formations_two

## For Region 3
associated_taxa_three <- region.three.data[region.three.data$order != 'Scleractinia', ]
taxa_table_three <- table(associated_taxa_three$formation, associated_taxa_three$genus)
summed_abundance_three <- colSums(taxa_table_three)
shannon_diversity_region_three <- diversity(summed_abundance_three, index = "shannon")
shannon_diversity_formations_three <- apply(taxa_table_three, 1, function(x) diversity(x, index = "shannon"))
avg_shannon_diversity_across_formations_three <- mean(shannon_diversity_formations_three, na.rm = TRUE)

# Store results in the temporary vectors
temp_region_three_total_shannons[i] <- shannon_diversity_region_three
temp_region_three_average_shannons[i] <- avg_shannon_diversity_across_formations_three

## For Region 4
associated_taxa_four <- region.four.data[region.four.data$order != 'Scleractinia', ]
taxa_table_four <- table(associated_taxa_four$formation, associated_taxa_four$genus)
summed_abundance_four <- colSums(taxa_table_four)
shannon_diversity_region_four <- diversity(summed_abundance_four, index = "shannon")
shannon_diversity_formations_four <- apply(taxa_table_four, 1, function(x) diversity(x, index = "shannon"))
avg_shannon_diversity_across_formations_four <- mean(shannon_diversity_formations_four, na.rm = TRUE)

# Store results in the temporary vectors
temp_region_four_total_shannons[i] <- shannon_diversity_region_four
temp_region_four_average_shannons[i] <- avg_shannon_diversity_across_formations_four
}

write.csv(temp_region_one_total_shannons, "tri_region_one_total_shannons.csv", row.names = FALSE)
write.csv(temp_region_one_average_shannons, "tri_region_one_average_shannons.csv", row.names = FALSE)
write.csv(temp_region_two_total_shannons, "tri_region_two_total_shannons.csv", row.names = FALSE)
write.csv(temp_region_two_average_shannons, "tri_region_two_average_shannons.csv", row.names = FALSE)
write.csv(temp_region_three_total_shannons, "tri_region_three_total_shannons.csv", row.names = FALSE)
write.csv(temp_region_three_average_shannons, "tri_region_three_average_shannons.csv", row.names = FALSE)
write.csv(temp_region_four_total_shannons, "tri_region_four_total_shannons.csv", row.names = FALSE)
write.csv(temp_region_four_average_shannons, "tri_region_four_average_shannons.csv", row.names = FALSE)


# After all iterations are complete, calculate the mean Shannon diversity values across iterations for each region
shannon_diversity_results <- data.frame(
  Region = c("Triassic Region 1", "Triassic Region 2", "Triassic Region 3", "Triassic Region 4"),
  Total_Shannon_Diversity_Region = c(mean(temp_region_one_total_shannons, na.rm = TRUE),
                                     mean(temp_region_two_total_shannons, na.rm = TRUE),
                                     mean(temp_region_three_total_shannons, na.rm = TRUE),
                                     mean(temp_region_four_total_shannons, na.rm = TRUE)),
  Total_Shannon_Diversity_Region_SD = c(sd(temp_region_one_total_shannons, na.rm = TRUE),
                                        sd(temp_region_two_total_shannons, na.rm = TRUE),
                                        sd(temp_region_three_total_shannons, na.rm = TRUE),
                                        sd(temp_region_four_total_shannons, na.rm = TRUE)),
  Average_Shannon_Diversity_Across_Formations = c(mean(temp_region_one_average_shannons, na.rm = TRUE),
                                                  mean(temp_region_two_average_shannons, na.rm = TRUE),
                                                  mean(temp_region_three_average_shannons, na.rm = TRUE),
                                                  mean(temp_region_four_average_shannons, na.rm = TRUE)),
  Average_Shannon_Diversity_Across_Formations_SD = c(sd(temp_region_one_average_shannons, na.rm = TRUE),
                                                     sd(temp_region_two_average_shannons, na.rm = TRUE),
                                                     sd(temp_region_three_average_shannons, na.rm = TRUE),
                                                     sd(temp_region_four_average_shannons, na.rm = TRUE))
)

# Print the results
print(shannon_diversity_results)
