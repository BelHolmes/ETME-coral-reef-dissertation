
  # Analysis 1 - calculating regional diversity and average community diversity across formations (corals and coral associated taxa)
  # 12/03/25
  # BH
  # TRIASSIC SCRIPT

  library(dplyr)
  library(vegan)
  
  #Example: generic richness of corals for region one
  #region.one.corals <- subset(region.one.data, order=='Scleractinia')
  # region.one.coral.richness <- length(unique(region.one.corals$genus))
  #temp_region.one.coral.richness[i] <- region.one.coral.richness
  
  # Separate corals (order 'Scleractinia') and associated taxa (everything else)
  region.one.corals <- subset(region.one.data, order == 'Scleractinia')
  region.one.coral.richness <- length(unique(region.one.corals$genus))
  temp_region_one_coral_richness[i] <- region.one.coral.richness
  region.two.corals <- subset(region.two.data, order == 'Scleractinia')
  region.two.coral.richness <- length(unique(region.two.corals$genus))
  temp_region_two_coral_richness[i] <- region.two.coral.richness
  region.three.corals <- subset(region.three.data, order == 'Scleractinia')
  region.three.coral.richness <- length(unique(region.three.corals$genus))
  temp_region_three_coral_richness[i] <- region.three.coral.richness
  region.four.corals <- subset(region.four.data, order == 'Scleractinia')
  region.four.coral.richness <- length(unique(region.four.corals$genus))
  temp_region_four_coral_richness[i] <- region.four.coral.richness
  
  region.one.associated.taxa <- subset(region.one.data, order != 'Scleractinia')
  region.one.associated.taxa.richness <- length(unique(region.one.associated.taxa$genus))
  temp_region_one_associated_taxa_richness[i] <- region.one.associated.taxa.richness
  region.two.associated.taxa <- subset(region.two.data, order != 'Scleractinia')
  region.two.associated.taxa.richness <- length(unique(region.two.associated.taxa$genus))
  temp_region_two_associated_taxa_richness[i] <- region.two.associated.taxa.richness
  region.three.associated.taxa <- subset(region.three.data, order != 'Scleractinia')
  region.three.associated.taxa.richness <- length(unique(region.three.associated.taxa$genus))
  temp_region_three_associated_taxa_richness[i] <- region.three.associated.taxa.richness
  region.four.associated.taxa <- subset(region.four.data, order != 'Scleractinia')
  region.four.associated.taxa.richness <- length(unique(region.four.associated.taxa$genus))
  temp_region_four_associated_taxa_richness[i] <- region.four.associated.taxa.richness
  
  # Calculate formation-level diversity
  # Coral richness per formation
  region_one_coral_richness_per_formation <- tapply(region.one.corals$genus, region.one.corals$formation, function(x) length(unique(x)))
  temp_region_one_avg_community_diversity_corals[i] <- mean(region_one_coral_richness_per_formation, na.rm = TRUE)
  region_two_coral_richness_per_formation <- tapply(region.two.corals$genus, region.two.corals$formation, function(x) length(unique(x)))
  temp_region_two_avg_community_diversity_corals[i] <- mean(region_two_coral_richness_per_formation, na.rm = TRUE)
  region_three_coral_richness_per_formation <- tapply(region.three.corals$genus, region.three.corals$formation, function(x) length(unique(x)))
  temp_region_three_avg_community_diversity_corals[i] <- mean(region_three_coral_richness_per_formation, na.rm = TRUE)
  region_four_coral_richness_per_formation <- tapply(region.four.corals$genus, region.four.corals$formation, function(x) length(unique(x)))
  temp_region_four_avg_community_diversity_corals[i] <- mean(region_four_coral_richness_per_formation, na.rm = TRUE)
  
  # Associated taxa richness per formation
  region_one_associated_richness_per_formation <- tapply(region.one.associated.taxa$genus, region.one.associated.taxa$formation, function(x) length(unique(x)))
  temp_region_one_avg_community_diversity_associated_taxa[i] <- mean(region_one_associated_richness_per_formation, na.rm = TRUE)
  region_two_associated_richness_per_formation <- tapply(region.two.associated.taxa$genus, region.two.associated.taxa$formation, function(x) length(unique(x)))
  temp_region_two_avg_community_diversity_associated_taxa[i] <- mean(region_two_associated_richness_per_formation, na.rm = TRUE)
  region_three_associated_richness_per_formation <- tapply(region.three.associated.taxa$genus, region.three.associated.taxa$formation, function(x) length(unique(x)))
  temp_region_three_avg_community_diversity_associated_taxa[i] <- mean(region_three_associated_richness_per_formation, na.rm = TRUE)
  region_four_associated_richness_per_formation <- tapply(region.four.associated.taxa$genus, region.four.associated.taxa$formation, function(x) length(unique(x)))
  temp_region_four_avg_community_diversity_associated_taxa[i] <- mean(region_four_associated_richness_per_formation, na.rm = TRUE)
  
}

write.csv(temp_region_one_coral_richness, "tri_region_one_coral_richness.csv", row.names = FALSE)
write.csv(temp_region_two_coral_richness, "tri_region_two_coral_richness.csv", row.names = FALSE)
write.csv(temp_region_three_coral_richness, "tri_region_three_coral_richness.csv", row.names = FALSE)
write.csv(temp_region_four_coral_richness, "tri_region_four_coral_richness.csv", row.names = FALSE)

write.csv(temp_region_one_associated_taxa_richness, "tri_region_one_associated_taxa_richness.csv", row.names = FALSE)
write.csv(temp_region_two_associated_taxa_richness, "tri_region_two_associated_taxa_richness.csv", row.names = FALSE)
write.csv(temp_region_three_associated_taxa_richness, "tri_region_three_associated_taxa_richness.csv", row.names = FALSE)
write.csv(temp_region_four_associated_taxa_richness, "tri_region_four_associated_taxa_richness.csv", row.names = FALSE)

write.csv(temp_region_one_avg_community_diversity_corals, "tri_region_one_avg_community_diversity_corals.csv", row.names = FALSE)
write.csv(temp_region_two_avg_community_diversity_corals, "tri_region_two_avg_community_diversity_corals.csv", row.names = FALSE)
write.csv(temp_region_three_avg_community_diversity_corals, "tri_region_three_avg_community_diversity_corals.csv", row.names = FALSE)
write.csv(temp_region_four_avg_community_diversity_corals, "tri_region_four_avg_community_diversity_corals.csv", row.names = FALSE)

write.csv(temp_region_one_avg_community_diversity_associated_taxa, "tri_region_one_avg_community_diversity_associated_taxa.csv", row.names = FALSE)
write.csv(temp_region_two_avg_community_diversity_associated_taxa, "tri_region_two_avg_community_diversity_associated_taxa.csv", row.names = FALSE)
write.csv(temp_region_three_avg_community_diversity_associated_taxa, "tri_region_three_avg_community_diversity_associated_taxa.csv", row.names = FALSE)
write.csv(temp_region_four_avg_community_diversity_associated_taxa, "tri_region_four_avg_community_diversity_associated_taxa.csv", row.names = FALSE)

rownames(regional_generic_diversity_results) <- regional_generic_diversity_results$Region
regional_generic_diversity_results['Triassic Region 1','Coral_Richness'] <- mean(temp_region_one_coral_richness, na.rm=TRUE)
regional_generic_diversity_results['Triassic Region 1','Coral_Richness_SD'] <- sd(temp_region_one_coral_richness, na.rm=TRUE)
regional_generic_diversity_results['Triassic Region 1','Associated_Taxa_Richness'] <- mean(temp_region_one_associated_taxa_richness, na.rm=TRUE)
regional_generic_diversity_results['Triassic Region 1','Associated_Taxa_Richness_SD'] <- sd(temp_region_one_associated_taxa_richness, na.rm=TRUE)
regional_generic_diversity_results['Triassic Region 1','Average_Community_Diversity_Corals'] <- mean(temp_region_one_avg_community_diversity_corals, na.rm=TRUE)
regional_generic_diversity_results['Triassic Region 1','Average_Community_Diversity_Corals_SD'] <- sd(temp_region_one_avg_community_diversity_corals, na.rm=TRUE)
regional_generic_diversity_results['Triassic Region 1','Average_Community_Diversity_Associated_Taxa'] <- mean(temp_region_one_avg_community_diversity_associated_taxa, na.rm=TRUE)
regional_generic_diversity_results['Triassic Region 1','Average_Community_Diversity_Associated_Taxa_SD'] <- sd(temp_region_one_avg_community_diversity_associated_taxa, na.rm=TRUE)

regional_generic_diversity_results['Triassic Region 2','Coral_Richness'] <- mean(temp_region_two_coral_richness, na.rm=TRUE)
regional_generic_diversity_results['Triassic Region 2','Coral_Richness_SD'] <- sd(temp_region_two_coral_richness, na.rm=TRUE)
regional_generic_diversity_results['Triassic Region 2','Associated_Taxa_Richness'] <- mean(temp_region_two_associated_taxa_richness, na.rm=TRUE)
regional_generic_diversity_results['Triassic Region 2','Associated_Taxa_Richness_SD'] <- sd(temp_region_two_associated_taxa_richness, na.rm=TRUE)
regional_generic_diversity_results['Triassic Region 2','Average_Community_Diversity_Corals'] <- mean(temp_region_two_avg_community_diversity_corals, na.rm=TRUE)
regional_generic_diversity_results['Triassic Region 2','Average_Community_Diversity_Corals_SD'] <- sd(temp_region_two_avg_community_diversity_corals, na.rm=TRUE)
regional_generic_diversity_results['Triassic Region 2','Average_Community_Diversity_Associated_Taxa'] <- mean(temp_region_two_avg_community_diversity_associated_taxa, na.rm=TRUE)
regional_generic_diversity_results['Triassic Region 2','Average_Community_Diversity_Associated_Taxa_SD'] <- sd(temp_region_two_avg_community_diversity_associated_taxa, na.rm=TRUE)

regional_generic_diversity_results['Triassic Region 3','Coral_Richness'] <- mean(temp_region_three_coral_richness, na.rm=TRUE)
regional_generic_diversity_results['Triassic Region 3','Coral_Richness_SD'] <- sd(temp_region_three_coral_richness, na.rm=TRUE)
regional_generic_diversity_results['Triassic Region 3','Associated_Taxa_Richness'] <- mean(temp_region_three_associated_taxa_richness, na.rm=TRUE)
regional_generic_diversity_results['Triassic Region 3','Associated_Taxa_Richness_SD'] <- sd(temp_region_three_associated_taxa_richness, na.rm=TRUE)
regional_generic_diversity_results['Triassic Region 3','Average_Community_Diversity_Corals'] <- mean(temp_region_three_avg_community_diversity_corals, na.rm=TRUE)
regional_generic_diversity_results['Triassic Region 3','Average_Community_Diversity_Corals_SD'] <- sd(temp_region_three_avg_community_diversity_corals, na.rm=TRUE)
regional_generic_diversity_results['Triassic Region 3','Average_Community_Diversity_Associated_Taxa'] <- mean(temp_region_three_avg_community_diversity_associated_taxa, na.rm=TRUE)
regional_generic_diversity_results['Triassic Region 3','Average_Community_Diversity_Associated_Taxa_SD'] <- sd(temp_region_three_avg_community_diversity_associated_taxa, na.rm=TRUE)

regional_generic_diversity_results['Triassic Region 4','Coral_Richness'] <- mean(temp_region_four_coral_richness, na.rm=TRUE)
regional_generic_diversity_results['Triassic Region 4','Coral_Richness_SD'] <- sd(temp_region_four_coral_richness, na.rm=TRUE)
regional_generic_diversity_results['Triassic Region 4','Associated_Taxa_Richness'] <- mean(temp_region_four_associated_taxa_richness, na.rm=TRUE)
regional_generic_diversity_results['Triassic Region 4','Associated_Taxa_Richness_SD'] <- sd(temp_region_four_associated_taxa_richness, na.rm=TRUE)
regional_generic_diversity_results['Triassic Region 4','Average_Community_Diversity_Corals'] <- mean(temp_region_four_avg_community_diversity_corals, na.rm=TRUE)
regional_generic_diversity_results['Triassic Region 4','Average_Community_Diversity_Corals_SD'] <- sd(temp_region_four_avg_community_diversity_corals, na.rm=TRUE)
regional_generic_diversity_results['Triassic Region 4','Average_Community_Diversity_Associated_Taxa'] <- mean(temp_region_four_avg_community_diversity_associated_taxa, na.rm=TRUE)
regional_generic_diversity_results['Triassic Region 4','Average_Community_Diversity_Associated_Taxa_SD'] <- sd(temp_region_four_avg_community_diversity_associated_taxa, na.rm=TRUE)
print(regional_generic_diversity_results) 
