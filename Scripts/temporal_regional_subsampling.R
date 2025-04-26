# Temporal and regional subsampling 
# BH

setwd("~/GitHub/UoS_student_projects/24-25_MB_BelH/Data")

# create output data frame
regional_generic_diversity_results <- data.frame(
  Region = c("Triassic Region 1", "Triassic Region 2", "Triassic Region 3", "Triassic Region 4",
             "Jurassic Region 1", "Jurassic Region 2", "Jurassic Region 3", "Jurassic Region 4"),
  
  # create columns with NAs
  Coral_Richness = rep(NA, 8),
  Coral_Richness_SD = rep(NA, 8),
  Associated_Taxa_Richness = rep(NA, 8),
  Associated_Taxa_Richness_SD = rep(NA, 8),
  Average_Community_Diversity_Corals = rep(NA, 8),
  Average_Community_Diversity_Corals_SD = rep(NA, 8),
  Average_Community_Diversity_Associated_Taxa = rep(NA, 8),
  Average_Community_Diversity_Associated_Taxa_SD = rep(NA, 8)
)

# print empty data frame
print("Regional Generic Diversity Results (Metrics and their SDs as columns, Regions as rows):")
print(regional_generic_diversity_results)

# same thing for shannons diversity 
shannon_diversity_results <- data.frame(
  Region = c("Triassic Region 1", "Triassic Region 2", "Triassic Region 3", "Triassic Region 4",
             "Jurassic Region 1", "Jurassic Region 2", "Jurassic Region 3", "Jurassic Region 4"),
  
  Total_Shannon_Diversity_Region = rep(NA, 8),
  Total_Shannon_Diversity_Region_SD = rep(NA, 8),
  Average_Shannon_Diversity_Across_Formations = rep(NA, 8),
  Average_Shannon_Diversity_Across_Formations_SD = rep(NA, 8)
)

# print
print("Shannon's Diversity Results (Metrics and their SDs as columns, Regions as rows):")
print(shannon_diversity_results)

# functional groups dataframe

# list of all FG numbers in large dataset
fg_numbers <- c(115, 125, 241, 261, 310, 311, 312, 314, 315, 321, 322, 324, 325, 
                331, 341, 344, 351, 360, 361, 362, 364, 414, 415, 422, 425, 
                431, 432, 441, 451, 456, 461, 462, 464, 512, 521, 522, 523, 
                525, 526, 531, 532, 533, 535, 541, 546, 561, 631, 632, 636, 646)

# create empty dataframes, separate for each region
create_empty_region_df <- function(region_name) {
  data.frame(FG_number = fg_numbers, 
             relative_abundance = rep(NA, length(fg_numbers)),
             relative_abundance_sd = rep(NA, length(fg_numbers)), 
             Region = region_name)
}

t_region_1_df <- create_empty_region_df("Triassic Region 1")
t_region_2_df <- create_empty_region_df("Triassic Region 2")
t_region_3_df <- create_empty_region_df("Triassic Region 3")
t_region_4_df <- create_empty_region_df("Triassic Region 4")

j_region_1_df <- create_empty_region_df("Jurassic Region 1")
j_region_2_df <- create_empty_region_df("Jurassic Region 2")
j_region_3_df <- create_empty_region_df("Jurassic Region 3")
j_region_4_df <- create_empty_region_df("Jurassic Region 4")

#===== ATC start here ========#
# define iterations and quota
iter <- 1000
n.quota.time <- 400 # we have to bring this down since we're removing data that doesn't fall into our regions
n.quota.region <- 100

#ATC:
# Edited by BH on 18/03/25
#==== data input ===#
#load individual regional datasets 
load("FG regional data/func_triassic_reg_one_data.RData")
func_triassic_reg_one$region_name <- 'one'
load("FG regional data/func_triassic_reg_two_data.RData")
func_triassic_reg_two$region_name <- 'two'
load("FG regional data/func_triassic_reg_three_data.RData")
func_triassic_reg_three$region_name <- 'three'
load("FG regional data/func_triassic_reg_four_data.RData")
func_triassic_reg_four$region_name <- 'four'
load("FG regional data/func_jurassic_reg_one_data.RData")
func_jurassic_reg_one$region_name <- 'one'
load("FG regional data/func_jurassic_reg_two_data.RData")
func_jurassic_reg_two$region_name <- 'two'
load("FG regional data/func_jurassic_reg_three_data.RData")
func_jurassic_reg_three$region_name <- 'three'
load("FG regional data/func_jurassic_reg_four_data.RData")
func_jurassic_reg_four$region_name <- 'four'

#put together into one big dataset 
tj_data <- rbind(func_triassic_reg_one, func_triassic_reg_two, func_triassic_reg_three, func_triassic_reg_four,
                 func_jurassic_reg_one, func_jurassic_reg_two, func_jurassic_reg_three, func_jurassic_reg_four)
our_stages <- unique(tj_data$stage)
our_stages

library(divDyn)
data("stages", package="divDyn")
tj_rows <- stages[which(stages$stage %in% our_stages),'stg'] #what stages are present in our data?
tj_stages <- stages$stage[tj_rows] #what are the stage names?
tj_mids <- stages$mid[tj_rows] #what are the mid-point ages of the stages?
triassic_stages <- tj_stages[1:4]
triassic_mids <- tj_mids[1:4]
jurassic_stages <- tj_stages[5:8]
jurassic_mids <- tj_stages[5:8]

tj_periods <- c('Triassic', 'Jurassic')

tj_time_data <- data.frame(periods=c(rep('Triassic',3),rep('Jurassic',5)),
                           stages=tj_stages)
tj_time_data

#===== SUMBSAMPLING SUMMARY =====#
#For the triassic, then the jurassic 
#For each stage in the period,
#Get stage data corresponding to each stage within the triassic
#For # iterations, 
#Subsample for temporal quota for every stage in the period, and assemble together 
#Subsamlpe again for regional quota for every region
#Perform analyses on final subsampled dataset 

#===== Example for the Triassic ======#
this.period.stages <- subset(tj_time_data, periods == 'Triassic')
Carnian.data <- subset(tj_data, stage==this.period.stages$stages[1])
Norian.data <- subset(tj_data, stage==this.period.stages$stages[2])
Rhaetian.data <- subset(tj_data, stage==this.period.stages$stages[3])


Carnian.data.max <- nrow(Carnian.data)
Norian.data.max <- nrow(Norian.data)
Rhaetian.data.max <- nrow(Rhaetian.data)

temp_region_one_coral_richness <- rep(NA, iter)
temp_region_two_coral_richness <- rep(NA, iter)
temp_region_three_coral_richness <- rep(NA, iter)
temp_region_four_coral_richness <- rep(NA, iter)

temp_region_one_associated_taxa_richness <- rep(NA, iter)
temp_region_two_associated_taxa_richness <- rep(NA, iter)
temp_region_three_associated_taxa_richness <- rep(NA, iter)
temp_region_four_associated_taxa_richness <- rep(NA, iter)

temp_region_one_avg_community_diversity_corals <- rep(NA, iter)
temp_region_two_avg_community_diversity_corals <- rep(NA, iter)
temp_region_three_avg_community_diversity_corals <- rep(NA, iter)
temp_region_four_avg_community_diversity_corals <- rep(NA, iter)

temp_region_one_avg_community_diversity_associated_taxa <- rep(NA, iter)
temp_region_two_avg_community_diversity_associated_taxa <- rep(NA, iter)
temp_region_three_avg_community_diversity_associated_taxa <- rep(NA, iter)
temp_region_four_avg_community_diversity_associated_taxa <- rep(NA, iter)

temp_region_one_total_shannons <- rep(NA, iter)
temp_region_two_total_shannons <- rep(NA, iter)
temp_region_three_total_shannons <- rep(NA, iter)
temp_region_four_total_shannons <- rep(NA, iter)

temp_region_one_average_shannons <- rep(NA, iter)
temp_region_two_average_shannons <- rep(NA, iter)
temp_region_three_average_shannons <- rep(NA, iter)
temp_region_four_average_shannons <- rep(NA, iter)

temp_region_one_fg_abundance <- vector("list", iter)
temp_region_two_fg_abundance <- vector("list", iter)
temp_region_three_fg_abundance <- vector("list", iter)
temp_region_four_fg_abundance <- vector("list", iter)

#start subsampling
for(i in 1:iter){
  
  #first up: temporal subsampling
  Carnian.rand.idxs <- sample(1:Carnian.data.max, n.quota.time, replace=FALSE)
  Norian.rand.idxs <- sample(1:Norian.data.max, n.quota.time, replace=FALSE)
  Rhaetian.rand.idxs <- sample(1:Rhaetian.data.max, n.quota.time, replace=FALSE)
  
  #get temporal subsampled data
  Carnian.time.subbed <- Carnian.data[Carnian.rand.idxs,]
  Norian.time.subbed <- Norian.data[Norian.rand.idxs,]
  Rhaetian.time.subbed <- Rhaetian.data[Rhaetian.rand.idxs,]
  
  data.time.subbed <- rbind(Carnian.time.subbed, Norian.time.subbed, Rhaetian.time.subbed)
  
  #second round: regional subsampling
  region.one.data <- subset(data.time.subbed, region_name=='one')
  region.one.max <- nrow(region.one.data)
  region.two.data <- subset(data.time.subbed, region_name=='two')
  region.two.max <- nrow(region.two.data)
  region.three.data <- subset(data.time.subbed, region_name=='three')
  region.three.max <- nrow(region.three.data)
  region.four.data <- subset(data.time.subbed, region_name=='four')
  region.four.max <- nrow(region.four.data)
  
  if(region.one.max>n.quota.region){
    region.one.rand.idxs <- sample(1:region.one.max, n.quota.region, replace=FALSE)
    region.one.data <- region.one.data[region.one.rand.idxs,]
  } else(print(paste(tj_periods[i], ':region one not subsampled in iteration', i)))
  
  if(region.two.max>n.quota.region){
    region.two.rand.idxs <- sample(1:region.two.max, n.quota.region, replace=FALSE)
    region.two.data <- region.two.data[region.two.rand.idxs,]
  } else(print(paste(tj_periods[i], ':region two not subsampled in iteration', i)))
  
  if(region.three.max>n.quota.region){
    region.three.rand.idxs <- sample(1:region.three.max, n.quota.region, replace=FALSE)
    region.three.data <- region.three.data[region.three.rand.idxs,]
  } else(print(paste(tj_periods[i], ':region three not subsampled in iteration', i)))
  
  if(region.four.max>n.quota.region){
    region.four.rand.idxs <- sample(1:region.four.max, n.quota.region, replace=FALSE)
    region.four.data <- region.four.data[region.four.rand.idxs,]
  } else(print(paste(tj_periods[i], ':region four not subsampled in iteration', i)))
  
  
  #we can rbind those together for any global analyses
  subsampled.data <- rbind(region.one.data, region.two.data, region.three.data, region.four.data)