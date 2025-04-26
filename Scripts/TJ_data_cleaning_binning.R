## Initial Cleaning of PBDB TJ dataset
# BH 
# 28/10/24
# Last edited 13/11/24

#loading the raw data
library(divDyn)
dat <- read.csv("TJ_raw_data_23-10-2024.csv", header=TRUE)
original_data <- dat

#taxonomic processing
dat <- dat[dat$accepted_rank %in% c("genus", "species"), ]
dat <- dat[dat$genus!="", ]
nrow(dat)
#99995
print(paste('fossils removed from taxonomic resolution cleaning:', nrow(original_data)-nrow(dat)))
#fossils removed from taxonomic resolution cleaning: 1435

# levels(factors(dat$phylum))
#A. phyla
marineNoPlant <- c("",
                   "Agmata",
                   "Annelida",
                   "Bilateralomorpha",
                   "Brachiopoda",
                   "Bryozoa",
                   "Calcispongea",
                   "Chaetognatha",
                   "Cnidaria",
                   "Ctenophora",
                   "Echinodermata",
                   "Entoprocta",
                   "Foraminifera",
                   "Hemichordata",
                   "Hyolitha",
                   "Mollusca",
                   "Nematoda",
                   "Nematomorpha",
                   "Nemertina",
                   "Onychophora",
                   "Petalonamae",
                   "Phoronida",
                   "Platyhelminthes",
                   "Porifera",
                   "Problematica",
                   "Rhizopodea",
                   "Rotifera",
                   "Sarcomastigophora",
                   "Sipuncula",
                   "Uncertain",
                   "Vetulicolia",
                   ""
)

# logical vector of rows indicating these
bByPhyla <- dat$phylum %in% marineNoPlant

# noNeed <- dat[!bByPhyla,]
#B. classes
#levels(factor(noNeed$class))
needClass <- c(
  "Acanthodii",
  "Actinopteri",
  "Actinopterygii",
  "Agnatha",
  "Cephalaspidomorphi",
  "Chondrichthyes",
  "Cladistia",
  "Coelacanthimorpha",
  "Conodonta",
  "Galeaspida",
  "Myxini",
  "Osteichthyes",
  "Petromyzontida",
  "Plagiostomi",
  "Pteraspidomorphi",
  "Artiopoda",
  "Branchiopoda",
  "Cephalocarida",
  "Copepoda",
  "Malacostraca",
  "Maxillopoda",
  "Megacheira",
  "Merostomoidea",
  "Ostracoda",
  "Paratrilobita",
  "Pycnogonida",
  "Remipedia",
  "Thylacocephala",
  "Trilobita",
  "Xiphosura"
)

# logical vector of rows indicating occurrences
bNeedClass <- dat$class %in% needClass

#C. mammals
# mammals <- dat[dat$class=="Mammalia", ]
# levels(factor(mammals$order))
needMammalOrd <- c("Cetacea", "Sirenia")
bMammalOrder <- dat$order %in% needMammalOrd

# the carnivores
# carnivores <- dat[dat$order=="Carnivora", ]
# levels(factor(carnivores$family))
needFam <- c("Otariidae", "Phocidae", "Desmatophocidae")
bNeedMamFam <- dat$family %in% needFam

# D. Reptiles
# reptiles <- dat[dat$class=="Reptilia", ]
# levels(factor(reptiles$order))
needReptOrd<-c(
  "Eosauropterygia",
  "Hupehsuchia",
  "Ichthyosauria",
  "Placodontia",
  "Sauropterygia",
  "Thalattosauria"
)
# the logical vector for the total data
bRept <- dat$order %in% needReptOrd

# E. Sea turtles
# turtles <- dat[dat$order=="Testudines", ]
# levels(factor(turtles$family))
needTurtleFam <- c(
  "Cheloniidae",
  "Protostegidae",
  "Dermochelyidae",
  "Dermochelyoidae",
  "Toxochelyidae",
  "Pancheloniidae"
)
bTurtle <- dat$family%in%needTurtleFam

dat <- dat[bByPhyla | bNeedClass | bMammalOrder | bNeedMamFam | bRept | bTurtle, ]

nrow(dat)
#78064
nrow(original_data)
#101430
print((paste('cleaning has removed ', nrow(original_data)-nrow(dat), 'fossil occurrences')))
#cleaning has removed  23366 fossil occurrences

# resolve the potential homonymy problem
dat$clgen <- paste(dat$class, dat$genus)

#environmental filtering
levels(factor((dat$environment)))

omitEnv <- c(
  "\"floodplain\"", "alluvial fan", "cave", "\"channel\"", "channel lag" ,
  "coarse channel fill", "crater lake", "crevasse splay", "dry floodplain",
  "delta plain", "dune", "eolian indet.", "fine channel fill", "fissure fill",
  "fluvial indet.", "fluvial-lacustrine indet.", "fluvial-deltaic indet.",
  "glacial", "interdune", "karst indet.", "lacustrine - large",
  "lacustrine - small", "lacustrine delta front", "lacustrine delta plain",
  "lacustrine deltaic indet.", "lacustrine indet.",
  "lacustrine interdistributary bay", "lacustrine prodelta", "levee", "loess",
  "mire/swamp", "pond", "sinkhole", "spring", "tar", "terrestrial indet.",
  "wet floodplain")

# omit the occurrences
dat <- dat[!dat$environment%in%omitEnv, ]

nrow(dat)
#78025
print((paste('cleaning has removed ', nrow(original_data)-nrow(dat), 'fossil occurrences')))
#cleaning has removed  23405 fossil occurrences

dat <- dat[dat$lithification1!="unlithified", ]
nrow(dat)
#77858
print((paste('cleaning has removed ', nrow(original_data)-nrow(dat), 'fossil occurrences')))
#cleaning has removed  23572 fossil occurrences

data(keys)
# using the others will be included in an appendix to this vignette
names(keys)

# siliciclastic or carbonate?
dat$lith <- categorize(dat$lithology1,keys$lith)
# batyhmetry
dat$bath <- categorize(dat$environment,keys$bath)
# grain size
dat$gra <- categorize(dat$lithology1,keys$grain)
# reef or not?
dat$reef <- categorize(dat$environment, keys$reef)
dat$reef[dat$lith=="clastic" & dat$environment=="marine indet."] <- "non-reef"
table(dat$reef)

#non-reef 31064
#reef 7410
#unknown 39384

## Stratigraphic binning 
#             column names = $stage
#.                           #series
#Author: Alison Cribb (A.T.Cribb@soton.ac.uk)
#Created: 20 September 2023
#Last edited: 1 November 2023
# Added to script 14/11/25 BH

library(divDyn)
library(dplyr)

data(stages)
data(keys)

stage_binning <- function(Paleozoic_data){
  
  #Following ddPhanero (Kocscis et al.)
  #for the Paleozoic_data
  stgMin <- categorize(Paleozoic_data[,'early_interval'], keys$stgInt)
  stgMin <- as.numeric(stgMin)
  stgMax <- categorize(Paleozoic_data[,'late_interval'], keys$stgInt)
  stgMax <- as.numeric(stgMax)
  
  Paleozoic_data$stg <- rep(NA, nrow(Paleozoic_data))
  
  stgCondition <- c(
    which(stgMax==stgMin),
    which(stgMax==-1))
  Paleozoic_data$stg[stgCondition] <- stgMin[stgCondition]
  
  #remove any data without a stage assignment 
  Paleozoic_data <- Paleozoic_data[!is.na(Paleozoic_data$stg),] 
  
  #and finally put names to stgs 
  for(i in 1:length(Paleozoic_data$stg)){
    #find the stage number assinged from earl and late interval
    stage_number <- Paleozoic_data$stg[i]
    #what is the corrseponding stage in stages?
    stagename <- stages$stage[stage_number]
    #put that back in the dataset
    Paleozoic_data$stage[i] <- stagename
  }
  
  return(Paleozoic_data)
  
}

binned_dat <- stage_binning(dat)
table(binned_dat$stage)

#remove data without a stage assignment
binned_dat <- subset(binned_dat, !is.na(stage))
print(paste('stratigraphic binning removed ', nrow(dat)-nrow(binned_dat), 'fossil occurrences')) 
#stratigraphic binning removed  6565 fossil occurrences

print(paste('total fossil occurrences:', nrow(binned_dat)))
#total fossil occurrences: 71293
print(paste('total genera:', length(unique(binned_dat$genus))))
#total genera: 3238

#final data
TJ_data <- binned_dat
save(TJ_data, file='TJ_cleaned_binned_final-14-11-2024.RData')
write.csv(TJ_data, file='TJ_cleaned_binned_final-14-11-2024.csv')
