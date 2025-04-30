# Script for genetic analyses  #####

# Check if the required packages are installed -----

# If not, install and load them
{list.of.packages <- 
  c("dartR", "tidyverse")
new.packages <- 
  list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
rm(list.of.packages, new.packages)}


# Load and check the genlight object -----

gl <- readRDS("Data/gl.Rdata") #has original species classifications as populations

# Check that the data looks as expected
names(gl@other$loc.metrics) # check names of available loc.metrics
gl@other$ind.metrics$id[1:10] # return first 10 IDs from individual metadata file
nInd(gl) # number of individuals in data
indNames(gl) # name of indivs in data
nPop(gl) # number of pops in data
popNames(gl) # list of pops in data
pop(gl) # list of pop assignments for each individual 
nLoc(gl) # number of loci
locNames(gl)[1:10] # list of loci (first 10)


# Generate a matrix of SNP scores 
# (0 = homozygous reference, 2 = homozygous alternate, 1 = heterozygous)
# matrix <- as.matrix(gl) 
# View(matrix)


# Filtering routine -----

# filter out loci with repeatability (reproducibility) < the threshold
gl_rep <- gl.filter.reproducibility(gl, threshold = 1)
nInd(gl_rep)
# filter out loci for which call rate (rate of non-missing values) is < threshold
gl_loc <- gl.filter.callrate(gl_rep, method = "loc", threshold = 0.95) 
nInd(gl_loc)
# filter out specimens for which call rate is < threshold
gl_ind <- gl.filter.callrate(gl_loc, method = "ind", threshold = 0.70) 
nInd(gl_ind)
indNames(gl_ind)
# filter out SNPs that share a sequence tag except one retained at random
gl_sec <- gl.filter.secondaries(gl_ind)
nInd(gl_sec)
# filter out monomorphic loci and loci that are scored all NA
gl_mon <- gl.filter.monomorphs(gl_sec) %>%  
  # recalculate metrics
  gl.recalc.metrics(.)
nInd(gl_mon)

# Summary of filtering results
gl_mon


# Look at the data -----

# Smear plot of individual against locus 
# (useful for gross pattern identification and assessment of allelic dropout) 
# and check filtering
glPlot(gl)
glPlot(gl_mon)

# PCA
pca1 <- glPca(gl_mon, parallel = FALSE) 
# look at the plot- how many PC's look important? (2-4?)

pca1

pca1$eig[1]/sum(pca1$eig) # proportion of variation explained by 1st axis
pca1$eig[2]/sum(pca1$eig) # proportion of variation explained by 2nd axis
pca1$eig[3]/sum(pca1$eig) # proportion of variation explained by 3rd axis
pca1$eig[4]/sum(pca1$eig) # proportion of variation explained by 4th axis

myCol <- colorplot(pca1$scores,pca1$scores, transp=TRUE, cex=4)
abline(h=0,v=0, col="grey")

# Alternative
pca <- gl.pcoa(gl_mon)
# plot eigenvalues 
gl.pcoa.scree(pca)

# Make csv file of PCs if doesn't already exist
if ("result_PC_scores.csv" %in% list.files("Data/")) {
  PC_scores <- read_csv("Data/result_PC_scores.csv")
  } else {
  write.csv(pca1$scores, file = "Data/result_PC_scores.csv")
  }


# NewHybrids analysis -----

# Load NewHybrids results or run the analysis

# Make csv file of hybrid status if doesn't already exist
if ("hybrid_status.csv" %in% list.files("Data/NewHybrids_results")) {
  hybrid_status <- read_csv("Data/NewHybrids_results/hybrid_status.csv")
} else {
  # note that this will take a bit of time to run
  gl.nhybrids(gl_mon, 
              outpath = getwd(), 
              outfile = "gl_mon.nhy", 
              method = "AvgPIC",
              # Enter the directory containing the NewHybrids program below -
              # this is where the output files will be saved.
              # You may need to reset your working directory after this
              nhyb.directory = "C:/Users")
  
  
  # look at the data
  hyb <- read_csv("Data/NewHybrids_results/aa-pofZ.csv") 
  hyb_dat <- 
    hyb %>% 
    pivot_longer(cols = c(P0:F1xP1), 
                 names_to = "h_status", 
                 values_to = "probability") %>% 
    filter(probability > 0.5) %>% 
    rename(likely_h_status = h_status, 
           prob = probability) %>% 
    left_join(
      hyb %>% 
        pivot_longer(cols = c(P0:F1xP1), 
                     names_to = "h_status", 
                     values_to = "probability") %>% 
        filter(probability < 0.5 & probability > 0) %>% 
        rename(alt_h_status = h_status, 
               alt_prob = probability),
      by = c("id", "pop")
    ) %>% 
    mutate(likely_h_status = gsub("P0", "boo", likely_h_status)) %>% 
    mutate(likely_h_status = gsub("P1", "wil", likely_h_status))
  
  write.csv(hyb_dat, file = "Data/NewHybrids_results/hybrid_status.csv")
}
