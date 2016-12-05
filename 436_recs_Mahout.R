#==============================================================================
#==============================================================================
# 00_recs_Mahout
# Last Updated: 2016-11-18 by MJG
#==============================================================================
#==============================================================================

# Clear workspace
rm(list = ls())

# Load libraries
library(jsonlite)

#==============================================================================
# Data Import, Quality Check, Prep, and Export
#==============================================================================

#------------------------------------------------------------------------------
# Import
#------------------------------------------------------------------------------
# Load data
recs.in = stream_in(file("reviews_Grocery_and_Gourmet_Food_5.json.gz"))

#------------------------------------------------------------------------------
# Quality Check
#------------------------------------------------------------------------------
# Check for duplicated rows
anyDuplicated(recs.in,
              fromLast = TRUE)
anyDuplicated(recs.in[c("reviewerID", "asin")],
              fromLast = TRUE)

# Add a primary key to use for convenience
recs.in$recsPK = seq.int(nrow(recs.in))

# Reorder to put recsPK first
recs.in = recs.in[c(10, 1:9)]

#------------------------------------------------------------------------------
# Prep
#------------------------------------------------------------------------------
# Rename existing variables
names(recs.in)[names(recs.in) == "overall"] = "overall.num"

# Convert character features to factors
recs.in$reviewerID = as.factor(recs.in$reviewerID)
recs.in$asin = as.factor(recs.in$asin)

# Create numeric versions of factors
recs.in$reviewerID.num = recs.in$reviewerID
recs.in$asin.num = recs.in$asin

# Convert factor levels to numeric values
levels(recs.in$reviewerID.num) = seq(from  = 1,
                                     to = nlevels(recs.in$reviewerID.num),
                                     by = 1)
levels(recs.in$asin.num) = seq(from = 1,
                               to = nlevels(recs.in$asin.num),
                               by = 1)

# Convert factor features to numeric
recs.in$reviewerID.num = as.numeric(as.character(recs.in$reviewerID.num))
recs.in$asin.num = as.numeric(as.character(recs.in$asin.num))

# Create flag for subset of recs.in
recs.in$helpful.nan = sapply(recs.in$helpful,
                             function(x) ifelse(x[2] == 0, 1, 
                                                ifelse(x[1]>x[2], 1, 0)))

# Subset recs for export
recs.in = recs.in[recs.in$helpful.nan == 0, ]

# Drop unecessary features
recs.in = recs.in[c(1:3, 7, 11:13)]

#------------------------------------------------------------------------------
# Export
#------------------------------------------------------------------------------
# Export for use in Mahout
#   Note: order matters, must follow form of [userID], [productID], [rating]
write.table(x = recs.in[, c(5, 6, 4)],
            file = "recs_input.txt",
            quote = FALSE,
            sep = ",",
            col.names = FALSE,
            row.names = FALSE)

#==============================================================================
# Mahout Results
#==============================================================================

# Load recommendations
temp = read.csv(file = "part-r-00000",
                header = FALSE,
                sep = "\t",
                stringsAsFactors = FALSE)

# Remove brackets
temp$V2 = gsub(pattern = "\\[|\\]",
               replacement = "",
               x = temp$V2)

# Reshape from semi-wide to long
recs.out = data.frame()
for (row in 1:nrow(temp)){
    V1 = temp$V1[row]
    V2 = unlist(strsplit(x = temp$V2[row],
                         split = ",",
                         fixed = TRUE))
    recs.out = rbind(recs.out,
                     cbind(rep(x = V1,
                               times = length(V2)),
                           V2))
}

# Only keep asin value (not score)
recs.out$V2 = gsub(pattern = "\\:.*",
                   replacement = "\\1",
                   x = recs.out$V2)

# Change columns back to numeric for join
recs.out$V1 = as.numeric(as.character(recs.out$V1))
recs.out$V2 = as.numeric(recs.out$V2)

# Rename columns
colnames(recs.out) = c("reviewerID.num",
                       "asin.num")

# Clean up
rm(temp); rm(row); rm(V1); rm(V2)

#==============================================================================
# Recommendations Dataset
#==============================================================================

# Replace numeric values with original values
recs.out$reviewerID = recs.in$reviewerID[match(recs.out$reviewerID.num,
                                               recs.in$reviewerID.num)]
recs.out$asin = recs.in$asin[match(recs.out$asin.num,
                                   recs.in$asin.num)]

# Export
write.table(x = recs.out[, 3:4],
            file = "recs_output.csv",
            quote = FALSE,
            sep = ",",
            row.names = FALSE)

# Clean up
rm(list = ls(pattern = "recs."))

#==============================================================================
# FIN
#==============================================================================
sessionInfo()
