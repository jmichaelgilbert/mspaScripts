###############################################################################
# R_EDA_Sandbox.R
# Last updated: 2016-07-16 by MJG
###############################################################################

# A compilation of useful functions to [ideally] deploy on any data set
# Deploy after prepping data (e.g. converting variables, removing NAs, etc.)
# Requires a list of variable names to execute on, don't forget to update list!

# Suggested order of deployment:
#   Convert variables as necessary (e.g. to factors)
#   Plots for EDA on numeric and factor variables
#   Missing flags
#   Missing imputes
#   Trims
#   Transforms

# **See 'Bonus: Demo' at end of code**

#==============================================================================
# Staging & Prep
#==============================================================================

# These commands must be adapted to the data set, they **cannot** be run blind!

# Store data set name for use in titles, etc. later
data.name = "df$"

# Set response variable
data.response = "var"

# Assign rownames
df.rn = as.numeric(rownames(df))

# Assign full column names
df.cn.all = colnames(df)

# Assign numeric column names
df.cn.num = colnames(df[, !sapply(df, is.factor)])

# Assign factor column names
df.cn.fac = colnames(df[, sapply(df, is.factor)])

# Assign column names *except* for missing flag variables
# This is necessary due to order of deployment
df.cn.all = grep("^[MF_]", colnames(df), value = T, invert = T)

#==============================================================================
# Accuracy
#==============================================================================

#--------------------------------------
# fit()
#--------------------------------------
# Function to add MSE to other measures from forecast::accuracy
fit <- function(f, x){
    require(forecast)
    temp <- data.frame(forecast::accuracy(f, x), 
                       forecast::accuracy(f, x)[, 2]^2)
    temp <- temp[, -c(1)]
    colnames(temp)[6] <- "MSE"
    temp <- temp[c(6, 1, 2, 3, 4, 5)]
    print(temp)
}

#==============================================================================
# Missing Observations
#==============================================================================

#--------------------------------------
# miss.flag()
#--------------------------------------
# Function to create indicator variables as missing flags
miss.flag <- function(df, list){
    for (var in list){
        if (sum(is.na(df[, var])) > 0){
            df[paste("MF", var, sep = "_")] <- 
                ifelse(is.na(df[, var]), 1, 0)
        }
    }
    return(df)
}

#==============================================================================
# Numeric Variables
#==============================================================================

#------------------------------------------------------------------------------
# Plots
#------------------------------------------------------------------------------

#--------------------------------------
# num.boxplot()
#--------------------------------------
# Function to create boxplots of numeric variables
num.boxplot = function(df, list, var, vs = F){
    for (num in list){
        if (vs){
            boxplot(df[, num] ~ df[, var], col = "grey",
                    main = paste(data.name, num," versus ",
                                 data.name, var, sep = ""),
                    ylab = "Values")
        }
        if (!vs){
            boxplot(df[, num], col = "grey",
                    main = paste("Boxplot of ", data.name, num, sep = ""),
                    xlab = paste(data.name, num, sep = ""),
                    ylab = "Values")
        }
    }
}

#--------------------------------------
# num.hist()
#--------------------------------------
# Function to create histograms of numeric variables
# Optional choice of normal curve overlay
num.hist = function(df, list, norm = F){
    for (num in list){
        main = paste("Histogram of ", data.name, num, sep = "")
        sub = ifelse(norm, "normal curve overlay (blue)", "")
        y = hist(df[, num], plot = F)
        h = hist(df[, num], col = "grey", main = main, sub = sub,
                 ylim = c(0, 1.1*max(y$counts)),
                 xlab = paste(data.name, num, sep = ""))
        if (norm){
            xfit = seq(min(df[, num]), max(df[, num]), length = 100)
            yfit = dnorm(xfit, mean = mean(df[, num]), sd = sd(df[, num]))
            yfit = yfit * diff(h$mids[1:2]) * length(df[, num])
            lines(xfit, yfit, col = "blue", lwd = 2)
        }
    }
}

#--------------------------------------
# num.qq()
#--------------------------------------
# Function to create Q-Q plots of numeric variables
num.qq = function(df, list){
    for (num in list){
        qqnorm(df[, num], pch = 21, bg = "grey",
               main = paste("Normal Q-Q Plot of ", data.name, num, sep = ""))
        qqline(df[, num], lwd = 2, col = "blue")
    }
}

#--------------------------------------
# num.scatter()
#--------------------------------------
# Function to create scatterplots of numeric variables
num.scatter = function(df, list, var){
    for (num in list){
        plot(df[, num], df[, var], pch = 21, bg = "grey",
             main = paste(data.name, var, " versus ", 
                          data.name, num, sep = ""),
             ylab = paste(data.name, var, sep = ""),
             xlab = paste(data.name, num, sep = ""))
    }
}

#--------------------------------------
# num.plots()
#--------------------------------------
# Function to produce four plots per variable:
#   Scatterplot, Q-Q Plot, Histogram, Boxplot
num.plots = function(df, list, var, norm = F, vs = F){
    par(mfcol = c(2, 2))
    for (num in list){
        num.hist(df, num, norm)
        num.scatter(df, num, var)
        num.boxplot(df, num, var, vs)
        num.qq(df, num)
    }
    return(par(mfcol = c(1, 1)))
}

#------------------------------------------------------------------------------
# Variable Manipulation
#------------------------------------------------------------------------------

#--------------------------------------
# num.freq()
#--------------------------------------
# Summary statistics split by named factor for numeric variables
num.freq = function(df, list, var){
    for (fac in list){
        name.var = rep(paste(data.name, var, sep = ""),
                       each = nlevels(df[, fac]))
        name.split = rep(paste(data.name, fac, sep = ""),
                         each = nlevels(df[, fac]))
        table.level = levels(df[, fac])
        table.agg = format(aggregate(df[, var], by = list(Var = df[, fac]),
                                     summary)$x, nsmall = 2)
        table.results = as.data.frame(cbind(name.var, name.split, 
                                            table.level, table.agg))
        colnames(table.results)[1] = "Variable"
        colnames(table.results)[2] = "Split On"
        colnames(table.results)[3] = "Levels"
        print(table.results)
    }
}

#--------------------------------------
# num.trims()
#--------------------------------------
# Function to trim numeric variables at various percentiles
num.trims = function(df, list){
    require(scales)
    for (num in list){
        # 1st and 99th
        T99 = quantile(df[, num], c(0.01, 0.99))
        df[paste(num, "T99", sep = "_")] = squish(df[, num], T99)
        # 5th and 95th
        T95 = quantile(df[, num], c(0.05, 0.95))
        df[paste(num, "T95", sep = "_")] = squish(df[, num], T95)
        # 10th and 90th
        T90 = quantile(df[, num], c(0.10, 0.90))
        df[paste(num, "T90", sep = "_")] = squish(df[, num], T90)
        # 25th and 75th
        T75 = quantile(df[, num], c(0.25, 0.75))
        df[paste(num, "T75", sep = "_")] = squish(df[, num], T75)
    }
    return(df)
}

#--------------------------------------
# num.trans()
#--------------------------------------
# Function to transform numeric variables
num.trans = function(df, list){
    for (num in list){
        # Natural Log
        num_ln = paste(num, "ln", sep = "_")
        df[num_ln] = (sign(df[, num]) * log(abs(df[, num])+1))
        # Square Root
        num_rt = paste(num, "rt", sep = "_")
        df[num_rt] = (sign(df[, num]) * sqrt(abs(df[, num])+1))
        # Square
        num_sq = paste(num, "sq", sep = "_")
        df[num_sq] = (df[, num] * df[, num])
    }
    return(df)
}

#==============================================================================
# Factor Variables
#==============================================================================

#------------------------------------------------------------------------------
# Plots
#------------------------------------------------------------------------------

#--------------------------------------
# fac.boxplot()
#--------------------------------------
fac.boxplot = function(df, list, var){
    for (num in list){
        boxplot(df[, var] ~ df[, num], col = "grey",
                main = paste(data.name, num," versus ",
                             data.name, var, sep = ""),
                ylab = "Values")
    }
}

#--------------------------------------
# fac.barplot()
#--------------------------------------
# Function to create barplots of factor variables
fac.barplot = function(df, list, var = NULL, cat = F){
    for (fac in list){
        if (cat){
            barplot(table(df[, var], df[, fac]),
                    main = paste("Variable: ", data.name, fac, sep = ""),
                    ylim = c(0, 1.1*max(summary(df[, fac]))),
                    ylab = "Frequency", beside = T)
        }
        if (!cat){
            plot(df[, fac],
                 main = paste("Variable: ", data.name, fac, sep = ""),
                 ylim = c(0, 1.1*max(summary(df[, fac]))),
                 ylab = "Frequency")
        }
    }
}

#--------------------------------------
# fac.mosaic()
#--------------------------------------
# Function to create mosaic plots of factor variables
fac.mosaic = function(df, list, var){
    require(RColorBrewer)
    for (fac in list){
        plot(df[, var], df[, fac], 
             col = brewer.pal(nlevels(df[, fac]), "Spectral"),
             main = paste(data.name, var," versus ",
                          data.name, fac, sep = ""),
             xlab = paste(data.name, var, sep = ""),
             ylab = paste(data.name, fac, sep = ""))
    }
}

#------------------------------------------------------------------------------
# Variable Manipulation
#------------------------------------------------------------------------------

#--------------------------------------
# fac.freq()
#--------------------------------------
# Frequency of occurence split by named factor for factor variables
fac.freq = function(df, list, var, cat = T){
    for (fac in list){
        if (cat){
            name.var = rep(paste(data.name, var, sep = ""),
                           each = nlevels(df[, fac]))
            name.split = rep(paste(data.name, fac, sep = ""),
                             each = nlevels(df[, fac]))
            table.level = levels(df[, fac])
            table.agg = aggregate(df[, var], by = list(Var = df[, fac]), 
                                  summary)$x
            table.prop = format(round(prop.table(table.agg, 1) * 100, 
                                      digits = 2), nsmall = 2)
            table.results = as.data.frame(cbind(name.var, name.split, 
                                                table.level, table.prop))
            colnames(table.results)[1] = "Variable"
            colnames(table.results)[2] = "Split On"
            colnames(table.results)[3] = "Levels"
            print(table.results)
        }
        if (!cat){
            name.fac = rep(paste(data.name, fac, sep = ""), each = 2)
            name.type = c("Raw", "Percent")
            table.agg = t(summary(df[, fac]))
            table.prop = format(round(prop.table(table.agg) * 100, 
                                      digits = 2), nsmall = 2)
            table.row = rbind(table.agg, table.prop)
            table.col = cbind(name.fac, name.type, table.row)
            table.results = as.data.frame(table.col)
            colnames(table.results)[1] = "Variable"
            colnames(table.results)[2] = "Type"
            print(table.results)
        }
    }
}

#--------------------------------------
# fac.flag()
#--------------------------------------
# Function to create indicator variables from factor variable levels
fac.flag = function(df, list){
    for (fac in list){
        for (level in unique(df[, fac])){
            df[paste(fac, level, sep = "_")] = 
                ifelse(df[, fac] == level, 1, 0)
        }
    }
    return(df)
}

#==============================================================================
# FIN
#==============================================================================

###############################################################################
# Bonus: Demo
###############################################################################

# Download and assign data
library(ISLR)
auto = Auto

#------------------------------------------------------------------------------
# Data Prep
#------------------------------------------------------------------------------

# Assign column names
auto.cn.all = colnames(auto)

# Treat "?" as NA
auto[auto == "?"] = NA

# Create missing flags
auto = m.flag(auto, auto.cn.all)

# Assign data.frame with missing values removed
# Note: have to do this until a missing impute solution is coded
auto = na.omit(auto)

# Handle variable conversions
auto$horsepower = as.numeric(as.character(auto$horsepower))
auto$cylinders = as.factor(auto$cylinders)
auto$origin = as.factor(auto$origin)

# Drop 'name'
auto = subset(auto, select = -name)

# Assign column names, excluding missing flags
# all = all, numeric = num, factor = fac
auto.cn.all = grep("^MF_", colnames(auto), value = T, invert = T)
auto.cn.num = grep("^MF_", colnames(auto[, !sapply(auto, is.factor)]), 
                    value = T, invert = T)
auto.cn.fac = grep("^MF_", colnames(auto[, sapply(auto, is.factor)]), 
                    value = T, invert = T)

# Assign data name and response name
data.name = "auto$"
data.response = "mpg"

#------------------------------------------------------------------------------
# Visual EDA
#------------------------------------------------------------------------------

# Numeric variables
num.plots(auto, auto.cn.num, norm = T)

# Factor variables
fac.barplot(auto, auto.cn.fac)

#------------------------------------------------------------------------------
# Quantitative EDA
#------------------------------------------------------------------------------

# Numeric variables
summary(auto[, auto.cn.num])

# Factor variables
fac.freq(auto, auto.cn.fac)

#------------------------------------------------------------------------------
# Trims, Transforms, and Flags
#------------------------------------------------------------------------------

#--------------------------------------
# Numeric variables
#--------------------------------------
# Trims
auto = num.trims(auto, auto.cn.num)

# Re-assign column names, excluding missing flags
auto.cn.num = grep("^MF_", colnames(auto[, !sapply(auto, is.factor)]), 
                    value = T, invert = T)

# Transforms
auto = num.trans(auto, auto.cn.num)

#--------------------------------------
# Factor variables
#--------------------------------------
# Flags
auto = fac.flag(auto, auto.cn.fac)

#--------------------------------------
# Verification
#--------------------------------------
auto.names = data.frame(colnames(auto), sapply(auto, class))
rownames(auto.names) = seq(1, nrow(auto.names), by = 1)
colnames(auto.names) = c("Variable", "Class")
View(auto.names)

# All looks good, data are now ready for AVS!

#==============================================================================
# FIN
#==============================================================================
