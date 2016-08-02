###############################################################################
# R_EDA_Sandbox_Functions.R
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

#==============================================================================
# Accuracy
#==============================================================================

#--------------------------------------
# fit()
#--------------------------------------
# Function to add MSE to other measures from forecast::accuracy
fit = function(f, x){
    require(forecast)
    temp = data.frame(forecast::accuracy(f, x),
                      forecast::accuracy(f, x)[, 2]^2)
    temp = temp[, -c(1)]
    colnames(temp)[6] <- "MSE"
    temp = temp[c(6, 1, 2, 3, 4, 5)]
    print(temp)
}

#==============================================================================
# Missing Observations
#==============================================================================

#--------------------------------------
# miss.flag()
#--------------------------------------
# Function to create indicator variables as missing flags
miss.flag = function(df, cn = c("num", "fac")){
    if (missing(cn)){
        cols = colnames(df)
    } else if (cn == "num"){
        cols = colnames(df[, !sapply(df, is.factor)])
    } else if (cn == "fac"){
        cols = colnames(df[, sapply(df, is.factor)])
    }
    for (i in cols){
        if (sum(is.na(df[, i])) > 0){
            df[paste("MF", i, sep = "_")] =
                as.factor(ifelse(is.na(df[, i]), 1, 0))
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
num.boxplot = function(df, df.fac){
    df.name = deparse(substitute(df))
    cols = colnames(df[, !sapply(df, is.factor)])
    for (i in cols){
        if (missing(df.fac)){
            boxplot(df[, i], col = "grey", horizontal = T,
                    main = paste("Boxplot of ", df.name, "$", i, sep = ""),
                    xlab = paste(df.name, "$", i, sep = ""),
                    ylab = "Values")
        } else if (!class(df.fac) %in% c("factor")){
            stop("Please supply a factor variable to df.fac")
        } else {
            fac = unlist(strsplit(deparse(substitute(df.fac)),
                                  split = "$", fixed = T))[2]
            boxplot(df[, i] ~ df[, fac], col = "grey", horizontal = T,
                    main = paste(df.name, "$", i," versus ",
                                 deparse(substitute(df.fac)), sep = ""),
                    ylab = "Values")
        }
    }
}

#--------------------------------------
# num.hist()
#--------------------------------------
# Function to create histograms of numeric variables
# Optional choice of normal curve overlay
num.hist = function(df, prob = F, norm = T){
    df.name = deparse(substitute(df))
    cols = colnames(df[, !sapply(df, is.factor)])
    for (i in cols){
        main = paste("Histogram of ", df.name, "$", i, sep = "")
        sub = ifelse(norm, "normal curve overlay (blue)", "")
        y = hist(df[, i], plot = F)
        if (prob){
            seq = seq(0.0, 1.0, by = 0.1)
            h = hist(df[, i], col = "grey", main = main, sub = sub,
                     breaks = quantile(df[, i], probs = seq),
                     xlab = paste(df.name, "$", i, sep = ""))
        }
        if (!prob){
            h = hist(df[, i], col = "grey", main = main, sub = sub,
                     ylim = c(0, 1.1*max(y$counts)),
                     xlab = paste(df.name, "$", i, sep = ""))
        }
        if (norm){
            xfit = seq(min(df[, i]), max(df[, i]), length = 100)
            yfit = dnorm(xfit, mean = mean(df[, i]), sd = sd(df[, i]))
            if (norm & !prob){
                yfit = yfit * diff(h$mids[1:2]) * length(df[, i])
            }
            lines(xfit, yfit, col = "blue", lwd = 2)
        }
    }
}

#--------------------------------------
# num.qq()
#--------------------------------------
# Function to create Q-Q plots of numeric variables
num.qq = function(df){
    df.name = deparse(substitute(df))
    cols = colnames(df[, !sapply(df, is.factor)])
    for (i in cols){
        qqnorm(df[, i], pch = 21, bg = "grey",
               main = paste("Normal Q-Q Plot of ", df.name, "$", i, sep = ""))
        qqline(df[, i], lwd = 2, col = "blue")
    }
}

#--------------------------------------
# num.scatter()
#--------------------------------------
# Function to create scatterplots of numeric variables
num.scatter = function(df, df.num){
    df.name = deparse(substitute(df))
    num = unlist(strsplit(deparse(substitute(df.num)),
                          split = "$", fixed = T))[2]
    cols = colnames(df[, !sapply(df, is.factor)])
    for (i in cols){
        plot(df[, i], df[, num], pch = 21, bg = "grey",
             main = paste(df.name, "$", num, " versus ",
                          df.name, "$", i, sep = ""),
             ylab = paste(df.name, "$", num, sep = ""),
             xlab = paste(df.name, "$", i, sep = ""))
    }
}

#--------------------------------------
# num.plots()
#--------------------------------------
#--------------------------------------
# num.plots()
#--------------------------------------
# Function to produce four plots per variable:
# num.plots(which = ) corresponds as follows:
#   1 = Histogram
#   2 = Scatterplot
#   3 = Boxplot
#   4 = QQ Plot
num.plots = function(df, df.num, df.fac, prob = F, norm = T,
                     which = c(1, 2, 3, 4)){
    if (missing(which)){
        par(mfcol = c(2, 2))
    }
    df.name = deparse(substitute(df))
    cols = colnames(df[, !sapply(df, is.factor)])
    for (i in cols){
        #------------------------------
        # Histograms
        #------------------------------
        if (1 %in% which){
            main = paste("Histogram of ", df.name, "$", i, sep = "")
            sub = ifelse(norm, "normal curve overlay (blue)", "")
            y = hist(df[, i], plot = F)
            if (prob){
                seq = seq(0.0, 1.0, by = 0.1)
                h = hist(df[, i], col = "grey", main = main, sub = sub,
                         breaks = quantile(df[, i], probs = seq),
                         xlab = paste(df.name, "$", i, sep = ""))
            }
            if (!prob){
                h = hist(df[, i], col = "grey", main = main, sub = sub,
                         ylim = c(0, 1.1*max(y$counts)),
                         xlab = paste(df.name, "$", i, sep = ""))
            }
            if (norm){
                xfit = seq(min(df[, i]), max(df[, i]), length = 100)
                yfit = dnorm(xfit, mean = mean(df[, i]), sd = sd(df[, i]))
                if (norm & !prob){
                    yfit = yfit * diff(h$mids[1:2]) * length(df[, i])
                }
                lines(xfit, yfit, col = "blue", lwd = 2)
            }
        }
        #------------------------------
        # Scatterplots
        #------------------------------
        if (2 %in% which){
            num = unlist(strsplit(deparse(substitute(df.num)),
                                  split = "$", fixed = T))[2]
            plot(df[, i], df[, num], pch = 21, bg = "grey",
                 main = paste(df.name, "$", num, " versus ",
                              df.name, "$", i, sep = ""),
                 ylab = paste(df.name, "$", num, sep = ""),
                 xlab = paste(df.name, "$", i, sep = ""))
        }
        #------------------------------
        # Boxplots
        #------------------------------
        if (3 %in% which){
            if (missing(df.fac)){
                boxplot(df[, i], col = "grey", horizontal = T,
                        main = paste("Boxplot of ", df.name, "$", i, sep = ""),
                        xlab = paste(df.name, "$", i, sep = ""),
                        ylab = "Values")
            } else if (!class(df.fac) %in% c("factor")){
                stop("Please supply a factor variable to df.fac")
            } else {
                fac = unlist(strsplit(deparse(substitute(df.fac)),
                                      split = "$", fixed = T))[2]
                boxplot(df[, i] ~ df[, fac], col = "grey", horizontal = T,
                        main = paste(df.name, "$", i," versus ",
                                     deparse(substitute(df.fac)), sep = ""),
                        ylab = "Values")
            }
        }
        #------------------------------
        # QQ Plots
        #------------------------------
        if (4 %in% which){
            qqnorm(df[, i], pch = 21, bg = "grey",
                   main = paste("Normal Q-Q Plot of ", df.name, "$", i, sep = ""))
            qqline(df[, i], lwd = 2, col = "blue")
        }
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
num.freq = function(df.fac, df.cn){
    if (!class(df.fac) %in% c("factor")){
        stop("Please supply a factor variable to df.fac")
    }
    table.results = data.frame()
    df = eval(as.name(paste(unlist(strsplit(deparse(substitute(df.fac)),
                            split = "$", fixed = T))[1])))
    fac = unlist(strsplit(deparse(substitute(df.fac)),
                          split = "$", fixed = T))[2]
    if (missing(df.cn)){
        cols = colnames(df[, !sapply(df, is.factor)])
    } else if (!class(df.cn) %in% c("numeric", "integer")){
        stop("Please supply a numeric or integer variable to df.cn")
    } else {
        cols = unlist(strsplit(deparse(substitute(df.cn)),
                               split = "$", fixed = T))[2]
    }
    for (i in cols){
        name.var = rep(paste(i), each = nlevels(df[, fac]))
        name.split = rep(paste(fac), each = nlevels(df[, fac]))
        table.level = levels(df[, fac])
        table.agg = format(aggregate(df[, i], by = list(Var = df[, fac]),
                                     summary)$x, nsmall = 2)
        table.row = as.data.frame(cbind(name.var, name.split,
                                        table.level, table.agg))
        table.results = rbind(table.results, table.row)
    }
    colnames(table.results)[1] = "Variable"
    colnames(table.results)[2] = "Split On"
    colnames(table.results)[3] = "Levels"
    return(table.results)
}

#--------------------------------------
# num.scale()
#--------------------------------------
# Function to scale (normalize: mean = 0, sd = 1) numeric variables
num.scale = function(df){
    cols = colnames(df[, !sapply(df, is.factor)])
    for (i in cols){
        i_cs = paste(i, "cs", sep = "_")
        df[i_cs] = scale(df[, i])
    }
    return(df)
}

#--------------------------------------
# num.trans()
#--------------------------------------
# Function to transform numeric variables
num.trans = function(df){
    cols = colnames(df[, !sapply(df, is.factor)])
    for (i in cols){
        # Natural Log
        i_ln = paste(i, "ln", sep = "_")
        df[i_ln] = (sign(df[, i]) * log(abs(df[, i])+1))
        # Square Root
        i_rt = paste(i, "rt", sep = "_")
        df[i_rt] = (sign(df[, i]) * sqrt(abs(df[, i])+1))
        # Square
        i_sq = paste(i, "sq", sep = "_")
        df[i_sq] = (df[, i] * df[, i])
    }
    return(df)
}

#--------------------------------------
# num.trims()
#--------------------------------------
# Function to trim numeric variables at various percentiles
num.trims = function(df){
    require(scales)
    cols = colnames(df[, !sapply(df, is.factor)])
    for (i in cols){
        # 1st and 99th
        T99 = quantile(df[, i], c(0.01, 0.99))
        df[paste(i, "T99", sep = "_")] = squish(df[, i], T99)
        # 5th and 95th
        T95 = quantile(df[, i], c(0.05, 0.95))
        df[paste(i, "T95", sep = "_")] = squish(df[, i], T95)
        # 10th and 90th
        T90 = quantile(df[, i], c(0.10, 0.90))
        df[paste(i, "T90", sep = "_")] = squish(df[, i], T90)
        # 25th and 75th
        T75 = quantile(df[, i], c(0.25, 0.75))
        df[paste(i, "T75", sep = "_")] = squish(df[, i], T75)
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
fac.freq = function(df.fac, df.cn, cat = T){
    if (!class(df.fac) %in% c("factor")){
        stop("Please supply a factor variable to df.fac")
    }
    table.results = data.frame()
    df = eval(as.name(paste(unlist(strsplit(deparse(substitute(df.fac)),
                                            split = "$", fixed = T))[1])))
    fac = unlist(strsplit(deparse(substitute(df.fac)),
                          split = "$", fixed = T))[2]
    if (missing(df.cn)){
        cols = colnames(df[, sapply(df, is.factor)])
    } else if (!class(df.cn) %in% c("factor")){
        stop("Please supply a factor variable to df.cn")
    } else {
        cols = unlist(strsplit(deparse(substitute(df.cn)),
                               split = "$", fixed = T))[2]
    }
    if (cat){
        for (i in cols){
            name.var = rep(paste(i), each = nlevels(df[, fac]))
            name.split = rep(paste(fac), each = nlevels(df[, fac]))
            table.level = levels(df[, fac])
            table.agg = aggregate(df[, i], by = list(Var = df[, fac]),
                                  summary)$x
            table.prop = format(round(prop.table(table.agg, 1) * 100,
                                      digits = 2), nsmall = 2)
            table.results = as.data.frame(cbind(name.var, name.split,
                                                table.level, table.prop))
            colnames(table.results)[1] = "Variable"
            colnames(table.results)[2] = "Split On"
            colnames(table.results)[3] = "Levels"
            if (missing(df.cn)){
                print(table.results)
            } else {
                return(table.results)
            }
        }
    }
    if (!cat){
        name.var = rep(paste(fac), each = 2)
        name.type = c("Count", "Percent")
        table.agg = t(summary(df[, fac]))
        table.prop = format(round(prop.table(table.agg) * 100,
                                  digits = 2), nsmall = 2)
        table.row = rbind(table.agg, table.prop)
        table.col = cbind(name.var, name.type, table.row)
        table.results = as.data.frame(table.col)
        colnames(table.results)[1] = "Variable"
        colnames(table.results)[2] = "Type"
        return(table.results)
    }
}

#--------------------------------------
# fac.flag()
#--------------------------------------
# Function to create indicator variables from factor variable levels
fac.flag = function(df){
    cols = colnames(df[, sapply(df, is.factor)])
    for (i in cols){
        for (level in unique(df[, i])){
            df[paste(i, level, sep = "_")] =
                as.factor(ifelse(df[, i] == level, 1, 0))
        }
    }
    return(df)
}

#==============================================================================
# FIN
#==============================================================================
