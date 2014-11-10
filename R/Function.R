#########################
# subset and droplevels #
#########################
subsetD <- function(...) droplevels(subset(...))

###########################################
# produce box plots with transformed data #
###########################################
# log OR sqrt OR power(1/3) OR inverse OR box-cox
bxplts <- function(value, ofst = 0, data, ...){
  data$y <- data[[value]] + ofst #ofst is added to make y >0
  a <- boxcox(y ~ CO2 * time, data = data)
  par(mfrow = c(2, 3))
  boxplot(y ~ CO2*time, data, main = "raw")
  boxplot(log(y) ~ CO2*time, main = "log", data)
  boxplot(sqrt(y) ~ CO2*time, main = "sqrt", data)
  boxplot(y^(1/3) ~ CO2*time, main = "power(1/3)", data)
  boxplot(1/y ~ CO2*time, main = "inverse", data)
  BCmax <- a$x[a$y == max(a$y)]
  texcol <- ifelse(BCmax < 0, "red", "black") 
  boxplot(y^(BCmax) ~ CO2*time, 
          main = "", sep = "=", 
          data = data)
  title(main = paste("Box Cox", round(BCmax, 4)), 
        col.main = texcol)
  par(mfrow = c(1,1))
}

##############################
# Save ggplot in PDF and PNG #
##############################
ggsavePP <- function(filename, plot, width, height){
  ggsave(filename = paste(filename, ".pdf", sep = ""), 
         plot = plot, 
         width = width, 
         height = height)
  
  ggsave(filename = paste(filename, ".png", sep = ""), 
         plot = plot, 
         width = width, 
         height = height, 
         dpi = 600)
}

###############
# sample size #
###############
lengthN <- function(x) sum(!is.na(x))

################################
# Return star based on P value #
################################
FormatPval <- function(Pval) {
  stars <- ifelse(Pval > .1, "ns",
                  ifelse(Pval > .05, ".",
                         ifelse(Pval > .01, "*",
                                ifelse(Pval > .001, "**",
                                       c("***")))))
  
  p <- as.character(ifelse(Pval > .1, round(Pval, 3),
                           ifelse(Pval < .001, "bold('<0.001')", 
                                  # shown with bold font. Note that inside of
                                  # bold needs to be in ''
                                  paste("bold(", round(Pval, 3), ")", sep = "'"))))
  return(data.frame(stars, p))
} 

########################################
# Create summary stat table from anova #
########################################
StatTable <- function(x) { # x is anova result
  df <- data.frame(predictor = c(row.names(x)),
                   rbind(FormatPval(x$Pr)))
  
  # add a row for column name of the table in the fig 
  df <- rbind(df, data.frame(predictor = "", 
                             stars = "italic('P<F')", 
                             p = "italic('P<F')"))
  
  result <- merge(df, data.frame(predictor = c("co2", "year", "co2:year")), all = TRUE)
  
  # replace NA with ns
  result <- within(result, {
    p <- ifelse(is.na(p), "ns", as.character(p)) 
    # ifelse tries to return factor, so use as.character
    stars <- ifelse(is.na(stars), "ns", as.character(stars))
  })
  
  # relabel for plotting
  result$predictor <- factor(result$predictor, 
                             labels = c("", "CO[2]", "Year", "CO[2]*~x~Year"), 
                             levels = c("", "co2", "year", "co2:year"))
  result <- result[order(result$predictor), ]
  return(result)
}

############################################
# Create df to add a stat table to figures #
############################################
StatPositionDF <- function(StatRes, ytop, ylength, gap = .07){
  d <- data.frame(ytop, gap = gap * ylength) 
  # ytop is y coordinate for the top (i.e. CO2), ylength is the difference of
  # max and min value of the plot (i.e. max(mean+SE) - min(mean-SE)). 0.1 *
  # ylength is used to determine the gap between each row of the table
  
  predictor <- levels(StatRes$predictor)
  
  # create df which contains variable, predictor and y coordinates for the other
  # predictors (i.e. Time, CO2xTime) which is ylength*0.1 (= gap) lower than one above
  d2 <- data.frame(predictor, ldply(1:length(predictor), function(z) d$ytop - z * d$gap))

  names(d2)[2] <- "yval"
  
  # mege every thing
  d3 <- merge(d2, StatRes, by = "predictor")
  d3$co2 <- "amb" # co2 column is required for ggplot
  return(d3)
}
