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
