#############
# all depth #
#############
# Ring mean
# RAD = Ring All Depth
RAD_lst <- llply(list(mean, sd, lengthN),
                  function(x) dcast(phDF, year + depth + midD ~ ring, 
                                    value.var = "ph", x))
# modify names
names(RAD_lst[[2]])[4:9] <- paste(c(1:6), "SD", sep = ".")
names(RAD_lst[[3]])[4:9] <- paste(c(1:6), "N", sep = ".")


# merge
RADdf <- Reduce(function(x, y) merge(x, y, by = c("year", "depth", "midD")), RAD_lst)

# organise the table
RADdf <- RADdf[order(RADdf$year, RADdf$midD),
               c(1:3, order(names(RADdf)[c(-1:-3, -16:-21)]) + 3, 16:21)]
RADdf$midD <- NULL

############# 
# Top 30 cm #
#############

## ring mean ##

RSdf_lst <- llply(list(mean, sd, function(x) sum(!is.na(x))), 
                  function(y) dcast(DepMean, year ~ ring, value.var = "M", y))

# modify column names
names(RSdf_lst[[2]])[-1] <- paste(c(1:6), "SD", sep = ".")
names(RSdf_lst[[3]])[-1] <- paste(c(1:6), "N", sep = ".")

# merge
RSdf <- Reduce(function(x, y) merge(x, y, by = "year"), RSdf_lst)

# organise the table
RSdf <- RSdf[, c(1, order(names(RSdf)[c(-1, -14:-19)]) + 1, 14:19)]

## co2 mean ##
co2Sdf_mlt <- melt(CO2Mean, id = c("co2", "year"))

co2Sdf <- dcast(co2Sdf_mlt, year ~ co2 + variable)
  
# organise the table
co2Sdf <- co2Sdf[, c(1:4, 6:8, 5, 9)]

#################
# Save as excel #
#################

## create xcel workbook ##
wb <- createWorkbook()

# worksheet for rawdata
sheet <- createSheet(wb, sheetName="raw_data")
addDataFrame(phDF, sheet, showNA=TRUE, row.names=FALSE, characterNA="NA")

# worksheets for ring summary
sheet <- createSheet(wb, sheetName="Ring_mean")
addDataFrame(RADdf, sheet, showNA=TRUE, row.names=FALSE, characterNA="NA")

# worksheets for ring summary at top 30 cm
sheet <- createSheet(wb, sheetName="Ring_mean_30cm")
addDataFrame(RSdf, sheet, showNA=TRUE, row.names=FALSE, characterNA="NA")

# worksheets for co2 summary at top 30 cm
sheet <- createSheet(wb, sheetName="CO2_mean_30cm")
addDataFrame(co2Sdf, sheet, showNA=TRUE, row.names=FALSE, characterNA="NA")

#save file
saveWorkbook(wb,"Output/Table/FACE_FreshSoil_pH.xlsx")
