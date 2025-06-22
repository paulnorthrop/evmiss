# Provide the Plymouth Ozone annual maxima data in the same format as
# BrestSurgeMaxima
# (Also provide the raw data)

# Source: [Data Selector - DEFRA UK Air - GOV.UK](https://uk-air.defra.gov.uk/data/data_selector)

## Read in the raw daily ozone values

PlymouthOzone <- read.csv("data-raw/OzonePlymouth.csv", skip = 10,
                          na.strings = "No data")
# Make PlymouthOzone$Date a proper date object
PlymouthOzone$Date <- as.Date(PlymouthOzone$Date, "%d/%m/%Y")
head(PlymouthOzone)
tail(PlymouthOzone)

# Add a column for year
year <- as.numeric(format(PlymouthOzone$Date, format="%Y"))

PlymouthOzone <- data.frame(Date = PlymouthOzone$Date, Year = year,
                            Ozone = PlymouthOzone$Ozone)
head(PlymouthOzone)
tail(PlymouthOzone)

# 1997 and 2025 are incomplete
# We could retain them, but 1997 has only 2 months and 2025 almost 5 months
# Let's remove 1997 and 2025

retain <- PlymouthOzone$Year > 1997 & PlymouthOzone$Year < 2025
PlymouthOzone <- PlymouthOzone[retain, ]
head(PlymouthOzone)
tail(PlymouthOzone)

# Calculate the annual maxima, notNA, block size n and block indicator

maxima <- tapply(PlymouthOzone$Ozone, INDEX = PlymouthOzone$Year,
                 FUN = max, na.rm = TRUE)
notNA <- tapply(PlymouthOzone$Ozone, INDEX = PlymouthOzone$Year,
                FUN = function(x) sum(!is.na(x)))
n <- evmiss::days_in_year(names(maxima))
block <- 1:length(maxima)

# Create the data frame

PlymouthOzoneMaxima <- data.frame(maxima = maxima,notNA = notNA, n = n,
                                  block = block)
rownames(PlymouthOzoneMaxima) <- names(maxima)

# Fit using the raw data
fit1 <- evmiss::gev_mle(PlymouthOzone$Ozone, block = PlymouthOzone$Year)
fit2 <- evmiss::gev_mle(PlymouthOzone$Ozone, block = PlymouthOzone$Year,
                        adjust = FALSE)
coef(fit1)
coef(fit2)

# Fit using the data frame of maxima
fit1 <- evmiss::gev_mle(PlymouthOzoneMaxima)
fit2 <- evmiss::gev_mle(PlymouthOzoneMaxima, adjust = FALSE)
coef(fit1)
coef(fit2)

# Create the package data
usethis::use_data(PlymouthOzoneMaxima, overwrite = TRUE)
usethis::use_data(PlymouthOzone, overwrite = TRUE)
