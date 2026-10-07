# ================================
# CST 2330 - CW, Exercise 1 - NPV
# ================================

# Storing data into a data-frame

cf.BEV <- data.frame(year = 2026:2031,
                     outflow = c(30, 15, 10, 10, 5, 5),
                     inflow = c(0, 0, 5, 10, 30, 40))

# Storing the interest rate

rate <- 0.03



# -----
# 1.
# -----

# Compute return
cf.BEV$return <- cf.BEV$inflow - cf.BEV$outflow
# Compute discount years
cf.BEV$n <- cf.BEV$year - cf.BEV$year[1]
# Compute PVF
cf.BEV$pvf <- (1 + rate)^-cf.BEV$n
# Compute PV return for each year
cf.BEV$pvreturn <- cf.BEV$return * cf.BEV$pvf
# Compute the total NPV of the investment
npv <- sum(cf.BEV$pvreturn)

# Printing the data-frame
cf.BEV



# -----
# 2.
# -----

# Function for adding (computing) new values of the data-frame - for NPV

analyse.cf <- function(cf, r) {
  # Compute return
  cf$return <- cf$inflow - cf$outflow
  # Compute discount years
  cf$n <- cf$year - cf$year[1]
  # Compute PVF
  cf$pvf <- (1 + r)^-cf$n
  # Compute PV return for each year
  cf$pvreturn <- cf$return * cf$pvf
  return(cf)
}

# Results of the "analyse.cf" function for BEV data-frame

analyse.cf(cf.BEV, rate)

# Function for computing the NPV value

cf.npv <- function(cf, rate) {
  df <- analyse.cf(cf, rate)
  # Compute NPV value
  npv <- sum(df$pvreturn)
  return(npv)
}

# Result NPV value for BEV data-frame

cf.npv(cf.BEV, rate)



# -----
# 3.
# -----


print("The project is worth doing with the discount rate of 3%. After discounting the future cash flows to present value, the investment will be plus $3.13B in current value (NPV). Since NPV is positive, the investment is considered to be a good investment.")




# -----
# 4.
# -----


# Creating variables for the discount rates (1%, 5%, 10%)

rates <- c(0.01, 0.05, 0.1)

# Printing the NPV for each discount rate

for (rate in rates) cat("Discount rate", rate, "% :", round(cf.npv(cf.BEV, rate), 2), "NPV \n")




# -----
# 5.
# -----


# Store rates from 1% to 10%

rates <- seq(0, 0.1, by = 0.001)

# Empty vector for NPV's

npvs <- c()

# Loop to compute all NPV values

for (rate in rates) {
  npvs <- c(npvs, cf.npv(cf.BEV, rate))
}

# Plotting the graph

plot(rates * 100, npvs, type = "l", col = "blue", xlab = "Discount rate (%)", 
     ylab = "NPV", xaxp = c(0, 10, 10), yaxp = c(-10, 10, 10))



# -----
# 6.
# -----


# Plotting the interception lines of discount rate to NPV = 0

abline(h = 0, col = "red", lty = "dashed")
abline(v = 4.6, col = "red", lty = "dashed")


print("The significance is that the value zero of NPV is the break-point between the investment being worth doing and not being worth doing. So above approximately 4.6% discount rate, the NPV is negative and because of that, it is a bad investment. On the other hand, the less the discount rate (below the value approximately 4.6%),the NPV is positive and the better the investment is. This point plays a key role when deciding if the investment is a reasonable choice.")


