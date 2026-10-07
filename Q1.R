######### ||| CST2330 COURSEWORK 2 ||| #########

##### Question 1:

# Create the dataframe with given columns for BEV
cf.bev <- data.frame(
  year = 2026:2031,
  outflow = c(30, 15, 10, 10, 5, 5),
  inflow = c(0, 0, 5, 10, 30, 40)
)

# Assign discount rate
discount_rate <- 0.03

#------ Task 1 ------

# Find the return
cf.bev$return <- cf.bev$inflow - cf.bev$outflow

# Find the numbers of future years to discount
cf.bev$n <- cf.bev$year - cf.bev$year[1]

# Find present value factors
cf.bev$pvf <- (1 + discount_rate)^(-cf.bev$n)

# Find the present values of returns for each year
cf.bev$pvreturn <- cf.bev$pvf * cf.bev$return

# Display the results in the table
cf.bev

# Calculate the Net Present Value (NPV)
sum(cf.bev$pvreturn)

#------Task 2 ------

# Function to analyse cashflow at different discount rates
analyse.cashflow <- function(data, rate) {
  
  # Find the return 
  data$return <- data$inflow - data$outflow
  
  # Find the years for discount factor
  data$n <- data$year - data$year[1]
  
  # Find Present value factors
  data$pvf <- (1 + rate)^(-data$n)
  
  # Find present value of returns (annually)
  data$pvreturn <- data$pvf * data$return
  
  # Print the table
  print(data)
  
  # Return the total NPV
  return (sum(data$pvreturn))
}

# Test the function
analyse.cashflow(cf.bev, 0.03)

#------Task 3 ------

# Plot a graphof NPV vs Discount Rate for better reasoning 
plot(
  Vectorize(function(x) analyse.cashflow(cf.bev, rate = x)),
  xlim = c(0, 0.10),
  type = "l",
  col = "blue",
  xlab = "Discount Rate",
  ylab = "Net Present Value (£B)",
  main = "NPV vs Discount Factor"
)

# Create a point at 0.03 discount rate
points(0.03, analyse.cashflow(cf.bev, 0.03), pch = 16, col = "purple")

#------ Task 4 ------

# The NPV at 1% with 2 decimal places
round(analyse.cashflow(cf.bev, 0.01), 2)

# The NPV at 5% with 2 decimal places
round(analyse.cashflow(cf.bev, 0.05), 2)

# The NPV at 10% with 2 decimal places
round(analyse.cashflow(cf.bev, 0.10), 2)

#------ Task 5 ------

# Plot NPV vs Discount rate graph from 0% to 10%
plot(
  Vectorize(function(x) analyse.cashflow(cf.bev, rate = x)),
  xlim = c(0, 0.10),
  type = "l",
  col = "orange",
  xlab = "Discount Rate",
  ylab = "Net Present Value",
  main = "NPV vs Discount Rate"
)

# Add NPV = 0 line
abline(h = 0, lty = 2)

#------ Task 6 ------

# Function to find NPV with given discount rate
npv_func <- function(discount_rate) analyse.cashflow(cf.bev, discount_rate)

# Find the discount rate where NPV=0 
uniroot(npv_func, c(0, 0.10))$root