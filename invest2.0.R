###########################################
# invest.R
###########################################
#
# Build a multi-objective constrained model
# for an investment portfolio
###########################################
#install.packages("mco")
library(mco)

# The 30 investments that will be used to select
# a portfolio are in "invest.tab"
#
invest <- read.table("invest.tab")
#
# Determine number of options from the invest table
#
numberOptions <- nrow(invest)
#
# The format of invest is:
# Each row is an option
# Columns are : ROI (return on investment), Risk, Type
# 			    where Type = 1 (Stock), 2 (bond), 3 (cash)
#
#############################################################
# Each option from the table must either not be selected
# OR must be between an amount
#   minAMOUNT <= amount <= maxAMOUNT
#
# We will assume that anything < minAMOUNT
# is equivalent to zero (and therefore not included)
#############################################################
minAMOUNT = 0.05
maxAMOUNT = 0.2

##########################################
# CONSTRAINTS
##########################################
# The sum of the portfolios must be between
# 0.95 and 1.0
# Constraints are satisfied by being >= 0
#############################################
portfolioSUM <- function(x)
{
  selected <- which(x >= minAMOUNT)
  sumx = sum(x[selected])
  if ((sumx >=0.95) && (sumx <=1.0)) return(1) # ok
  return(-1) # Fails constraint
}

###########################################################################
# Constraint - each selected option must be  >= minAMOUNT
#              and <= maxAMOUNT
# We ignore options with value < minAMOUNT because they
# aren't part of the final selection of investments.
###########################################################################
portfolioRANGE <- function(x)
{
  selected <- which(x >= minAMOUNT)  # Only count if at least minimum
  over <- which(x[selected] > maxAMOUNT)
  if (length(over) > 0) return(-1)
  return(1)
}

##################################################################
# Constraints on the minimum and maximum number of selected
# stocks/bonds/cash
##################################################################
minNumber = 8
maxNumber = 12

portfolioNUM <- function(x)
{
  numselected <- length(which(x >= minAMOUNT))
  if (numselected < minNumber) return(-1)
  if (numselected > maxNumber) return(-1)
  return(numselected)
}

############################################################
# Functions to be minimised/maximised
############################################################
###############################
# Return on Investment (ROI)
###############################
# This wants to be MAXIMISED
# Only include options that are greater than the minAMOUNT
#############################################################
ROI <- function(x)
{
  selected <- which(x >= minAMOUNT)
  roi <- sum(invest$ROI[selected]*x[selected])
  return(-roi) # Since nsgaII minimises we take the negative
}
mycorr <- function(x)
{
  selected <- which(x >= minAMOUNT)
  
}
#################################
# Risk (RISK)
#################################
# This is to be MINIMISED
# Only include options that are greater than the minAMOUNT
##############################################################
RISK <- function(x)
{
  selected <- which(x >= minAMOUNT)
  risk <- sum(invest$Risk[selected]*x[selected])  # Just the sum of risk
  return(risk) # we want to minimise the risk
}
##################################################
# Here are the functions that are to be minimised
# Note ROI is actually maximised, while RISK is
# minimised.
###################################################
funs <- function(x)
{
  return(c(ROI(x),RISK(x)))
}
######################################################
# Here are the constraints
# Since nsga2 assumes a single constraint function, we
# call each constraint in turn, and return the results
# of all the constraints as a concatenated list
######################################################
constraintFNS <- function(x)
{
  psum = portfolioSUM(x)
  prange = portfolioRANGE(x)
  pnum = portfolioNUM(x)
  return(c(prange,pnum,psum))
}
#############################################################
# Set the lower and upper bounds for
# each investment option
# The lower bound is 0; upper bound is maxAMOUNT
#############################################################

lower = rep(0,numberOptions)
# The upper bound is the maximum amount of an option, which is maxAMOUNT
upper = rep(maxAMOUNT,numberOptions)
#
###########################################################
# CALL nsga2 to find the pareto optimal solutions
###########################################################
portfolio <- nsga2(funs,
                   idim=numberOptions, # inputs for each option,
                   odim=2, # outputs (ROI,RISK)
                   popsize=52,
                   generations=500,
                   lower.bounds=lower,
                   upper.bounds=upper,
                   constraints = constraintFNS,
                   cdim=3) # 3 constraints

######## Plot the pareto front using default plotting
###########################################################
plot(portfolio,xlab="-ROI (%)",ylab="RISK",main="Objective Space")

