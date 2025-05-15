# In this script you can find some examples of how to obtain
# the PCS reported in tables 6 to 11

# To load the functions
url <- "https://raw.githubusercontent.com/fhernanb/T_exp_lin/main/Scripts/exp_lin_test.R"
source(url)

url <- "https://raw.githubusercontent.com/fhernanb/T_exp_lin/refs/heads/main/Scripts/PCS.R"
source(url)

# Example with to cell from Table 6
PCS_exp_complete(lambda=c(0.1, 0.5), n=c(20, 40))

# Example with to cell from Table 7
PCS_lin_complete(theta=c(0.1, 0.5), n=c(20, 40))

# Example with to cell from Table 8
PCS_exp_censored(lambda=c(0.1, 0.5), n=c(20, 40), delta=0.9)
