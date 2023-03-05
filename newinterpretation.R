#Author: Deepankar Basu
#Edited: Chandni Dwarkasing

# ------------------------------------------- #
# ------------ New Interpretation ----------- #

# Key quantities that are given
# A = input output matrix
# l = labour input vector
# w = nominal wage rate
# v = value of labour power
# y = net output vector

# ----- Quantities that are given ------- #

# -- Input-output matrix
A <- matrix(c(186/450, 12/450, 9/450,
              54/21, 6/21, 6/21, 
              30/60, 3/60, 15/60), 
            ncol = 3)

# -- Labour input vector
(l <- c(18/450, 12/21, 30/60))

# -- Nominal wage rate
(w <- 1)

# -- Value of labour power
(v <- 1/3)

# -- Net output
y <- c(180, 0, 30)


# --------- Computed quantities --------- #

# -- Compute Gross Output
I <- diag(3)
(Q <- solve(I - A) %*% y)


# -- Maximum eigenvalue of A
jj_A <- eigen(A)$values
(lambda_mA <- max(jj_A))

# -- Maximal rate of profit
(R <- (1/lambda_mA)-1)


# -- Define Function
# The roots of this function will be 
# used to compute the uniform rate of profit
myfunc <- function(r2){
  # Given variables
  D2=(I-A)%*%Q
  l2=l
  A2=A
  L2=(l%*%Q)/v
  # Compute
  C2 = solve(I-(1+r2)*A2)
  E2 = C2%*%D2
  B2 = (1+r2)*l2%*%E2
  i2 = B2-L2
  return(i2)
}

# Find root to get uniform rate of profit
(r <- uniroot(myfunc,c(0,R-0.1))$root)

# Solve: p = (1+r)wl*[I-(1+r)A]^{-1}
# To get the vector of prices of production
(p <- (1+r)*w*l%*%(solve(I-(1+r)*A)))

# --- Calculating the MEV (monetary expression of value)
# Create identity matrix
n <- ncol(A)
I <- diag(n)

# Vector of values
(lambda <- t(l)%*%solve(I - A))

# MEV/MELT (monetary expression of value)
(melt <- (p%*%y)/(lambda %*%y))
