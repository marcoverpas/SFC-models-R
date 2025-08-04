# Continuous-Time SIM Model (Differential Equations) - R version
# #Created by Mrco Veronese Passarella on 4 August 2025

# Recall libraries ####
library(deSolve)  # For solving differential equations

# Model parameters ####
alpha1 <- 0.6    # Marginal propensity to consume out of disposable income
alpha2 <- 0.4    # Marginal propensity to consume out of wealth
theta <- 0.2     # Tax rate
w <- 1           # Wages (fixed)

# Time parameters ####
t_start <- 0
t_end <- 65      # Simulate for 65 time units
t_switch <- 15   # Time when government spending increases

# Government spending function (step increase at t_switch) ####
government_spending <- function(t) {
  ifelse(t >= t_switch, 20, 0)
}

# Define the system of ODEs ####
sim_model <- function(t, state, params) {
  with(as.list(c(state, params)), {
    
    # Government spending (exogenous)
    g <- government_spending(t)
    
    # Taxes (from income)
    t_ <- theta * y
    
    # Disposable income
    yd <- y - t_
    
    # Consumption (SIM version)
    c <- alpha1 * yd + alpha2 * h_h
    
    # Income determination (aggregate demand)
    y_dot <- c + g - y  # Adjusts y toward equilibrium
    
    # Wealth dynamics (household cash holdings)
    h_h_dot <- yd - c
    
    # Government cash supply 
    h_s_dot <- g - t_
    
    # Employment (assuming production = income / wage)
    n <- y / w
    
    # Return the derivatives (ODE system)
    return(list(c(y_dot, h_h_dot, h_s_dot)))
  })
}

# Initial conditions (start from zero) ####
initial_state <- c(
  y = 0,      # Initial income
  h_h = 0,    # Initial household wealth
  h_s = 0     # Initial government money supply
)

# Time grid for simulation ####
times <- seq(t_start, t_end, by = 0.1)  # Fine grid for smooth ODE solution

# Solve the ODE system ####
solution <- ode(
  y = initial_state,
  times = times,
  func = sim_model,
  parms = NULL
)

# Convert to data frame for plotting ####
sim_results <- as.data.frame(solution)

# Calculate additional variables for plotting ####
sim_results$g <- government_spending(sim_results$time)
sim_results$t <- theta * sim_results$y
sim_results$yd <- sim_results$y - sim_results$t
sim_results$c <- alpha1 * sim_results$yd + alpha2 * sim_results$h_h
sim_results$y_star <- sim_results$g / theta
sim_results$n <- sim_results$y / w

# Plot results ####
#par(mfrow = c(2, 2))

# Figure 3.1: Y and Y* over time
plot(sim_results$time, sim_results$y_star, type = "l", col = "blue", lwd = 2,
     font.main=1,cex.main=1,
     main = "Figure 3.1: Impact of Y and Y* of a permanent increase in G",
     xlab = "Time", ylab = "", ylim = c(0, 130))
lines(sim_results$time, sim_results$y, col = "green", lwd = 2)
legend("bottomright", legend = c("Steady state Y*", "Income Y"), cex = 0.8, 
       col = c("blue", "green"), lwd = 2)

# Figure 3.2: Disposable income and consumption
plot(sim_results$time, sim_results$yd, type = "l", col = "red", lwd = 2,
     font.main=1,cex.main=1,
     main = "Figure 3.2: YD and C starting from scratch",
     xlab = "Time", ylab = "", ylim = c(0, 90))
lines(sim_results$time, sim_results$c, col = "green", lwd = 2)
abline(h = 80, lty = 2)
legend("bottomright", legend = c("Disposable income YD", "Consumption C"), cex = 0.8, 
       col = c("red", "green"), lwd = 2)

# Figure 3.3: Wealth and its change
plot(sim_results$time, sim_results$h_h, type = "l", col = "blue", lwd = 2,
     font.main=1,cex.main=1,
     main = "Figure 3.3: Wealth level and change",
     xlab = "Time", ylab = "Wealth level H")
par(new = TRUE)
plot(sim_results$time, c(0, diff(sim_results$h_h)/diff(sim_results$time)), 
     type = "l", col = "red", lwd = 2, xlab = "", ylab = "", axes = FALSE)
axis(4)
mtext("Change in H", side = 4, line = 2)
legend("right", legend = c("Wealth H", "Household saving (dH/dt)"), cex = 0.8, 
       col = c("blue", "red"), lwd = 2)