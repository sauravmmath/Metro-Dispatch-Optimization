# Install required packages if you don't have them yet:
# install.packages(c("readxl", "dplyr"))
install.packages("lpSolve")
library(readxl)
library(dplyr)
library(lpSolve)
library(ggplot2)
library(tidyr)
library(dplyr)
# 1. Load the dataset
file_path <- "C:\\Users\\gagan\\OneDrive\\Desktop\\Optimisation\\a4ef58a3-29de-4787-b68e-56d716d0a95d.xlsx"
df <- read_excel(file_path, sheet = "Sheet1")

# 2. Define the Sets (Mapping stations to Line l)
purple_stations <- c(
  '11-Baiyappanahalli', '12-SV Road', '13-Indiranagar', '14-Halasuru', '15-Trinity', 
  '16-MG Road', '17-Cubbon Park', '18-Vidhana Soudha', '19-Sir M Visveshwaraya', 
  '110-Kempegowda', '111-City Railway Station', '112-Magadi Road', '113-Hosahalli', 
  '114-Vijayanagar', '115-Attiguppe', '116-Deepanjali Nagar', '117-Mysore Road', 
  '118-Nayandahalli', '119-Rajarajeshwari Nagar', '120-Jnanabharathi', '121-Pattanagere', 
  '122-Kengeri Bus Terminal', '123-Kengeri', '124-Challaghatta', '125-Benniganahalli', 
  '126-Krishnarajapura', '127-Singayyanapalya', '128-Garudacharpalya', '129-Hoodi Junction', 
  '130-Seetharampalya', '131-Kundalahalli', '132-Nallurhalli', '133-Sri Sathya Sai Hospital', 
  '134-Pattandur Agrahara', '135-Kadugodi Tree Park', '136-Hopefarm Channasandra', '137-Whitefield'
)

green_stations <- c(
  '21-Nagasandra', '22-Dasarahalli', '23-Jalahalli', '24-Peenya Industry', '25-Peenya', 
  '26-Goraguntepalya', '27-Yeshwanthpur', '28-Sandal Soap Factory', '29-Mahalakshmi', 
  '210-Rajajinagar', '211-Kuvempu Road', '212-Srirampura', '213-Mantri Square Sampige Road', 
  '110-Kempegowda', '215-Chickpete', '216-Krishna Rajendra Market', '217-National College', 
  '218-Lalbagh', '219-South End Circle', '220-Jayanagar', '221-Rashtriya Vidyalaya Road', 
  '222-Banashankari', '223-Jaya Prakash Nagar', '224-Yelachenahalli', '225-Konanakunte Cross', 
  '226-Doddakallasandra', '227-Vajarahalli', '228-Talaghattapura', '229-Silk Institute', 
  '230-Manjunatha Nagar', '231-Chikkabidarakallu', '232-Madavara'
)

# Split data into lines using dplyr filter
purple_df <- df %>% filter(STATION %in% purple_stations)
green_df <- df %>% filter(STATION %in% green_stations)

# 3. Isolate the operating hours (t)
# grep looks for column names containing "To", and we exclude the 23:00+ catch-all

hourly_cols <- grep("To", names(df), value = TRUE)
num_hours <- length(hourly_cols) # This will now mathematically evaluate to 24
num_days <- n_distinct(df$`BUSINESS DATE`)

# 4. Calculate the Expected Hourly Demand Vectors (D_lt)
# Select only the time columns, sum them down the rows, and divide by the number of days
D_P_vector <- colSums(purple_df %>% select(all_of(hourly_cols)), na.rm = TRUE) / num_days
D_G_vector <- colSums(green_df %>% select(all_of(hourly_cols)), na.rm = TRUE) / num_days

# Print the final constraint arrays to plug into an LP solver
cat("\nD_{P,t} (Purple Line Expected Hourly Demand):\n")
print(round(D_P_vector, 2))

cat("\n----------------------------------\n")

cat("\nD_{G,t} (Green Line Expected Hourly Demand):\n")
print(round(D_G_vector, 2))

num_hours <- length(hourly_cols)



# ==========================================
# --- 2. PLAN PHASE: PARAMETERS & MATRIX ---
# ==========================================

K <- 2068      # Train Capacity
F_max <- 58    # Max Fleet
R <- 27        # Revenue per passenger
C <- 22818     # Cost per train

# Decision Variables mapping per hour: [x_P, x_G, y_P, y_G] -> Total 92 variables
num_vars <- num_hours * 4 

# Objective Function (Maximize Profit: 27*y - 22818*x)
obj_vector <- rep(c(-C, -C, R, R), times = num_hours)

# Initialize Constraint Matrix
const_mat <- matrix(0, nrow = 0, ncol = num_vars)
const_dir <- character(0)
const_rhs <- numeric(0)

# Helper function to append constraints cleanly
add_constraint <- function(row_vec, dir, rhs) {
  const_mat <<- rbind(const_mat, row_vec)
  const_dir <<- c(const_dir, dir)
  const_rhs <<- c(const_rhs, rhs)
}

# Build the Matrix constraints loop
for (t in 1:num_hours) {
  idx_xP <- (t - 1) * 4 + 1
  idx_xG <- (t - 1) * 4 + 2
  idx_yP <- (t - 1) * 4 + 3
  idx_yG <- (t - 1) * 4 + 4
  
  # A. Train Capacity: y - K*x <= 0
  row_cap_P <- numeric(num_vars); row_cap_P[idx_yP] <- 1; row_cap_P[idx_xP] <- -K
  add_constraint(row_cap_P, "<=", 0)
  
  row_cap_G <- numeric(num_vars); row_cap_G[idx_yG] <- 1; row_cap_G[idx_xG] <- -K
  add_constraint(row_cap_G, "<=", 0)
  
  # B. Passenger Demand: y <= D_lt
  row_dem_P <- numeric(num_vars); row_dem_P[idx_yP] <- 1
  add_constraint(row_dem_P, "<=", D_P_vector[t])
  
  row_dem_G <- numeric(num_vars); row_dem_G[idx_yG] <- 1
  add_constraint(row_dem_G, "<=", D_G_vector[t])
  
  # C. Fleet Constraint: x_P + x_G <= F
  row_fleet <- numeric(num_vars); row_fleet[idx_xP] <- 1; row_fleet[idx_xG] <- 1
  add_constraint(row_fleet, "<=", F_max)
  
  # D. Minimum Public Service (Lower Bounds): x >= 3
  row_min_P <- numeric(num_vars); row_min_P[idx_xP] <- 1
  add_constraint(row_min_P, ">=", 3)
  
  row_min_G <- numeric(num_vars); row_min_G[idx_xG] <- 1
  add_constraint(row_min_G, ">=", 3)
  
  # E. Signaling Safety (Upper Bounds): x <= 20
  row_max_P <- numeric(num_vars); row_max_P[idx_xP] <- 1
  add_constraint(row_max_P, "<=", 20)
  
  row_max_G <- numeric(num_vars); row_max_G[idx_xG] <- 1
  add_constraint(row_max_G, "<=", 20)
  
  # (Note: lpSolve handles Non-Negativity y >= 0 automatically)
}

# ==========================================
# --- 3. ANALYSIS PHASE: THE SOLVER ---
# ==========================================

# Execute the Linear Programming Simplex Solver
# We explicitly do NOT use integers to maintain the Convex Polyhedron requirement
opt_solution <- lp(direction = "max", 
                   objective.in = obj_vector, 
                   const.mat = const_mat, 
                   const.dir = const_dir, 
                   const.rhs = const_rhs)

# ==========================================
# --- 4. PRINTING FORMATTED RESULTS ---
# ==========================================

cat("\n============================================\n")
cat("      NAMMA METRO LP OPTIMIZATION RESULTS     \n")
cat("============================================\n\n")

if (opt_solution$status == 0) {
  cat(sprintf("Optimal Expected Daily Profit: ₹ %s\n\n", formatC(opt_solution$objval, format="f", big.mark=",", digits=2)))
  
  total_rev <- 0
  total_cost <- 0
  total_xP <- 0
  total_xG <- 0
  
  for (t in 1:num_hours) {
    idx_xP <- (t - 1) * 4 + 1
    idx_xG <- (t - 1) * 4 + 2
    idx_yP <- (t - 1) * 4 + 3
    idx_yG <- (t - 1) * 4 + 4
    
    xP_val <- opt_solution$solution[idx_xP]
    xG_val <- opt_solution$solution[idx_xG]
    yP_val <- opt_solution$solution[idx_yP]
    yG_val <- opt_solution$solution[idx_yG]
    
    total_rev <- total_rev + (yP_val + yG_val) * R
    total_cost <- total_cost + (xP_val + xG_val) * C
    total_xP <- total_xP + xP_val
    total_xG <- total_xG + xG_val
  }
  
  cat(sprintf("Total Expected Revenue/day: ₹ %s\n", formatC(total_rev, format="f", big.mark=",", digits=2)))
  cat(sprintf("Total Expected Cost/day:    ₹ %s\n", formatC(total_cost, format="f", big.mark=",", digits=2)))
  cat(sprintf("Total Purple Trains (x_P):  %.2f\n", total_xP))
  cat(sprintf("Total Green Trains (x_G):   %.2f\n", total_xG))
  
} else {
  cat("The solver failed to find an optimal solution. Status code:", opt_solution$status, "\n")
}


# Install ggplot2 and tidyr if you haven't already
# install.packages(c("ggplot2", "tidyr"))



# ==========================================
# 1. SUMMARY STATISTICS 
# ==========================================

# Create a clean summary table for the report
summary_stats <- data.frame(
  Line = c("Purple", "Green"),
  Total_Daily_Expected = round(c(sum(D_P_vector), sum(D_G_vector)), 2),
  Mean_Hourly = round(c(mean(D_P_vector), mean(D_G_vector)), 2),
  Max_Hourly_Peak = round(c(max(D_P_vector), max(D_G_vector)), 2),
  Min_Hourly_OffPeak = round(c(min(D_P_vector), min(D_G_vector)), 2),
  Std_Deviation = round(c(sd(D_P_vector), sd(D_G_vector)), 2)
)

cat("--- Summary Statistics for Expected Demand ---\n")
print(summary_stats)

# ==========================================
# 2. DATA PREPARATION FOR PLOTTING
# ==========================================

# Create a combined dataframe. 
# We use a simple 1 to 23 index for the operating hours to keep the X-axis clean.
num_hours <- length(D_P_vector)

demand_df <- data.frame(
  Hour_Block = 1:num_hours,
  Purple = D_P_vector,
  Green = D_G_vector
)

# Convert from "wide" to "long" format, which is required by ggplot2
demand_long <- pivot_longer(demand_df, 
                            cols = c("Purple", "Green"), 
                            names_to = "Line", 
                            values_to = "Expected_Demand")


# ==========================================
# 3. CHART 1: TIME-SERIES LINE PLOT
# ==========================================



# Extract just the first 5 characters of the column names to get clean times
# E.g., "00:00 Hrs To 01:00 Hrs" becomes exactly "00:00"
clean_time_labels <- substr(hourly_cols, 1, 5)

plot_1 <- ggplot(demand_long, aes(x = Hour_Block, y = Expected_Demand, color = Line)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Purple" = "purple", "Green" = "forestgreen")) +
  scale_x_continuous(breaks = 1:num_hours, labels = clean_time_labels) + 
  labs(title = "Expected Hourly Passenger Demand Across 24 Hours",
       subtitle = "Illustrating System Dead Zones and Bimodal Peak Rushes",
       x = "Operating Hour (24-Hour Clock, starting at Midnight)",
       y = "Expected Passengers (D_lt)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

print(plot_1)
# Save the updated plot
ggsave("Chart1_HourlyTrend_24Hr.png", plot = plot_1, width = 8, height = 5, dpi = 300)

# ==========================================
# 4. CHART 2: DISTRIBUTION BOXPLOT
# ==========================================

# This chart shows the variance and asymmetry between the two lines
plot_2 <- ggplot(demand_long, aes(x = Line, y = Expected_Demand, fill = Line)) +
  geom_boxplot(alpha = 0.6, outlier.size = 2, outlier.color = "red") +
  scale_fill_manual(values = c("Purple" = "purple", "Green" = "forestgreen")) +
  labs(title = "Distribution of Expected Hourly Demand by Line",
       subtitle = "Highlighting Line Variance and Peak Outliers",
       x = "Metro Line",
       y = "Expected Passengers per Hour") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "none") # Hide legend since X-axis already labels the lines

# Print the plot to the Viewer
print(plot_2)

# Save the plot as a high-quality PNG for your LaTeX report
ggsave("Chart2_Distribution.png", plot = plot_2, width = 6, height = 5, dpi = 300)

