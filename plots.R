library(ggplot2)
library(dplyr)

times <- read.csv("./times.csv")
times$impl <- as.factor(times$impl)

# Throw out the data where the loop_scans setting is not
# optimal for the task on which we've applied checkpoints.
times <- filter(times, loop_scans == 2 | impl != "rust-naive")
times <- filter(times, loop_scans == 3 | impl != "rust-safe")
times <- filter(times, loop_scans == 1 | impl != "rust-online")

extrapolate <- function(df) {
  df_split <- split(df, df$impl, drop=TRUE)
  
  # Create an empty data frame for the predictions
  df_pred <- data.frame()
  
  # Loop through the list of data frames
  for(i in seq_along(df_split)){
    # Fit the model
    model <- lm(log(runtime_ms) ~ prog_size, data=df_split[[i]])
    
    # Make the predictions
    df_split[[i]]$predicted <- exp(predict(model))
    df_split[[i]]$type <- "observed"
    
    # Create new data points for extrapolation, starting from the max of original x
    new_data <- data.frame(prog_size = seq(max(df_split[[i]]$prog_size), max(df_split[[i]]$prog_size) + 3))
    
    # Make predictions for the new data points
    new_data$predicted <- exp(predict(model, newdata = new_data))
    new_data$impl <- df_split[[i]]$impl[1]
    new_data$type <- "predicted"
    
    # Bind the data frames
    df_pred <- bind_rows(df_pred, df_split[[i]], new_data)
  }
  
  df_pred
}

# A plot for the Rosette slide, showing the scalability issue.
ggplot(data=filter(times, impl=="rosette",loop_scans==3), aes(x = prog_size, y = runtime_ms, col = loop_scans, shape=success)) +
  facet_wrap(~ impl) +
  geom_hline(yintercept=1000*60*60, color="gray", linetype="dashed") +
  geom_point() +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1)) +
  scale_y_log10(breaks = c(1, 1000, 1000*60, 1000*60*60, 24*1000*60*60, 7*24*1000*60*60),
                labels = c("1 ms", "1 sec.", "1 min.", "1 hour", "1 day", "1 week")) +
  #scale_y_log10(labels = function(x) paste(round(x / (1000 * 60 * 60), 2), "hrs")) +
  #geom_line(aes(y = predicted, linetype = type), color = "red") +
  labs(y = "Runtime", x = "Instructions") +
  theme(legend.position = "none")
ggsave("rosette.png", scale=1, width=1200, height=350*3, units="px")

# A plot for the plain Rust slide, showing the scalability issue.
rust_extrapolated <- extrapolate(filter(times, impl=="rust", loop_scans==3))
rust_extrapolated <- rust_extrapolated[order(rust_extrapolated$prog_size)]
ggplot(data=rust_extrapolated,
       aes(x = prog_size, y = runtime_ms, col = loop_scans, shape=success)) +
  facet_wrap(~ impl) +
  geom_hline(yintercept=1000*60*60, color="gray", linetype="dashed") +
  geom_point() +
  scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1), limits = c(1, 20)) +
  scale_y_log10(breaks = c(1, 1000, 1000*60, 1000*60*60, 24*1000*60*60, 7*24*1000*60*60, 30*24*1000*60*60),
                labels = c("1 ms", "1 sec.", "1 min.", "1 hour", "1 day", "1 week", "1 month"),
                limits = c(1, 30*24*1000*60*60)) +
  #scale_y_log10(labels = function(x) paste(round(x / (1000 * 60 * 60), 2), "hrs")) +
  geom_line(aes(y = predicted, linetype = type), color = "red") +
  labs(y = "Runtime", x = "Instructions") +
  theme(legend.position = "none")
ggsave("rust.png", scale=1, width=1200, height=350*3, units="px")


# A big combined plot with the data, the regression lines, and the extrapolations.
ggplot(data=extrapolate(times), aes(x = prog_size, y = runtime_ms, col = loop_scans, shape=success)) +
  facet_wrap(~ impl) +
  geom_hline(yintercept=1000*60*60, color="gray", linetype="dashed") +
  geom_point() +
  scale_y_log10(breaks = c(1, 1000, 1000*60, 1000*60*60, 24*1000*60*60, 7*24*1000*60*60),
                labels = c("1 ms", "1 sec.", "1 min.", "1 hour", "1 day", "1 week")) +
  #scale_y_log10(labels = function(x) paste(round(x / (1000 * 60 * 60), 2), "hrs")) +
  geom_line(aes(y = predicted, linetype = type), color = "red") +
  labs(y = "Runtime", x = "Instructions")
