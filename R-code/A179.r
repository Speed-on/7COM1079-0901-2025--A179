# ---- READ CSV ----
df <- read.csv("ford.csv", stringsAsFactors = FALSE)

# ---- CLEAN / CONVERT ----
df$price <- as.numeric(df$price)
df$mileage <- as.numeric(df$mileage)

# Remove NA rows
df_clean <- df[!is.na(df$price) & !is.na(df$mileage), ]

# ---- FIRST 5 ROWS ----
cat("FIRST 5 ROWS:\n")
print(head(df_clean, 5))

# ---- SUMMARY ----
cat("\nSUMMARY:\n")
cat("Observations:", nrow(df_clean), "\n")

cat("Price mean:", mean(df_clean$price),
    " median:", median(df_clean$price), "\n")

cat("Mileage mean:", mean(df_clean$mileage),
    " median:", median(df_clean$mileage), "\n\n")

# ---- HISTOGRAMS ----
# Price histogram
hist(df_clean$price,
     breaks = 50,
     main = "Histogram of Price (GBP)",
     xlab = "Price",
     col = "white",
     border = "black")

# Add normal curve
price_mean <- mean(df_clean$price)
price_sd <- sd(df_clean$price)
curve(dnorm(x, mean = price_mean, sd = price_sd) *
        diff(hist(df_clean$price, plot=FALSE)$breaks[1:2]) *
        nrow(df_clean),
      add = TRUE, lwd = 2)

# Mileage histogram
hist(df_clean$mileage,
     breaks = 50,
     main = "Histogram of Mileage",
     xlab = "Mileage",
     col = "white",
     border = "black")

# Add normal curve
mileage_mean <- mean(df_clean$mileage)
mileage_sd <- sd(df_clean$mileage)
curve(dnorm(x, mean = mileage_mean, sd = mileage_sd) *
        diff(hist(df_clean$mileage, plot=FALSE)$breaks[1:2]) *
        nrow(df_clean),
      add = TRUE, lwd = 2)

# ---- SCATTERPLOT + REGRESSION LINE ----
plot(df_clean$mileage, df_clean$price,
     main = "Scatterplot: Mileage vs Price",
     xlab = "Mileage",
     ylab = "Price",
     pch = 20, col = "#00000055")

# Linear model
model <- lm(price ~ mileage, data = df_clean)
abline(model, col = "blue", lwd = 2)

# ---- SAVE PLOTS (PNG) ----
png("hist_price_base.png", width = 700, height = 400)
hist(df_clean$price, breaks=50, main="Histogram of Price (GBP)",
     xlab="Price", col="white", border="black")
dev.off()

png("hist_mileage_base.png", width = 700, height = 400)
hist(df_clean$mileage, breaks=50, main="Histogram of Mileage",
     xlab="Mileage", col="white", border="black")
dev.off()

png("scatter_mileage_price_base.png", width = 700, height = 500)
plot(df_clean$mileage, df_clean$price,
     main="Scatterplot: Mileage vs Price",
     xlab="Mileage", ylab="Price",
     pch=20, col="#00000055")
abline(model, col="blue", lwd=2)
dev.off()

# ---- CORRELATIONS ----
cat("\nPEARSON CORRELATION:\n")
print(cor.test(df_clean$mileage, df_clean$price, method="pearson"))

cat("\nSPEARMAN CORRELATION:\n")
print(cor.test(df_clean$mileage, df_clean$price, method="spearman"))
