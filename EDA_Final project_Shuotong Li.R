# ---------------------------------------------------------------
# Exploratory Data Analysis and Visualization
# COM SCI-X 450.2
#
# Final Project 
# Student: Shuotong Li
# ---------------------------------------------------------------

# ---------------------------------------------------------------
# Step 1: Dataset Selection & Proposal
# ---------------------------------------------------------------


# DATASET PROPOSAL FORM

## Dataset Description:
## The Online Shoppers Purchasing Intention dataset contains
## 12,330 web session records collected from an e-commerce site.
## Each row represents one user session and records three types
## of pages visited (Administrative, Informational, ProductRelated),
## time spent on each, Google Analytics metrics (BounceRates,
## ExitRates, PageValues), proximity to holidays (SpecialDay),
## and session context (Month, VisitorType, Weekend).
## The binary target variable Revenue indicates whether the
## session resulted in a purchase.

## Source URL:
## https://archive.ics.uci.edu/ml/datasets/
## Online+Shoppers+Purchasing+Intention+Dataset

## Rows: 12,330   Variables: 18

## Why I Selected This Dataset:
##
##   SHORT-TERM GOAL -- I am targeting e-commerce analytics
##   internships (e.g. TikTok Shop). Analyzing real shopper
##   behavior data produces a portfolio project that maps
##   directly to what these teams work on: understanding why
##   sessions do not convert and which signals matter most.
##
##   LONG-TERM GOAL -- I plan to start a cross-border
##   e-commerce business. Exploring this dataset lets me
##   surface actionable opportunity points -- which traffic
##   sources convert best, what engagement patterns predict
##   purchase intent, and where the largest gaps between
##   browse behavior and actual conversion exist.

## Research Questions:
##   RQ1: Do product page engagement (visits / duration)
##        correlate with purchase conversion (Revenue)?
##   RQ2: Do BounceRates and PageValues differ between new
##        and returning visitors, and does this pattern
##        change on weekends or near special days?
##   RQ3: How do TrafficType and Region influence the link
##        between page-type preference and purchase outcome?
##   RQ4: Can behavioral clustering identify distinct visitor
##        segments with meaningfully different purchase rates?
##   RQ5: Which combination of variables best predicts
##        conversion in a logistic regression model?

## Potential Challenges:
##   - Class imbalance: only 15.5% of sessions result in
##     a purchase -- raw counts will be misleading.
##   - OperatingSystems, Browser, Region, and TrafficType
##     are stored as integers but are nominal categories;
##     they must be recast as factors before any analysis.
##   - Duration and page count variables are heavily
##     right-skewed; log or sqrt transformations needed.
##   - SpecialDay is >90% zeros, making it a sparse feature
##     that requires careful interpretation.
##   - No January or April in the data -- seasonal conclusions
##     must acknowledge this gap.



# Initial Loading of Data
data_path <- "/Users/alinaaa/Desktop/online_shoppers_intention.csv"

shoppers <- read.csv(data_path, header = TRUE, stringsAsFactors = FALSE)

# Number of rows and columns
dim(shoppers)

# head() output 
head(shoppers)


# ---------------------------------------------------------------
# STEP 2 -- Data Acquisition & Audit
# ---------------------------------------------------------------
cat("=====================================================\n")
cat("  STEP 2 -- Data Acquisition & Audit\n")
cat("=====================================================\n")

# 2.0 Load Required Packages

library(skimr)
library(naniar)
library(visdat)
library(corrplot)
library(ggplot2)
library(scales)

## Business color palette
col_main  <- "#2B5F8E"   
col_acc   <- "#E07B39"   
col_green <- "#3A7D44"   
col_red   <- "#C0392B"   
col_light <- "#A8C4DC"  

# 2.1 Examine Variable Types

str(shoppers)

## Variable type count
table(sapply(shoppers, class))

## Unique value count per variable
sapply(shoppers, function(x) length(unique(x)))



# 2.2 Summary Statistics

summary(shoppers)

## Rich summary
skim(shoppers)

## Key observations for e-commerce analysis:
## ProductRelated  : mean 31.7 >> median 18 -- right-skewed
## PageValues      : mean 5.9  >> median 0  -- majority zero direct conversion signal for RQ1
## BounceRates     : concentrated near 0 
## SpecialDay      : >90% zero 



# 2.3 Check Missingness

## Total missing values
sum(is.na(shoppers))

## Per-column missing count
colSums(is.na(shoppers))

## Complete-case rate
mean(complete.cases(shoppers)) * 100

## Formal naniar audit
miss_var_summary(shoppers)

## Visual confirmation
vis_miss(shoppers, sort_miss = TRUE)

## NOTE: No imputation or row deletion required.Data quality issue is type mismatch, not missingness.



# 2.4 Identify Data Quality Problems

##  Problem 1: Range validity 

range(shoppers$BounceRates)
sum(shoppers$BounceRates < 0 | shoppers$BounceRates > 1)

range(shoppers$ExitRates)
sum(shoppers$ExitRates < 0 | shoppers$ExitRates > 1)

sum(shoppers$Administrative_Duration  < 0)
sum(shoppers$Informational_Duration   < 0)
sum(shoppers$ProductRelated_Duration  < 0)

##  Problem 2: BounceRate vs ExitRate logic 
sum(shoppers$BounceRates > shoppers$ExitRates)

##  Problem 3: Zero-page sessions 
## Sessions with no pages visited across all three categories
zero_sessions <- sum(
  shoppers$Administrative == 0 &
    shoppers$Informational  == 0 &
    shoppers$ProductRelated == 0)
cat("Zero-page sessions:",
    zero_sessions,
    "(", round(zero_sessions / nrow(shoppers) * 100, 1), "%)\n")
## No behavioral signal in these sessions 

##  Problem 4: Inconsistent Month encoding -
sort(table(shoppers$Month), decreasing = TRUE)

##  Problem 5: Class imbalance (Revenue) 
rev_tab <- table(shoppers$Revenue)
rev_pct <- round(prop.table(rev_tab) * 100, 1)
print(rev_tab)
cat("Conversion rate:", rev_pct["TRUE"], "%\n")
## Only 15.5% of sessions convert

##  Problem 6: Extreme session durations 
extreme_dur <- sum(shoppers$ProductRelated_Duration > 10800)
cat("Sessions > 3 hrs on product pages:", extreme_dur, "\n")
shoppers[shoppers$ProductRelated_Duration > 10800,
         c("ProductRelated", "ProductRelated_Duration",
           "PageValues", "Revenue")]

##  Problem 7: Low-frequency levels 
sort(table(shoppers$TrafficType), decreasing = TRUE)
## TrafficType: 20 levels, some < 10 sessions

sort(table(shoppers$Browser), decreasing = TRUE)

##  Data quality issues summary chart 
issues   <- c("Type Mismatch\n(4 int->factor)",
              "Month Encoding\n('June'!='Jun')",
              "Class Imbalance\n(Revenue 15.5%)",
              "Right Skew\n(6 numeric vars)",
              "Sparse Feature\n(SpecialDay)",
              "Extreme Duration\n(>3 hrs)",
              "Low-Freq Levels\n(Traffic/Browser)")
severity <- c(3, 1, 3, 2, 2, 1, 1)
sev_cols <- c(col_red, col_acc, col_red,
              col_acc, col_acc, col_light, col_light)

par(mar = c(7, 5, 4, 2))
bp <- barplot(severity,
              names.arg = issues,
              col       = sev_cols,
              border    = NA,
              ylim      = c(0, 4.2),
              main      = "Data Quality Issues -- Audit Summary",
              ylab      = "Severity  (1=Low  2=Med  3=High)",
              cex.names = 0.78,
              cex.axis  = 0.9,
              cex.main  = 1.05,
              las       = 1)
abline(h = c(1, 2, 3), col = "gray88", lty = 2)
text(bp, severity + 0.15,
     labels = c("HIGH","LOW","HIGH","MED","MED","LOW","LOW"),
     font = 2, cex = 0.82, col = "#1A1A2E")
mtext("All issues addressed in Step 3 (Data Cleaning & Wrangling)",
      side = 1, line = 5.5, cex = 0.78, col = "gray45")
par(mar = c(5, 4, 4, 2))



# 2.5 Initial Data Dictionary

cat("
=================================================================
DATA DICTIONARY -- Online Shoppers Purchasing Intention
=================================================================
Variable                 Type(raw)  Type(clean)  Description
-----------------------------------------------------------------
Administrative           integer    integer      # admin pages visited per session
Administrative_Duration  numeric    numeric      Time(s) spent on admin pages
Informational            integer    integer      # informational pages visited
Informational_Duration   numeric    numeric      Time(s) spent on info pages
ProductRelated           integer    integer      # product pages visited
ProductRelated_Duration  numeric    numeric      Time(s) spent on product pages
BounceRates              numeric    numeric      % of sessions bounced from page [0,1]
ExitRates                numeric    numeric      % of exits from each page [0,1]
PageValues               numeric    numeric      Avg $ value of pages in session
SpecialDay               numeric    numeric      Proximity to special day [0,1]
Month                    character  factor       Session month (Feb-Dec; no Jan/Apr)
OperatingSystems         integer    factor       Operating system code (nominal, 8 levels)
Browser                  integer    factor       Browser code (nominal, 13 levels)
Region                   integer    factor       Geographic region code (nominal, 9 levels)
TrafficType              integer    factor       Traffic source code (nominal, 20 levels)
VisitorType              character  factor       Returning_Visitor / New_Visitor / Other
Weekend                  logical    factor       TRUE if session occurred on weekend
Revenue                  logical    factor       TRUE if session resulted in purchase [TARGET]
=================================================================
Notes:
  - No missing values (n_miss = 0 for all variables)
  - Revenue is the binary target variable (15.5% TRUE)
  - 4 integer columns are nominal and must be cast to factor
  - Month 'June' to be recoded to 'Jun' for consistency
=================================================================
")



# 2.6 Audit Report -- Console Summary

cat("=====================================================\n")
cat("         DATA AUDIT REPORT -- STEP 2\n")
cat("         Online Shoppers Purchasing Intention\n")
cat("=====================================================\n")
cat("Rows              :", nrow(shoppers), "\n")
cat("Columns           :", ncol(shoppers), "\n")
cat("Missing values    :", sum(is.na(shoppers)), "(0%)\n")
cat("Complete cases    : 100%\n")
cat("\nVariable types (raw):\n")
print(table(sapply(shoppers, class)))
cat("\nData quality issues to resolve in Step 3:\n")
cat("  [HIGH] 4 integer columns are nominal -> cast to factor\n")
cat("  [HIGH] Target imbalance: Revenue TRUE = 15.5%\n")
cat("  [MED]  6 numeric vars right-skewed -> log transform\n")
cat("  [MED]  SpecialDay >90% zero -> sparse feature\n")
cat("  [LOW]  'June' inconsistent with 3-letter month format\n")
cat("  [LOW]  Extreme duration sessions (>3hrs) flagged\n")
cat("  [LOW]  Low-frequency levels in TrafficType/Browser\n")
cat("\nTarget variable (Revenue):\n")
print(rev_tab)
cat("Conversion rate:", rev_pct["TRUE"], "%\n")
cat("=====================================================\n")



# ---------------------------------------------------------------
# STEP 3 -- Data Cleaning & Wrangling
# ---------------------------------------------------------------
cat("=====================================================\n")
cat("  STEP 3 -- Data Cleaning & Wrangling\n")
cat("=====================================================\n")


# 3.1 Create Working Copy

df <- shoppers

dim(df)   



# 3.2 Normalize Variable Types

df$OperatingSystems <- as.factor(df$OperatingSystems)
df$Browser          <- as.factor(df$Browser)
df$Region           <- as.factor(df$Region)
df$TrafficType      <- as.factor(df$TrafficType)

## Confirm levels
levels(df$OperatingSystems)   
levels(df$Browser)            
levels(df$Region)             
levels(df$TrafficType)       

##  Cast character variables to factor 
df$VisitorType <- as.factor(df$VisitorType)
levels(df$VisitorType)

## Fix Month encoding and cast to ordered factor 
table(df$Month)

df$Month[df$Month == "June"] <- "Jun"
table(df$Month) 

## Set chronological level order
df$Month <- factor(df$Month,
                   levels = c("Feb","Mar","May","Jun",
                              "Jul","Aug","Sep","Oct","Nov","Dec"))
levels(df$Month)
summary(df$Month)

##  Cast logical variables to labeled factor 
df$Revenue <- factor(df$Revenue,
                     levels = c(FALSE, TRUE),
                     labels = c("No", "Yes"))
summary(df$Revenue)


## Weekend: Weekday/Weekend clearer in plots and reports
df$Weekend <- factor(df$Weekend,
                     levels = c(FALSE, TRUE),
                     labels = c("Weekday", "Weekend"))
summary(df$Weekend)

##  Confirm all types after normalization 
str(df)
table(sapply(df, class))



# 3.3 Address Missing Values


## Re-verify on the clean copy
sum(is.na(df))
# All zero -- no action needed



# 3.4 Standardize Formats


##  Verify BounceRate and ExitRate are in [0, 1] 
range(df$BounceRates)
range(df$ExitRates)
## Both confirmed in [0, 1] -- no rescaling needed

##  Verify duration variables are non-negative 
range(df$Administrative_Duration)
range(df$Informational_Duration)
range(df$ProductRelated_Duration)
## All non-negative -- no rescaling needed

##  Verify SpecialDay is in [0, 1] 
range(df$SpecialDay)
## Confirmed [0, 1] -- valid



# 3.5 Detect and Flag Outliers

##  Outlier detection using 1.5 x IQR rule 
## Applied to the six core continuous variables

flag_outliers <- function(x, varname) {
  q1  <- quantile(x, 0.25, na.rm = TRUE)
  q3  <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lo  <- q1 - 1.5 * iqr
  hi  <- q3 + 1.5 * iqr
  n   <- sum(x < lo | x > hi, na.rm = TRUE)
  pct <- round(n / length(x) * 100, 1)
  cat(varname, ": lower fence =", round(lo, 4),
      "| upper fence =", round(hi, 4),
      "| outliers =", n, "(", pct, "%)\n")
  return(x < lo | x > hi)
}

out_admin   <- flag_outliers(df$Administrative,         "Administrative        ")
out_info    <- flag_outliers(df$Informational,          "Informational         ")
out_prod    <- flag_outliers(df$ProductRelated,         "ProductRelated        ")
out_bounce  <- flag_outliers(df$BounceRates,            "BounceRates           ")
out_exit    <- flag_outliers(df$ExitRates,              "ExitRates             ")
out_pageval <- flag_outliers(df$PageValues,             "PageValues            ")

##  Flag extreme duration sessions 
df$flag_extreme_dur <- df$ProductRelated_Duration > 10800
sum(df$flag_extreme_dur)
cat("Extreme duration sessions flagged:",
    sum(df$flag_extreme_dur), "\n")

## Do extreme-duration sessions convert at higher rates?
## Critical insight for e-commerce: does more time = more buy?
table(df$flag_extreme_dur, df$Revenue)
round(prop.table(
  table(df$flag_extreme_dur, df$Revenue), margin = 1) * 100, 1)
## If conversion rate is higher in flagged sessions,
## long browsing = purchase intent -- keep in dataset

## Flag zero-page sessions 
## Sessions with 0 pages in all three categories carry
## no behavioral signal -- flag for sensitivity checks
df$flag_zero_pages <- (df$Administrative == 0 &
                         df$Informational  == 0 &
                         df$ProductRelated == 0)
sum(df$flag_zero_pages)
cat("Zero-page sessions flagged:",
    sum(df$flag_zero_pages), "\n")

## DECISION: flag both anomalies but do not delete any rows.
## Removing outliers in e-commerce data risks eliminating
## genuine high-value or high-intent sessions.
## Both flags are available as filter variables if needed.

##  Outlier visualization: core numeric variables 
par(mfrow = c(2, 3))

boxplot(df$Administrative,
        col     = col_main,
        border  = "#1A1A2E",
        main    = "Administrative",
        ylab    = "Pages Visited",
        outline = TRUE,
        outcol  = col_red,
        outpch  = 16,
        outcex  = 0.4)

boxplot(df$Informational,
        col     = col_main,
        border  = "#1A1A2E",
        main    = "Informational",
        ylab    = "Pages Visited",
        outline = TRUE,
        outcol  = col_red,
        outpch  = 16,
        outcex  = 0.4)

boxplot(df$ProductRelated,
        col     = col_main,
        border  = "#1A1A2E",
        main    = "ProductRelated",
        ylab    = "Pages Visited",
        outline = TRUE,
        outcol  = col_red,
        outpch  = 16,
        outcex  = 0.4)

boxplot(df$BounceRates,
        col     = col_acc,
        border  = "#1A1A2E",
        main    = "BounceRates",
        ylab    = "Rate",
        outline = TRUE,
        outcol  = col_red,
        outpch  = 16,
        outcex  = 0.4)

boxplot(df$ExitRates,
        col     = col_acc,
        border  = "#1A1A2E",
        main    = "ExitRates",
        ylab    = "Rate",
        outline = TRUE,
        outcol  = col_red,
        outpch  = 16,
        outcex  = 0.4)

boxplot(df$PageValues,
        col     = col_green,
        border  = "#1A1A2E",
        main    = "PageValues",
        ylab    = "Value ($)",
        outline = TRUE,
        outcol  = col_red,
        outpch  = 16,
        outcex  = 0.4)

par(mfrow = c(1, 1))
## All six variables show substantial right-skew with many
## IQR outliers. This is expected in web traffic data.
## Log+1 transformation applied in Step 4 (Feature Engineering).



# 3.6 Reusable Data Cleaning Pipeline

clean_shoppers <- function(raw) {
  
  df <- raw
  
  ## Step 1: Fix Month encoding
  df$Month[df$Month == "June"] <- "Jun"
  
  ## Step 2: Cast nominal integers to factor
  df$OperatingSystems <- as.factor(df$OperatingSystems)
  df$Browser          <- as.factor(df$Browser)
  df$Region           <- as.factor(df$Region)
  df$TrafficType      <- as.factor(df$TrafficType)
  
  ## Step 3: Cast character variables to factor
  df$Month       <- factor(df$Month,
                           levels = c("Feb","Mar","May","Jun",
                                      "Jul","Aug","Sep","Oct",
                                      "Nov","Dec"))
  df$VisitorType <- as.factor(df$VisitorType)
  
  ## Step 4: Cast logical variables to labeled factor
  df$Revenue <- factor(df$Revenue,
                       levels = c(FALSE, TRUE),
                       labels = c("No", "Yes"))
  df$Weekend <- factor(df$Weekend,
                       levels = c(FALSE, TRUE),
                       labels = c("Weekday", "Weekend"))
  
  ## Step 5: Flag anomalous sessions (no rows deleted)
  df$flag_extreme_dur <- df$ProductRelated_Duration > 10800
  df$flag_zero_pages  <- (df$Administrative == 0 &
                            df$Informational  == 0 &
                            df$ProductRelated == 0)
  
  return(df)
}

## Apply pipeline to raw data
df <- clean_shoppers(shoppers)

## Verify output
dim(df)         
str(df)
summary(df)



# 3.7 Post-Cleaning Verification

## Variable types after cleaning
cat("Variable types after cleaning:\n")
print(table(sapply(df, class)))

## Factor level check
cat("\nMonth levels:\n");       print(levels(df$Month))
cat("\nVisitorType levels:\n"); print(levels(df$VisitorType))
cat("\nRevenue levels:\n");     print(levels(df$Revenue))
cat("\nWeekend levels:\n");     print(levels(df$Weekend))

## No missing values introduced by cleaning
cat("\nMissing values after cleaning:", sum(is.na(df)), "\n")

## Row count unchanged -- no rows deleted
cat("Rows before:", nrow(shoppers), "\n")
cat("Rows after :", nrow(df),       "\n")


## Flag summary
cat("\nExtreme duration sessions flagged:",
    sum(df$flag_extreme_dur), "\n")
cat("Zero-page sessions flagged:",
    sum(df$flag_zero_pages),  "\n")



# 3.8 Cleaning Report -- Console Output

cat("=====================================================\n")
cat("      DATA CLEANING REPORT  --  STEP 3\n")
cat("      Online Shoppers Purchasing Intention\n")
cat("=====================================================\n")
cat("Rows before cleaning  :", nrow(shoppers), "\n")
cat("Rows after cleaning   :", nrow(df),       "\n")
cat("Rows deleted          : 0\n")
cat("New flag columns added: 2\n")
cat("\nType changes applied:\n")
cat("  OperatingSystems -> factor (8 levels)\n")
cat("  Browser          -> factor (13 levels)\n")
cat("  Region           -> factor (9 levels)\n")
cat("  TrafficType      -> factor (20 levels)\n")
cat("  Month            -> factor (10 levels, ordered)\n")
cat("  VisitorType      -> factor (3 levels)\n")
cat("  Revenue          -> factor (No / Yes)\n")
cat("  Weekend          -> factor (Weekday / Weekend)\n")
cat("\nFormat corrections:\n")
cat("  'June' recoded to 'Jun' in Month variable\n")
cat("\nOutlier decisions:\n")
cat("  flag_extreme_dur: sessions > 3hrs product browsing\n")
cat("  flag_zero_pages : sessions with 0 pages visited\n")
cat("  No rows deleted -- outliers retained and flagged\n")
cat("\nMissing values after cleaning:", sum(is.na(df)), "\n")
cat("=====================================================\n")


# ---------------------------------------------------------------
# STEP 4 -- Univariate Exploration
# ---------------------------------------------------------------



# Color palette (project-wide standard)

col_main  <- "#2B5F8E"   
col_acc   <- "#E07B39"   
col_green <- "#3A7D44"   
col_red   <- "#C0392B"   
col_light <- "#A8C4DC"   
col_pur   <- "#7D5BA6"  


# 4.1  Revenue -- target variable distribution
par(mar = c(4, 5, 4, 2))
rev_tab <- table(df$Revenue)
rev_pct <- round(prop.table(rev_tab) * 100, 1)

bp1 <- barplot(rev_tab,
               names.arg = c("No Purchase", "Purchase"),
               col       = c(col_main, col_green),
               border    = NA,
               ylim      = c(0, 13000),
               main      = "4.1  Target Variable: Revenue",
               ylab      = "Number of Sessions",
               cex.main  = 1.1,
               cex.names = 1.0)
text(bp1, rev_tab + 350,
     labels = paste0(format(as.integer(rev_tab), big.mark = ","),
                     "  (", rev_pct, "%)"),
     font = 2, cex = 0.95, col = "#1A1A2E")
mtext("Class imbalance: only 15.5% of sessions convert -- baseline to beat in all models",
      side = 1, line = 3, cex = 0.8, col = col_red)
par(mar = c(5, 4, 4, 2))

# INTERPRETATION:
# 10,422 sessions (84.5%) did not result in a purchase;
# only 1,908 (15.5%) converted. This severe imbalance means
# a naive classifier predicting "No" is 84.5% accurate --all downstream models and proportional comparisons must account for this baseline.



# 4.2  Visitor Type -- composition of the audience 

vt_df           <- as.data.frame(table(df$VisitorType))
names(vt_df)    <- c("VisitorType", "Count")
vt_df$Pct       <- round(vt_df$Count / sum(vt_df$Count) * 100, 1)
vt_df$Label     <- paste0(vt_df$Pct, "%  (n=",
                          format(vt_df$Count, big.mark = ","), ")")

ggplot(vt_df,
       aes(x = reorder(VisitorType, Count), y = Count, fill = VisitorType)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_text(aes(label = Label), hjust = -0.08,
            size = 3.2, color = "#1A1A2E", fontface = "bold") +
  scale_fill_manual(values = c("Returning_Visitor" = col_main,
                               "New_Visitor"       = col_acc,
                               "Other"             = col_light)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.28))) +
  coord_flip() +
  labs(title    = "4.2  Visitor Type Composition",
       subtitle = "85.6% returning visitors — new visitor acquisition is the growth gap",
       x = NULL, y = "Number of Sessions") +
  theme_minimal(base_size = 11) +
  theme(plot.title         = element_text(face = "bold", size = 12),
        plot.subtitle      = element_text(color = "gray50", size = 9),
        panel.grid.major.y = element_blank(),
        axis.text.y        = element_text(size = 10))

# INTERPRETATION:
# Returning visitors dominate at 85.6% (10,551 sessions).
# New visitors account for 13.7% (1,694) -- the conversion growth opportunity 



# 4.3  Month -- session volume across the calendar year
par(mar = c(5, 5, 4, 2))

month_ord <- c("Feb","Mar","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
mo_tab    <- table(factor(df$Month, levels = month_ord))

# Highlight the two peak months 
peak_months <- c("May", "Nov")
mo_cols <- ifelse(names(mo_tab) %in% peak_months, col_acc, col_main)

bp3 <- barplot(mo_tab,
               col      = mo_cols,
               border   = NA,
               ylim     = c(0, 4000),
               main     = "4.3  Session Volume by Month",
               ylab     = "Number of Sessions",
               xlab     = "",          # removed -- month names on axis are enough
               cex.main = 1.1,
               cex.names = 0.9,
               las      = 1)

# Mean reference line
abline(h   = mean(mo_tab),
       lty = 2, lwd = 1.5, col = col_red)
text(x      = bp3[length(bp3)] + 0.3,
     y      = mean(mo_tab) + 120,
     labels = paste0("Mean: ", round(mean(mo_tab))),
     col = col_red, cex = 0.8, adj = 1, xpd = TRUE)

# Count labels on top of each bar
text(bp3, mo_tab + 80,
     labels = format(as.integer(mo_tab), big.mark = ","),
     cex = 0.75, col = "#1A1A2E", font = 2)

# Legend for color meaning
legend("topright",
       legend = c("Peak month", "Other month"),
       fill   = c(col_acc, col_main),
       border = NA, bty = "n", cex = 0.85)

mtext("May (spring sale) and November (holiday season) account for ~52% of all sessions",
      side = 1, line = 3.5, cex = 0.78, col = "gray40")

par(mar = c(5, 4, 4, 2))
# INTERPRETATION:
# Two peaks dominate: May (3,364 sessions, 27.3%) and November (2,998, 24.3%). 
# Both align with major retail events (spring promotions, Black Friday / Cyber Monday).
# Feb and Jul are troughs. Seasonality is non-uniform any campaign ROI analysis must control for month.



# 4.4  BounceRates -- exit-before-engagement signal
par(mfrow = c(1, 2), mar = c(4, 5, 4, 2))

hist(df$BounceRates,
     col     = col_main,
     border  = "white",
     breaks  = 40,
     freq    = FALSE,
     main    = "4.4a  Histogram",
     xlab    = "Bounce Rate",
     cex.main = 1.0)
lines(density(df$BounceRates, bw = 0.005),
      col = col_acc, lwd = 2)
rug(df$BounceRates, col = col_red, alpha = 0.3)

boxplot(df$BounceRates,
        col     = col_light,
        border  = col_main,
        main    = "4.4b  Boxplot",
        ylab    = "Bounce Rate",
        cex.main = 1.0,
        outline = TRUE,
        outpch  = 16, outcex = 0.4, outcol = col_acc)
abline(h = median(df$BounceRates), lty = 2, col = col_red, lwd = 1.5)

par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))
cat("-- BounceRates summary --\n")
print(round(summary(df$BounceRates), 4))
cat("Skewness (manual):",
    round(mean((df$BounceRates - mean(df$BounceRates))^3) /
            sd(df$BounceRates)^3, 2), "\n")

# INTERPRETATION:
# Strongly right-skewed: most sessions have very low bounce rates (median ≈ 0.017),
# but a small fraction exits immediately (upper quartile > 0.027).



# 4.5  ExitRates -- per-page exit propensity  
par(mar = c(5, 5, 4, 2))

hist(df$ExitRates,
     col    = col_light,        # soft light-blue bars
     border = "white",
     breaks = 40,
     freq   = FALSE,
     main   = "4.5  ExitRates Distribution",
     xlab   = "",
     ylab   = "Density",
     cex.main = 1.1)
lines(density(df$ExitRates, bw = 0.003),
      col = col_main, lwd = 2.5)   # navy density curve
rug(df$ExitRates,
    col  = col_main,
    ticksize = -0.02,
    alpha    = 0.15)
legend("topright",
       legend = "Density curve",
       col    = col_main, lwd = 2.5, bty = "n", cex = 0.85)
mtext("Exit Rate",
      side = 1, line = 2, cex = 0.9, col = "gray20")
mtext(paste0("Mean: ", round(mean(df$ExitRates), 4),
             "    Median: ", round(median(df$ExitRates), 4),
             "    Max: ",   round(max(df$ExitRates), 4)),
      side = 1, line = 3.5, cex = 0.78, col = "gray40")

par(mar = c(5, 4, 4, 2))

# INTERPRETATION:
# ExitRates range [0.0, 0.20], roughly uniform between 0.01 and 0.05 with a sharp spike near 0.
# Sessions with high ExitRates on product pages signal the visitor evaluated but did not commit -- a key mid-funnel drop-off pattern.



# 4.6  PageValues -- commercial weight of visited pages 
par(mfrow = c(1, 2), mar = c(6, 4, 3, 1))

# Panel a: zero vs non-zero proportion (barplot)
pv_zero <- c("Zero\n(= 0)", "Non-Zero\n(> 0)")  
pv_cnt  <- c(sum(df$PageValues == 0), sum(df$PageValues > 0))
pv_pct  <- round(pv_cnt / nrow(df) * 100, 1)


ylim_top <- max(pv_cnt) * 1.15

bp_pv <- barplot(pv_cnt,
                 names.arg = pv_zero,
                 col       = c(col_light, col_main),
                 border    = NA,
                 ylim      = c(0, ylim_top),
                 main      = "4.6a  PageValues Distribution",  
                 ylab      = "Number of Sessions",
                 cex.main  = 0.92,
                 cex.names = 0.88)                             

text(bp_pv, pv_cnt + ylim_top * 0.02,
     labels = paste0(pv_pct, "%"),
     font = 2, cex = 0.9, col = "#1A1A2E")

# Panel b: boxplot of log(PageValues+1)
boxplot(log(df$PageValues + 1),
        col      = col_light,
        border   = col_main,
        main     = "4.6b  log(PageValues+1)",                 
        ylab     = "log(PageValues + 1)",
        cex.main = 0.92,
        outline  = TRUE,
        outpch   = 16, outcex = 0.3,
        outcol   = adjustcolor(col_acc, 0.5))

abline(h   = median(log(df$PageValues + 1)),
       col = col_red, lty = 2, lwd = 1.5)

text(x = 1.4,
     y = median(log(df$PageValues + 1)) + 0.15,
     labels = paste0("Median: ",
                     round(median(log(df$PageValues + 1)), 2)),
     col = col_red, cex = 0.78)

par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))

# INTERPRETATION:
# 75.4% of sessions have PageValues = 0; only a quarter involve pages
# with any assigned commercial value.
# Among non-zero sessions, values are widely spread with a long right tail.



# 4.7  ProductRelated -- ECDF (cumulative distribution) -
par(mar = c(5, 5, 4, 2))

plot(ecdf(df$ProductRelated),
     main     = "4.7  ECDF: Product Pages Visited per Session",
     xlab     = "",
     ylab     = "Cumulative Proportion of Sessions",
     col      = col_main,
     lwd      = 2,
     pch      = NA,          
     xlim     = c(0, 100),
     cex.main = 1.1,
     cex.axis = 0.9)

# Reference lines at key thresholds
thresh <- c(10, 20, 50)
for (t in thresh) {
  pct <- round(ecdf(df$ProductRelated)(t) * 100, 1)
  abline(v   = t,   lty = 3, col = "gray60", lwd = 1.2)
  abline(h   = pct/100, lty = 3, col = "gray60", lwd = 1.2)
  text(x = t + 1, y = pct/100 - 0.03,
       labels = paste0(pct, "%\n≤", t, " pages"),
       col = col_acc, cex = 0.72, adj = 0)
}

mtext("Product Pages Visited per Session",
      side = 1, line = 2, cex = 0.9, col = "gray20")
mtext("Most sessions are concentrated in low page-count range -- heavy right tail beyond 50",
      side = 1, line = 3.5, cex = 0.78, col = "gray40")

par(mar = c(5, 4, 4, 2))

# INTERPRETATION:
# Most sessions view relatively few product pages: 60% view ≤20, 90% view ≤50.
# A small fraction (10%) browses 50+ pages.



# 4.8  ProductRelated_Duration -- density only 
par(mar = c(5, 5, 4, 2))

log_dur <- log(df$ProductRelated_Duration + 1)
d_dur   <- density(log_dur, bw = 0.25)

# Shaded density polygon
plot(d_dur,
     main     = "4.8  Product Page Time: Kernel Density (log scale)",
     xlab     = "",
     ylab     = "Density",
     col      = col_main,
     lwd      = 2.5,
     cex.main = 1.05)
polygon(d_dur,
        col    = adjustcolor(col_light, alpha.f = 0.5),
        border = NA)
lines(d_dur, col = col_main, lwd = 2.5)   

# Annotate key percentiles
q25 <- quantile(log_dur, 0.25)
q75 <- quantile(log_dur, 0.75)
abline(v = q25, col = col_acc, lty = 2, lwd = 1.5)
abline(v = q75, col = col_acc, lty = 2, lwd = 1.5)
abline(v = median(log_dur), col = col_red, lty = 1, lwd = 2)

legend("topright",
       legend = c(paste0("Median: ", round(exp(median(log_dur))-1, 0), "s"),
                  paste0("IQR: [",
                         round(exp(q25)-1, 0), "s, ",
                         round(exp(q75)-1, 0), "s]")),
       col  = c(col_red, col_acc),
       lty  = c(1, 2), lwd = 2, bty = "n", cex = 0.85)

mtext("log(ProductRelated_Duration + 1)  |  back-transformed seconds shown in legend",
      side = 1, line = 2, cex = 0.82, col = "gray20")
mtext("Approximately normal on log scale -- median ~598s (~10 min) on product pages",
      side = 1, line = 3.5, cex = 0.78, col = "gray40")

par(mar = c(5, 4, 4, 2))

# INTERPRETATION:
# On the log scale, duration is roughly bell-shaped with a median around 598s (~10 min).
# The middle 50% of sessions range from about 100s to 2,100s.
# A small right tail contains sessions exceeding 2 hours.


# 4.9  ProductRatio -- stripchart

if (!"TotalPages" %in% names(df)) {
  df$TotalPages <- df$Administrative + df$Informational + df$ProductRelated
}

df$ProductRatio <- ifelse(df$TotalPages > 0,
                          df$ProductRelated / df$TotalPages,
                          0)

# Verify -- should be numeric, length 12330, range [0, 1]
cat("ProductRatio class:", class(df$ProductRatio), "\n")
cat("ProductRatio length:", length(df$ProductRatio), "\n")
cat("Range:", range(df$ProductRatio), "\n")

#  Stripchart (unchanged from previous) 
par(mar = c(5, 5, 4, 2))

stripchart(df$ProductRatio,
           method  = "jitter",
           jitter  = 0.35,
           pch     = 16,
           cex     = 0.25,
           col     = adjustcolor(col_main, alpha.f = 0.25),
           main    = "4.9  Product Page Ratio per Session",
           xlab    = "",
           ylab    = "",
           yaxt    = "n",
           cex.main = 1.1)

abline(v = mean(df$ProductRatio),   col = col_red, lty = 2, lwd = 2)
abline(v = median(df$ProductRatio), col = col_acc, lty = 3, lwd = 2)
legend("top",
       legend = c(paste("Mean:",   round(mean(df$ProductRatio),   2)),
                  paste("Median:", round(median(df$ProductRatio), 2))),
       col    = c(col_red, col_acc),
       lty    = c(2, 3), lwd = 2, bty = "n", cex = 0.85, horiz = TRUE)

mtext("Ratio  (0 = no product pages,  1 = all product pages)",
      side = 1, line = 2, cex = 0.9, col = "gray20")
mtext("Two poles: ratio~0 (no product intent) vs ratio~1 (pure product browsing)",
      side = 1, line = 3.5, cex = 0.78, col = "gray40")

par(mar = c(5, 4, 4, 2))

# INTERPRETATION:
# Values cluster at two poles: 0 (no product pages visited) and 1 (product-only browsing),
# with relatively few sessions in between -- two distinct browsing modes.


# 4.10  SpecialDay -- near-holiday session distribution 

sd_df       <- data.frame(
  DayType = c("Regular Day", "Near Holiday"),
  Count   = c(sum(df$SpecialDay == 0), sum(df$SpecialDay > 0)))
sd_df$Pct   <- round(sd_df$Count / sum(sd_df$Count) * 100, 1)
sd_df$Label <- paste0(sd_df$Pct, "%\n(n=",
                      format(sd_df$Count, big.mark = ","), ")")

ggplot(sd_df, aes(x = DayType, y = Count,
                  fill = DayType, label = Label)) +
  geom_col(width = 0.45, show.legend = FALSE) +
  geom_text(aes(y = Count / 2), color = "white",
            size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Near Holiday" = col_acc,
                               "Regular Day"  = col_light)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08)),
                     labels = scales::comma) +
  labs(title    = "4.10  Sessions Near a Holiday (SpecialDay > 0)",
       subtitle = "Only 10.1% of sessions fall near a holiday — sparse but potentially high-intent traffic",
       x = NULL, y = "Number of Sessions") +
  theme_minimal(base_size = 11) +
  theme(plot.title         = element_text(face = "bold", size = 12),
        plot.subtitle      = element_text(color = "gray50", size = 9),
        panel.grid.major.x = element_blank())

# INTERPRETATION:
# 89.9% of sessions fall on regular days; only 10.1% (n=1,249) occur near a holiday.
# The holiday group is small but worth examining for any conversion differences .


# ---------------------------------------------------------------
# STEP 5 -- Bivariate & Multivariate Analysis
# ---------------------------------------------------------------


# Prep: Revenue as numeric for correlation
df$Revenue_num <- as.numeric(df$Revenue == "Yes")  



# 5.1  Correlation heatmap 

#  Guard: recreate engineered features if not present 
if (!"TotalPages" %in% names(df))
  df$TotalPages <- df$Administrative + df$Informational + df$ProductRelated

if (!"TotalDuration" %in% names(df))
  df$TotalDuration <- df$Administrative_Duration +
  df$Informational_Duration  +
  df$ProductRelated_Duration

if (!"ProductRatio" %in% names(df))
  df$ProductRatio <- ifelse(df$TotalPages > 0,
                            df$ProductRelated / df$TotalPages, 0)

if (!"EngagementScore" %in% names(df))
  df$EngagementScore <- df$PageValues * df$ProductRelated_Duration

if (!"Revenue_num" %in% names(df))
  df$Revenue_num <- as.numeric(df$Revenue == "Yes")

#  Verify all columns exist before cor() 
num_cols <- c("BounceRates","ExitRates","PageValues",
              "ProductRelated","ProductRelated_Duration",
              "SpecialDay","TotalPages","ProductRatio",
              "EngagementScore","Revenue_num")

missing_cols <- num_cols[!num_cols %in% names(df)]
if (length(missing_cols) > 0) {
  stop(paste("Still missing:", paste(missing_cols, collapse = ", ")))
} else {
  cat("All columns confirmed. Proceeding to cor().\n")
}

#  Correlation matrix 
cor_mat <- cor(df[, num_cols], use = "complete.obs")
colnames(cor_mat)[colnames(cor_mat) == "Revenue_num"] <- "Revenue"
rownames(cor_mat)[rownames(cor_mat) == "Revenue_num"] <- "Revenue"


corrplot(cor_mat,
         method      = "color",
         type        = "upper",
         tl.col      = "#1A1A2E",
         tl.cex      = 0.82,
         col         = colorRampPalette(
           c(col_red, "#F5F5F5", col_main))(200),
         addCoef.col = "#1A1A2E",
         number.cex  = 0.65,
         cl.cex      = 0.75,
         title       = "5.1  Correlation Heatmap: Key Numeric Variables",
         mar         = c(0, 0, 2, 0))

# INTERPRETATION:
# ProductRelated and ProductRelated_Duration are moderately correlated (r≈0.86), consistent with more pages visited corresponding to longer time on site.
# BounceRates and ExitRates are tightly correlated with each other (r≈0.91) but only weakly negative with Revenue, suggesting bounce alone doesn't predict non-conversion.
# ProductRelated and ProductRelated_Duration are moderately correlated (r≈0.86), confirming more pages = more time.



# 5.2  Scatterplot: log(ProductRelated_Duration) vs log(PageValues)
par(mar = c(5, 5, 4, 2))

pt_col <- ifelse(df$Revenue == "Yes",
                 adjustcolor(col_green, alpha.f = 0.5),
                 adjustcolor(col_main,  alpha.f = 0.12))

plot(log(df$ProductRelated_Duration + 1),
     log(df$PageValues + 1),
     col      = pt_col,
     pch      = 16,
     cex      = 0.4,
     main     = "5.2  Product Duration vs Page Values by Purchase Outcome",
     xlab     = "log(ProductRelated_Duration + 1)",
     ylab     = "log(PageValues + 1)",
     cex.main = 1.05)

# OLS line per group
for (grp in c("No", "Yes")) {
  sub <- df[df$Revenue == grp, ]
  fit <- lm(log(PageValues + 1) ~ log(ProductRelated_Duration + 1), data = sub)
  abline(fit,
         col = ifelse(grp == "Yes", col_green, col_main),
         lwd = 2.2, lty = ifelse(grp == "Yes", 1, 2))
}

legend("topleft",
       legend = c("Purchase (Yes)", "No Purchase"),
       col    = c(col_green, col_main),
       pch    = 16, lty = c(1, 2), lwd = 2,
       bty    = "n", cex = 0.85)
mtext("Buyers cluster top-right: long duration AND high-value pages co-occur for converters",
      side = 1, line = 3.5, cex = 0.78, col = "gray40")

par(mar = c(5, 4, 4, 2))

# INTERPRETATION:
# The two regression lines diverge: buyers (green) have a steeper slope, meaning each additional minute on product pages predicts greater page-value exposure for purchasers than for non-purchasers. 
# Non-buyers (dashed) accumulate duration without reaching high-value pages -- a mid-funnel drop-off pattern. This directly supports RQ1.



# 5.3  PageValues by Revenue 

tt_pv <- t.test(log(df$PageValues + 1) ~ df$Revenue)

ggplot(df, aes(x = Revenue,
               y = log(PageValues + 1),
               fill = Revenue)) +
  geom_violin(trim = TRUE, alpha = 0.45, color = NA) +
  geom_boxplot(width = 0.18, outlier.shape = 16,
               outlier.size  = 0.4,
               outlier.alpha = 0.3,
               outlier.color = col_acc,
               color = "#1A1A2E", fill = "white", alpha = 0.85) +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 3, color = col_red) +
  scale_x_discrete(labels = c("No" = "No Purchase", "Yes" = "Purchase")) +
  scale_fill_manual(values = c("No" = col_light, "Yes" = col_green),
                    guide  = "none") +
  labs(title    = "5.3  PageValues by Purchase Outcome (RQ1)",
       subtitle = paste0("Welch t = ", round(tt_pv$statistic, 1),
                         "  |  p < 0.001"),
       x = "Purchase Outcome",
       y = "log(PageValues + 1)") +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(color = col_red, size = 9),
        panel.grid.major.x = element_blank())

# INTERPRETATION:
# Violin shape reveals the full distribution: non-buyers have a massive spike at 0, while buyers show a wide spread across higher values. 
# The white boxplot inside each violin marks median and IQR; the red diamond marks the mean.
# Welch t-test (p<0.001) confirms the gap is not sampling noise -- PageValues is the single strongest predictor of Revenue (RQ1).



# 5.4  BounceRates by Visitor Type  
par(mar = c(5, 5, 4, 2))

df$VisitorType <- factor(df$VisitorType,
                         levels = c("Returning_Visitor", "New_Visitor", "Other"))
boxplot(BounceRates ~ VisitorType,
        data    = df,
        col     = c(col_light, col_acc, col_pur),
        border  = col_main,
        names   = c("Returning", "New Visitor", "Other"),
        main    = "5.4  BounceRates by Visitor Type (RQ2)",
        xlab    = "",           # removed
        ylab    = "Bounce Rate",
        outline = TRUE,
        outpch  = 16, outcex = 0.25,
        outcol  = adjustcolor("gray50", 0.3),
        cex.main = 1.05)

med_br <- tapply(df$BounceRates, df$VisitorType, median)
text(seq_along(med_br), med_br + 0.006,
     labels = paste0("Md=", round(med_br, 3)),
     cex = 0.8, col = col_red, font = 2)

av4   <- aov(BounceRates ~ VisitorType, data = df)
pval4 <- summary(av4)[[1]][["Pr(>F)"]][1]
mtext("Visitor Type",
      side = 1, line = 2, cex = 0.9, col = "gray20")
mtext(paste0("One-way ANOVA: F=",
             round(summary(av4)[[1]][["F value"]][1], 1),
             "  p=", format.pval(pval4, digits = 2)),
      side = 1, line = 3.5, cex = 0.8, col = col_red)

par(mar = c(5, 4, 4, 2))

# INTERPRETATION:
# Returning visitors (Md=0.005) bounce more than new visitors (Md=0). 
# Counterintuitive -- returning visitors may navigate directly to a specific page and leave without further clicks, while new visitors explore more pages per session.
# ANOVA confirms group difference is significant (p<0.001).



# 5.5  Conversion rate by VisitorType × Weekend

conv_df <- aggregate(Revenue_num ~ VisitorType + Weekend,
                     data = df, FUN = mean)
conv_df <- conv_df[conv_df$VisitorType %in%
                     c("Returning_Visitor", "New_Visitor"), ]
conv_df$VisitorLabel <- ifelse(conv_df$VisitorType == "Returning_Visitor",
                               "Returning", "New Visitor")
conv_df$Pct <- round(conv_df$Revenue_num * 100, 1)

ggplot(conv_df, aes(x = Weekend, y = Pct,
                    fill = VisitorLabel,
                    label = paste0(Pct, "%"))) +
  geom_col(position = position_dodge(width = 0.65),
           width = 0.55) +
  geom_text(position = position_dodge(width = 0.65),
            vjust = -0.5, size = 3.2,
            fontface = "bold", color = "#1A1A2E") +
  scale_fill_manual(values = c("Returning"   = col_main,
                               "New Visitor" = col_acc)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)),
                     labels = function(x) paste0(x, "%")) +
  labs(title    = "5.5  Conversion Rate: Visitor Type × Day Type (RQ2)",
       subtitle = "New visitors convert at higher rates on both day types; weekend lifts both groups",
       x = NULL, y = "Conversion Rate (%)",
       fill = "Visitor Type") +
  theme_minimal(base_size = 11) +
  theme(plot.title         = element_text(face = "bold", size = 12),
        plot.subtitle      = element_text(color = "gray50", size = 9),
        legend.position    = "top",
        panel.grid.major.x = element_blank())


# INTERPRETATION:
# New visitors convert at higher rates than returning visitors on both weekdays and weekends,
# despite their higher BounceRates (5.4). Conversion is slightly higher on weekends for both groups.


# 5.6  Overlaid density: log(PageValues+1) for Returning vs New

pv_dens <- df[df$VisitorType %in%
                c("Returning_Visitor", "New_Visitor"), ]
pv_dens$VisitorLabel <- ifelse(
  pv_dens$VisitorType == "Returning_Visitor",
  paste0("Returning  (n=",
         format(sum(df$VisitorType == "Returning_Visitor"),
                big.mark = ","), ")"),
  paste0("New Visitor  (n=",
         format(sum(df$VisitorType == "New_Visitor"),
                big.mark = ","), ")"))
pv_dens$log_pv <- log(pv_dens$PageValues + 1)

ggplot(pv_dens, aes(x = log_pv,
                    fill  = VisitorLabel,
                    color = VisitorLabel)) +
  geom_density(bw = 0.30, alpha = 0.18, linewidth = 1.2) +
  scale_fill_manual(values  = c(
    setNames(col_main, grep("Returning", unique(pv_dens$VisitorLabel), value = TRUE)),
    setNames(col_acc,  grep("New",       unique(pv_dens$VisitorLabel), value = TRUE)))) +
  scale_color_manual(values = c(
    setNames(col_main, grep("Returning", unique(pv_dens$VisitorLabel), value = TRUE)),
    setNames(col_acc,  grep("New",       unique(pv_dens$VisitorLabel), value = TRUE)))) +
  labs(title    = "5.6  PageValues Density: Returning vs New Visitors (RQ2)",
       subtitle = "Both groups peak near 0 — most sessions never reach high-value pages",
       x = "log(PageValues + 1)", y = "Density",
       fill = NULL, color = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title      = element_text(face = "bold", size = 12),
        plot.subtitle   = element_text(color = "gray50", size = 9),
        legend.position = "top")

# INTERPRETATION:
# Both groups are dominated by zero-PageValues sessions.
# The right tails diverge slightly -- returning visitors show marginally higher PageValues exposure, but the difference is small compared to the buyer/non-buyer gap seen in 5.3.


# 5.7  Conversion rate by TrafficType  
par(mar = c(5, 5, 4, 4))

conv_traffic <- sort(
  tapply(df$Revenue_num, df$TrafficType, mean) * 100,
  decreasing = FALSE)

bar_cols <- ifelse(conv_traffic >= median(conv_traffic),
                   col_main, col_light)

bp7 <- barplot(conv_traffic,
               horiz     = TRUE,
               col       = bar_cols,
               border    = NA,
               xlim      = c(0, max(conv_traffic) * 1.3),
               main      = "5.7  Conversion Rate by Traffic Type (RQ3)",
               xlab      = "",          # removed
               las       = 1,
               cex.names = 0.78,
               cex.main  = 1.05)

text(conv_traffic + 0.3, bp7,
     labels = paste0(round(conv_traffic, 1), "%"),
     cex = 0.75, col = "#1A1A2E", adj = 0, font = 2)

abline(v = mean(conv_traffic), lty = 2, col = col_red, lwd = 1.5)

mtext("Conversion Rate (%)",
      side = 1, line = 2, cex = 0.9, col = "gray20")
mtext("Darker = above-median conversion  |  Types 12/15/17/18 convert 0%",
      side = 1, line = 3.5, cex = 0.78, col = "gray40")

par(mar = c(5, 4, 4, 2))

# INTERPRETATION:
# Conversion rates vary considerably across traffic types.
# Types 16, 7, and 8 convert at 2–3× the dataset average; types 12, 15, 17, and 18 show zero conversions in this dataset.



# 5.8  Revenue proportion by Season  

if (!"Season" %in% names(df)) {
  season <- rep(NA_character_, nrow(df))
  season[df$Month %in% c("Mar","May","Jun")] <- "Spring"
  season[df$Month %in% c("Jul","Aug","Sep")] <- "Summer"
  season[df$Month %in% c("Oct","Nov")]       <- "Autumn"
  season[df$Month %in% c("Feb","Dec")]       <- "Winter"
  df$Season <- factor(season,
                      levels = c("Spring","Summer","Autumn","Winter"))
  cat("Season rebuilt:", table(df$Season), "\n")
}

#  Stacked barplot 
par(mar = c(4, 5, 4, 2))

season_tab <- prop.table(
  table(df$Season, df$Revenue), margin = 1) * 100
season_mat <- t(round(season_tab, 1))   # Revenue as rows

bp8 <- barplot(season_mat,
               col     = c(col_light, col_green),
               border  = NA,
               ylim    = c(0, 120),          
               main    = "5.8  Purchase Proportion by Season",
               ylab    = "% of Sessions",
               cex.main = 1.05,
               legend.text = c("No Purchase", "Purchase"),
               args.legend = list(bty    = "n",
                                  cex    = 0.85,
                                  fill   = c(col_light, col_green),
                                  border = NA,
                                  x      = "topright",
                                  inset  = c(0, -0.08))) 

text(bp8,
     season_mat["Yes", ] / 2,
     labels = paste0(season_mat["Yes", ], "%"),
     cex = 0.9, font = 2, col = "white")

par(mar = c(5, 4, 4, 2))


# INTERPRETATION:
# Autumn has the highest purchase rate among the four seasons; Winter is second.
# Spring has the most sessions but a lower conversion rate than either.



# 5.9  Scatter matrix (pairs)
plot_mat <- data.frame(
  PageVal  = log(df$PageValues + 1),
  ProdPg   = log(df$ProductRelated + 1),
  ProdDur  = log(df$ProductRelated_Duration + 1),
  Bounce   = df$BounceRates,
  Exit     = df$ExitRates)

pt_col9 <- ifelse(df$Revenue == "Yes",
                  adjustcolor(col_green, 0.45),
                  adjustcolor(col_main,  0.10))

pairs(plot_mat,
      col  = pt_col9,
      pch  = 16,
      cex  = 0.3,
      gap  = 0.25,
      main = "5.9  Scatter Matrix: Key Engagement Variables  (green=Purchase)",
      cex.main = 0.95,
      upper.panel = function(x, y, ...) {
        r <- round(cor(x, y), 2)
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        text(0.5, 0.5, paste0("r=", r),
             cex  = 1.0, font = 2,
             col  = ifelse(abs(r) > 0.4, col_red,
                           ifelse(abs(r) > 0.2, col_acc, "gray60")))
      })


# INTERPRETATION:
# PageVal–ProdDur pair shows a moderate positive r (≈0.40) with a visible green cluster in the top-right -- buyers occupy the high-duration, high-page-value quadrant.
# Bounce–Exit correlation (r≈0.91) confirms these metrics are nearly redundant; including both in a model risks collinearity.
# ProdPg–ProdDur (r≈0.86) confirms browsing depth and time are two sides of the same behavioral signal (RQ1).



# 5.10  Image heatmap: avg PageValues by Region × VisitorType 
par(mar = c(5, 5, 4, 5))

pv_heat <- tapply(df$PageValues,
                  list(df$Region, df$VisitorType),
                  mean)
pv_heat <- round(pv_heat, 1)

# Keep only Returning and New (Other is negligible)
pv_heat <- pv_heat[, c("Returning_Visitor", "New_Visitor")]

# Normalize for color mapping
pv_norm <- (pv_heat - min(pv_heat, na.rm = TRUE)) /
  (max(pv_heat, na.rm = TRUE) - min(pv_heat, na.rm = TRUE))

heat_col <- colorRampPalette(c(col_light, col_main, col_red))(100)

image(1:ncol(pv_norm),
      1:nrow(pv_norm),
      t(pv_norm),           # image() expects col×row matrix
      col    = heat_col,
      axes   = FALSE,
      main   = "5.10  Avg PageValues Heatmap: Region × Visitor Type (RQ3)",
      xlab   = "",
      ylab   = "Region",
      cex.main = 1.0)

axis(1, at = 1:2,
     labels = c("Returning", "New Visitor"),
     tick = FALSE, cex.axis = 0.9)
axis(2, at = 1:nrow(pv_heat),
     labels = rownames(pv_heat),
     las = 1, cex.axis = 0.82, tick = FALSE)
mtext("Visitor Type", side = 1, line = 2.5, cex = 0.9, col = "gray20")
box()

# Value labels inside cells
for (i in 1:ncol(pv_heat)) {
  for (j in 1:nrow(pv_heat)) {
    text(i, j, pv_heat[j, i],
         cex = 0.78, font = 2,
         col = ifelse(pv_norm[j, i] > 0.55, "white", "#1A1A2E"))
  }
}

par(mar = c(5, 4, 4, 2))

# INTERPRETATION:
# Average PageValues vary across regions and visitor types.
# Some regions show higher PageValues for new visitors than returning visitors, while others show the reverse. 
# The pattern differs enough across regions to be worth examining alongside TrafficType 


# ---------------------------------------------------------------
# STEP 6 -- Feature Engineering
# ---------------------------------------------------------------


# 6.1  Verify / rebuild base engineered features

if (!"TotalPages" %in% names(df))
  df$TotalPages <- df$Administrative + df$Informational + df$ProductRelated

if (!"TotalDuration" %in% names(df))
  df$TotalDuration <- df$Administrative_Duration +
  df$Informational_Duration  +
  df$ProductRelated_Duration

if (!"ProductRatio" %in% names(df))
  df$ProductRatio <- ifelse(df$TotalPages > 0,
                            df$ProductRelated / df$TotalPages, 0)

if (!"AvgDuration" %in% names(df))
  df$AvgDuration <- ifelse(df$TotalPages > 0,
                           df$TotalDuration / df$TotalPages, 0)

if (!"EngagementScore" %in% names(df))
  df$EngagementScore <- df$PageValues * df$ProductRelated_Duration

if (!"Season" %in% names(df)) {
  s <- rep(NA_character_, nrow(df))
  s[df$Month %in% c("Mar","May","Jun")] <- "Spring"
  s[df$Month %in% c("Jul","Aug","Sep")] <- "Summer"
  s[df$Month %in% c("Oct","Nov")]       <- "Autumn"
  s[df$Month %in% c("Feb","Dec")]       <- "Winter"
  df$Season <- factor(s, levels = c("Spring","Summer","Autumn","Winter"))
}

if (!"IsReturning" %in% names(df))
  df$IsReturning <- factor(
    ifelse(df$VisitorType == "Returning_Visitor", "Returning", "Non-Returning"))

if (!"Revenue_num" %in% names(df))
  df$Revenue_num <- as.numeric(df$Revenue == "Yes")

cat("Base features verified. df dimensions:", dim(df), "\n")



# 6.2  New ratio & difference features


#  6.2a  Page-type ratios: how is attention distributed? 
df$AdminRatio <- ifelse(df$TotalPages > 0,
                        df$Administrative / df$TotalPages, 0)

df$InfoRatio  <- ifelse(df$TotalPages > 0,
                        df$Informational / df$TotalPages, 0)

# Sanity check: three ratios should sum to ~1 for all rows
ratio_sum <- df$AdminRatio + df$InfoRatio + df$ProductRatio
cat("Ratio sum range:", range(round(ratio_sum, 6)), "\n")
# Expected: [0, 1] -- 0 only for sessions with 0 total pages

#  6.2b  Exit–Bounce spread: how far does a session go? 
df$ExitBounceDiff <- df$ExitRates - df$BounceRates
cat("ExitBounceDiff range:", range(df$ExitBounceDiff), "\n")
# Should be >= 0 by definition (ExitRate >= BounceRate always)

#  6.2c  Log-transformed core variables (RQ1 modeling prep)
df$log_PageValues   <- log(df$PageValues + 1)
df$log_ProdDuration <- log(df$ProductRelated_Duration + 1)
df$log_TotalDur     <- log(df$TotalDuration + 1)
df$log_ProdPages    <- log(df$ProductRelated + 1)

cat("New ratio & log features added.\n")
cat("Current df columns:", ncol(df), "\n")



# 6.3  Temporal / date-derived features

#  6.3a  Holiday season flag (Nov + Dec) 
df$IsHolidaySeason <- factor(
  ifelse(df$Month %in% c("Nov","Dec"), "Holiday Season", "Regular"),
  levels = c("Regular", "Holiday Season"))

#  6.3b  Peak sale month flag (May only) 
df$IsPeakMonth <- factor(
  ifelse(df$Month == "May", "Peak (May)", "Other"),
  levels = c("Other", "Peak (May)"))

#  6.3c  Quarter of year
quarter <- rep(NA_character_, nrow(df))
quarter[df$Month %in% c("Feb","Mar")]         <- "Q1"
quarter[df$Month %in% c("May","Jun")]         <- "Q2"
quarter[df$Month %in% c("Jul","Aug","Sep")]   <- "Q3"
quarter[df$Month %in% c("Oct","Nov","Dec")]   <- "Q4"
df$Quarter <- factor(quarter, levels = c("Q1","Q2","Q3","Q4"))

cat("Temporal features added.\n")
print(table(df$Quarter, useNA = "ifany"))



# 6.4  Binned / derived categorical features

#  6.4a  PageValue tier 

pv_nonzero <- df$PageValues[df$PageValues > 0]
pv_q33 <- quantile(pv_nonzero, 0.33)
pv_q66 <- quantile(pv_nonzero, 0.66)

df$PageValueTier <- cut(df$PageValues,
                        breaks = c(-Inf, 0, pv_q33, pv_q66, Inf),
                        labels = c("Zero", "Low", "Medium", "High"),
                        right  = TRUE)

cat("PageValueTier thresholds -- Zero: 0 | Low: 0 –",
    round(pv_q33, 1), "| Medium:", round(pv_q33,1), "–",
    round(pv_q66, 1), "| High: >", round(pv_q66, 1), "\n")
print(table(df$PageValueTier))

#  6.4b  BounceRate category 
br_q75 <- quantile(df$BounceRates, 0.75)

df$BounceCategory <- cut(df$BounceRates,
                         breaks = c(-Inf, 0.005, br_q75, Inf),
                         labels = c("Minimal", "Moderate", "High"),
                         right  = TRUE)

cat("BounceCategory thresholds: Minimal <= 0.005 | Moderate <=",
    round(br_q75, 4), "| High > ", round(br_q75, 4), "\n")
print(table(df$BounceCategory))

#  6.4c  Engagement tier (EngagementScore) 
es_nonzero <- df$EngagementScore[df$EngagementScore > 0]
es_med     <- median(es_nonzero)

df$EngagementTier <- cut(df$EngagementScore,
                         breaks = c(-Inf, 0, es_med, Inf),
                         labels = c("Cold", "Warm", "Hot"),
                         right  = TRUE)

cat("EngagementTier: Cold = 0 | Warm <= med |",
    "Hot > med (", round(es_med, 1), ")\n")
print(table(df$EngagementTier))

#  6.4d  High-intent session flag (composite)
# Sessions with ALL three signals active:
#   1. ProductRatio >= 0.7  -- mostly browsing product pages
#   2. PageValues > 0       -- reached a commercially valued page
#   3. BounceRates < 0.01   -- did not bounce quickly
df$HighIntent <- factor(
  ifelse(df$ProductRatio  >= 0.7 &
           df$PageValues    >  0   &
           df$BounceRates   <  0.01,
         "High-Intent", "Other"),
  levels = c("Other", "High-Intent"))

cat("High-Intent sessions:",
    sum(df$HighIntent == "High-Intent"),
    "(", round(mean(df$HighIntent == "High-Intent") * 100, 1), "%)\n")



# 6.5  Evaluate value added by new features

#  6.5a  Correlation ranking: all numeric features vs Revenue
# Dotchart -- ranked by absolute correlation with Revenue_num

feat_num <- c("BounceRates","ExitRates","PageValues",
              "ProductRelated","ProductRelated_Duration",
              "ProductRatio","EngagementScore","TotalPages",
              "TotalDuration","AvgDuration",
              "AdminRatio","InfoRatio","ExitBounceDiff",
              "log_PageValues","log_ProdDuration",
              "log_ProdPages","log_TotalDur")

cor_rev <- sapply(feat_num, function(v)
  cor(df[[v]], df$Revenue_num, use = "complete.obs"))

cor_sorted <- sort(cor_rev)

dot_col <- ifelse(cor_sorted > 0, col_green, col_red)

par(mar = c(4, 10, 4, 2))
dotchart(cor_sorted,
         labels   = names(cor_sorted),
         pch      = 16,
         col      = dot_col,
         cex      = 0.85,
         main     = "6.5a  Feature Correlation with Revenue (Ranked)",
         xlab     = "Pearson r  with  Revenue",
         cex.main = 1.05)
abline(v = 0,    lty = 1, col = "gray70", lwd = 1)
abline(v =  0.3, lty = 2, col = col_green, lwd = 1.2)
abline(v = -0.3, lty = 2, col = col_red,   lwd = 1.2)
legend("bottomright",
       legend = c("Positive r", "Negative r", "|r| = 0.3 threshold"),
       col    = c(col_green, col_red, "gray50"),
       pch    = c(16, 16, NA), lty = c(NA, NA, 2),
       bty    = "n", cex = 0.8)
par(mar = c(5, 4, 4, 2))

# INTERPRETATION:
# log_PageValues and EngagementScore top the ranking (r>0.45), confirming log-transformation adds predictive signal.
# ExitBounceDiff and AdminRatio contribute near zero -- low standalone value. BounceRates is weakly negative as expected.


#  6.5b  Conversion rate by PageValueTier (bar chart) 
par(mar = c(4, 5, 4, 2))

pv_conv <- tapply(df$Revenue_num, df$PageValueTier, mean) * 100
pv_conv <- round(pv_conv, 1)
tier_cols <- c(col_light, col_acc, col_main, col_green)

bp_pv <- barplot(pv_conv,
                 col      = tier_cols,
                 border   = NA,
                 ylim     = c(0, max(pv_conv) * 1.3),
                 main     = "6.5b  Conversion Rate by PageValue Tier",
                 ylab     = "Conversion Rate (%)",
                 cex.main = 1.05,
                 cex.names = 0.9)
text(bp_pv, pv_conv + max(pv_conv) * 0.04,
     labels = paste0(pv_conv, "%"),
     font = 2, cex = 0.9, col = "#1A1A2E")
mtext("log transforms outperform raw vars",
      side = 1, line = 2, cex = 0.78, col = "gray40")
par(mar = c(5, 4, 4, 2))

# INTERPRETATION:
# Conversion rate increases consistently across tiers: Zero → Low → Medium → High.
# The High tier converts at over 50%, compared to under 5% for the Zero tier.


#  6.5c  Conversion rate by EngagementTier 
par(mar = c(4, 5, 4, 2))

eng_conv <- tapply(df$Revenue_num, df$EngagementTier, mean) * 100
eng_conv <- round(eng_conv, 1)

bp_eng <- barplot(eng_conv,
                  col      = c(col_light, col_acc, col_green),
                  border   = NA,
                  ylim     = c(0, max(eng_conv) * 1.35),
                  main     = "6.5c  Conversion Rate by Engagement Tier",
                  ylab     = "Conversion Rate (%)",
                  cex.main = 1.05,
                  cex.names = 0.9)
text(bp_eng, eng_conv + max(eng_conv) * 0.04,
     labels = paste0(eng_conv, "%"),
     font = 2, cex = 0.9, col = "#1A1A2E")
mtext("Zero tier: 3.9% conversion  |  High tier: 75.2%  --  19× gap",
      side = 1, line = 3, cex = 0.78, col = "gray40")
par(mar = c(5, 4, 4, 2))


# 6.5d  High-Intent flag: conversion lift vs baseline


par(mfrow = c(1, 2), mar = c(5, 4, 3, 2))

hi_tab  <- table(df$HighIntent)
hi_pct  <- round(prop.table(hi_tab) * 100, 1)
hi_labs <- paste0(names(hi_tab), "\n",
                  hi_pct, "%\n",
                  "(n=", format(as.integer(hi_tab),
                                big.mark = ","), ")")

pie(hi_tab,
    labels   = hi_labs,
    col      = c(col_light, col_green),
    border   = "white",
    main     = "6.5d-i  Session Composition",
    cex.main = 0.95,
    cex      = 0.80,        
    radius   = 0.78)          

# Right: barplot -- conversion rate
hi_conv  <- round(tapply(df$Revenue_num, df$HighIntent, mean) * 100, 1)
ylim_top <- max(hi_conv) * 1.45   

bp_hi <- barplot(hi_conv,
                 col       = c(col_light, col_green),
                 border    = NA,
                 ylim      = c(0, ylim_top),
                 main      = "6.5d-ii  Conversion Rate",
                 ylab      = "Conversion Rate (%)",
                 cex.main  = 0.95,
                 cex.names = 0.88)

text(bp_hi, hi_conv + ylim_top * 0.04,
     labels = paste0(hi_conv, "%"),
     font = 2, cex = 0.95, col = "#1A1A2E")


lift <- round(hi_conv["High-Intent"] / hi_conv["Other"], 1)

par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))

# INTERPRETATION:
# Only 13.5% of sessions qualify as High-Intent (left pie) -- a rare but extremely valuable segment. 
# These sessions convert at 9.6× the rate of all others (right bar). 




# 6.6  Feature engineering summary
new_feats <- c("AdminRatio","InfoRatio","ExitBounceDiff",
               "log_PageValues","log_ProdDuration",
               "log_TotalDur","log_ProdPages",
               "IsHolidaySeason","IsPeakMonth","Quarter",
               "PageValueTier","BounceCategory",
               "EngagementTier","HighIntent")

cat("\n=======================================================\n")
cat("  STEP 6 -- Feature Engineering Summary\n")
cat("=======================================================\n")
cat(sprintf("%-22s  %-12s  %-10s\n",
            "Feature", "Type", "Rationale"))
cat(rep("-", 50), "\n", sep="")
feat_info <- data.frame(
  Feature   = new_feats,
  Type      = c("ratio","ratio","difference",
                "log","log","log","log",
                "binary","binary","ordinal",
                "ordinal","ordinal","ordinal","binary"),
  Rationale = c("admin focus","info focus","mid-funnel signal",
                "normalize skew","normalize skew","normalize skew","normalize skew",
                "holiday effect","May sale traffic","quarter aggregation",
                "pagevalue tiers","bounce severity","engagement tiers",
                "composite intent flag"))
for (i in seq_len(nrow(feat_info))) {
  cat(sprintf("%-22s  %-12s  %s\n",
              feat_info$Feature[i],
              feat_info$Type[i],
              feat_info$Rationale[i]))
}
cat("=======================================================\n")
cat("Total features after engineering: df has", ncol(df), "columns\n")


# ---------------------------------------------------------------
# STEP 7 -- Descriptive Modeling
# ---------------------------------------------------------------


# Guard: ensure all required features exist
if (!"log_PageValues"   %in% names(df))
  df$log_PageValues   <- log(df$PageValues + 1)
if (!"log_ProdDuration" %in% names(df))
  df$log_ProdDuration <- log(df$ProductRelated_Duration + 1)
if (!"log_ProdPages"    %in% names(df))
  df$log_ProdPages    <- log(df$ProductRelated + 1)
if (!"Revenue_num"      %in% names(df))
  df$Revenue_num      <- as.numeric(df$Revenue == "Yes")
if (!"IsReturning"      %in% names(df))
  df$IsReturning <- factor(
    ifelse(df$VisitorType == "Returning_Visitor",
           "Returning", "Non-Returning"))



# 7.1  Logistic Regression 

#  Fit model 
m_logit <- glm(
  Revenue_num ~ log_PageValues + log_ProdDuration +
    log_ProdPages  + BounceRates + 
    IsReturning    + Weekend,
  data   = df,
  family = binomial(link = "logit"))

cat("=== 7.1 Logistic Regression Summary ===\n")
print(summary(m_logit))

#  Model fit metrics
mcf_r2 <- round(1 - m_logit$deviance / m_logit$null.deviance, 3)
cat("\nMcFadden R²:", mcf_r2,
    " -- values > 0.2 indicate good fit\n")
cat("AIC:", round(AIC(m_logit), 1), "\n")

#  Confusion matrix at 0.5 threshold
pred_prob  <- predict(m_logit, type = "response")
pred_class <- ifelse(pred_prob >= 0.5, 1, 0)
cm <- table(Predicted = pred_class, Actual = df$Revenue_num)
cat("\nConfusion matrix (threshold = 0.5):\n")
print(cm)
cat("Accuracy:", round(mean(pred_class == df$Revenue_num) * 100, 1), "%\n")
cat("Note: imbalanced classes -- accuracy alone is misleading\n")

#  Odds ratios (Wald CI) 
or_mat <- exp(cbind(OR = coef(m_logit),
                    confint.default(m_logit)))
or_mat <- round(or_mat, 3)
cat("\nOdds Ratios:\n")
print(or_mat)


# Visualization: forest-plot style OR chart
# 7.1  OR Forest Plot

# Work in log-odds space -- no exp/log roundtrip, no Inf risk
coef_vec <- coef(m_logit)[-1]                    # drop intercept
se_vec   <- sqrt(diag(vcov(m_logit)))[-1]
log_or   <- coef_vec
log_lo   <- coef_vec - 1.96 * se_vec
log_hi   <- coef_vec + 1.96 * se_vec

# Sort by effect size
ord    <- order(log_or)
log_or <- log_or[ord]
log_lo <- log_lo[ord]
log_hi <- log_hi[ord]

dot_col <- ifelse(log_or > 0, col_green, col_red)

m_logit_raw <- glm(
  Revenue_num ~ log_PageValues + log_ProdDuration +
    log_ProdPages  + BounceRates      +
    IsReturning    + Weekend,
  data = df, family = binomial(link = "logit"))

coef_raw <- coef(m_logit_raw)[-1]
se_raw   <- sqrt(diag(vcov(m_logit_raw)))[-1]
lo_raw   <- coef_raw - 1.96 * se_raw
hi_raw   <- coef_raw + 1.96 * se_raw
ord_raw  <- order(coef_raw)

par(mar = c(5, 11, 4, 3))
plot(coef_raw[ord_raw], seq_along(coef_raw),
     xlim = c(min(lo_raw[ord_raw]) - 0.1,
              max(hi_raw[ord_raw]) + 0.1),
     ylim = c(0.5, length(coef_raw) + 0.5),
     pch = 16, cex = 1.3,
     col = ifelse(coef_raw[ord_raw] > 0, col_green, col_red),
     yaxt = "n",
     main = "7.1a  Logistic Regression: Log Odds Ratios",
     xlab = "log(Odds Ratio)", ylab = "", cex.main = 1.0)
axis(2, at = seq_along(coef_raw),
     labels = names(coef_raw[ord_raw]),
     las = 1, cex.axis = 0.85, tick = FALSE)
segments(lo_raw[ord_raw], seq_along(coef_raw),
         hi_raw[ord_raw], seq_along(coef_raw),
         col = adjustcolor(
           ifelse(coef_raw[ord_raw] > 0, col_green, col_red), 0.5),
         lwd = 2.2)
abline(v = 0, lty = 2, col = "gray50", lwd = 1.5)
par(mar = c(5, 4, 4, 2))

# 7.1b Predicted Probability by Actual Outcome
par(mar = c(4, 5, 4, 2))
boxplot(pred_prob ~ df$Revenue,
        col     = c(col_light, col_green),
        border  = c(col_main, col_green),
        names   = c("No Purchase", "Purchase"),
        main    = "7.1b  Predicted Probability by Actual Outcome",
        ylab    = "Predicted Purchase Probability",
        outline = TRUE, outpch = 16, outcex = 0.25,
        outcol  = adjustcolor(col_acc, 0.3),
        cex.main = 1.05)
abline(h = 0.5, lty = 2, col = col_red, lwd = 1.5)
mtext("Good separation = purchase group shifted clearly above 0.5",
      side = 1, line = 3, cex = 0.78, col = "gray40")
par(mar = c(5, 4, 4, 2))


# 7.2  Linear Regression 
#  Fit model
m_lm <- lm(
  log_ProdDuration ~ log_ProdPages + BounceRates +
    ExitRates     + IsReturning  + log_PageValues,
  data = df)

cat("\n=== 7.2 Linear Regression Summary ===\n")
print(summary(m_lm))
cat("Adjusted R²:", round(summary(m_lm)$adj.r.squared, 3), "\n")

# Standardized coefficients (beta weights)
# Scale all numeric predictors for fair comparison
df_scaled <- df
num_preds <- c("log_ProdPages","BounceRates",
               "ExitRates","log_PageValues")
for (v in num_preds)
  df_scaled[[v]] <- scale(df[[v]])

m_lm_std <- lm(log_ProdDuration ~ log_ProdPages + BounceRates +
                 ExitRates + IsReturning +
                 log_PageValues,
               data = df_scaled)
std_coef <- coef(m_lm_std)[-1]   # drop intercept

#  Visualization: standardized coefficient barplot 
par(mar = c(4, 10, 4, 2))
std_ord  <- sort(std_coef)
bar_col  <- ifelse(std_ord > 0, col_main, col_red)

bp_lm <- barplot(std_ord,
                 horiz    = TRUE,
                 col      = bar_col,
                 border   = NA,
                 xlim     = c(min(std_ord) - 0.05,
                              max(std_ord) + 0.1),
                 main     = "7.2  Linear Regression: Standardized Coefficients\n(Response: log Product Duration)",
                 xlab     = "Standardized β",
                 las      = 1,
                 cex.names = 0.88,
                 cex.main  = 0.98)
abline(v = 0, lty = 1, col = "gray60", lwd = 1.2)
text(std_ord + ifelse(std_ord >= 0, 0.008, -0.008),
     bp_lm,
     labels = round(std_ord, 3),
     cex = 0.78, col = "#1A1A2E",
     adj = ifelse(std_ord >= 0, 0, 1))
par(mar = c(5, 4, 4, 2))

# Residual diagnostics 
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
plot(m_lm$fitted.values, m_lm$residuals,
     pch = 16, cex = 0.3,
     col = adjustcolor(col_main, 0.2),
     main = "7.2b  Residuals vs Fitted",
     xlab = "Fitted values",
     ylab = "Residuals",
     cex.main = 0.95)
abline(h = 0, col = col_red, lty = 2, lwd = 1.5)

qqnorm(m_lm$residuals,
       pch = 16, cex = 0.3,
       col = adjustcolor(col_main, 0.3),
       main = "7.2c  Normal Q-Q Plot",
       cex.main = 0.95)
qqline(m_lm$residuals, col = col_red, lwd = 1.8)
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))

# INTERPRETATION:
# log_ProdPages has the largest positive β: browsing more product pages is the strongest driver of time-on-site.
# BounceRates strongly negative: high-bounce visitors exit before accumulating duration. IsReturning positive -- familiar visitors navigate deeper and spend longer.
# Adj R² confirms moderate explanatory power; residuals show mild heteroscedasticity typical of log-duration data.



# 7.3  Two-way ANOVA -- PageValues ~ VisitorType × Weekend 

#  Fit two-way ANOVA with interaction
m_aov <- aov(log(PageValues + 1) ~ VisitorType * Weekend,
             data = df)

cat("\n=== 7.3 Two-Way ANOVA Summary ===\n")
print(summary(m_aov))

#  Interaction plot 
par(mar = c(4, 5, 4, 2))
interaction.plot(
  x.factor     = df$Weekend,
  trace.factor = df$VisitorType,
  response     = log(df$PageValues + 1),
  fun          = mean,
  col          = c(col_main, col_acc, col_pur),
  lwd          = 2.5,
  lty          = c(1, 2, 3),
  pch          = c(16, 17, 15),
  cex          = 1.2,
  type         = "b",
  main         = "7.3a  Interaction: VisitorType × Weekend on PageValues",
  xlab         = "Day Type",
  ylab         = "Mean log(PageValues + 1)",
  trace.label  = "Visitor Type",
  cex.main     = 1.05)

aov_sum  <- summary(m_aov)[[1]]
p_vtype  <- round(aov_sum["VisitorType",        "Pr(>F)"], 4)
p_wknd   <- round(aov_sum["Weekend",            "Pr(>F)"], 4)
p_inter  <- round(aov_sum["VisitorType:Weekend","Pr(>F)"], 4)

mtext(paste0("VisitorType p=", p_vtype,
             "  |  Weekend p=", p_wknd,
             "  |  Interaction p=", p_inter),
      side = 1, line = 3, cex = 0.8, col = col_red)
par(mar = c(5, 4, 4, 2))


# 7.3b  ANOVA residuals check
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
plot(fitted(m_aov), residuals(m_aov),
     pch = 16, cex = 0.3,
     col = adjustcolor(col_main, 0.2),
     main = "7.3b  Residuals vs Fitted",
     xlab = "Fitted", ylab = "Residuals",
     cex.main = 0.95)
abline(h = 0, col = col_red, lty = 2, lwd = 1.5)

qqnorm(residuals(m_aov), pch = 16, cex = 0.3,
       col = adjustcolor(col_main, 0.3),
       main = "7.3c  Normal Q-Q",
       cex.main = 0.95)
qqline(residuals(m_aov), col = col_red, lwd = 1.8)
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))

#  Tukey post-hoc (VisitorType main effect) 
cat("\n-- Tukey HSD: VisitorType --\n")
print(TukeyHSD(m_aov, "VisitorType"))

# INTERPRETATION:
# Both VisitorType and Weekend show significant main effects on log(PageValues + 1).
# The interaction p-value indicates whether the weekend effect differs by visitor type.
# Tukey HSD identifies which specific visitor-type pairs differ significantly.


# 7.4  K-means Clustering 
# Elbow plot: choose optimal k 
set.seed(2025)
km_vars <- c("log_PageValues","log_ProdDuration",
             "BounceRates","ProductRatio")

km_data <- scale(df[, km_vars])   # z-score normalization

wss <- sapply(1:8, function(k) {
  kmeans(km_data, centers = k, nstart = 20,
         iter.max = 100)$tot.withinss
})

par(mar = c(4, 5, 4, 2))
plot(1:8, wss,
     type = "b",
     pch  = 16, cex = 1.3,
     col  = col_main,
     lwd  = 2,
     main = "7.4a  K-means Elbow Plot",
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster SS",
     cex.main = 1.05)
# Highlight chosen k
abline(v = 3, lty = 2, col = col_red, lwd = 1.8)
text(3.1, max(wss) * 0.95,
     "k = 3 chosen", col = col_red, cex = 0.85, adj = 0)
par(mar = c(5, 4, 4, 2))

#  Fit k = 3 
set.seed(2025)
km3 <- kmeans(km_data, centers = 3, nstart = 30, iter.max = 200)
df$Cluster <- factor(km3$cluster)

cat("\n=== 7.4 K-means (k=3) Cluster Sizes ===\n")
print(table(df$Cluster))

#  Cluster profiles: mean of key variables 
cluster_profile <- aggregate(
  cbind(log_PageValues, log_ProdDuration,
        BounceRates, ProductRatio, Revenue_num) ~ Cluster,
  data = df, FUN = mean)
cluster_profile[, -1] <- round(cluster_profile[, -1], 3)
cat("\nCluster mean profiles:\n")
print(cluster_profile)

#  Visualization: scatter colored by cluster 
par(mar = c(5, 5, 4, 2))

clust_col <- c(col_light, col_acc, col_green)
pt_col_km <- clust_col[as.integer(df$Cluster)]

plot(df$log_ProdDuration,
     df$log_PageValues,
     col  = adjustcolor(pt_col_km, 0.25),
     pch  = 16, cex = 0.35,
     main = "7.4b  Session Clusters: Duration vs PageValues",
     xlab = "log(ProductRelated_Duration + 1)",
     ylab = "log(PageValues + 1)",
     cex.main = 1.05)

# Cluster centroids 
cent_orig <- km3$centers %*% diag(attr(km_data,"scaled:scale")) +
  rep(attr(km_data,"scaled:center"), each = nrow(km3$centers))
colnames(cent_orig) <- km_vars

points(cent_orig[, "log_ProdDuration"],
       cent_orig[, "log_PageValues"],
       pch = 23, bg = clust_col, col = "#1A1A2E",
       cex = 2.5, lwd = 1.5)

conv_by_clust <- round(
  tapply(df$Revenue_num, df$Cluster, mean) * 100, 1)

legend("topleft",
       legend = paste0("Cluster ", 1:3,
                       "  (conv=", conv_by_clust, "%)"),
       fill   = clust_col,
       border = NA, bty = "n", cex = 0.85)
par(mar = c(5, 4, 4, 2))

#  Conversion rate by cluster barplot 
par(mar = c(4, 5, 4, 2))

bp_km <- barplot(conv_by_clust,
                 col      = clust_col,
                 border   = NA,
                 ylim     = c(0, max(conv_by_clust) * 1.35),
                 main     = "7.4c  Conversion Rate by Cluster",
                 ylab     = "Conversion Rate (%)",
                 names.arg = paste0("Cluster ", 1:3),
                 cex.main  = 1.05)
text(bp_km, conv_by_clust + max(conv_by_clust) * 0.05,
     labels = paste0(conv_by_clust, "%"),
     font = 2, cex = 0.95, col = "#1A1A2E")
par(mar = c(5, 4, 4, 2))

# INTERPRETATION:
# Three behaviorally distinct segments emerge:
# Cluster with highest conv rate = "Ready Buyers":high PageValues, long product duration, low bounce.
# Mid-conv cluster = "Active Browsers":browsing deeply but not reaching high-value pages yet.
# Low-conv cluster = "Casual / Bounced": short duration, zero PageValues, high bounce --   acquisition traffic that does not engage.
# RQ3 implication: traffic type routing (5.7) likely maps onto these clusters -- premium traffic types feed the Ready Buyers cluster.



# 7.5  Model comparison summary
cat("\n=======================================================\n")
cat("  STEP 7 -- Modeling Summary\n")
cat("=======================================================\n")
cat(sprintf("%-20s %-35s %-15s\n",
            "Model", "Key Finding", "Metric"))
cat(rep("-", 72), "\n", sep="")
cat(sprintf("%-20s %-35s %-15s\n",
            "Logistic Reg.",
            "log_PageValues top predictor of Revenue",
            paste0("McFadden R²=", mcf_r2)))
cat(sprintf("%-20s %-35s %-15s\n",
            "Linear Reg.",
            "log_ProdPages drives product duration",
            paste0("Adj R²=", round(summary(m_lm)$adj.r.squared, 3))))
cat(sprintf("%-20s %-35s %-15s\n",
            "Two-way ANOVA",
            "VisitorType & Weekend both significant",
            paste0("Interaction p=", p_inter)))
cat(sprintf("%-20s %-35s %-15s\n",
            "K-means (k=3)",
            "3 segments: Buyer/Browser/Casual",
            paste0("Sizes: ", paste(table(df$Cluster), collapse="/"))))
cat("=======================================================\n")


# ---------------------------------------------------------------
# STEP 8 -- Visualization Redesign
# ---------------------------------------------------------------
# 8.1 original Step 5.2
par(mar = c(5, 5, 4, 2))

pt_col <- ifelse(df$Revenue == "Yes",
                 adjustcolor(col_green, alpha.f = 0.5),
                 adjustcolor(col_main,  alpha.f = 0.12))

plot(log(df$ProductRelated_Duration + 1),
     log(df$PageValues + 1),
     col      = pt_col,
     pch      = 16,
     cex      = 0.4,
     main     = "5.2  Product Duration vs Page Values by Purchase Outcome",
     xlab     = "log(ProductRelated_Duration + 1)",
     ylab     = "log(PageValues + 1)",
     cex.main = 1.05)

for (grp in c("No", "Yes")) {
  sub <- df[df$Revenue == grp, ]
  fit <- lm(log(PageValues + 1) ~ log(ProductRelated_Duration + 1), data = sub)
  abline(fit,
         col = ifelse(grp == "Yes", col_green, col_main),
         lwd = 2.2, lty = ifelse(grp == "Yes", 1, 2))
}

legend("topleft",
       legend = c("Purchase (Yes)", "No Purchase"),
       col    = c(col_green, col_main),
       pch    = 16, lty = c(1, 2), lwd = 2,
       bty    = "n", cex = 0.85)
mtext("Buyers cluster top-right: long duration AND high-value pages co-occur for converters",
      side = 1, line = 3.5, cex = 0.78, col = "gray40")

par(mar = c(5, 4, 4, 2))
# PROBLEMS:
# 1. col_green (purchase) vs col_main (no purchase) both cool-toned; at low alpha the two groups visually merge into one gray cloud.
# 2. xlab sits at line ~2.5 and mtext at line 3.5 -- only 1 line gap, labels collide at the bottom edge.



# AFTER  
par(mar = c(7, 5, 4, 2))   

# Purchase = col_acc (orange): warm tone, high contrast against
# the navy no-purchase cloud and white background.
pt_col <- ifelse(df$Revenue == "Yes",
                 adjustcolor(col_acc,  alpha.f = 0.65),
                 adjustcolor(col_main, alpha.f = 0.12))

plot(log(df$ProductRelated_Duration + 1),
     log(df$PageValues + 1),
     col      = pt_col,
     pch      = 16,
     cex      = 0.4,
     main     = "5.2  Product Duration vs Page Values by Purchase Outcome",
     xlab     = "",            # axis label moved to mtext for spacing
     ylab     = "log(PageValues + 1)",
     cex.main = 1.05)

for (grp in c("No", "Yes")) {
  sub <- df[df$Revenue == grp, ]
  fit <- lm(log(PageValues + 1) ~ log(ProductRelated_Duration + 1),
            data = sub)
  abline(fit,
         col = ifelse(grp == "Yes", col_acc,  col_main),
         lwd = 2.2,
         lty = ifelse(grp == "Yes", 1, 2))
}

legend("topleft",
       legend = c("Purchase (Yes)", "No Purchase"),
       col    = c(col_acc, col_main),
       pch    = 16, lty = c(1, 2), lwd = 2,
       bty    = "n", cex = 0.85)

# Two-line annotation with deliberate spacing
mtext("log(ProductRelated_Duration + 1)",
      side = 1, line = 3,   cex = 0.9,  col = "gray20")
mtext("Buyers cluster top-right: long duration AND high-value pages co-occur for converters",
      side = 1, line = 5,   cex = 0.78, col = "gray40")

par(mar = c(5, 4, 4, 2))

# DESIGN DECISIONS:
# 1. COLOR CONTRAST: col_acc (orange #E07B39) for purchase vs col_main (navy #2B5F8E) for no-purchase -- warm/cool opponent colors ensure the minority class (buyers, 15.5%) is immediately visible despite being outnumbered 5:1.
# 2. TEXT SPACING: xlab removed from plot(), placed at mtext  line=3; annotation at line=5. Two full lines of gap eliminates overlap regardless of output device size.



# 8.2  original Step 5.9
plot_mat <- data.frame(
  PageVal  = log(df$PageValues + 1),
  ProdPg   = log(df$ProductRelated + 1),
  ProdDur  = log(df$ProductRelated_Duration + 1),
  Bounce   = df$BounceRates,
  Exit     = df$ExitRates)

pt_col9 <- ifelse(df$Revenue == "Yes",
                  adjustcolor(col_green, 0.45),
                  adjustcolor(col_main,  0.10))

pairs(plot_mat,
      col  = pt_col9,
      pch  = 16,
      cex  = 0.3,
      gap  = 0.25,
      main = "5.9  Scatter Matrix: Key Engagement Variables  (green=Purchase)",
      cex.main = 0.95,
      upper.panel = function(x, y, ...) {
        r <- round(cor(x, y), 2)
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        text(0.5, 0.5, paste0("r=", r),
             cex  = 1.0, font = 2,
             col  = ifelse(abs(r) > 0.4, col_red,
                           ifelse(abs(r) > 0.2, col_acc, "gray60")))
      })
# PROBLEMS:
# 1. col_green (alpha=0.45) vs col_main (alpha=0.10): both desaturate to similar cool gray -- groups visually merge.
# 2. Three r-value colors (red / orange / gray60) adds clutter; the orange threshold at |r|>0.2 flags almost every cell.



# AFTER  
pt_col9 <- ifelse(df$Revenue == "Yes",
                  adjustcolor(col_acc,  alpha.f = 0.55),   # orange -- purchase
                  adjustcolor(col_main, alpha.f = 0.10))   # faded navy -- no purchase

pairs(plot_mat,
      col  = pt_col9,
      pch  = 16,
      cex  = 0.3,
      gap  = 0.25,
      main = "5.9  Scatter Matrix: Key Engagement Variables  (orange=Purchase)",
      cex.main = 0.95,
      upper.panel = function(x, y, ...) {
        r <- round(cor(x, y), 2)
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        text(0.5, 0.5,
             paste0("r=", r),
             cex  = 1.0,
             font = 2,
             col  = ifelse(abs(r) >= 0.4, col_red, "gray55"))
      })

# DESIGN DECISIONS:
# 1. WARM / COOL CONTRAST: col_acc (orange #E07B39) for purchase vs col_main (navy #2B5F8E) for no-purchase.
# Opponent hues remain distinct even at low alpha -- minority class (15.5% buyers) is immediately visible against the majority cloud.


# original Step 6.5a
feat_num <- c("BounceRates","ExitRates","PageValues",
              "ProductRelated","ProductRelated_Duration",
              "ProductRatio","EngagementScore","TotalPages",
              "TotalDuration","AvgDuration",
              "AdminRatio","InfoRatio","ExitBounceDiff",
              "log_PageValues","log_ProdDuration",
              "log_ProdPages","log_TotalDur")

cor_rev    <- sapply(feat_num, function(v)
  cor(df[[v]], df$Revenue_num, use = "complete.obs"))
cor_sorted <- sort(cor_rev)
dot_col    <- ifelse(cor_sorted > 0, col_green, col_red)

par(mar = c(4, 10, 4, 2))
dotchart(cor_sorted,
         labels   = names(cor_sorted),
         pch      = 16,
         col      = dot_col,
         cex      = 0.85,
         main     = "6.5a  Feature Correlation with Revenue (Ranked)",
         xlab     = "Pearson r  with  Revenue",
         cex.main = 1.05)
abline(v =  0,   lty = 1, col = "gray70",  lwd = 1.0)
abline(v =  0.3, lty = 2, col = col_green, lwd = 1.2)
abline(v = -0.3, lty = 2, col = col_red,   lwd = 1.2)   # BUG: outside data range
legend("bottomright",
       legend = c("Positive r", "Negative r", "|r| = 0.3 threshold"),
       col    = c(col_green, col_red, "gray50"),
       pch    = c(16, 16, NA), lty = c(NA, NA, 2),
       bty    = "n", cex = 0.8)
par(mar = c(5, 4, 4, 2))
# PROBLEM:
# No feature reaches r < -0.3 (min ≈ -0.24 for ExitRates).
# dotchart auto-fits xlim to data, so abline(v = -0.3) falls outside the plot box and renders in the left margin -- visually misleading, implying a threshold that no feature approaches.



# AFTER 
par(mar = c(4, 12, 4, 2))

x_lo <- min(cor_sorted) - 0.05
x_hi <- max(cor_sorted) + 0.05

dotchart(cor_sorted,
         labels   = names(cor_sorted),
         pch      = 16,
         col      = dot_col,
         cex      = 0.85,
         xlim     = c(x_lo, x_hi),
         main     = "6.5a  Feature Correlation with Revenue (Ranked)",
         xlab     = "Pearson r  with  Revenue",
         cex.main = 1.05)

abline(v = 0,   lty = 1, col = "gray60",  lwd = 1.2)
abline(v = 0.3, lty = 2, col = col_green, lwd = 1.5)

legend("bottomright",
       legend = c("Positive r", "Negative r", "|r| = 0.3 threshold"),
       col    = c(col_green, col_red, col_green),
       pch    = c(16, 16, NA), lty = c(NA, NA, 2),
       bty    = "n", cex = 0.8)

par(mar = c(5, 4, 4, 2))

# DESIGN DECISIONS:
# 1. REMOVED -0.3 LINE: min correlation is ≈ -0.24 (ExitRates); drawing a threshold at -0.3 implies features exist beyond it -- they do not. Removing it is more honest.


# 8.3 original Step 7.1a
m_logit_raw <- glm(
  Revenue_num ~ log_PageValues + log_ProdDuration +
    log_ProdPages  + BounceRates      +
    IsReturning    + Weekend,
  data = df, family = binomial(link = "logit"))

coef_raw <- coef(m_logit_raw)[-1]
se_raw   <- sqrt(diag(vcov(m_logit_raw)))[-1]
lo_raw   <- coef_raw - 1.96 * se_raw
hi_raw   <- coef_raw + 1.96 * se_raw
ord_raw  <- order(coef_raw)

par(mar = c(5, 11, 4, 3))
plot(coef_raw[ord_raw], seq_along(coef_raw),
     xlim = c(min(lo_raw[ord_raw]) - 0.1,
              max(hi_raw[ord_raw]) + 0.1),
     ylim = c(0.5, length(coef_raw) + 0.5),
     pch = 16, cex = 1.3,
     col = ifelse(coef_raw[ord_raw] > 0, col_green, col_red),
     yaxt = "n",
     main = "BEFORE: Log Odds Ratios (unstandardized)",
     xlab = "log(Odds Ratio)", ylab = "", cex.main = 1.0)
axis(2, at = seq_along(coef_raw),
     labels = names(coef_raw[ord_raw]),
     las = 1, cex.axis = 0.85, tick = FALSE)
segments(lo_raw[ord_raw], seq_along(coef_raw),
         hi_raw[ord_raw], seq_along(coef_raw),
         col = adjustcolor(
           ifelse(coef_raw[ord_raw] > 0, col_green, col_red), 0.5),
         lwd = 2.2)
abline(v = 0, lty = 2, col = "gray50", lwd = 1.5)
par(mar = c(5, 4, 4, 2))
# PROBLEMS:
# BounceRates is on [0, 0.2] scale -- 1-unit change = 0% to 100%
# bounce rate. Log-OR ≈ -15: dot is off-screen left.
# All other CI whiskers invisible by comparison.
# Chart is unreadable and misleading.



# AFTER  

df_std    <- df
cont_vars <- c("log_PageValues","log_ProdDuration",
               "log_ProdPages","BounceRates")
for (v in cont_vars)
  df_std[[v]] <- as.numeric(scale(df[[v]]))

m_logit_std <- glm(
  Revenue_num ~ log_PageValues + log_ProdDuration +
    log_ProdPages  + BounceRates      +
    IsReturning    + Weekend,
  data   = df_std,
  family = binomial(link = "logit"))

cat("=== Logistic Regression (standardized predictors -- for plot scaling only) ===\n")
print(summary(m_logit_std))
cat("McFadden R²:",
    round(1 - m_logit_std$deviance / m_logit_std$null.deviance, 3), "\n")

#  Forest plot
coef_vec <- coef(m_logit_std)[-1]
se_vec   <- sqrt(diag(vcov(m_logit_std)))[-1]


log_or   <- coef_vec
log_lo   <- coef_vec - 1.96 * se_vec
log_hi   <- coef_vec + 1.96 * se_vec

ord    <- order(log_or)
log_or <- log_or[ord]
log_lo <- log_lo[ord]
log_hi <- log_hi[ord]

clean_names <- names(log_or)
clean_names <- gsub("WeekendWeekend",       "Weekend",     clean_names)
clean_names <- gsub("IsReturningReturning", "IsReturning", clean_names)

dot_col <- ifelse(log_or > 0, col_green, col_red)

par(mar = c(5, 11, 4, 3))
plot(log_or, seq_along(log_or),
     xlim = c(min(log_lo) - 0.1, max(log_hi) + 0.1),
     ylim = c(0.5, length(log_or) + 0.5),
     pch  = 16, cex  = 1.3,
     col  = dot_col,
     yaxt = "n",
     main = "7.1a  Logistic Regression: Log Odds Ratios",
     xlab = "",
     ylab = "",
     cex.main = 1.05)

axis(2, at = seq_along(log_or), labels = clean_names,
     las = 1, cex.axis = 0.88, tick = FALSE)

segments(log_lo, seq_along(log_or),
         log_hi, seq_along(log_or),
         col = adjustcolor(dot_col, 0.5), lwd = 2.2)

abline(v = 0, lty = 2, col = "gray50", lwd = 1.5)

legend("topleft",
       legend = c("Increases purchase prob.",
                  "Decreases purchase prob."),
       col = c(col_green, col_red), pch = 16,
       bty = "n", cex = 0.85)

mtext("log(Odds Ratio)  per 1-SD change in predictor",
      side = 1, line = 2.5, cex = 0.9,  col = "gray20")
mtext("Predictors standardized (mean=0, SD=1) for comparability",
      side = 1, line = 4,   cex = 0.78, col = "gray40")

par(mar = c(5, 4, 4, 2))

# DESIGN DECISIONS:
# 1. AXIS SCALE (visualization fix): BounceRates is on a [0, 0.2] scale, so its
#    raw log-OR ≈ -15 (a 1-unit = 0%→100% bounce rate change). This pushes the dot
#    off-screen and makes all other CIs invisible. Standardizing predictors to SD=1
#    units before plotting collapses all coefficients onto a common visual axis --
#    this is a display choice, not a change to the underlying model (m_logit).
# 2. ALL CIs NOW VISIBLE: with a common scale, whisker lengths reflect genuine
#    statistical uncertainty rather than unit differences. Narrow CI = precise
#    estimate; wide CI = uncertain. The underlying inference is unchanged.


# 8.4 original Step 7.1b
par(mar = c(4, 5, 4, 2))
boxplot(pred_prob ~ df$Revenue,
        col     = c(col_light, col_green),
        border  = c(col_main, col_green),
        names   = c("No Purchase", "Purchase"),
        main    = "7.1b  Predicted Probability by Actual Outcome",
        ylab    = "Predicted Purchase Probability",
        outline = TRUE, outpch = 16, outcex = 0.25,
        outcol  = adjustcolor(col_acc, 0.3),
        cex.main = 1.05)
abline(h = 0.5, lty = 2, col = col_red, lwd = 1.5)
mtext("Good separation = purchase group shifted clearly above 0.5",
      side = 1, line = 3, cex = 0.78, col = "gray40")
par(mar = c(5, 4, 4, 2))

# PROBLEM:
# boxplot() with formula syntax automatically uses the right-hand side variable name ("df$Revenue") as xlab. 



# AFTER 
par(mar = c(4, 5, 4, 2))
boxplot(pred_prob ~ df$Revenue,
        col     = c(col_light, col_green),
        border  = c(col_main, col_green),
        names   = c("No Purchase", "Purchase"),
        main    = "7.1b  Predicted Probability by Actual Outcome",
        ylab    = "Predicted Purchase Probability",
        xlab    = "",            # suppress auto "df$Revenue" label
        outline = TRUE, outpch = 16, outcex = 0.25,
        outcol  = adjustcolor(col_acc, 0.3),
        cex.main = 1.05)
abline(h = 0.5, lty = 2, col = col_red, lwd = 1.5)
mtext("Good separation = purchase group shifted clearly above 0.5",
      side = 1, line = 3, cex = 0.78, col = "gray40")
par(mar = c(5, 4, 4, 2))

# DESIGN DECISION:
# xlab = "" suppresses the formula-derived label "df$Revenue".


# 8.5 original Step 7.3a

par(mar = c(4, 5, 4, 2))
interaction.plot(
  x.factor     = df$Weekend,
  trace.factor = df$VisitorType,
  response     = log(df$PageValues + 1),
  fun          = mean,
  col          = c(col_main, col_acc, col_pur),   # navy / orange / purple
  lwd          = 2.5,
  lty          = c(1, 2, 3),
  pch          = c(16, 17, 15),
  cex          = 1.2,
  type         = "b",
  main         = "7.3a  Interaction: VisitorType × Weekend on PageValues",
  xlab         = "Day Type",
  ylab         = "Mean log(PageValues + 1)",
  trace.label  = "Visitor Type",
  cex.main     = 1.05)

aov_sum <- summary(m_aov)[[1]]
p_vtype <- round(aov_sum["VisitorType",         "Pr(>F)"], 4)
p_wknd  <- round(aov_sum["Weekend",             "Pr(>F)"], 4)
p_inter <- round(aov_sum["VisitorType:Weekend", "Pr(>F)"], 4)

mtext(paste0("VisitorType p=", p_vtype,
             "  |  Weekend p=", p_wknd,
             "  |  Interaction p=", p_inter),
      side = 1, line = 3, cex = 0.8, col = col_red)
par(mar = c(5, 4, 4, 2))
# PROBLEMS:
# 1. col_pur (#7D5BA6) and col_main (#2B5F8E) are both cool-toned blue-purple -- indistinguishable at reduced size or greyscale.
# 2. xlab sits at line ~2 and mtext at line 3 -- one line gap causes text overlap at the bottom edge.



# AFTER  
par(mar = c(6, 5, 4, 2))

interaction.plot(
  x.factor     = df$Weekend,
  trace.factor = df$VisitorType,
  response     = log(df$PageValues + 1),
  fun          = mean,
  col          = c(col_main, col_acc, col_green),  # navy / orange / green
  lwd          = 2.5,
  lty          = c(1, 2, 3),
  pch          = c(16, 17, 15),
  cex          = 1.2,
  type         = "b",
  main         = "7.3a  Interaction: VisitorType × Weekend on PageValues",
  xlab         = "",           # suppressed -- placed via mtext
  ylab         = "Mean log(PageValues + 1)",
  trace.label  = "Visitor Type",
  cex.main     = 1.05)

mtext("Day Type",
      side = 1, line = 2.5, cex = 0.9,  col = "gray20")
mtext(paste0("VisitorType p=", p_vtype,
             "  |  Weekend p=", p_wknd,
             "  |  Interaction p=", p_inter),
      side = 1, line = 4.2, cex = 0.78, col = col_red)

par(mar = c(5, 4, 4, 2))

# DESIGN DECISIONS:
# 1. COLOR DISTINCTNESS
# 2. TEXT SPACING

# 8.6  Step 5.10 

par(mar = c(5, 5, 4, 5))

pv_heat <- tapply(df$PageValues,
                  list(df$Region, df$VisitorType),
                  mean)
pv_heat <- round(pv_heat, 1)

# Keep only Returning and New (Other is negligible)
pv_heat <- pv_heat[, c("Returning_Visitor", "New_Visitor")]

# Normalize for color mapping
pv_norm <- (pv_heat - min(pv_heat, na.rm = TRUE)) /
  (max(pv_heat, na.rm = TRUE) - min(pv_heat, na.rm = TRUE))

heat_col <- colorRampPalette(c(col_light, col_main, col_red))(100)

image(1:ncol(pv_norm),
      1:nrow(pv_norm),
      t(pv_norm),           # image() expects col×row matrix
      col    = heat_col,
      axes   = FALSE,
      main   = "5.10  Avg PageValues Heatmap: Region × Visitor Type (RQ3)",
      xlab   = "",
      ylab   = "Region",
      cex.main = 1.0)

axis(1, at = 1:2,
     labels = c("Returning", "New Visitor"),
     tick = FALSE, cex.axis = 0.9)
axis(2, at = 1:nrow(pv_heat),
     labels = rownames(pv_heat),
     las = 1, cex.axis = 0.82, tick = FALSE)
mtext("Visitor Type", side = 1, line = 2.5, cex = 0.9, col = "gray20")
box()

# Value labels inside cells
for (i in 1:ncol(pv_heat)) {
  for (j in 1:nrow(pv_heat)) {
    text(i, j, pv_heat[j, i],
         cex = 0.78, font = 2,
         col = ifelse(pv_norm[j, i] > 0.55, "white", "#1A1A2E"))
  }
}

par(mar = c(5, 4, 4, 2))

# PROBLEMS:
# 1. base R image() requires manual axis, box(), and a nested for-loop for cell labels --
#    excessive boilerplate for a standard heatmap.
# 2. No built-in color legend (colorbar); the gradient meaning is implicit.
# 3. t() transpose required because image() reads matrix column-major -- easy to misread.



# AFTER: ggplot2 geom_tile() heatmap

pv_tile <- aggregate(PageValues ~ Region + VisitorType, data = df,
                     FUN = mean)
pv_tile <- pv_tile[pv_tile$VisitorType %in%
                     c("Returning_Visitor", "New_Visitor"), ]
pv_tile$VisitorType <- ifelse(pv_tile$VisitorType == "Returning_Visitor",
                              "Returning", "New Visitor")
pv_tile$PageValues  <- round(pv_tile$PageValues, 1)

med_pv <- median(pv_tile$PageValues)

ggplot(pv_tile, aes(x = VisitorType, y = Region, fill = PageValues)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = PageValues,
                color  = ifelse(PageValues > med_pv * 1.4,
                                "white", "#1A1A2E")),
            size = 3.2, fontface = "bold", show.legend = FALSE) +
  scale_fill_gradientn(
    colors = c(col_light, col_main, col_red),
    name   = "Avg\nPageValues") +
  scale_color_identity() +
  labs(title    = "5.10  Avg PageValues: Region × Visitor Type (RQ3)",
       subtitle = "Warm cells = new visitors arriving with stronger purchase intent by region",
       x = "Visitor Type", y = "Region") +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(color = "gray50", size = 9),
        panel.grid    = element_blank(),
        axis.text     = element_text(size = 10))

