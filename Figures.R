# === GBD === ----
# Figure 1 ----
load("EPI.rda") # COPD data
load("GBD_db.rda")
load("EPI_Sugar.rda")
load("EPI_Depression.rda")
#
# ... Figure 1A, 1B ----
# Deaths trending
locs <- grep("Global|SDI", unique(EPI$location), value = T)
pdf("GBD_COPD_YLDs_trend_Number.pdf", width = 8, height = 5)
trendGBD(EPI[EPI$location %in% locs, ], group = "location", ribbon = F, Measure = "YLDs", Metric = "Number", Sex = "Male") +
    ylim(1500, 8.2e6) + ggtitle("Male")
trendGBD(EPI[EPI$location %in% locs, ], group = "location", ribbon = F, Measure = "YLDs", Metric = "Number", Sex = "Female") +
    ylim(1500, 8.2e6) + ggtitle("Female")
dev.off()

# ... Figure 1C, 1D ----
pdf("GBD_Sugar_YLDs_trend_Number.pdf", width = 8, height = 5)
trendGBD(EPI_Sugar[EPI_Sugar$location %in% locs, ], group = "location", ribbon = F, Measure = "YLDs", Metric = "Number", Sex = "Male") +
    ylim(100, 4e6) + ggtitle("Male")
trendGBD(EPI_Sugar[EPI_Sugar$location %in% locs, ], group = "location", ribbon = F, Measure = "YLDs", Metric = "Number", Sex = "Female") +
    ylim(100, 4e6) + ggtitle("Female")
dev.off()

# ... Figure 1E, 1F  ----
pdf("GBD_Depression_YLDs_trend.pdf", width = 8, height = 5)
trendGBD(EPI_Depression[EPI_Depression$location %in% locs, ], group = "location", ribbon = F, Measure = "YLDs", Metric = "Number", Sex = "Male") +
    ylim(0, 3.5e7) + ggtitle("Male")
trendGBD(EPI_Depression[EPI_Depression$location %in% locs, ], group = "location", ribbon = F, Measure = "YLDs", Metric = "Number", Sex = "Female") +
    ylim(0, 3.5e7) + ggtitle("Female")
dev.off()

# Figure 2 ----
periods <- c("35", "45", "55", "65", "75", "85", "95")
# ... Figure 2A ----
# frocast data
pdf("COPD_Global_Simulate_YLDs.pdf", width = 10, height = 4)
FSMM(EPI, GBD_db, nPredict = 20, periods = periods, Measure = "YLDs", Sex = "Both", StMM = T, adjxy = c(0, 2))
FSMM(EPI, GBD_db, nPredict = 20, periods = periods, Measure = "YLDs", Sex = "Male", StMM = T, adjxy = c(0, 2))
FSMM(EPI, GBD_db, nPredict = 20, periods = periods, Measure = "YLDs", Sex = "Female", StMM = T, adjxy = c(0, 2))
dev.off()
# ... Figure 2B, 2C ----
# APC-YLDs
pdf("COPD_APC_YLDs.pdf", width = 6, height = 4)
res_m <- PreBAPC(EPI, GBD_db, 20, "YLDs", Region = "Global", Sex = "Male", col.fan = cm.colors, ylim = c(150, 300))
res_f <- PreBAPC(EPI, GBD_db, 20, "YLDs", Region = "Global", Sex = "Female", col.fan = cm.colors, ylim = c(150, 300))
dev.off()
# Save Table S2
rates_m <- data.frame(res_m@agestd.rate)
rates_m$year <- as.numeric(rownames(rates_m))
rates_m$Gender <- "Male"
rates_f <- data.frame(res_f@agestd.rate)
rates_f$year <- as.numeric(rownames(rates_f))
rates_f$Gender <- "Female"
results <- rbind(rates_m, rates_f)
xlsx::write.xlsx(results, "BAPC_Global_YLDs_Rates.xlsx")

# ... Figure 2D ----
# format data
locs <- grep("SDI", unique(EPI$location), value = T)
COPD <- filter(EPI, measure == "YLDs", metric == "Number", sex %in% c("Male", "Female"), location %in% locs) %>%
    dplyr::select(location, val, sex, year) %>%
    dplyr::group_by(sex, year, location) %>%
    dplyr::mutate(val = sum(val)) %>%
    unique()
#
Sugar <- filter(EPI_Sugar, measure == "YLDs", metric == "Number", sex %in% c("Male", "Female"), location %in% locs) %>%
    dplyr::select(location, val, sex, year) %>%
    dplyr::group_by(sex, year, location) %>%
    dplyr::mutate(val = sum(val)) %>%
    unique()
Depression <- filter(EPI_Depression, measure == "YLDs", metric == "Number", sex %in% c("Male", "Female")) %>%
    dplyr::select(location, val, sex, year) %>%
    dplyr::group_by(sex, year) %>%
    dplyr::mutate(val = sum(val)) %>%
    unique()
# ggps function
ggps <- function(Data) {
    ggscatter(Data, "val.x", "val.y",
        conf.int = T, fullrange = T, # fill = "sex",
        color = "location", size = "val.x", # label = 'year',
        shape = "sex", repel = T, combine = T,
        add = "loess", add.params = list(color = "sex", fill = "sex"),
        cor.coef = T, cor.coeff.args = list(aes(group = Data$sex, color = Data$sex), size = 6), cor.method = "spearman"
    ) + guides(
        size = F,
        shape = guide_legend(override.aes = list(size = 4)),
        color = guide_legend(override.aes = list(size = 4))
    ) + scale_size(range = c(.5, 3)) +
        theme(legend.title = element_blank())
}

# COPD + Sugar
Data = merge(COPD, Sugar, by = c("location", "sex", "year"))
pdf("YLDs_corr_COPD_Sugar.pdf", width = 6, height = 6)
ggps(Data) + labs(x = "COPD caused YLDs per year", y = "High-sugar caused YLDs per year")
dev.off()

# COPD + Depression
Data = merge(COPD, Depression, by = c("location", "sex", "year"))
pdf("YLDs_corr_COPD_Depression.pdf", width = 6, height = 6)
ggps(Data) + labs(x = "COPD caused YLDs per year", y = "Depression caused YLDs per year")
dev.off()

# Sugar + Depression
Data = merge(Sugar, Depression, by = c("location", "sex", "year"))
pdf("YLDs_corr_Sugar_Depression.pdf", width = 6, height = 6)
ggps(Data) + labs(x = "High-Sugar caused YLDs per year", y = "Depression caused YLDs per year")
dev.off()


# === NHANES === ----
load("NH_COPD.rda") # COPD
load("NH_DEP.rda") # depression
load("NH_Sugar.rda") # sugar






# === GWAS === ----
# Sweets + Depress
# Figure 5 ----
## ... Figure 5A ----
load("bbj-a-103_P6_mr_res.rda")
load("bbj-a-103_P6_mr_dat.rda")

