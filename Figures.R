"R version 4.3.2"
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
# ... Figure 2A，Table S1 ----
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
# ... Table S2 ----
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
load("NH_Sugar.rda") # sugar intak

# merge data
data <- merge(COPD, SUGAR, by = "SEQN", all = T)
data <- merge(data, DEP, by = "SEQN", all = T)
table(is.na(data[["MCQ160O"]]))
data <- data[!is.na(data[["MCQ160O"]]), ]
#
subDat <- data[, c(
    "SEQN", "RIAGENDR", "RIDAGEYR", "MCQ160O", "DR1ISUGR",
    "DPQ020", "RIDRETH1", "DMDEDUC2", "DMDMARTL", "SIAPROXY",
    "DMDFMSIZ", "DMDHHSZA", "DMDHHSZB", "DMDHHSZE"
)]
colnames(subDat) <- c(
    "SEQN", "Gender", "Age", "COPD", "Sugar",
    "Depressed", "Race", "Education", "Marital", "Proxy",
    "Total", "TotalChild", "TotalYouth", "TotalOld"
)

# Table 1 ----
vars <- c(
    "Age", "Gender", "Race", "Education", "Marital",
    "Proxy", "Sugar", "Depressed", "Total", "TotalChild", 
    "TotalYouth", "TotalOld"
)
tab_unadjust <- tableone::CreateTableOne(
    vars = vars,
    strata = "COPD",
    data = dsubDat,
    test = T, smd = F, 
    addOverall = TRUE
)

# unadjusted SMD
baseLine = print(tab_unadjust, showAllLevels = TRUE, smd = TRUE, nonnormal = "age")
write.csv(baseLine, "baseLine_RAW.csv")

# Table S3 ----
library(MatchIt)
model <- as.formula(paste0('COPD', "~", paste0(c('Gender', 'Age'), collapse = "+")))
f <- matchit(model, dsubDat, method = "nearest", caliper = 0.1, verbose = T)
mdata <- match.data(f)
#
tab_adjust <- tableone::CreateTableOne(
    vars = vars,
    strata = "COPD",
    data = mdata,
    test = T, smd = F, 
    addOverall = TRUE
)
# 
baseLine = print(tab_adjust, showAllLevels = TRUE, smd = TRUE, nonnormal = "age")
write.csv(baseLine, "baseLine_PSM.csv")

# Figure 3 ----
## ... Figure 3A ----
# Forestplot
subDat$COPD <- as.numeric(subDat$COPD) - 1
data("MORT", package = "BBX")
head(MORT)
data2 <- merge(subDat, MORT, by.x = "SEQN", by.y = "seqn")
#
pdf("Forestplot_Quantiles.pdf", width = 8, height = 5)
ForestFit(subDat, "COPD", "Gender", subgroups = c("Sugar", "Depressed"))
ForestFit(data2, "mortstat", "Gender", "permth_int", subgroups = c("Sugar", "Depressed"))
dev.off()

unique(subDat$COPD)
# subDat = subDat[!is.na(subDat$Sugar), ]
# subDat$Sugar = as.numeric(as.character(subDat$Sugar))
subDat$COPD <- as.numeric(as.character(subDat$COPD))
## ... Figure 3B ----
# model 1, without covariates
# left part
Res <- ForestFitXX(subDat, "COPD", c("Sugar", "Depressed"),
    PSM = c("Gender", "Age"),
    fullPlot = F, footnote = "Match gender & age",
    theme = NULL, Combine = T
)
#
pdf("COPD_Sugar_DEP_PSM_C.pdf", width = 8, height = 6)
Res$Plot
graphics.off()

# add time
data("MORT", package = "BBX")
head(MORT)
data2 <- merge(subDat, MORT, by.x = "SEQN", by.y = "seqn")
# right part
Res <- ForestFitXX(data2, "COPD", c("Sugar", "Depressed"), "permth_int",
    PSM = c("Gender", "Age"),
    fullPlot = F, footnote = "Match gender & age",
    theme = NULL, Combine = T
)
#
pdf("COPD_Sugar_DEP_PSM_C_T.pdf", width = 8, height = 6)
Res$Plot
graphics.off()

## ... Figure 3C ----
# model 2, with covariates: gender, age
# left part
Res <- ForestFitXX(subDat, "COPD", c("Sugar", "Depressed"),
    Covs = c("Gender", "Age"), PSM = c("Gender", "Age"),
    fullPlot = F, footnote = "Adjust/Match gender & age",
    theme = NULL, Combine = T
)
#
pdf("COPD_Sugar_DEP_adjust_PSM_C.pdf", width = 8, height = 6)
Res$Plot
graphics.off()

# right part, add time
Res <- ForestFitXX(data2, "COPD", c("Sugar", "Depressed"), "permth_int",
    Covs = c("Gender", "Age"), PSM = c("Gender", "Age"),
    fullPlot = F, footnote = "Adjust/Match gender & age",
    theme = NULL, Combine = T
)
#
pdf("COPD_Sugar_DEP_adjust_PSM_C_T.pdf", width = 8, height = 6)
Res$Plot
graphics.off()


# Figure 4 ----
## ... Figure 4A ----
# Male
head(subDat)
subDat_M <- subDat[subDat$Gender == "Male", ]
# model 2, with covariates: age
Res <- ForestFitXX(subDat_M, "COPD", c("Sugar", "Depressed"),
    Covs = c("Age"), PSM = c("Age"),
    fullPlot = F, footnote = "Male. Adjust/Match age",
    theme = NULL, Combine = T
)
#
pdf("COPD_Sugar_DEP_adjust_PSM_C_Male.pdf", width = 8, height = 6)
Res$Plot
graphics.off()

#
data2_M <- data2[data2$Gender == "Male", ]
Res <- ForestFitXX(data2_M, "COPD", c("Sugar", "Depressed"), "permth_int",
    Covs = c("Age"), PSM = c("Age"),
    fullPlot = F, footnote = "Male. Adjust/Match age",
    theme = NULL, Combine = T
)
#
pdf("COPD_Sugar_DEP_adjust_PSM_C_T_Male.pdf", width = 8, height = 6)
Res$Plot
graphics.off()

## ... Figure 4B ----
# Female
subDat_F <- subDat[subDat$Gender == "Female", ]
# model 2, with covariates: age
Res <- ForestFitXX(subDat_F, "COPD", c("Sugar", "Depressed"),
    Covs = c("Age"), PSM = c("Age"),
    fullPlot = F, footnote = "Female. Adjust/Match age",
    theme = NULL, Combine = T
)
#
pdf("COPD_Sugar_DEP_adjust_PSM_C_Female.pdf", width = 8, height = 6)
Res$Plot
graphics.off()

#
data2_F <- data2[data2$Gender == "Female", ]
Res <- ForestFitXX(data2_F, "COPD", c("Sugar", "Depressed"), "permth_int",
    Covs = c("Age"), PSM = c("Age"),
    fullPlot = F, footnote = "Female. Adjust/Match age",
    theme = NULL, Combine = T
)
#
pdf("COPD_Sugar_DEP_adjust_PSM_C_T_Female.pdf", width = 8, height = 6)
Res$Plot
graphics.off()

# ... Figure 4C, RCS ----
reValues <- list("MCQ160O" = c("2" = "0", "7" = NA, "9" = NA))
# NHrcs(Xitem = "Sugar", Yitem = "COPD", data = subDat, reValue = reValues)
pdf("Sugar_RCS.pdf", width = 8, height = 6)
NHrcs("Sugar", "COPD", subDat, nknots = 3, reValue = reValues)
NHrcs("Sugar", "COPD", subDat, nknots = 3, reValue = reValues, facet = "Gender")
dev.off()


# === GWAS === ----
# Sweets + Depress
# Figure 5 ----
## ... Figure 5A ----
load("bbj-a-103_P6_mr_res.rda")
load("bbj-a-103_P6_mr_dat.rda")

pdf("Sweets_DEP_bbj-a-103.pdf", width = 10, height = 6)
forest_mr(res,
    append = T, exponentiate = T, wrap_str = 30,
    rmID = F, combine = T, xlim = c(0, 10)
)
graphics.off()

## ... Figure S1, Table S5 ----
dat$pos <- dat$pos.outcome
dat$chr <- dat$chr.outcome
dat_Gene <- chromSNP(dat, dat$SNP[1:3], Genes = T)
sapply(dat$SNP[1:3], function(s) dat_Gene$transcripts[[s]][[1]][1]) %>% unlist()
#
head(dat)
dat$outcome <- paste0("Chronic obstructive pulmonary disease || id:", dat$outcome)
SelVars <- c(
    "SNP", "exposure", "outcome", "effect_allele.exposure", "effect_allele.outcome",
    "beta.exposure", "beta.outcome", "eaf.exposure", "eaf.outcome", "pval.exposure", "pval.outcome"
)
xlsx::write.xlsx(dat[, SelVars], "Table S5.xlsx")

pdf("MR_scatter_plot_bbj-a-103.pdf", width = 8, height = 5)
MR_plot(dat, res, rmID = F)
graphics.off()

## ... Figure 5B ----
load("ebi-a-GCST90018587_P6_mr_res.rda")
load("ebi-a-GCST90018587_P6_mr_dat.rda")

pdf("Temp_ebi-a-GCST90018587.pdf", width = 10, height = 6)
forest_mr(res,
    append = T, exponentiate = T, wrap_str = 30,
    rmID = F, combine = T, xlim = c(0, 10)
)
graphics.off()

## ... Figure S1, Table S6 ----
pdf("MR_scatter_plot_ebi-a-GCST90018587.pdf", width = 8, height = 5)
MR_plot(dat, res, rmID = F)
graphics.off()
#
head(dat)
xlsx::write.xlsx(dat[, SelVars], "Table S6.xlsx")

# Figure S2, Table S7 ----
## ... Figure S2A ----
load("ukb-b-20464_P6_mr_res.rda")

pdf("Temp_ukb-b-20464.pdf", width = 10, height = 6)
forest_mr(res,
    append = T, exponentiate = T, wrap_str = 30,
    rmID = F, combine = T
)
graphics.off()
#
load("ukb-b-20464_P6_mr_dat.rda")
head(dat)
xlsx::write.xlsx(dat[, SelVars], "Table S7.xlsx", sheetName = "ukb-b-20464")

## ... Figure S2B ----
load("ebi-a-GCST90018807_P6_mr_res.rda")

pdf("Temp_ebi-a-GCST90018807.pdf", width = 10, height = 6)
forest_mr(res,
    append = T, exponentiate = T, wrap_str = 30,
    rmID = F, combine = T
)
graphics.off()
#
load("ebi-a-GCST90018807_P6_mr_dat.rda")
head(dat)
xlsx::write.xlsx(dat[, SelVars], "Table S7.xlsx", sheetName = "ukb-b-GCST90018807", append = T)

# Figure S3, Table S8 ----
## ... Figure S3A ----
# reverse_bbj-a-103
load("reverse_bbj-a-103_mr_res.rda")

pdf("reverse_bbj-a-103.pdf", width = 10, height = 6)
forest_mr(res,
    append = T, exponentiate = T, wrap_str = 30,
    rmID = F, combine = F
)
graphics.off()
#
load("reverse_bbj-a-103_mr_dat.rda")
head(dat)
xlsx::write.xlsx(dat[, SelVars], "Table S8.xlsx", sheetName = "bbj-a-103")

## ... Figure S3B ----
# reverse_ebi-a-GCST90018587
load("reverse_ebi-a-GCST90018587_mr_res.rda")

pdf("reverse_ebi-a-GCST90018587.pdf", width = 10, height = 6)
forest_mr(res,
    append = T, exponentiate = T, wrap_str = 30,
    rmID = F, combine = F
)
graphics.off()
#
load("reverse_ebi-a-GCST90018587_mr_dat.rda")
head(dat)
xlsx::write.xlsx(dat[, SelVars], "Table S8.xlsx", sheetName = "ebi-a-GCST90018587", append = T)
