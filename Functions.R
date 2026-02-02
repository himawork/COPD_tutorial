#' @title Visualization trend of GBD data
#' @param EPI data.frame queried by function [query_GBD].
#' @param group grouped by variable.
#' @param Measure `Deaths`, `Incidence`, `DALYs` (Disability-Adjusted Life Years), `YLDs` (Years Lived with Disability) or `YLLs` (Years of Life Lost).
#' @param Metric type of ratio plot: `Rate` or `Counts`.
#' @param Sex Can be "Both" (default), "Male", 'Female'.
#' @param fun calculate method, e.g., sum, mean, median, etc.
#' 
#' @export 
#' @return a ggplot object
#' 
trendGBD <- function(EPI, group = "sex", Measure = "Deaths", Metric = "Rate", Sex = "Both", fun = 'sum') {
    library(ggplot2, quietly = T)
    group <- group
    #
    EPI %>%
        dplyr::filter(measure == Measure, metric == Metric, sex %in% Sex) %>%
        dplyr::group_by(get(group), year) %>%
        dplyr::mutate(
            mval = do.call(fun, list(val)),
            mlower = do.call(fun, list(lower)),
            mupper = do.call(fun, list(upper))
        ) %>%
        ggplot(aes_string("year", "mval", color = group)) +
        geom_ribbon(
            aes_string(ymin = "mlower", ymax = "mupper", fill = group),
            alpha = 0, linetype = 0
        ) +
        geom_line(aes_string(color = group), lwd = 1) +
        labs(
            y = paste(Measure, switch(Metric,
                "Rate" = "(per 10^5)",
                "Percent" = "(%)"
            )),
            x = NULL,
            caption = if (standard) "Standardized" else NULL
        ) +
        theme_pubclean(15) +
        theme(legend.title = element_blank())
}


#' @title Predict a Bayesian age-period-cohort model
#' @param EPI,GBD data.frames of disease counts and populations.
#' @param nPredict number of years to predict.
#' @param Measure `Deaths`, `Incidence`, `DALYs` (Disability-Adjusted Life Years), `YLDs` (Years Lived with Disability) or `YLLs` (Years of Life Lost).
#' @param Region Region name of country or area.
#' @param Sex `Female`, `Male`, or `Both`
#' @param plot show plot.
#' @param type `ageSpecProj` — projected age-specific counts, `ageStdRate` — projected age-standardized rates, `ageStdProj` — projected age-standardized counts
#' @param ... Other parameters used in [plotBAPC].
#'
#' @importFrom reshape2 dcast
#' @import BAPC
#' @import dplyr
#'
#' @export
#'
PreBAPC <- function(EPI, GBD, nPredict = 10, Measure = "Deaths", Risk = NULL,
                    Region = "Global", Sex = "Both", plot = T, type = "ageStdRate", ...) {
    library(BAPC)
    library(dplyr)
    library(reshape2)
    #
    ageGrps <- c("<5", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95+")
    # build epi_mat
    epi_dat <- if (!is.null(Risk)) filter(EPI, location == Region, sex == Sex, Measure == Measure, risk == Risk, metric == "Number") else filter(EPI, location == Region, sex == Sex, measure == Measure, metric == "Number")
    epi_dat <- dcast(epi_dat, year ~ age, sum, value.var = "val") |> round()
    rownames(epi_dat) <- epi_dat$year
    epi_dat$year <- NULL
    epi_dat <- epi_dat[, na.omit(match(ageGrps, colnames(epi_dat)))]
    epi_dat <- epi_dat[, colSums(epi_dat, na.rm = T) != 0]
    epi_dat <- epi_dat[rowSums(epi_dat, na.rm = T) != 0, ]

    # project NAs for predict years
    m <- as.numeric(max(rownames(epi_dat)))
    predNA <- matrix(NA,
        nrow = nPredict, ncol = ncol(epi_dat),
        dimnames = list(seq(m + 1, m + nPredict, 1), colnames(epi_dat))
    )
    epi_dat <- rbind(epi_dat, predNA) |> data.frame(check.names = F)

    # pyrs_dat
    pyrs_dat <- filter(
        GBD, location_name == Region, sex == Sex,
        year_id %in% rownames(epi_dat),
        age_group_name %in% colnames(epi_dat)
    )

    #
    pyrs_dat <- dcast(pyrs_dat, year_id ~ age_group_name, sum, value.var = "val")
    rownames(pyrs_dat) <- pyrs_dat$year_id
    pyrs_dat$year_id <- NULL
    pyrs_dat <- pyrs_dat[, na.omit(match(ageGrps, colnames(pyrs_dat)))]

    # check dim
    if (nrow(epi_dat) != nrow(pyrs_dat)) {
        sub_years <- intersect(rownames(epi_dat), rownames(pyrs_dat))
        pyrs_dat <- pyrs_dat[sub_years, ]
        epi_dat <- epi_dat[sub_years, ]
    }

    # weights used to derive age-standardized projections
    whostandard <- data.frame(
        agegroup = ageGrps,
        whoStandard = c(7.910, 9.568, 8.990, 8.324, 7.866, 7.633, 7.332, 6.811, 6.137, 5.509, 4.922, 4.346, 3.684, 2.991, 2.272, 1.607, 1.113, 0.617, 0.255, 0.085)
    )

    li_APC <- APCList(epi_dat, pyrs_dat, gf = 5)
    result <- BAPC(li_APC,
        predict = list(npredict = nPredict, retro = T),
        secondDiff = FALSE,
        stdweight = whostandard[match(colnames(epi_dat), whostandard$agegroup), 2]
    )
    # plot
    if (plot) {
        par(mfrow = c(1, 1))
        plotBAPC(result, scale = 10^5, type = type, ...)
        rates <- data.frame(result@agestd.rate)
        rates$year <- as.numeric(rownames(rates))
        lfit <- lm(rates$mean * 10^5 ~ seq_along(rates$year))
        summ <- summary(lfit)
        abline(coef = coef(lfit), lty = 1, col = "red", lwd = 2)
        # lines(seq_along(rates$year), rates$mean * 10^5, type = "b", col = "red", lwd = 2, pch = 16)
        fitPval <- summ$coefficients[2, "Pr(>|t|)"]
        annot <- paste0("Coef = ", round(summ$coefficients[2, 1], 3), ", R^2 = ", round(summ$r.squared, 3), ", Pval", ifelse(fitPval < 0.001, " < 0.001", round(fitPval)))
        title(paste0(Measure, " rate of gender:", Sex, ifelse(is.null(Risk), "", paste0(" due to ", Risk)), "\nfor populations of ", Region), cex.main = 1)
        mtext(annot, 3, adj = 0, font = 4, col = "grey", cex = 0.8)
        if (!is.null(Risk)) title(sub = paste("Risk:", Risk), col.sub = "grey", font.sub = 3)
    }
    return(result)
}



#' @title Forcasting Stochastic Mortality Model
#' @param EPI,GBD data.frames of disease counts and populations.
#' @param nPredict number of years to predict.
#' @param Measure `Deaths`, `Incidence`, `DALYs` (Disability-Adjusted Life Years), `YLDs` (Years Lived with Disability) or `YLLs` (Years of Life Lost).
#' @param Risk Risk factor depend on the `EPI` data type.
#' @param Region Region name of country or area.
#' @param Sex `Female`, `Male`, or `Both`
#' @param model `LCA` is a standard Lee-Carter model by default, although many other options are available.
#' @param StMM logical, add plot of simulate results of fitted model.
#' @param periods a vector of three periods in `StMM`. Default is c('65', '75', '85').
#' @param adjxy Adjust x,y postions of annotated text of `periods`.
#' @param cols Colors of dotplot and fanplot of `periods`.
#' @param ylims ylims of plot `StMM`.
#' @param saveSim logical, save simulation results.
#' @param ...
#'
#' @importFrom fanplot fan
#' @importFrom reshape2 dcast
#'
#' @export
#' @family APC functions
#' @import demography
#' @import StMoMo
#'
FSMM <- function(EPI, GBD, nPredict = 10, Measure = "Deaths", Risk = NULL, Region = "Global", Sex = "Both", model = "LCA", StMM = F, periods = c("65", "75", "85"), adjxy = c(0, 3), cols = NULL, ylims = NULL, saveSim = T, ...) {
    library(demography, quietly = T)
    library(StMoMo, quietly = T)
    #
    if (is.null(cols)) cols <- c("black", "green", "red", "blue", "cyan", "gold", "violet")
    #
    ageGrps <- c("<5", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95+")
    # build epi_mat
    epi_dat <- if (!is.null(Risk)) filter(EPI, location == Region, sex == Sex, Measure == Measure, risk == Risk, metric == "Number") else filter(EPI, location == Region, sex == Sex, measure == Measure, metric == "Number")
    epi_dat <- dcast(epi_dat, year ~ age, sum, value.var = "val") |> round()
    rownames(epi_dat) <- epi_dat$year
    epi_dat$year <- NULL
    epi_dat <- epi_dat[, na.omit(match(ageGrps, colnames(epi_dat)))]
    # NOTE zero is not allowed in epi_dat
    epi_dat <- epi_dat[, colSums(epi_dat) != 0]

    # pyrs_dat
    pyrs_dat <- filter(
        GBD, location_name == Region, sex == Sex,
        year_id %in% rownames(epi_dat),
        age_group_name %in% colnames(epi_dat)
    )
    #
    pyrs_dat <- dcast(pyrs_dat, year_id ~ age_group_name, sum, value.var = "val")
    rownames(pyrs_dat) <- pyrs_dat$year_id
    pyrs_dat$year_id <- NULL
    pyrs_dat <- pyrs_dat[, na.omit(match(ageGrps, colnames(pyrs_dat)))]

    # check dim
    if (nrow(epi_dat) != nrow(pyrs_dat)) {
        # sub_years = setdiff(rownames(epi_dat), rownames(pyrs_dat))
        sub_years <- intersect(rownames(epi_dat), rownames(pyrs_dat))
        pyrs_dat <- pyrs_dat[sub_years, ]
        epi_dat <- epi_dat[sub_years, ]
    }
    #
    colnames(epi_dat) <- substring(colnames(epi_dat), 1, 2)
    epi_dat <- rename(epi_dat, any_of(c("0" = "<5", "5" = "5-")))
    # colnames(pyrs_dat) = substring(colnames(pyrs_dat), 1, 2)
    ratio_dat <- t(epi_dat / pyrs_dat)
    ages <- as.numeric(colnames(epi_dat))
    years <- as.numeric(rownames(epi_dat))

    # fit Lee-Carter model
    demoDat <- demogdata(ratio_dat, t(pyrs_dat),
        ages = ages, years = years,
        type = "mortality",
        label = Sex, name = Region
    )
    #
    fit <- demography::lca(demoDat, adjust = "dt", minperiod = 5, interpolate = T)
    par(mfrow = c(1, ifelse(StMM, 3, 2)))
    plot(demoDat,
        ylab = paste(Measure, " rate (log scale)"),
        main = paste0(Sex, ": ", Region, " ", Measure, " rates \n(", paste0(range(fit$year), collapse = "-"), ")")
    )
    forfit <- forecast(fit, h = nPredict, jumpchoice = "actual")
    plot(forfit,
        ylab = paste(Measure, " rate (log scale)"),
        main = paste0(Sex, ": ", Region, " ", Measure, " rates \n(", paste0(range(forfit$year), collapse = "-"), ")")
    )
    # save forecast result
    if (saveSim) {
        library(dplyr)
        simFile <- paste0(Sex, "_", Region, "_", Measure, ".xlsx")
        if (file.exists(simFile)) file.remove(simFile)
        #
        df_forfit <- expand.grid(Age = paste0(forfit$age, "~"), Year = forfit$year) %>%
            mutate(
                Rate = as.vector(forfit$rate[[1]]),
                Lower = as.vector(forfit$rate[[2]]),
                Upper = as.vector(forfit$rate[[3]])
            )
        #
        xlsx::write.xlsx(df_forfit, simFile, sheetName = paste0(model, "_model"))
    }

    # add simulate plot
    if (StMM) {
        periods <- as.character(periods)
        StDemoIniData <- StMoMoData(demoDat)
        wxt <- genWeightMat(ages = ages, years = years, clip = 3)
        LC <- lc(link = "logit") # Lee-Carter model
        LCfit <- fit(LC, data = StDemoIniData, ages.fit = ages, wxt = wxt)
        LCfor <- forecast(LCfit, h = nPredict)
        set.seed(1234)
        LCsim <- simulate(LCfit, nsim = 500, h = nPredict)
        # save forecast result
        if (saveSim) {
            rates <- LCsim$rates
            df_simu <- expand.grid(Age = paste0(dimnames(rates)[[1]], "~"), Year = dimnames(rates)[[2]], Sim = 1:dim(rates)[3]) %>%
                mutate(Rate = as.vector(rates)) %>%
                group_by(Age, Year) %>%
                summarise(
                    IQR = IQR(Rate, na.rm = T),
                    Mean_Rate = mean(Rate, na.rm = T),
                    SD_Rate = sd(Rate, na.rm = T)
                )
            xlsx::write.xlsx(data.frame(df_simu), simFile, sheetName = "Lee_Carter", append = T)
        }

        # fan plot
        library(fanplot)
        probs <- c(2.5, 10, 25, 50, 75, 90, 97.5)
        qxt <- StDemoIniData$Dxt / StDemoIniData$Ext
        # par(mar = c(4.5, 4, 1, 1))
        xlims <- range(LCfit$years, LCsim$years)
        if (is.null(ylims)) ylims <- range(qxt[periods, ], LCsim$rates[periods, , ])

        # add fan plot of periods
        for (i in seq_along(periods)) {
            if (i == 1) {
                plot(LCfit$years, qxt[periods[i], ],
                    xlim = xlims, ylim = ylims,
                    main = paste0(Sex, ": ", Region, " ", Measure, " rate\n of ages (", paste0(periods, collapse = ", "), ")"),
                    xlab = "Year", ylab = paste0(Measure, " rate (log scale)"),
                    pch = 20, log = "y", col = cols[i]
                )
                fan(t(LCsim$rates[periods[i], , ]),
                    start = LCsim$years[1],
                    probs = probs, n.fan = 4,
                    fan.col = colorRampPalette(c(cols[i], "white")), ln = NULL
                )
                abline(v = LCsim$years[1], lty = 2)
            } else {
                points(LCfit$years, qxt[periods[i], ], pch = 20, col = cols[i])
                fan(t(LCsim$rates[periods[i], , ]),
                    start = LCsim$years[1],
                    probs = probs, n.fan = 4,
                    fan.col = colorRampPalette(c(cols[i], "white")), ln = NULL
                )
            }
        }
        # add labels
        text(LCfit$years[1], qxt[periods, as.character(LCfit$years[1])],
            labels = paste0("Age:", periods), adj = -adjxy, col = cols, font = 2
        )
    }
    par(mfrow = c(1, 1))
    if (!is.null(Risk)) title(sub = paste("Risk:", Risk), col.sub = "grey", font.sub = 3)
}


#' @title Fit results
#' @param data data used to analysis
#' @param YY,XX,Covs Variables in data
#' @param Time time used to fit Cox model.
#' @param InterXX Interactive efect analysis of XX.
#' @param family family of glm.
#'
#' @noRd
#'
.fitResXX <- function(data, YY, XX, Time = NULL, Covs = NULL, InterXX = F, family = binomial, ...) {
    #
    # if (!require(epiR)) install.packages("epiR")
    suppressPackageStartupMessages(library(epiR))
    if (length(XX) > 2) stop("length of XX should not be longer than two.")
    data[, YY] <- as.numeric(data[, YY])
    #
    if (InterXX) {
        if (length(XX) != 2) stop("length of XX should be two.")
        if (!is.factor(data[, XX[1]])) {
            if (length(unique(data[, XX[1]])) > 5) {
                qs <- quantile(data[, XX[1]], na.rm = T)
                qsn <- which(!duplicated(qs))[-1] - 1
                data[, XX[1]] <- cut(data[, XX[1]], unique(qs),
                    labels = paste0("~Q", qsn),
                    include.lowest = T, right = T, ordered_result = F
                )
            } else {
                data[, XX[1]] <- factor(data[, XX[1]])
            }
        }
        if (!is.factor(data[, XX[2]])) {
            if (length(unique(data[, XX[2]])) > 5) {
                qs <- quantile(data[, XX[2]], na.rm = T)
                qsn <- which(!duplicated(qs))[-1] - 1
                data[, XX[2]] <- cut(data[, XX[2]], unique(qs),
                    labels = paste0("~Q", qsn),
                    include.lowest = T, right = T, ordered_result = F
                )
            } else {
                data[, XX[2]] <- factor(data[, XX[2]])
            }
        }
        #
        if (is.null(Time)) {
            fit <- glm(as.formula(paste0(YY, "~", paste0(XX, collapse = "+"), "+", paste0(c(paste0(XX, collapse = ":"), Covs), collapse = "+"))), data = data, family = family, ...)
            # equal to
            # fit = glm(as.formula(paste0(YY, "~", paste0(XX, collapse = "*"), "+", paste0(Covs, collapse = '+'))), data = data, family = binomial)
            summ <- summary(fit)$coefficients[-1, ]
            pos <- c(grep(XX[1], rownames(summ)), grep(XX[2], rownames(summ))) |>
                unique() |>
                sort()
            summ_dat <- apply(summ[pos, ], 1, function(s) {
                CI <- exp(s[1])
                LCI <- exp(s[1] - s[2] * 1.96)
                UCI <- exp(s[1] + s[2] * 1.96)
                Pval <- ifelse(s[4] < 0.001, "<0.001", round(s[4], 3))
                CIdat <- data.frame(CI = round(CI, 3), LCI = round(LCI, 3), UCI = round(UCI, 3), Pval = Pval)
                names(CIdat) <- c("CI", "LCI", "UCI", "Pval")
                CIdat
            })
            summ_dat <- do.call(rbind, summ_dat)
        } else {
            fit <- coxph(as.formula(paste0("Surv(", Time, ", ", YY, ") ~ ", paste0(XX, collapse = "+"), "+", paste0(c(paste0(XX, collapse = ":"), Covs), collapse = "+"))), data = data, ...)
            summ <- summary(fit)
            summ_dat <- data.frame(summ$conf.int)[, -2]
            pos <- c(grep(XX[1], rownames(summ_dat)), grep(XX[2], rownames(summ_dat))) |>
                unique() |>
                sort()
            summ_dat$Pval <- summ$coefficients[, "Pr(>|z|)"]
            summ_dat <- summ_dat[pos, ]
            names(summ_dat) <- c("CI", "LCI", "UCI", "Pval")
            summ_dat$Pval <- ifelse(summ_dat$Pval < 0.001, "<0.001", round(summ_dat$Pval, 3))
        }

        #
        RAS <- epi.interaction(fit, coef = pos + ifelse(is.null(Time), 1, 0), param = "product", conf.level = 0.95)
        RAS_dat <- do.call(rbind, RAS)
        RAS_dat$Pval <- ""
        names(RAS_dat) <- c("CI", "LCI", "UCI", "Pval")
        #
        CIdat <- rbind(summ_dat, RAS_dat)
    } else {
        if (is.null(Time)) {
            fit <- glm(as.formula(paste0(YY, "~", paste0(c(XX, Covs), collapse = "+"))), data = data, family = family, ...)
            summ <- summary(fit)$coefficients[-1, ]
            pos <- c(grep(XX[1], rownames(summ)), grep(XX[2], rownames(summ))) |>
                unique() |>
                sort()
            ES <- ifelse(is.null(Covs), summ[1], summ[pos, 1])
            SE <- ifelse(is.null(Covs), summ[2], summ[pos, 2])
            Pval <- ifelse(is.null(Covs), summ[4], summ[pos, 4])
            Pval <- ifelse(Pval < 0.001, "<0.001", round(Pval, 3))
            CI <- exp(ES)
            LCI <- exp(ES - SE * 1.96)
            UCI <- exp(ES + SE * 1.96)
            # LCI = CI - summ[pos, 2] * 1.96 #NOTE a little different, but it's not true
            # UCI = CI + summ[pos, 2] * 1.96
            CIdat <- data.frame(round(CI, 3), round(LCI, 3), round(UCI, 3), Pval)
        } else {
            fit <- coxph(as.formula(paste0("Surv(", Time, ", ", YY, ") ~ ", paste0(c(XX, Covs), collapse = "+"))), data = data, ...)
            summ <- summary(fit)
            summ_dat <- data.frame(summ$conf.int)[, -2]
            pos <- c(grep(XX[1], rownames(summ_dat)), grep(XX[2], rownames(summ_dat))) |>
                unique() |>
                sort()
            summ_dat$Pval <- summ$coefficients[, "Pr(>|z|)"]
            summ_dat$Pval <- ifelse(summ_dat$Pval < 0.001, "<0.001", round(summ_dat$Pval, 3))
            CIdat <- summ_dat[pos, ]
        }
        #
        names(CIdat) <- c("CI", "LCI", "UCI", "Pval")
    }
    #
    return(CIdat)
}



#' @title 🌲 Plot the interactive results of fitted models.
#' @description Calculate the primary, additive and multiplicative effects of every two of `XXs`.
#' @param YY,XXs,Covs Variables in data
#' @param Time time used to fit Cox model.
#' @param Plot,fullPlot logical, show forestplot. `fullPlot` will plot the results of additive and multiplicative efect.
#' @param PSM Variables used for estimating propensity scores.
#' @param PSOW logical, whether fitting propensity score-overlap weighted model.
#' @param Combine logical. Combined results of Raw and PSM in plot.
#' @param pch Symbol used in forest plot.
#' @param theme theme of forestplot. More symbols of ci_pch refer to [points].
#' @param ... other parameters used in [forest]. Such as:
#' * `title`: The text for the title.
#' * `footnote`: Footnote for the forest plot.
#' * `xlim`: Limits for the x axis as a vector of length 2, i.e. c(low, high).
#'
#' @return a list of data.frame and forest plot if `Plot` is TRUE.
#' @export
#' @family forestplot functions
#'
#' @examples
#' data(lung)
#' lung$status <- factor(lung$status, levels = 1:2, labels = c(0, 1))
#' lung$karno <- ifelse(lung$pat.karno > median(lung$pat.karno, na.rm = T), "High", "Low")
#' lung$karno <- factor(lung$karno, levels = c("Low", "High"), labels = c(0, 1))
#' lung$meal <- ifelse(lung$meal.cal > median(lung$meal.cal, na.rm = T), "High", "Low")
#' lung$meal <- factor(lung$meal, levels = c("Low", "High"), labels = c(0, 1))
#' ForestFitXX(lung, "status", c("karno", "meal"), Covs = c("age", "sex"), fullPlot = F, footnote = "Adjust")
#'
ForestFitXX <- function(data, YY, XXs, Time = NULL, Covs = NULL, Plot = T, fullPlot = T,
                        PSM = NULL, PSOW = F, Combine = F, pch = 16,
                        theme = forest_theme(ci_Theight = 0.2, ci_pch = pch, ci_col = "#9e3333"), ...) {
    suppressPackageStartupMessages({
        library(forestploter)
        library(MatchIt)
        library(epiR)
    })
    # remove NA events
    # data = data[!is.na(data[[YY]]), ]
    #
    fitRes <- function(dat, YY, XX, Covs = NULL, Time = NULL, weights = NULL) {
        # Fitting models
        res1 <- .fitResXX(dat, YY, XX[1], Covs = Covs, InterXX = F, Time = Time, weights = weights)
        res1$Var1 <- XX[1]
        res1$Var2 <- "-"
        res2 <- .fitResXX(dat, YY, XX[2], Covs = Covs, InterXX = F, Time = Time, weights = weights)
        res2$Var1 <- "-"
        res2$Var2 <- XX[2]
        resX <- .fitResXX(dat, YY, XX, Covs = Covs, InterXX = T, Time = Time, weights = weights)
        rownames(resX)[match(c("reri", "apab", "s"), rownames(resX))] <- c("RERI", "AP", "S")
        resX[c("RERI", "AP", "S", "multiplicative"), "Pval"] <- "±"
        if (resX["RERI", "LCI"] > 0) resX["RERI", "Pval"] <- "+"
        if (resX["RERI", "UCI"] < 0) resX["RERI", "Pval"] <- "-"
        if (resX["AP", "LCI"] > 0) resX["AP", "Pval"] <- "+"
        if (resX["AP", "UCI"] < 0) resX["AP", "Pval"] <- "-"
        if (all(resX[c("RERI", "AP"), "LCI"] > 0, na.omit(resX["S", "LCI"] > 1))) resX["S", "Pval"] <- "+"
        if (all(resX[c("RERI", "AP"), "UCI"] < 0, na.omit(resX["S", "UCI"] < 1))) resX["S", "Pval"] <- "-"
        if (resX["multiplicative", "LCI"] > 1) resX["multiplicative", "Pval"] <- "+"
        resX$Var1 <- rownames(resX)
        resX$Var1[3] <- XX[1]
        resX$Var2 <- ""
        resX$Var2[3] <- XX[2]
        #
        xlab <- ifelse(is.null(Time), "OR (95% CI)", "HR (95% CI)")
        xxRes <- rbind(res1, res2, resX)
        xxRes[[xlab]] <- ifelse(is.na(xxRes$CI), "", sprintf("%.3f (%.3f, %.3f)", xxRes$CI, xxRes$LCI, xxRes$UCI))
        xxRes$` ` <- paste(rep(" ", 20), collapse = " ")

        return(xxRes)
    }

    # generate result
    pairXX <- data.frame(combn(XXs, 2))
    allRes <- list()
    #
    for (i in 1:NCOL(pairXX)) {
        Res <- NULL
        XX <- pairXX[, i]
        # fit raw model
        Res <- fitRes(data, YY, XX, Covs = Covs, Time = Time)
        allRes[["RAW"]][[paste0(XX, collapse = "+")]] <- Res[, c("Var1", "Var2", "CI", "LCI", "UCI", "Pval")]
        Res$Type <- "RAW"
        # Res$N = nrow(data)
        Res <- Res[-c(3, 4), ]
        if (!fullPlot) Res[c(4:7), c("CI", "LCI", "UCI")] <- NA
        # Matching data
        if (!is.null(PSM)) {
            data <- data[!is.na(data[[YY]]), ] # NA is not allowed in matchit function
            data[[YY]] <- as.numeric(data[[YY]])
            model <- as.formula(paste0(YY, "~", paste0(PSM, collapse = "+")))

            # PSOW, add PS-Overlap-Weight with `PSweight`
            if (PSOW) {
                SumSt <- PSweight::SumStat(
                    ps.formula = model, data = data,
                    weight = "overlap", method = "glm", delta = 0.05
                )
                odata <- data[rownames(SumSt$propensity), ]
                # odata$Weights = SumSt$ps.weights$overlap
                oRes <- fitRes(odata, YY, XX, Covs = Covs, Time = Time, weights = NULL)
                oRes$Type <- "PSOW"
                allRes[[oRes$Type[1]]][[paste0(XX, collapse = "+")]] <- oRes[, c("Var1", "Var2", "CI", "LCI", "UCI", "Pval")]
                oRes <- oRes[-c(3, 4), ]
                # xRes$N = nrow(data)
                if (!fullPlot) oRes[c(4:7), c("CI", "LCI", "UCI")] <- NA
                Res <- if (Combine) rbind(oRes, Res) else oRes
            } else {
                # PSM
                f <- matchit(model, data, method = "nearest", caliper = 0.1, verbose = T)
                data <- match.data(f)
                xRes <- fitRes(data, YY, XX, Covs = Covs, Time = Time)
                xRes$Type <- "PSM"
                allRes[[xRes$Type[1]]][[paste0(XX, collapse = "+")]] <- xRes[, c("Var1", "Var2", "CI", "LCI", "UCI", "Pval")]
                xRes <- xRes[-c(3, 4), ]
                # xRes$N = nrow(data)
                if (!fullPlot) xRes[c(4:7), c("CI", "LCI", "UCI")] <- NA
                Res <- if (Combine) rbind(xRes, Res) else xRes
            }
        }

        # Plot
        if (!Plot) next
        xlab <- ifelse(is.null(Time), "OR (95% CI)", "HR (95% CI)")
        if (Combine & !is.null(PSM)) {
            # merge labels
            Res$CateSub <- paste0(Res$Var1, Res$Var2)
            for (ns in unique(Res$CateSub)) {
                nsPos <- which(Res$CateSub == ns)
                Res[[xlab]][nsPos] <- paste0(Res[[xlab]][nsPos], collapse = "\n")
                Res$Pval[nsPos] <- paste0(Res$Pval[nsPos], collapse = "\n")
                # Res$N[nsPos] <- paste0(Res$N[nsPos], collapse = "\n")
            }
            Reslab <- Res[!duplicated(Res$CateSub), ]
            #
            Pf <- forestploter::forest(Reslab[, c("Var1", "Var2", " ", xlab, "Pval")],
                est = split(as.numeric(Res$CI), Res$Type),
                lower = split(as.numeric(Res$LCI), Res$Type),
                upper = split(as.numeric(Res$UCI), Res$Type),
                sizes = split(abs(scale(as.numeric(Res$CI), center = F)), Res$Type),
                ci_column = 3,
                ref_line = 1,
                nudge_y = .3,
                xlab = xlab,
                theme = if (is.null(theme)) {
                    forest_theme(
                        legend_value = unique(Res$Type), ci_pch = pch,
                        legend_position = "top"
                    )
                } else {
                    theme
                },
                ...
            ) |>
                add_border(part = "header", row = 1, where = "bottom") |>
                add_border(part = "header", row = 0, where = "bottom") |>
                edit_plot(
                    row = which(Res$Var2 == ""),
                    col = 1:ncol(Res),
                    which = "text",
                    gp = grid::gpar(font = 4)
                )
        } else {
            Pf <- forestploter::forest(Res[, c("Var1", "Var2", " ", xlab, "Pval")],
                est = as.numeric(Res$CI),
                lower = as.numeric(Res$LCI),
                upper = as.numeric(Res$UCI),
                sizes = abs(scale(as.numeric(Res$CI), center = F)),
                ci_column = 3,
                ref_line = 1,
                xlab = xlab,
                theme = theme, ...
            ) |>
                add_border(part = "header", row = 1, where = "bottom") |>
                add_border(part = "header", row = 0, where = "bottom") |>
                edit_plot(
                    row = which(Res$Var2 == ""),
                    col = 1:ncol(Res),
                    which = "text",
                    gp = grid::gpar(font = 4)
                )
        }
        allRes[["Plot"]] <- Pf
    }
    return(allRes)
}


#' @title 🌲 Fit Glm and Cox models, and get forest plot.
#' @param data data.frame used fit a model.
#' @param YY,XX Variables in data. *YY* usually refer to the status while *XX* is the comparison factor.
#' @param Time time variable in data aims to perform survival analysis.
#' @param ... other parameters used in functions [ForestGlm] or [ForestCox].
#'
#' @export
#' @family forestplot functions
#' 
ForestFit <- function(data, YY, XX, Time = NULL, ...) {
    if (is.null(Time)) {
        ForestGlm(data, YY, XX, ...)
    } else {
        ForestCox(data, paste0('Surv(', Time, ',', YY, ')~', XX), ...)
    }
}



#' @title 🌲 Forestplot of fitted results
#' @param data data.frame used fit a model.
#' @param YY,XX,Covs Variables in data
#' @param fitype fitted types: glm, MultiGLM,
#' @param subgroups Categorical subgroups for interactive analysis in `MultiGLM`.
#' @param family family type of model.
#' @param legend_name names of legend groups.
#' @param legend_position position of legend: bottom, top, right or none.
#' @param combine whether combine reference group to one row. #TODO refer to [ForestCox]
#' @param Prefix Prefix name of saved file.
#' @param CombineCovs Combine univariates and multivariates model of `Covs`, plot with forestploter::forest.
#' @param addTrend logical, add trend regression model result.
#' @param addQval logical, append quantiles value to subcategory.
#' @param xlim xlims in forest plot.
#' @param Ref Reference level of *XX*. It should have the same length of *XX*.
#' @param theme theme of forestplot.
#' @param ... other para used in {forestploter}::[forest] plot.. Such as:
#' * `title`: The text for the title.
#' * `footnote`: Footnote for the forest plot.
#'
#' @import forestploter
#' @import interactionR
#' @import jstable
#' @importFrom autoReg autoReg
#'
#' @export
#' @family forestplot functions
#'
#' @examples
#' library(forestploter)
#' library(survival)
#' library(autoReg)
#' library(jstable)
#'
#' data(lung)
#'
#' lung$sex = lung$sex - 1
#' lung$status = lung$status - 1
#' ForestGlm(lung, "status", c("time", "meal.cal"))
#' # subgroups
#' ForestGlm(lung, "status", c("time", "meal.cal"), subgroups = 'sex')
#' # 
#' ForestGlm(lung, "status", c("time", "meal.cal"), subgroups = 'sex', addTrend = T)
#' #
#' ForestGlm(data = lung, YY = "status", XX = c("time", "meal.cal"), Covs = c("pat.karno"))
#'
ForestGlm <- function(data, YY, XX, fitype = 'glm', subgroups = NULL, Covs = NULL, family = 'binomial', 
    legend_name = NULL, legend_position = 'top', combine = T, Prefix = 'Fitted',
    CombineCovs = F, addTrend = F, addQval = F, xlim = NULL, pch = 16, Ref = NULL,
    theme = forest_theme(ci_Theight = 0.2, ci_col = "#9e3333", ci_pch = pch), ...) {

    fitRes = NULL
    if (!is.null(subgroups)) fitype = "MultiGLM"
    #
    for (i in seq_along(XX)) {
        # stopifnot(length(unique(data[[XX[i]]])) < 6)
        if (length(unique(data[[XX[i]]])) < 6) data[[XX[i]]] = factor(data[[XX[i]]])
        # glm
        if (fitype == "glm") {
            # Combine univariates and multivariates model, plot with forestploter::forest
            if (CombineCovs) {
                .fitResCI(data, c(XX, Covs), YY, NULL, T, "glm", theme, legend_position, legend_name, pch, xlim)
                return(NULL)
            }
            #
            fit = glm(as.formula(paste0(YY, "~ `", paste0(c(XX[i], Covs), collapse = "` + `"), '`')), data = data, family = switch(family,
                "binomial" = binomial(),
                "gaussian" = gaussian()
            ))

            dat_i = glmshow.display(fit, decimal = 3)$table |> data.frame()
            dat_i <- if (any(grepl(paste0("^", XX[i]), rownames(dat_i)))) dat_i[grepl(paste0("^", XX[i]), rownames(dat_i)), ] else dat_i[grepl(XX[i], rownames(dat_i)), ] 
            dat_i$Category = XX[i]
            dat_i$Subtype = rownames(dat_i)
            dat_i$`OR (95% CI)` <- if (is.null(Covs)) dat_i[, 1] else dat_i[, 3]
            dat_i$Pval = if (is.null(Covs)) dat_i[, 2] else dat_i[, 4]
            dat_i$` ` <- paste(rep(" ", 20), collapse = " ")
            dat_i$Pval <- ifelse(is.na(dat_i$Pval), "", dat_i$Pval)
            dat_i$`OR (95% CI)` <- ifelse(is.na(dat_i$`OR (95% CI)`), "", dat_i$`OR (95% CI)`)
            #
            CIlist = strsplit(dat_i[, 1], " ")
            dat_i$OR = unlist(sapply(CIlist, function(x) x[1]))
            dat_i$Lower = unlist(sapply(CIlist, function(x) gsub("\\(", "", strsplit(x[2], ",")[[1]][1])))
            dat_i$Upper = unlist(sapply(CIlist, function(x) gsub("\\)", "", strsplit(x[2], ",")[[1]][2])))
            #
            fitRes = rbind(fitRes, dat_i)
            selectV = c("Category", "Subtype", " ", "OR (95% CI)", "Pval")
            if (i == 1) {
                selectV = selectV[-2]
                ci_column = 2
            } else {
                fitRes$Category[duplicated(fitRes$Category)] = ""
                ci_column = 3
            }
        }
        # MultiGLM
        if (fitype == "MultiGLM") {
            # NOTE: Ref only support one XX variable
            if (is.null(Ref)) {
                Ref = levels(data[[XX[i]]])[1]
            } else {
                data[[XX[i]]] = relevel(data[[XX[i]]], ref = Ref[i])
            }
            if (combine) {
                # if (length(unique(data[, YY])) != 2) stop("Levels of YY should be 2")
                # if (!is.factor(data[, YY])) data[, YY] = factor(data[, YY])
                # if (!is.factor(data[, XX[i]])) data[, XX[i]] = factor(data[, XX[i]])
                # BUG sapply factor variables failed!!
                if (F) {
                    data[, c(subgroups, YY)] = sapply(data[, c(subgroups, YY)], function(XX) {
                        if (length(unique(XX)) > 5) {
                            cut(XX, quantile(XX, na.rm = T),
                                labels = paste0("Q", 1:4),
                                include.lowest = T, right = T, ordered_result = F
                            )
                        } else {
                            as.factor(XX)
                        }
                    })
                }
                #
                for (sub in c(subgroups, YY)) {
                    if (length(unique(data[[sub]])) > 5) {
                        qs = quantile(data[[sub]], na.rm = T)
                        qsn = which(!duplicated(qs))[-1] - 1
                        data[[sub]] = cut(data[[sub]], unique(qs),
                            labels = if (addQval) paste0(paste0("~Q", qsn), '(', qs[qsn + 1], ')') else paste0("~Q", qsn),
                            include.lowest = T, right = T, ordered_result = F
                        )
                    } else {
                        data[[sub]] = as.factor(data[[sub]])
                    }
                }
                
                # Multiple sub-group analysis
                dat <- TableSubgroupMultiGLM(
                    formula = as.formula(paste0(YY, "~", XX[i])),
                    var_subgroups = subgroups,
                    decimal.estimate = 2,
                    data = data,
                    family = family,
                    var_cov = Covs
                )

                dat <- dat[-1, ]
                dat$`P value` = gsub('<0.', 'p<0.', dat$`P value`)
                dat$`P value` = gsub('=0.', 'p=0.', dat$`P value`)
                # Create confidence interval column to display
                dat$`OR (95%CI, P value)` <- ifelse(is.na(dat$OR), "", sprintf("%s (%s-%s, %s)", dat$OR, dat$Lower, dat$Upper, dat$`P value`))
                selectV = c("Category", "Subtype", "Count", "Percent", " ", "OR (95%CI, P value)", "P.int")
                # add trend of model
                if (addTrend) {
                    datTrend = lapply(setdiff(subgroups, XX[i]), function(subgroup) {
                        fforumla = as.formula(paste0(YY, "~", paste0(c(subgroup, Covs), collapse = "+")))
                        fit <- glm(fforumla, data = data, family = switch(family,
                            "binomial" = binomial(),
                            "gaussian" = gaussian()
                        ))
                        dat = data.frame(autoReg(fit))[,c(-3, -4)]
                        dat = dat[grep(paste0("^", subgroup), dat$id), ]
                        CIlist <- strsplit(dat[, "OR..multivariable."], " ")
                        dat$CI <- unlist(sapply(CIlist, function(x) x[1])) |> as.numeric()
                        CIs <- strsplit(sapply(CIlist, function(x) gsub("\\(", "", strsplit(x[2], ",")[[1]][1])), "-")
                        dat$LCI <- unlist(sapply(CIs, function(x) {
                            ifelse(is.na(x[1]), 1, x[1])
                        })) |> as.numeric()
                        dat$UCI <- unlist(sapply(CIs, function(x) {
                            ifelse(is.na(x[1]), 1, x[2])
                        })) |> as.numeric()
                        dat$CI[is.na(dat$CI)] <- 1
                        dat[, "OR..multivariable."][1] = "Ref"
                        dat[, "OR..multivariable."] = gsub('p=.', 'p=0.', dat[, "OR..multivariable."])
                        dat[, "OR..multivariable."] = gsub('p<.', 'p<0.', dat[, "OR..multivariable."])
                        dat$`OR (95% CI, P value)` <- dat[, "OR..multivariable."]
                        #
                        dat = rbind(c(subgroup, subgroup, rep("", ncol(dat) - 2)), dat)
                        dat$P.trend = ""
                        dat$P.trend[1] = summary(fit)$coefficients[1,4] |> round(3)
                        dat
                    })
                    datTrend = do.call(rbind, datTrend)
                    datTrend$`  ` = paste(rep(" ", 20), collapse = " ")
                    # merge to dat
                    dat$id = ""
                    subPos = match(subgroups, dat$Variable)
                    for (j in seq_along(subPos)) {
                        pos = subPos[j]
                        dat$id[pos] = dat$Variable[pos]
                        for (p in pos:nrow(dat)) {
                            if (nrow(dat) > p) {
                                dat$id[p + 1] = paste0(dat$Variable[pos], dat$Variable[p + 1])
                                dat$id[p + 1] = gsub(' ', '', dat$id[p + 1])
                                if (dat$Variable[p + 1] == "") next
                            }
                            if (length(subPos) > j) if (p == subPos[j + 1]) break
                        }
                    }
                    for (j in seq_along(datTrend$id)) {
                        if (datTrend$desc[j] %in% subgroups) datTrend$id[j] = datTrend$desc[j]
                    }
                    matTrend = matrix('', nrow = nrow(dat), ncol = ncol(datTrend)) |> data.frame()
                    colnames(matTrend) = colnames(datTrend)
                    dat = cbind(dat, matTrend)
                    dat[match(datTrend$id, dat$id), c((ncol(dat) - ncol(datTrend) + 1):ncol(dat))] = datTrend
                    # dat = cbind(dat, datTrend)
                    selectV = c(selectV, "  ", "OR (95% CI, P value)", "P.trend")
                } else {
                    dat$CI = dat$OR
                    dat$LCI = dat$Lower
                    dat$UCI = dat$Upper
                }

                # plot area to draw the CI
                dat$` ` <- paste(rep(" ", 20), collapse = " ")
                dat$`P value` <- ifelse(is.na(dat$`P value`), "", dat$`P value`)
                dat$`P.int` <- ifelse(is.na(dat$`P for interaction`), "", dat$`P for interaction`)
                dat$Count <- ifelse(is.na(dat$Count), "", dat$Count)
                dat$Percent <- ifelse(is.na(dat$Percent), "", dat$Percent)

                #
                dat$Subtype = dat$Variable
                dat$Category = XX[i]

                fitRes = rbind(fitRes, dat)
                if (i == 1) {
                    selectV = selectV[-1]
                    ci_column = 4
                } else {
                    fitRes$Category[duplicated(fitRes$Category)] = ""
                    ci_column = 5
                }
            } else {
               stop('Waiting to TODO')
            }
        }
    }

    # save Fitted results
    write.csv(fitRes, paste0(Prefix, "_GLM.csv"))

    # plot
    # check Inf value
    fitRes$Size = fitRes$OR
    fitRes$Size[fitRes$Size == 0] = .1
    fitRes$Size[fitRes$Upper %in% c("Inf", "-Inf")] = .1
    fitRes$Size[fitRes$Lower %in% c("Inf", "-Inf")] = .1
    fitRes$Lower[fitRes$Lower %in% c("Inf", "-Inf")] <- ""
    fitRes$Upper[fitRes$Upper %in% c("Inf", "-Inf")] <- ""
    #
    if (addTrend) {
        fitRes$Size2 = fitRes$CI
        fitRes$Size2[fitRes$Size2 == 0] = .1
        fitRes$Size2[fitRes$UCI %in% c("Inf", "-Inf")] = .1
        fitRes$Size2[fitRes$LCI %in% c("Inf", "-Inf")] = .1
        fitRes$LCI[fitRes$LCI %in% c("Inf", "-Inf")] <- ""
        fitRes$UCI[fitRes$UCI %in% c("Inf", "-Inf")] <- ""
    }
    #
    if (length(unique(fitRes$Category)) == length(unique(fitRes$Subtype))) {
        # fitRes$Subtype = NULL
        selectV = selectV[selectV != "Subtype"]
        ci_column = ci_column - 1
    }
    #
    # tm <- forest_theme(ci_Theight = 0.2, ci_col = "#9e3333")
    if (fitype == "MultiGLM") {
        p = forestploter::forest(fitRes[, selectV],
            est = if (addTrend) list(as.numeric(fitRes$OR), as.numeric(fitRes$CI)) else as.numeric(fitRes$OR),
            lower = if (addTrend) list(as.numeric(fitRes$Lower), as.numeric(fitRes$LCI)) else as.numeric(fitRes$Lower),
            upper = if (addTrend) list(as.numeric(fitRes$Upper), as.numeric(fitRes$UCI)) else as.numeric(fitRes$Upper),
            # sizes = as.numeric(dat$OR),
            sizes = if (addTrend) list(abs(scale(as.numeric(fitRes$Size), F, T)), abs(scale(as.numeric(fitRes$Size2), F, T))) else abs(scale(as.numeric(fitRes$Size), F, T)),
            xlim = xlim,
            ci_column = if (addTrend) c(ci_column, 7) else ci_column,
            ref_line = 1,
            # title = "Effection of TSH on ...",
            xlab = "OR (95% CI)",
            # arrow_lab = c("Left Better", "Right Better"),
            # arrow_lab = rev(unique(data[, XX[i]])),
            theme = theme, ...
        ) |>
            add_border(part = "header", row = 1, where = "bottom") |>
            edit_plot(
                row = which(fitRes$Count == ""), which = "background",
                gp = grid::gpar(fill = "grey80")
            )
    } else {
        p <- forestploter::forest(fitRes[, selectV],
            est = as.numeric(fitRes$OR),
            lower = as.numeric(fitRes$Lower),
            upper = as.numeric(fitRes$Upper),
            sizes = abs(scale(as.numeric(fitRes$OR), F)),
            ci_column = ci_column,
            ref_line = 1,
            xlab = "OR (95% CI)",
            xlim = xlim,
            theme = theme, ...
        ) |>
            add_border(part = "header", row = 1, where = "bottom") |>
            add_border(part = "header", row = 0, col = 1:length(selectV), where = "bottom") |>
            edit_plot(
                row = which(fitRes$CI == ""), which = "background",
                gp = grid::gpar(fill = "grey80")
            )
    }
    #
    plot.new()
    print(p)
    mtext(paste0("Ref: ", Ref), 1, adj = 0, font = 3)
}




#' @title MR plot
#' @description Integrate plots of MR: scatter_plot, forest_plot, funnel_plot, leaveoneout_plot, density_plot, rucker_jackknife, plot_radial, etc.
#' @param dat,mr_res harmonized `dat` and result of `mr()`.
#' @param type `scatter_plot`, `forest_plot`, `funnel_plot`, `leaveoneout_plot`, `density_plot`, `radial_plot`, `jackknife`.
#' @param all Combine all plots to one file if study was one-arm. Or all plots will be merged by group of plot types.
#' @param rmID logical, remove ID of exposure and outcome.
#' @param add.genes logical, add nearest genes of SNP.
#' @param outliers logical, show outliers name on plot.
#' @param scatter,interactive Plot of scatter. Default plot by \link[TwoSampleMR]{mr_scatter_plot}, or set to `MR` to use \link[MendelianRandomization]{mr_plot}, `MRall` to use all methods. `interactive`: logical, interactive plot.
#' @param radial type of plot: `egger` or `ivw`. Default if `egger`.
#' @param ... other parameters used in \link[cowplot]{plotgrid}.
#'
#' @family MR analysis
#' @export
#'
MR_plot <- function(dat, mr_res, type = 'scatter_plot', all = F, rmID = T,
                    add.genes = F, outliers = F,
                    scatter = '', interactive = F, radial = 'egger', ...) {
  if (missing(dat) | missing(mr_res)) stop('Please provide dat and mr_res.')
  p1 <- p2 <- p3 <- p4 <- p5 <- p6 <- NULL
  if (rmID) {
    dat$outcome = gsub(' \\|.*$', '', dat$outcome)
    dat$exposure = gsub(' \\|.*$', '', dat$exposure)
    mr_res$outcome = gsub(' \\|.*$', '', mr_res$outcome)
    mr_res$exposure = gsub(' \\|.*$', '', mr_res$exposure)
  } else {
    dat$outcome = gsub(' \\|\\|', '\n', dat$outcome)
    dat$exposure = gsub(' \\|\\|', '\n', dat$exposure)
    mr_res$outcome = gsub(' \\|\\|', '\n', mr_res$outcome)
    mr_res$exposure = gsub(' \\|\\|', '\n', mr_res$exposure)
  }
  # add.genes
  if (add.genes) {
    geneRes = chromSNP(dat, dat$SNP, Genes = T)
    dat$genes = sapply(dat$SNP, function(s) geneRes$transcripts[[s]][[1]][1]) %>% unlist()
    dat$SNP = paste0(dat$SNP, '(', dat$genes, ')')
  }
  # scatter
  if (type == 'scatter_plot' | all) {
    if (scatter %in% c('MR', 'MRall')) {
      onLoadpkg('MendelianRandomization')
      mr_dat = dat_to_MRInput(dat, get_correlations = F)
      #
      if (scatter == 'MR') {
        p1 = lapply(seq_along(mr_dat), function(x) {
          mr_plot(mr_input(bx = mr_dat[[x]]@betaX, bxse = mr_dat[[x]]@betaXse,
                           by = mr_dat[[x]]@betaY, byse = mr_dat[[x]]@betaYse,
                           # correlation = mr_dat[[1]]@correlation,
                           exposure = mr_dat[[x]]@exposure,
                           outcome = mr_dat[[x]]@outcome,
                           snps = mr_dat[[x]]@snps),
                  interactive = interactive, labels = T, line = 'egger')
        })
      }
      #
      if (scatter == 'MRall') {
        p1 = lapply(seq_along(mr_dat), function(x) {
          mr_plot(mr_allmethods(mr_input(bx = mr_dat[[x]]@betaX, bxse = mr_dat[[x]]@betaXse,
                                         by = mr_dat[[x]]@betaY, byse = mr_dat[[x]]@betaYse,
                                         exposure = mr_dat[[x]]@exposure,
                                         outcome = mr_dat[[x]]@outcome), method = "all"))
        })
      }
    } else p1 = mr_scatter_plot(mr_res, dat)
    #
    # if (!all) print(p1[[1]])
    if (length(p1) > 1) {
      message('The result including ', length(p1), ' plots.')
      resAsk = readline("Do you want show all of them? (Y/N)")
      if (resAsk %in% c('y', 'Y')) showAll = T else showAll = F
      if (all | showAll) cowplot::plot_grid(plotlist = p1, ...) %>% print() else print(p1[[1]])
    } else print(p1[[1]])
  }

  #
  if (all & length(p1) == 1 & length(p4) == 1) {
    cowplot::plot_grid(p1[[1]], p4[[1]], p5[[1]], p6[[1]], rel_heights = c(3, 2)) %>% print()
    cowplot::plot_grid(p2[[1]], p3[[1]]) %>% print()
  }
}


#' @title Forest plot of MR results
#' @param mr_res mr_res or harmonized data of exposure(s) and outcome(s)
#' @param combine logical, combine multiple exposures and outcomes
#' @param x_trans Change axis scale, Allowed values are one of c("none", "log", "log2", "log10"). Default is "none",
#' @param wrap_str length of wrap labels
#' @param nSNP minimum number of SNPs.
#' @param col_pal a vector of colors
#' @param xlim xlims used in forest plot.
#' @param rmID logical, remove ID of exposure and outcome.
#' @param addCol add additional columns in mr_res, e.g., Q for Cochrane's Q, R2, F, Power, etc.
#' @param main title of plot.
#' @param arrow_lab Labels for the arrows, string vector of length two (left and right).
#' @param metagen logical, used for type of `estimator`, combine mr_res with \link[meta]{metagen}.
#' @param genType combine mr_res by `method` or `exposure`.
#' @param append logical, append results of `metagen` to `mr_res`.
#' @param append.color color of appended rows.
#' @param exponentiate Convert effects to OR? Default is TRUE.
#' @param ... other parameters used in \link[forestploter]{forest_theme} or \link[meta]{forest}.
#'
#' @family MR analysis
#' @export
#' 
#' @return a data.frame of MR result, and associated data sets of xxx_mr_res.rda and xxx_mr_dat.rda will be saved to current working directory.
#' @examples 
#' data(availableGWAS)
#' # Diet sweets intake and Depressed affect on COPD
#' expoID = c("ukb-e-102320_AFR", 'ebi-a-GCST006475')
#' res <- MRexplore(expoID, "ebi-a-GCST90018587",
#'     ao = ao, query_local = "GWAS", noSave = T
#' )
#' 
#' forest_mr(res,
#'     append = T, exponentiate = F, wrap_str = 30,
#'     rmID = F, combine = T, xlim = c(-1, 2)
#' )
#'
forest_mr <- function(mr_res, combine = F, x_trans = 'none', wrap_str = 50, nSNP = 2,
                      col_pal = NULL, xlim = NULL, rmID = T, addCol = '', main = NULL, arrow_lab = NULL,
                      metagen = F, genType = 'exposure', append = F, append.color = 'blue', exponentiate = T,
                      footnotes = NULL, vert_line = NULL, ...) {
  suppressPackageStartupMessages({library(forestploter); library(ggplot2)})
  if (is.null(col_pal))
    col_pal <- c('#2BCE48FF', '#FF5005FF', "#F0A0FFFF", "#0075DCFF", '#993F00FF', '#4C005CFF')

  # estimator
    {
    mr_res = mr_res[mr_res$nsnp > nSNP,]
    mr_res = generate_odds_ratios(mr_res)
    if (!exponentiate) {
      mr_res$or = mr_res$b
      mr_res$or_lci95 = mr_res$lo_ci
      mr_res$or_uci95 = mr_res$up_ci
    }
    dups = duplicated(paste0(mr_res$exposure, mr_res$outcome))
    addCol = addCol[addCol %in% names(mr_res)]
    if (length(addCol) > 0) mr_res[dups, addCol] = ''
    if (rmID) {
      mr_res$outcome = gsub(' \\|.*$', '', mr_res$outcome)
      mr_res$exposure = gsub(' \\|.*$', '', mr_res$exposure)
    }
    # combine with meta
    suppressPackageStartupMessages({library(meta)})
    mr_res$expo_out = paste0(mr_res$exposure, ' -> ', mr_res$outcome)
    sm = ifelse(exponentiate, 'OR', 'SMD')
    if (genType == 'method') {
      if (length(unique(mr_res$outcome)) == 1) {
        metaB = metagen(b, se, data = mr_res, studlab = exposure, sm = sm, byvar = method)
      } else {
        metaB = metagen(b, se, data = mr_res, studlab = expo_out, sm = sm, byvar = method)
        }
    } else {
      if (length(unique(mr_res$outcome)) == 1) {
        metaB = metagen(b, se, data = mr_res, studlab = method, sm = sm, byvar = exposure)
      } else {
        metaB = metagen(b, se, data = mr_res, studlab = method, sm = sm, byvar = expo_out)
      }
    }

    # plot of metagen
    ## meta forest ----
    if (metagen) {
      if (is.null(xlim)) xlim = 's'
      meta::forest(metaB, layout = 'RevMan5', # study.results = T, allstudies = T,
             print.subgroup.name = F, subgroup.name = 'Exposure',
             test.subgroup = F, print.subgroup.labels = T, print.byvar = F, col.by = 'black',
             test.effect.subgroup.fixed = T, label.test.effect.subgroup.fixed = "Test fixed effect: ",
             print.pval.Q = F, print.I2 = T, print.tau2 = F,  xlim = xlim,
             overall.hetstat = F, resid.hetstat = F,
             overall = F, test.overall = F, random = F,
             header.line = T,  col.header.line = 'black', col.lines = 'grey',
             # col.inside.fixed = 'red',
             col.study = 'black', col.square = 'gray50', col.square.lines = 'gray50',
             digits.se = 2, 
             ...)
      return(paste0("Forest plot grouped by ", genType))
    }

    # combine with forestploter
    # mr_res = subset(mr_res, mr_res$exposure %in% unique(mr_res$exposure)[1:3])
    {
      mr_res$` ` <- paste0(rep(' ', 20), collapse = ' ')
      if (exponentiate) {
        mr_res$or = ifelse(mr_res$or > 1e3 | mr_res$or < 1e-3,
                           format(mr_res$or, scientific = T, digits = 3),
                           round(mr_res$or, 3))
        mr_res$or[which(as.numeric(mr_res$or) == 0)] = 0
        mr_res$or_lci95 = ifelse(mr_res$or_lci95 > 1e3 | mr_res$or_lci95 < 1e-3,
                                 format(mr_res$or_lci95, scientific = T, digits = 3),
                                 round(mr_res$or_lci95, 3))
        mr_res$or_lci95[which(as.numeric(mr_res$or_lci95) == 0)] = 0
        mr_res$or_uci95 = ifelse(mr_res$or_uci95 > 1e3 | mr_res$or_uci95 < 1e-3,
                                 format(mr_res$or_uci95, scientific = T, digits = 3),
                                 round(mr_res$or_uci95, 3))
        mr_res$or_uci95[which(as.numeric(mr_res$or_uci95) == 0)] = 0
      } else {
        mr_res$or = round(mr_res$or, 3)
        mr_res$or_lci95 = round(mr_res$or_lci95, 3)
        mr_res$or_uci95 = round(mr_res$or_uci95, 3)
      }
      mr_res$`OR (95% CI)` = paste0(mr_res$or, " (", mr_res$or_lci95, ", ", mr_res$or_uci95, ")")
      mr_res$or = as.numeric(mr_res$or)
      mr_res$or_lci95 = as.numeric(mr_res$or_lci95)
      mr_res$or_uci95 = as.numeric(mr_res$or_uci95)
      mr_res$pSig = cut(as.numeric(mr_res$pval),
                        c(0, 0.001, 0.01, 0.05, 1), c('***', '**', '*', ''), right = F)
      mr_res$pval = format(mr_res$pval, scientific = T, digits = 3)
      mr_res$pSig[which(is.na(mr_res$pSig))] = ''
      mr_res$pval = paste0(mr_res$pval, mr_res$pSig)
      mr_res$`exposure(n)` = paste0(mr_res$exposure, ' (', mr_res$nsnp, ')')
      mr_res$`exposure(n)`[which(dups)] = ''
      mr_res$`outcome(n)` = paste0(mr_res$outcome, ' (', mr_res$nsnp, ')')
      mr_res$`outcome(n)`[which(dups)] = ''
      #
      if (x_trans %in% c("log", "log2", "log10")) {
        # do.call('log', list(10))
        # eval(call('log', 10))
        mr_res$or = ifelse(mr_res$or > 0, eval(call(x_trans, mr_res$or)), -eval(call(x_trans, mr_res$or)))
        mr_res$or_lci95 = ifelse(mr_res$or_lci95 > 0,
                                 eval(call(x_trans, mr_res$or_lci95)),
                                 -eval(call(x_trans, mr_res$or_lci95)))
        mr_res$or_uci95 = ifelse(mr_res$or_uci95 > 0,
                                 eval(call(x_trans, mr_res$or_uci95)),
                                 -eval(call(x_trans, mr_res$or_uci95)))
      } else x_trans = 'none'
    }
    # append results of meta
    if (append) {
      meteRes = meta.res(metaB, save = F, showPlot = T)
      meteRes = meteRes[meteRes$levels != '',]
      meteRes$or = lapply(1:nrow(meteRes), function(x) strsplit(meteRes$`ES[LCI; UCI]`[x], '\\[')[[1]][1]) %>% unlist() %>% as.numeric()
      meteRes$LUCI = lapply(1:nrow(meteRes), function(x) strsplit(meteRes$`ES[LCI; UCI]`[x], '\\[|\\]')[[1]][2]) %>% unlist()
      meteRes$or_lci95 = lapply(1:nrow(meteRes), function(x) strsplit(meteRes$LUCI[x], ';')[[1]][1]) %>% unlist() %>% as.numeric()
      meteRes$or_uci95 = lapply(1:nrow(meteRes), function(x) strsplit(meteRes$LUCI[x], ';')[[1]][2]) %>% unlist() %>% as.numeric()
      meteRes$`ES[LCI; UCI]` = paste0(meteRes$or, ' (', meteRes$or_lci95, ', ', meteRes$or_uci95, ')')
      #
      app.dat = data.frame(matrix(nrow = nrow(meteRes), ncol = ncol(mr_res)))
      names(app.dat) = names(mr_res)
      app.dat$or = meteRes$or
      app.dat$or_lci95 = meteRes$or_lci95
      app.dat$or_uci95 = meteRes$or_uci95
      app.dat$pval = as.character(meteRes$P.val)
      if (length(unique(mr_res$outcome)) == 1) {
        app.dat$exposure = meteRes$levels
        app.dat$outcome = mr_res$outcome[match(app.dat$exposure, mr_res$exposure)]
      } else {
        app.dat$exposure = unlist(lapply(meteRes$levels, function(x) strsplit(x, ' -> ')[[1]][1]))
        app.dat$outcome = unlist(lapply(meteRes$levels, function(x) strsplit(x, ' -> ')[[1]][2]))
      }
      app.dat$`OR (95% CI)` = meteRes$`ES[LCI; UCI]`
      # if ('P.het' %in% names(app.dat)) app.dat$P.het = meteRes$P.val.Q
      # app.dat$`exposure(n)` = mr_res$`exposure(n)`[match(app.dat$exposure, mr_res$exposure)]
      app.dat$`exposure(n)` = paste0("(I-square: ", meteRes$`I2(%)`, '%, pval.Q: ', meteRes$P.val.Q, ')')
      app.dat$`outcome(n)` = app.dat$`exposure(n)`
      app.dat$method = 'Pooled (I-V)'
      app.dat$se = meteRes$seTE
      #
      app.dat[is.na(app.dat)] = ''
      # joinVars = intersect(names(mr_res), names(app.dat))
      # mr_res = rbind(mr_res[,joinVars], app.dat[,joinVars])
      # or
      mr_res = merge(mr_res, app.dat, all = T, sort = F)
    }

    # combine forest ----
    mr_list = split(mr_res, ~outcome)
    if (length(mr_list) > 1 & combine) {
      if (append) {
        mr_res = mr_res[order(factor(mr_res$exposure, levels = unique(mr_res$exposure)),
                              factor(mr_res$method, levels = unique(mr_res$method))),]
        summaryRows = ifelse(mr_res$method == 'Pooled (I-V)', TRUE, FALSE)
        # mr_res$summary = summaryRows
        summaryRows = split(summaryRows, mr_res$outcome)
      } else summaryRows = NULL
      # combine with ggplot
      mr_res$ypos = 1:nrow(mr_res)
      labels = unique(mr_res$exposure)
      # breaks
      breaks <- 1:length(unique(mr_res$exposure)) * length(unique(mr_res$outcome)) * length(unique(mr_res$method))
      if (max(breaks) != nrow(mr_res)) {
        breaks <- which(!duplicated(mr_res$exposure))
        breaks <- c(breaks[-1] - 1, nrow(mr_res))
        # breaks[length(breaks)] <- nrow(mr_res)
      }
      # diff(which(!duplicated(mr_res$exposure)))
      # table(mr_res$exposure)
      # unique(mr_res$outcome)
      # showColor()
      mr_res$sig = ifelse(grepl('\\*', mr_res$pSig), '"*"', '')
      mr_res$method = factor(mr_res$method, unique(mr_res$method))
      mr_res$outcome = factor(mr_res$outcome, names(mr_list))
      p = ggplot(mr_res, aes(x = or, y = ypos, color = outcome)) +
        geom_vline(xintercept = ifelse(x_trans == 'none', 1, 0), size = 2, alpha = .5, color = "grey50") +
        geom_segment(aes(x = or_lci95,
                         xend = or_uci95,
                         yend = ypos), size = 1, alpha = .5) +
        geom_point(aes(shape = method), size = 4) +
        scale_color_manual(values = col_pal) +
        scale_y_continuous(breaks = breaks,
                           labels = stringr::str_wrap(labels, width = wrap_str), expand = c(.05, .05)) +
        theme_minimal(base_family = "Times New Roman", base_size = 12) +
        theme(legend.position = "right",
              panel.grid.minor.y = element_blank(),
              panel.grid.major.y = element_line(size = 4, color = "grey90"),
              axis.text.y = element_text(vjust = .3, size = 12)) +
        ggtitle('Estimated causal effects of exposure(s) on outcome(s)') +
        ylab(' ') + xlab(ifelse(exponentiate, 'OR (95% CI)', 'ES (95% CI)'))

      p <- p + annotate('text', x = mr_res$or, y = mr_res$ypos, col = 'black',
                        label = mr_res$sig, vjust = .7, size = 5, parse = T)
      if (x_trans != 'none') p <- p + xlab(paste0(x_trans, '_OR (95% CI)'))
      if (any(c(mr_res$or > 100, mr_res$or_uci95 > 100))) p <- p + scale_x_log10() + xlab('log10_OR (95% CI)')
      print(p)

      # check paired list
      NumLen = sapply(names(mr_list), function(x) nrow(mr_list[[x]]))
      if (length(unique(NumLen)) != 1) {
        message('Length of list were not equal. \nYou can set "combine = F" to see all results.')
        # Nc = names(which.max(NumLen))
        # for (i in Nc) mr_list[[i]] = mr_list[[i]][mr_list[[2]]$method %in% mr_list[[1]]$method,]
        Allm = intersect(mr_list[[which.max(NumLen)]]$method, mr_list[[which.min(NumLen)]]$method)
        mr_res = mr_res[mr_res$method %in% Allm,]
      }
      # combine multiple plots
      tm <- forest_theme(base_size = 10,
                         refline_lty = "solid",
                         ci_col = col_pal[1:length(mr_list)],
                         footnote_col = "blue",
                         legend_name = "Group",
                         legend_value = names(mr_list),
                         ...)
      mr_dat = unique(mr_res[,c('exposure', 'method', ' ', 'nsnp')])
      mr_dat$`exposure(n)` = paste0(mr_dat$`exposure`, ' (', mr_dat$nsnp, ')')
      mr_dat = mr_dat[!duplicated(mr_dat[,c('exposure', 'method')]),]
      mr_dat$`exposure(n)`[which(duplicated(mr_dat$`exposure`))] = ' '
      mr_dat$`exposure(n)` = stringr::str_wrap(mr_dat$`exposure(n)`, width = wrap_str)
      if (append) {
        mr_dat = mr_dat[order(factor(mr_dat$exposure, levels = unique(mr_dat$exposure)),
                              factor(mr_dat$method, levels = unique(mr_dat$method))),]
      }
      # mr_res$Sig = cut(as.numeric(mr_res$pval),
      if (is.null(xlim)) {
        xlim = c(floor(range(mr_res$or)[1]), ceiling(range(mr_res$or)[2]))
        diffOR = max(mr_res$or_uci95 - mr_res$or_lci95)
        rangeOr = range(c(mr_res$or_lci95, mr_res$or_uci95))
        if (diffOR > 10) {
          ORs = c(mr_res$or_lci95, mr_res$or_uci95)
          rangeOr = range(ORs[!ORs %in% rangeOr])
          xlim = c(floor(rangeOr[1]), ceiling(rangeOr[2]))
        } else if (diffOR < 1) {
          xlim = c(rangeOr[1] * .95, rangeOr[2] * 1.05)
        }
      }
      if (is.null(footnotes)) {
        footnotes = ifelse(x_trans == 'none', ' ', paste0(x_trans, ' transformed'))
      } else {
        transX = ifelse(x_trans == 'none', ' ', paste0(x_trans, ' transformed'))
        footnotes = paste0(footnotes, '\n', transX)
      }
      #
      # sizes <- sqrt(1/mr_res$se)
      # sizes[is.infinite(sizes)] = NA
      # mr_res$sizes = sizes / max(sizes, na.rm = TRUE)
      mr_res$sizes = scale(abs(mr_res$or), F)
      if (is.null(main)) main = 'Estimated causal effects of exposure(s) on outcome(s)'
      if (is.null(arrow_lab)) arrow_lab = c("Negative", "Positive")
      forestploter::forest(mr_dat[,c('exposure(n)', 'method', ' ')],
                           est = split(mr_res$or, mr_res$outcome),
                           lower = split(mr_res$or_lci95, mr_res$outcome),
                           upper = split(mr_res$or_uci95, mr_res$outcome),
                           sizes = split(mr_res$sizes, mr_res$outcome),
                           # is_summary = summaryRows,
                           arrow_lab = arrow_lab, xlim = xlim,
                           title = main, ci_column = 3,
                           ref_line = ifelse(x_trans == 'none' & exponentiate, 1, 0),
                           vert_line = vert_line,
                           footnote = footnotes,
                           #x_trans = x_trans, # only suit to number > 0
                           theme = tm) %>%
        add_border(part = "header", row = 1, where = "bottom") %>%
        # Edit background of multiple exposures
        edit_plot(row = which(mr_dat$`exposure(n)` != ''), which = "background",
                  gp = grid::gpar(fill = "grey80")) %>% print()
    } else {
      # single forest ----
      tm <- forest_theme(...)
      # check length of exposure
      if (length(unique(mr_res$exposure)) == 1 & length(unique(mr_res$outcome)) > 1) {
        mr_dat = mr_res
        mr_dat$`outcome(n)` = stringr::str_wrap(mr_dat$`outcome(n)`, width = wrap_str)
        # xlim
        if (is.null(xlim)) {
          xlim = c(floor(range(mr_dat$or)[1]), ceiling(range(mr_dat$or)[2]))
          diffOR = max(mr_dat$or_uci95 - mr_dat$or_lci95)
          rangeOr = range(c(mr_dat$or_lci95, mr_dat$or_uci95))
          if (diffOR > 10) {
            ORs = c(mr_res$or_lci95, mr_res$or_uci95)
            rangeOr = range(ORs[!ORs %in% rangeOr])
            xlim = c(floor(rangeOr[1]), ceiling(rangeOr[2]))
          } else if (diffOR < 1) {
            xlim = c(rangeOr[1] * .9, rangeOr[2] * 1.1)
          }
        }
        # highlight
        if (append) {
          mr_dat = mr_dat[order(factor(mr_dat$outcome, levels = unique(mr_dat$outcome)),
                                factor(mr_dat$method, levels = unique(mr_dat$method))),]
          summaryRows = ifelse(mr_dat$method == 'Pooled (I-V)', TRUE, FALSE)
        } else summaryRows = NULL
        mrk = grep('\\*', mr_dat$pval)
        if (length(mrk) != 0) mrk = mrk else mrk = NULL
        # plot
        # sizes <- sqrt(1/mr_dat$se)
        # sizes[is.infinite(sizes)] = NA
        # sizes <- sizes/max(sizes, na.rm = TRUE)
        if (is.null(main)) title = paste0('Estimated causality of outcomes for exposure\n', unique(mr_res$exposure))
        if (is.null(arrow_lab)) arrow_lab = c("Negative", "Positive")
        sel_cols = c('outcome(n)', 'method', ' ', 'OR (95% CI)', 'pval', addCol)
        if (!exponentiate) {
          mr_dat$`ES (95% CI)` = mr_dat$`OR (95% CI)`
          sel_cols = c('outcome(n)', 'method', ' ', 'ES (95% CI)', 'pval', addCol)
        }
        if (is.null(footnotes)) {
          footnotes = ifelse(x_trans == 'none', ' ', paste0(x_trans, ' transformed'))
        } else {
          transX = ifelse(x_trans == 'none', ' ', paste0(x_trans, ' transformed'))
          footnotes = paste0(footnotes, '\n', transX)
        }
        #
        forestploter::forest(mr_dat[,sel_cols],
                             est = mr_dat$or,
                             lower = mr_dat$or_lci95,
                             upper = mr_dat$or_uci95,
                             sizes = scale(abs(mr_dat$or), F),
                             ci_column = 3,
                             is_summary = summaryRows,
                             # x_trans = x_trans,
                             arrow_lab = arrow_lab,
                             title = ifelse(is.null(main), title, main),
                             ref_line = ifelse(x_trans == 'none' & exponentiate, 1, 0),
                             vert_line = vert_line, xlim = xlim,
                             footnote = footnotes,
                             ticks_at = NULL, theme = tm) %>%
          add_border(part = "header", row = 1, where = "bottom") %>%
          # Edit fontface of results
          edit_plot(row = which(mr_dat$method == 'Pooled (I-V)'), col = c(1, 2, 4, 5), which = "text",
                    gp = grid::gpar(fontface = "italic", col = append.color)) %>%
          edit_plot(row = mrk, col = ifelse(is.null(mrk), 6, 5), which = "text",
                    gp = grid::gpar(fontface = "bold")) %>%
          # Edit background of multiple exposures
          edit_plot(row = which(mr_dat$`outcome(n)` != '' & mr_dat$method != 'Pooled (I-V)'),
                    which = "background", gp = grid::gpar(fill = "grey80")) %>% print()
        #
        return('Reverse them automatically.')
      }
      # seperate exposures by more outcomes
      for (i in seq_along(mr_list)) {
        mr_dat = mr_list[[i]]
        mr_dat$`exposure(n)` = stringr::str_wrap(mr_dat$`exposure(n)`, width = wrap_str)
        if (is.null(xlim)) {
          xlim = c(floor(range(mr_dat$or)[1]), ceiling(range(mr_dat$or)[2]))
          diffOR = max(mr_dat$or_uci95 - mr_dat$or_lci95)
          rangeOr = range(c(mr_dat$or_lci95, mr_dat$or_uci95))
          if (diffOR > 10) {
            ORs = c(mr_res$or_lci95, mr_res$or_uci95)
            rangeOr = range(ORs[!ORs %in% rangeOr])
            xlim = c(floor(rangeOr[1]), ceiling(rangeOr[2]))
          } else if (diffOR < 1) {
            xlim = c(rangeOr[1] * .9, rangeOr[2] * 1.1)
          }
        }
        # rangeOr = range(c(mr_dat$or_lci95, mr_dat$or_uci95))
        # if (rangeOr[1] > xlim[1] & rangeOr[2] < xlim[2]) xlim = rangeOr
        # highlight
        if (append) {
          mr_dat = mr_dat[order(factor(mr_dat$exposure, levels = unique(mr_dat$exposure)),
                                factor(mr_dat$method, levels = unique(mr_dat$method))),]
          summaryRows = ifelse(mr_dat$method == 'Pooled (I-V)', TRUE, FALSE)
        } else summaryRows = NULL
        mrk = grep('\\*', mr_dat$pval)
        if (length(mrk) != 0) mrk = mrk else mrk = NULL
        # plot
        if (is.null(main)) title = paste0('Estimated causal effects of exposure(s) on\n', names(mr_list)[i])
        if (is.null(arrow_lab)) arrow_lab = c("Negative", "Positive")
        sel_cols = c('exposure(n)', 'method', ' ', 'OR (95% CI)', 'pval', addCol)
        if (!exponentiate) {
          mr_dat$`ES (95% CI)` = mr_dat$`OR (95% CI)`
          sel_cols = c('exposure(n)', 'method', ' ', 'ES (95% CI)', 'pval', addCol)
        }
        if (is.null(footnotes)) {
          footnotes = ifelse(x_trans == 'none', ' ', paste0(x_trans, ' transformed'))
        } else {
          transX = ifelse(x_trans == 'none', ' ', paste0(x_trans, ' transformed'))
          footnotes = paste0(footnotes, '\n', transX)
        }
        #
        forestploter::forest(mr_dat[,sel_cols],
                             est = mr_dat$or,
                             lower = mr_dat$or_lci95,
                             upper = mr_dat$or_uci95,
                             sizes = scale(abs(mr_dat$or), F),
                             ci_column = 3,
                             is_summary = summaryRows,
                             # x_trans = x_trans,
                             # arrow_lab = c("Outcome", "Exposure"),
                             arrow_lab = arrow_lab,
                             title = ifelse(is.null(main), title, main),
                             ref_line = ifelse(x_trans == 'none' & exponentiate, 1, 0),
                             vert_line = vert_line, xlim = xlim,
                             footnote = footnotes,
                             ticks_at = NULL, theme = tm) %>%
          add_border(part = "header", row = 1, where = "bottom") %>%
          # Edit fontface of results
          edit_plot(row = which(mr_dat$method == 'Pooled (I-V)'), col = c(1, 2, 4, 5), which = "text",
                    gp = grid::gpar(fontface = "italic", col = append.color)) %>%
          edit_plot(row = mrk, col = ifelse(is.null(mrk), 6, 5), which = "text",
                    gp = grid::gpar(fontface = "bold")) %>%
          # Edit background of multiple exposures
          edit_plot(row = which(mr_dat$`exposure(n)` != '' & mr_dat$method != 'Pooled (I-V)'),
                    which = "background", gp = grid::gpar(fill = "grey80")) %>% print()
      }
    }
  }
}
