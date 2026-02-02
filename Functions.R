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


