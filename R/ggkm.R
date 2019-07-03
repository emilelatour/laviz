#' @title
#' Kaplan-Meier survival plot
#'
#' @description
#' I mostly user the package \code{survminer} when making quick and easy
#' survival plots. But when I want to make a plot for publication or for higher
#' quality, I will use this function \code{ggkm}. Most of the code was cobbled
#' together from many different versions of this function that people have
#' shared online. Just google "ggkm" and you'll see them. My version allows me
#' to make the tweaks and adjustments that I want when making these plots.
#'
#' @param sfit A survfit object
#' @param main String; Main plot title. Default is "Kaplan-Meier Plot".
#' @param subtitle String; Subtitle for the plot. Default is NA.
#' @param risk_table Logical; \code{TRUE} will show the risk table. Default is
#'   \code{FALSE}
#' @param timeby Integer; amount to break the x-axis by.
#' @param xlims A numeric vector; The minimum and maximum values for the x-axis.
#'   Default is \code{0} and max time in the survfit obeject.
#' @param ylims A numeric vector; The minimum and maximum values for the y-axis.
#'   Default is \code{0} to \code{1.01} to give a little space at the top.
#' @param xlabs String; Label for the x-axis. Default is "Time".
#' @param ylabs String; Label for the y-axis. Default is "Survival Probability
#'   (\%)"
#' @param ystratalabs String vector; Labels for the strata
#' @param ystrataname String; The title for the strata.
#' @param pval Logical; \code{TRUE} is default and shows a p-value onn the plot.
#' @param pval_threshold Numeric; threshold to showing actual p-value or "P <
#'   ...". Defualt is \code{0.001}.
#' @param pvalpos A numeric vector; Position for the p-value on the plot if
#'   \code{pval} is \code{TRUE}. Default is in the lower left corner of the plot.
#' @param text_annotate String; Text to show instead of p-value.
#' @param marks Logical; \code{TRUE} will show marks for censoring. Default is
#'   \code{FALSE}
#' @param shape Integer; Controls the shape of the censoring mark if \code{TRUE}.
#'   Default is \code{3}, a cross shape. Other good option is \code{4} = X
#' @param linetype Logical; if \code{TRUE} then lines will be different types:
#'   solid, dashed, dotted, etc. Default is \code{FALSE}.
#' @param linecolor Logical; if \code{TRUE} then lines are different colors.
#'   Default is \code{TRUE}.
#' @param blackandwhite Logical; if \code{TRUE}, plot is made in black and white
#'   using R's \code{gray.colors}
#' @param palette The color paletter to be used. Default are opinionated
#'   choices.
#' @param legend Logical; if \code{TRUE} then the legend is shown.
#'   Default is \code{TRUE}.
#' @param legpos Numeric vector; Position of the legend. Default is the upper
#'   right corner of the plot.
#' @param subs Default is NULL
#' @param font_family String name of the font to be used; Default is "Arial".
#' @param adj_table_title Numeric; adjusts the postion of the title for the Risk
#'   Table when used. Combining plots will move this label around. This argument
#'   will artifically move the label out of place so that it ends up in the
#'   right place. Default is \code{-0.20}. __More negative moves left__. This
#'   takes trial and error.
#' @param adj_y_axis_label Numeric; adjusts the postion of the title for the
#'   Risk Table when used. Combining plots will move this label around. This
#'   argument will artifically move the label out of place so that it ends up
#'   in the right place. Default is \code{-12.5}. __More negative moves right__.
#'   This takes trial and error.
#' @param surv_plot_height the height of the survival plot on the grid. Default
#'   is \code{0.75}. Ignored when risk.table = \code{FALSE}.
#' @param risk_table_height the height of the risk table on the grid. Increase
#'   the value when you have many strata. Default is \code{0.25}. Ignored when
#'   risk.table = \code{FALSE}.
#'
#' @import ggplot2
#' @import dplyr
#' @import survival
#' @import ggpubr
#' @import extrafont
#' @import forcats
#' @importFrom grDevices gray.colors
#' @importFrom stats pchisq
#' @importFrom stats quantile
#' @importFrom stats sd
#' @importFrom stats time
#'
#' @return A plot
#' @export
#'
#' @examples \dontrun{
#' library(survival)
#' library(extrafont)
#' #### Load fonts --------------------------------
#' # extrafont::fonts()  # Vector of font family names
#' # extrafont::fonttable()  # Show entire table
#' # extrafont::font_import()  # imports fonts installed on the system
#' extrafont::loadfonts(device = "win", quiet = TRUE)
#' data(colon)
#' fit <- survfit(Surv(time, status) ~ 1, data = colon)
#' ggkm(fit, timeby = 500, marks = TRUE)
#' ggkm(fit, timeby = 500)
#'
#' fit <- survfit(Surv(time, status) ~ rx, data = colon)
#' ggkm(fit,
#'      timeby = 500,
#'      marks = TRUE,
#'      ystratalabs = c("Observed",
#'                      "Level",
#'                      "Level+"),
#'      adj_table_title = -0.12)
#'
#' sfit <- survfit(Surv(time, status) ~ rx, data = colon)
#' foo <- ggkm(sfit = sfit,
#'             risk_table = TRUE,
#'             xlabs = "Time (in months)",
#'             ylabs = "Survival Probability (%)",
#'             xlims = c(0, max(sfit$time)),
#'             ylims = c(0, 1.01),
#'             ystratalabs = NULL,
#'             ystrataname = NULL,
#'             timeby = 500,
#'             main = "Kaplan-Meier Plot",
#'             pval = FALSE,
#'             marks = TRUE,
#'             linetype = FALSE,
#'             linecolor = TRUE,
#'             shape = 3,  # 3 = cross, 4 = X
#'             legend = TRUE,
#'             subs = NULL,
#'             legpos = c(1, 1),
#'             text_annotate = "Here is some text",
#'             pvalpos = c(500, 0.25),
#'             font_family = "Times New Roman")
#'
#' foo
#' }
#'
ggkm <- function(sfit,
                 main = "Kaplan-Meier Plot",
                 subtitle = NA,
                 risk_table = TRUE,
                 timeby = 100,
                 xlims = c(0, max(sfit$time)),
                 ylims = c(0, 1.01),
                 xlabs = "Time",
                 ylabs = "Survival Probability (%)",
                 ystratalabs = NULL,
                 ystrataname = NULL,
                 pval = TRUE,
                 pval_threshold = 0.001,
                 pvalpos = c(max(sfit$time) / 6, 0.20),
                 text_annotate = NA,
                 marks = FALSE,
                 shape = 3,  # 3 = cross, 4 = X
                 linetype = FALSE,
                 linecolor = TRUE,
                 blackandwhite = FALSE,
                 palette = c("#0073C2", "#EFC000", "#868686", "#CD534C",
                             "#7AA6DC", "#003C67", "#8F7700", "#3B3B3B",
                             "#A73030", "#4A6990"),
                 legend = TRUE,
                 legpos = c(0.9, 0.8),
                 subs = NULL,
                 font_family = "Arial",
                 adj_table_title = -0.20,  # more negative moves left
                 adj_y_axis_label = -12.5, # more negative moves right
                 surv_plot_height = 0.75,
                 risk_table_height = 0.25
                 ) {


  #### Color palettes --------------------------------

  # sb_deep <- c("#4C72B0", "#55A868", "#C44E52",
  #              "#8172B2", "#CCB974", "#64B5CD")

  # grays <- c("black", "dark gray", "gray", "light gray", "white")
  grays <- gray.colors(9)

  if (blackandwhite == FALSE) {
    # col_palette <- sb_deep
    col_palette <- palette

  } else if (blackandwhite == TRUE) {
    col_palette <- grays

  }


  #### Sorting to be used in subsetting --------------------------------

  times <- seq(0, max(sfit$time), by = timeby)

  if (is.null(subs)) {
    if (length(levels(summary(sfit)$strata)) == 0) {
      subs1 <- 1
      subs2 <- 1:length(summary(sfit, censored = T)$time)
      subs3 <- 1:length(summary(sfit, times = times, extend = TRUE)$time)
    } else {
      subs1 <- 1:length(levels(summary(sfit)$strata))
      subs2 <- 1:length(summary(sfit, censored = T)$strata)
      subs3 <- 1:length(summary(sfit, times = times, extend = TRUE)$strata)
    }

  } else {
    for (i in 1:length(subs)) {
      if (i == 1) {
        ssvar <- paste("(?=.*\\b=", subs[i], sep = "")
      }

      if (i == length(subs)) {
        ssvar <- paste(ssvar, "\\b)(?=.*\\b=", subs[i], "\\b)", sep = "")
      }

      if (!i %in% c(1, length(subs))) {
        ssvar <- paste(ssvar, "\\b)(?=.*\\b=", subs[i], sep = "")
      }

      if (i == 1 & i == length(subs)) {
        ssvar <- paste("(?=.*\\b=", subs[i], "\\b)", sep = "")
      }

    }

    # The number of strata
    subs1 <- which(regexpr(pattern = ssvar,
                           text = levels(summary(sfit)$strata),
                           perl = T)
                   != -1)

    # The number of time points with the censoring times included
    subs2 <- which(regexpr(pattern = ssvar,
                           text = summary(sfit, censored = T)$strata,
                           perl = T)
                   != -1)

    # Not really sure here
    subs3 <- which(regexpr(pattern = ssvar,
                           text =
                             summary(sfit, times = times, extend = TRUE)$strata,
                           perl = T)
                   != -1)
  }

  if (!is.null(subs)) {
    pval <- FALSE

  }


  #### Data manipulation for plotting --------------------------------

  if (length(levels(summary(sfit)$strata)) == 0) {
    #[subs1]
    if (is.null(ystratalabs)) {
      ystratalabs <- as.character(sub("group=*", "", "All"))
    }

  } else {
    #[subs1]
    if (is.null(ystratalabs)) {
      ystratalabs <- as.character(sub("group=*", "", names(sfit$strata)))
    }
  }

  if (is.null(ystrataname)) {
    ystrataname <- "Strata"
  }

  m <- max(nchar(ystratalabs))

  times <- seq(0, max(sfit$time), by = timeby)

  if (length(levels(summary(sfit)$strata)) == 0) {
    Factor <- factor(rep("All", length(subs2)))
  } else {
    Factor <- factor(summary(sfit, censored = T)$strata[subs2])
  }


  ## Data to be used in the survival plot ----------------
  .df <- data.frame(
    time = sfit$time[subs2],
    n.risk = sfit$n.risk[subs2],
    n.event = sfit$n.event[subs2],
    n.censor = sfit$n.censor[subs2],
    surv = sfit$surv[subs2],
    strata = Factor,
    upper = sfit$upper[subs2],
    lower = sfit$lower[subs2]
  )

  ## Final changes to data for survival plot ----------------
  # levels(.df$strata) <- ystratalabs

  .df$strata <- factor(.df$strata,
                       levels = levels(.df$strata),
                       labels = ystratalabs)

  zeros <- data.frame(time = 0,
                      surv = 1,
                      strata = factor(ystratalabs,
                                      levels = levels(.df$strata)),
                      upper = 1,
                      lower = 1)

  .df <- dplyr::bind_rows(zeros, .df)

  d <- length(levels(.df$strata))


  #### Specify the plot --------------------------------

  if (is.na(subtitle)) {subtitle = NULL}

  ## Create plot shell and theme ----------------
  p <- ggplot(data = .df,
              aes(x = time, y = surv)) +
    theme_classic() +
    theme(plot.margin =
            unit(c(0, 1, 0.5, ifelse(m < 10, 1.5, 2.5)), "lines"),
          axis.title.x = element_text(vjust = -1),
          axis.title.y = element_text(vjust = adj_y_axis_label),
          # legend.position = c(ifelse(m <= 10, 0.85, 0.75),
          #                     ifelse(d < 4, 0.85, 0.8)),
          legend.position = legpos,
          legend.title = element_blank(),
          text = element_text(family = font_family),
          axis.text.x = element_text(colour = "black"),
          axis.text.y = element_text(colour = "black")
          ) +
    scale_x_continuous(breaks = times,
                       limits = xlims,
                       expand = c(0, 0)
                       ) +
    scale_y_continuous(labels = function(x) {100 * x},
                       limits = ylims,
                       expand = c(0, 0)
                       ) +
    labs(title = main,
         subtitle = subtitle,
         x = xlabs,
         y = ylabs)

  ## Assign linecolor and linetype by strata ---------------
  # Linecolor -- default is true
  # Linetype -- default is false
  if (linecolor == TRUE & linetype == TRUE) {
    p <- p +
      geom_step(aes(color = strata,
                    linetype = strata),
                size = 1.0,
                alpha = 0.7) +
      scale_color_manual(values = col_palette)
  } else if (linecolor == TRUE & linetype == FALSE) {
    p <- p +
      geom_step(aes(color = strata),
                size = 1.0,
                alpha = 0.7) +
      scale_color_manual(values = col_palette)
  } else if (linecolor == FALSE & linetype == TRUE) {
    p <- p +
      geom_step(aes(linetype = strata),
                size = 1.0,
                alpha = 0.7)
  }


  ## override aesthetic values (alpha) in the legend ----------------
  p <- p +
    guides(colour = guide_legend(override.aes = list(alpha = 1)))


  ## Removes the legend ----------------
  if (legend == FALSE) {
    p <- p + theme(legend.position = "none")
  }

  ## Add censoring marks to the line ----------------
  if (marks == TRUE && linecolor == TRUE) {
    p <- p +
      geom_point(data = subset(.df, n.censor >= 1),
                 aes(x = time, y = surv, color = strata),
                 shape = shape,
                 show.legend = FALSE)
    # + scale_color_manual(values = col_palette)
  } else if (marks == TRUE && linecolor == FALSE) {
    p <- p +
      geom_point(data = subset(.df, n.censor >= 1),
                 aes(x = time, y = surv),
                 shape = shape,
                 show.legend = FALSE)
  }


  #### p-value placement --------------------------------
  if (length(levels(summary(sfit)$strata)) == 0) {
    pval <- FALSE
  }

  if (pval == TRUE) {
    sdiff <-
      survival::survdiff(eval(sfit$call$formula), data = eval(sfit$call$data))
    pvalue <-
      pchisq(sdiff$chisq, length(sdiff$n) - 1, lower.tail = FALSE)
    pvaltxt <-
      ifelse(pvalue < pval_threshold,
             paste("italic(P)", "<", signif(pval_threshold, 3), sep = " "),
             paste("italic(P)", "==", signif(pvalue, 3), sep = " "))
             # paste("p <", signif(pval_threshold, 3)),
             # paste("p =", signif(pvalue, 3)))

    # MOVE P-VALUE LEGEND HERE BELOW [set x and y]
    p <- p +
      annotate("text",
               x = pvalpos[1],
               y = pvalpos[2],
               label = pvaltxt,
               # label = as.character(as.expression(pvaltxt)),
               family = font_family,
               parse = TRUE)

  } else if (pval == FALSE && !is.na(text_annotate)) {
    p <- p +
      annotate("text",
               x = pvalpos[1],
               y = pvalpos[2],
               label = text_annotate,
               family = font_family,
               parse = FALSE)

  }


  #### Create the at risk table --------------------------------

  if (length(levels(summary(sfit)$strata)) == 0) {
    Factor <- factor(rep("All", length(subs3)))
  } else {
    Factor <-
      factor(summary(sfit, times = times, extend = TRUE)$strata[subs3])
  }

  if (risk_table) {
    risk.data <- data.frame(
      strata = Factor,
      time = summary(sfit, times = times, extend = TRUE)$time[subs3],
      n.risk = summary(sfit, times = times, extend = TRUE)$n.risk[subs3]
    )

    # risk.data$strata <- factor(risk.data$strata,
    #                            levels = rev(levels(risk.data$strata)))

    risk.data$strata <- factor(risk.data$strata,
                               levels = levels(risk.data$strata),
                               labels = ystratalabs)

    data.table <- ggplot(data = risk.data,
                         aes(x = time,
                             # y = as.character(strata),
                             y = forcats::fct_rev(strata),
                             label = format(n.risk, nsmall = 0))) +
      geom_text(size = 3.25,
                family = font_family) +
      theme_classic() +
      scale_y_discrete("Number at risk",
                       # breaks = as.character(levels(risk.data$strata)),
                       # labels = rev(ystratalabs),
                       expand = c(0, 0)
                       ) +
      scale_x_continuous(breaks = times,
                         limits = xlims,
                         expand = c(0, 0)
                         )  +
      ggtitle("Number at risk") +
      theme(axis.title.x = element_text(vjust = 1),
            axis.title.y = element_blank(),
            axis.line = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            axis.text.y =
              element_text(margin = margin(t = 0, r = 12, b = 0, l = 0),
                           colour = "black"),
            legend.position = "none",
            plot.title = element_text(hjust = adj_table_title,
                                      size = 10,
                                      face = "bold"),
            text = element_text(family = font_family)) +
      labs(x = NULL,
           y = NULL)

  }


  #### Plotting the graphs --------------------------------

  if (risk_table) {

    gb <- ggplot2::ggplot_build(data.table)
    gt <- ggplot2::ggplot_gtable(gb)

    gt$layout$clip[gt$layout$name == "panel"] <- "off"
    data.table <- gt

    ggpubr::ggarrange(p, data.table,
                      # heights = c(2.5, .5),
                      # heights = c(3.5, .5),
                      # heights = c(4.0, 0.9),
                      heights = c(surv_plot_height,
                                  risk_table_height),
                      ncol = 1,
                      nrow = 2,
                      align = "v")
  } else {

    ggpubr::ggarrange(p,
                      heights = c(surv_plot_height),
                      ncol = 1,
                      nrow = 1,
                      align = "v")

  }

}

#### THE END --------------------------------

