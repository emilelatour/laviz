

#### Continuous variables plot --------------------------------

plot_cont <-
  function(data,
           var,
           binw_select = "FD",
           subtitle = "Histogram (left), summary statistics (right)") {

    require(dplyr)
    require(tidyr)
    require(rlang)
    require(forcats)
    require(janitor)
    require(ggplot2)
    require(ggpubr)
    require(scales)


    var_enq <- rlang::enquo(var)
    var_name <- rlang::quo_name(var_enq)


    #### histogram --------------------------------

    #### Different ways to calc bin width --------------------------------

    calc_bin_width <- function(x, binw_select) {

      if (binw_select == "FD") {
        # Freedman-Diaconis (1981)
        pretty(range(x), n = nclass.FD(x), min.n = 1, right = TRUE)[[2]] -
          pretty(range(x), n = nclass.FD(x), min.n = 1, right = TRUE)[[1]]

      } else if (binw_select == "Sturges") {
        # Sturges (1926)
        pretty(range(x), n = nclass.Sturges(x), min.n = 1, right = TRUE)[[2]] -
          pretty(range(x), n = nclass.Sturges(x), min.n = 1, right = TRUE)[[1]]

      } else if (binw_select == "Scott") {
        # Scott (1979)
        pretty(range(x), n = nclass.scott(x), min.n = 1, right = TRUE)[[2]] -
          pretty(range(x), n = nclass.scott(x), min.n = 1, right = TRUE)[[1]]

      } else if (binw_select == "Square-root") {
        # Square-root (N/A)
        diff(pretty((max(x) - min(x)) / sqrt(length(x))))[[1]]

      } else if (binw_select == "Rice") {
        # Rice (1944)
        diff(pretty((max(x) - min(x)) / 2 * (length(x) ^ (1 / 3))))[[1]]

      }
    }

    bin_width <- data %>%
      dplyr::select(!! var_enq) %>%
      dplyr::filter(!is.na(!! var_enq)) %>%
      purrr::map_dbl(.x = .,
                     .f = ~ calc_bin_width(x = .x, binw_select = binw_select))

    ## Make the base plot ----------------

    p <- data %>%
      dplyr::filter(!is.na(!! var_enq)) %>%
      ggplot(data = ., ggplot2::aes_(x = var_enq)) +
      geom_histogram(stat = "bin",
                     binwidth = bin_width,
                     alpha = 0.8,
                     fill = "steelblue",
                     colour = "black") +
      theme_minimal() +
      labs(
        title = var_name,
        x = "",
        subtitle = subtitle
      )


    ## Plot it ----------------

    # print(p)
    # return(p)

    # plot it later ...


    #### Table of stats --------------------------------

    tbl <- data %>%
      dplyr::select(!! var_enq) %>%
      tidyr::gather(key = variable, value = value) %>%
      group_by(variable) %>%
      summarise(
        missing = sum(is.na(value)),
        observed = sum(!is.na(value)),
        n = n(),
        mean = round(mean(value, na.rm = TRUE), digits = 3),
        sd = round(sd(value, na.rm = TRUE), digits = 3),
        range = max(value, na.rm = TRUE) - min(value, na.rm = TRUE),
        min = min(value, na.rm = TRUE),
        p25 = quantile(value, probs = 0.25, na.rm = TRUE),
        median = quantile(value, probs = 0.5, na.rm = TRUE),
        p75 = quantile(value, probs = 0.75, na.rm = TRUE),
        max = max(value, na.rm = TRUE)
      ) %>%
      mutate_all(.tbl = ., .funs = funs(as.character)) %>%
      dplyr::select(-variable) %>%
      tidyr::gather(key = stat, value = value) %>%
      ggpubr::ggtexttable(rows = NULL, theme = ttheme(base_size = 9))


    #### Finish plot --------------------------------

    ## Arrange the plots on the same page ----------------

    ggpubr::ggarrange(
      p, tbl,
      # ncol = 1, nrow = 2,
      # heights = c(1, 0.5))
      ncol = 2, nrow = 1
    )
  }

#### Example of plot_cont() --------------------------------

# plot_cont(data = mtcars, var = disp)
# plot_cont(data = mtcars, var = disp, binw_select = "Sturges")
# plot_cont(data = mtcars, var = disp, binw_select = "Scott")
# plot_cont(data = mtcars, var = disp, binw_select = "Hadley")
#
# ggplot(data = mtcars, aes(x = disp)) +
#   geom_histogram()
#
# ggplot(data = mtcars, aes(x = disp)) +
#   geom_histogram(aes(y = ..density..), binwidth = 40) +
#   geom_density()


#### Categorical variables plot --------------------------------

plot_categ <-
  function(data,
           var,
           subtitle = paste0("Bar graph (left), ",
                             "frequency table of top 5 levels (right)")) {

    require(dplyr)
    require(tidyr)
    require(rlang)
    require(forcats)
    require(janitor)
    # Need to have the development version of ggplot2 installed
    # devtools::install_github("tidyverse/ggplot2", force = TRUE)
    require(ggplot2)
    require(ggpubr)
    require(scales)



    var_enq <- rlang::enquo(var)
    var_name <- rlang::quo_name(var_enq)


    #### Bar graph --------------------------------

    p2 <- data %>%
      # ggplot(data = data,
      #   ggplot2::aes_(x = forcats::fct_rev(forcats::fct_infreq(var_quo)))) +
      mutate(
        !! var_name := as.character(!! var_enq),
        !! var_name := factor(!! var_enq),
        !! var_name := forcats::fct_explicit_na(!! var_enq, na_level = "NA"),
        !! var_name := forcats::fct_infreq(!! var_enq),
        !! var_name := forcats::fct_rev(!! var_enq)
      ) %>%
      ggplot(
        data = .,
        ggplot2::aes_(x = var_enq)
      ) +
      geom_bar(
        stat = "count",
        width = 0.75,
        alpha = 0.8,
        fill = "gray33",
        color = "black"
      ) +
      theme_minimal() +
      # theme(axis.text.x = element_text(angle = 90,
      #                                  hjust = 1,
      #                                  vjust = 0.5
      #                                  )) +
      # scale_x_discrete(label = function(x) abbreviate(x, minlength=7)) +
      scale_x_discrete(label = function(x) strtrim(x, width = 10)) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(),
        # expand = c(0, 0),
        expand = c(0, 0, 0.05, 0),
        limits = c(0, NA)
      ) +
      labs(
        title = var_name,
        subtitle = subtitle,
        x = ""
      ) +
      coord_flip()


    #### Frequency table --------------------------------

    tbl2 <- data %>%
      mutate(
        !! var_name := as.character(!! var_enq),
        !! var_name := factor(!! var_enq),
        !! var_name := forcats::fct_lump(!! var_enq, n = 5),
        !! var_name := forcats::fct_infreq(!! var_enq)
      ) %>%
      janitor::tabyl(!! var_enq) %>%
      janitor::adorn_totals("row") %>%
      janitor::adorn_pct_formatting() %>%
      dplyr::rename("levels" = !! names(.[1])) %>%
      mutate(levels = strtrim(levels, width = 36)) %>%
      ggpubr::ggtexttable(rows = NULL, theme = ttheme(base_size = 9))


    #### Finish plot --------------------------------

    ## Arrange the plots on the same page ----------------

    ggpubr::ggarrange(
      p2, tbl2,
      # ncol = 1, nrow = 2,
      # heights = c(1, 0.5))
      ncol = 2, nrow = 1
    )
  }

#### Example of plot_cont() --------------------------------

# mt2 <- mtcars %>%
#   mutate(cyl = factor(cyl))
#
# plot_categ(data = mt2, var = cyl)

# dplyr::glimpse(mtcars)


#### make_plots() --------------------------------

# prints a plot depending on the class of variables

make_plots <- function(df){

  # for (i in names(df)) {
  #   if (class(df[[i]])[1] %in% c("ordered", "factor", "character"))
  #     print(plot_categ(data = df, var = !! sym(i)))
  #   else if (class(df[[i]])[1] %in% c("numeric", "integer"))
  #     print(plot_cont(data = df, var = !! sym(i)))
  #   else
  #     print(paste0("Variable is of class `", class(df[[i]]), "`,
  #                  not a `factor` or `numeric`."))
  # }

  varClasses <- lapply(df, class)

  for (i in names(df)) {
    if (any(varClasses[[i]] %in% c("factor", "ordered", "logical", "character",
                       "labelled")))
      print(plot_categ(data = df, var = !! sym(i)))
    else if (any(varClasses[[i]] %in% c("numeric", "integer")))
      print(plot_cont(data = df, var = !! sym(i)))
    else
      print(paste0("Variable is of class `", class(df[[i]]), "`,
                   not a `factor` or `numeric`."))
  }

}


#### Example of make_plots() --------------------------------

# make_plots(diamonds)


#### make_plots2() --------------------------------

# prints a plot depending on the class of variables

# make_plots2 <- function(data) {
#
#   if (class(x)[1] %in% c("ordered", "factor", "character"))
#     print(plot_categ(data = x, var = x[, 1]))
#   else if (class(x)[1] %in% c("numeric", "integer"))
#     print(plot_cont(data = x, var = x[, 1]))
#   else
#     print(paste0("Variable is of class `", class(x), "`,
#                    not a `factor` or `numeric`."))
#
# }
#
# library(ggplot2)
# library(dplyr)
# make_plots2(diamonds$carat)
#
# plot_categ(data = diamonds, var = cut)
#
# glimpse(diamonds)
#
# df <- diamonds %>%
#   dplyr::select(cut, color, clarity) %>%
#   mutate(cut = factor(cut))
# map(.x = df, .f = ~ plot_categ(.x[["cut"]]))



