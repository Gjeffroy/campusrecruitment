#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

library('dplyr')
library('ggplot2')
library('plotly')



## UTILS for EXPLORE module
# distribution plot for continuous variable


#' Create a distribution plot
#'
#' Create a distribution plot using ggplot and plotly for a given var
#' and possibility to split on row, col and color.
#' Median and mean are automatically added as vertical lines
#' @param dataset An object of class "data.frame", the dataset to plot
#' @param var An object of class "string", name of the variable to plot (must be numerical)
#' @param type An object of class "string", plot type ("count", "density")
#' @param colsplit_by (optional) An object of class "string", name of the variable to use in facetgrid col (must be categorical)
#' @param rowsplit_by (optional) An object of class "string", name of the variable to use in facetgrid row (must be categorical)
#' @param colorsplit_by (optional) An object of class "string", name of the variable to spli by color (must be categorical)
#' @return Returns an object of class "plotly". the function create a distribution plot based on input
#'
#' @examples
#' distribution_plot(dataset, "hsc_p","count",rowsplit_by = "gender")
#'
distribution_plot <-
  function(dataset,
           var,
           type = "count",
           colsplit_by = "No split",
           rowsplit_by = "No split",
           colorsplit_by = "No split",
           plot_height = 470) {

    # check that var is defined
    validate(need(var, ''))

    # prevent splitting on same variable for col and row
    if (colsplit_by != "No split" & rowsplit_by != "No split") {
      validate(
        need(
          colsplit_by != rowsplit_by,
          'Choose different variables to split by row and column'
        ),
      )
    }

    ## compute median and mean
    if ((colsplit_by != "No split") & (rowsplit_by != "No split")) {
      STATS <-
        dataset %>% group_by((!!sym(colsplit_by)), (!!sym(rowsplit_by))) %>%
        dplyr::summarize(Avg = mean((!!sym(var)), na.rm = TRUE),
                         Median = median((!!sym(var)), na.rm = TRUE)) %>%
        tidyr::pivot_longer(Avg:Median, names_to = "Stat", values_to = "Value")

    } else if ((colsplit_by != "No split") &
               (rowsplit_by == "No split")) {
      STATS <- dataset %>% group_by((!!sym(colsplit_by))) %>%
        dplyr::summarize(Avg = mean((!!sym(var)), na.rm = TRUE),
                         Median = median((!!sym(var)), na.rm = TRUE)) %>%
        tidyr::pivot_longer(Avg:Median, names_to = "Stat", values_to = "Value")

    }
    else if ((colsplit_by == "No split") &
             (rowsplit_by != "No split")) {
      STATS <- dataset %>% group_by((!!sym(rowsplit_by))) %>%
        dplyr::summarize(Avg = mean((!!sym(var)), na.rm = TRUE),
                         Median = median((!!sym(var)), na.rm = TRUE)) %>%
        tidyr::pivot_longer(Avg:Median, names_to = "Stat", values_to = "Value")

    } else{
      STATS <- dataset %>%
        dplyr::summarize(Avg = mean((!!sym(var)), na.rm = TRUE),
                         Median = median((!!sym(var)), na.rm = TRUE)) %>%
        tidyr::pivot_longer(Avg:Median, names_to = "Stat", values_to = "Value")
    }

    ## split by color
    if (colorsplit_by != "No split") {
      gg <-
        ggplot2::ggplot(dataset, ggplot2::aes(x = (!!sym(var)), fill = (!!sym(colorsplit_by))))
    } else {
      gg <- ggplot2::ggplot(dataset,  ggplot2::aes(x = (!!sym(var))))
    }

    # density or count
    if (type == 'count') {
      if (colorsplit_by != "No split") {
        gg <- gg +
          ggplot2::geom_histogram(aes(
            y = (..count..),
            bins = 20,
            alpha = 0.5
          ))
      } else {
        gg <- gg +
          ggplot2::geom_histogram(fill = '#67B7D1',
                                  aes(
                                    y = (..count..),
                                    bins = 20,
                                    alpha = 0.5
                                  ))
      }
    } else {
      gg <- gg +
        ggplot2::geom_density(alpha = 0.5)
    }

    # customize legend, color scale and add stats
    gg <- gg +
      ggplot2::geom_rug(color = '#67B7D1') +
      ggplot2::theme(legend.title = ggplot2::element_blank()) +
      ggplot2::scale_fill_brewer(palette = "Accent") +
      ggplot2::geom_vline(data = STATS,
                          mapping = ggplot2::aes(xintercept = Value, color = Stat))

    ## facet grid, split by col or / and row
    if ((colsplit_by != "No split") & (rowsplit_by == "No split")) {
      gg <- gg + facet_grid(cols = ggplot2::vars(!!sym(colsplit_by)))
    }

    if ((colsplit_by == "No split") & (rowsplit_by != "No split")) {
      gg <- gg + facet_grid(rows = ggplot2::vars(!!sym(rowsplit_by)))
    }
    if ((colsplit_by != "No split") & (rowsplit_by != "No split")) {
      gg <- gg + facet_grid(rows = ggplot2::vars(!!sym(rowsplit_by)),
                            cols = ggplot2::vars(!!sym(colsplit_by)))
    }

    ## convert to a plotly object
    gg <- plotly::ggplotly(gg) %>%
      plotly::layout(plot_bgcolor = '#e5ecf6',
                     height = plot_height)

    return(gg)

  }



#' Create a bar plot
#'
#' Create a bar plot using ggplot and plotly for a given variable
#' and possibility to split on row, col and color.
#' @param dataset An object of class "data.frame", the dataset to plot
#' @param var An object of class "string", name of the variable to plot (must be categorical)
#' @param colsplit_by (optional) An object of class "string", name of the variable to use in facetgrid col (must be categorical)
#' @param rowsplit_by (optional) An object of class "string", name of the variable to use in facetgrid row (must be categorical)
#' @param colorsplit_by (optional) An object of class "string", name of the variable to spli by color (must be categorical)
#' @return Returns an object of class "plotly". the function create a bar plot based on input
#'
#' @examples
#' bar_plot(dataset, "workex", colsplit_by = "gender")
#'
bar_plot <- function(subset_df,
                     var,
                     colsplit_by = "No split",
                     rowsplit_by = "No split",
                     colorsplit_by = "No split",
                     plot_height = 470) {


  # check that var is defined
  validate(need(var, ''))

  # prevent splitting on same variable for col and row
  if (colsplit_by != "No split" & rowsplit_by != "No split") {
    validate(
      need(
        colsplit_by != rowsplit_by,
        'Choose different variables to split by row and column'
      ),
    )
  }


  # color split
  if (colorsplit_by != "No split") {
    gg <- ggplot2::ggplot(subset_df, ggplot2::aes(x = (!!sym(var)),
                                                  fill = factor((
                                                    !!sym(colorsplit_by)
                                                  )))) +
      ggplot2::geom_bar(ggplot2::aes(y = ..count..),
                        alpha = 0.5,
                        position = "dodge")
  } else {
    gg <- ggplot2::ggplot(subset_df, ggplot2::aes(x = (!!sym(var)))) +
      ggplot2::geom_bar(
        ggplot2::aes(y = ..count..),
        fill = '#67B7D1',
        alpha = 0.5,
        position = "dodge"
      )
  }

  ## facet grid, split by col or / and row
  if ((colsplit_by != "No split") & (rowsplit_by == "No split")) {
    gg <- gg + facet_grid(cols = ggplot2::vars(!!sym(colsplit_by)))
  }

  if ((colsplit_by == "No split") & (rowsplit_by != "No split")) {
    gg <- gg + facet_grid(rows = ggplot2::vars(!!sym(rowsplit_by)))
  }
  if ((colsplit_by != "No split") & (rowsplit_by != "No split")) {
    gg <- gg + facet_grid(rows = ggplot2::vars(!!sym(rowsplit_by)),
                          cols = ggplot2::vars(!!sym(colsplit_by)))
  }

  # customize label and legend and color scale
  gg <- gg +
    ggplot2::theme(legend.title = ggplot2::element_blank()) +
    ggplot2::scale_fill_brewer(palette = "Accent")

  # convert to a plotly object
  gg <- plotly::ggplotly(gg) %>%
    plotly::layout(plot_bgcolor = '#e5ecf6',
                   height = plot_height)

  return(gg)
}

#' Create a scatter plot
#'
#' Create a scatter plot using ggplot and plotly for x and y given variables and possibility to split on row, col and color
#' @param dataset An object of class "data.frame", the dataset to plot
#' @param var_x An object of class "string", name of the variable to plot on x axis (must be numerical)
#' @param var_y An object of class "string", name of the variable to plot on y axis (must be numerical)
#' @param colsplit_by (optional) An object of class "string", name of the variable to use in facetgrid col (must be categorical)
#' @param rowsplit_by (optional) An object of class "string", name of the variable to use in facetgrid row (must be categorical)
#' @param colorsplit_by (optional) An object of class "string",  name of the variable to spli by color (must be categorical)
#' @return Returns an object of class "plotly". the function create a scatter plot based on input
#'
#' @examples
#' scatter_plot(dataset, "hsc_s", "salary", colorsplit_by = "gender")
#'
scatter_plot <-
  function(subset_df,
           var_x,
           var_y,
           colsplit_by = "No split",
           rowsplit_by = "No split",
           colorsplit_by = "No split",
           plot_height = 470) {
    # make sure var x and y are defined
    # otherwise cause a error message to display due to dynamic input generation in the UI
    validate(need(var_x, ''),
             need(var_y, ''))

    # prevent splitting on same variable for col and row
    if (colsplit_by != "No split" & rowsplit_by != "No split") {
      validate(
        need(
          colsplit_by != rowsplit_by,
          'Choose different variables to split by row and column'
        ),
      )
    }

    # split by color
    if (colorsplit_by != "No split") {
      gg <-
        ggplot2::ggplot(subset_df, ggplot2::aes(x = (!!sym(var_x)), y = (!!sym(var_y)))) +
        ggplot2::geom_point(alpha = 0.7, ggplot2::aes(fill = factor((
          !!sym(colorsplit_by)
        )),
        color = factor((
          !!sym(colorsplit_by)
        ))))
    } else {
      gg <-
        ggplot2::ggplot(subset_df, ggplot2::aes(x = (!!sym(var_x)), y = (!!sym(var_y)))) +
        ggplot2::geom_point(fill = '#67B7D1',
                            alpha = 0.7)
    }

    # add geom smooth, color scale and remove title
    gg <- gg +
      ggplot2::geom_smooth() +
      ggplot2::theme(legend.title = ggplot2::element_blank()) +
      ggplot2::scale_fill_brewer(palette = "Accent") +
      ggplot2::scale_color_brewer(palette = "Accent")

    ## facet grid, split by col or / and row
    if ((colsplit_by != "No split") & (rowsplit_by == "No split")) {
      gg <- gg + facet_grid(cols = ggplot2::vars(!!sym(colsplit_by)))
    }

    if ((colsplit_by == "No split") & (rowsplit_by != "No split")) {
      gg <- gg + facet_grid(rows = ggplot2::vars(!!sym(rowsplit_by)))
    }
    if ((colsplit_by != "No split") & (rowsplit_by != "No split")) {
      gg <- gg + facet_grid(rows = ggplot2::vars(!!sym(rowsplit_by)),
                            cols = ggplot2::vars(!!sym(colsplit_by)))
    }

    # convert to plotly object
    gg <- plotly::ggplotly(gg) %>%
      plotly::layout(plot_bgcolor = '#e5ecf6',
                     height = plot_height)

    return(gg)
  }


## UTILS for PROFIL module

#' Prepare the data for radar plot
#'
#' Prepare the data for radar plot by selecting the right columns
#' grouping by the split_by var and computing the median
#' It has an option 'no_split' in case the used want a generaa profil
#' @param dataset An object of class "data.frame", the dataset to plot
#' @param split_by An object of class "string", name of the variable to group by the data before computing the median (must be categorical)
#' @return Returns an object of class "data.frame". Median for each group and all numerical variables
#'
#' @examples
#' prepare_radar_data(dataset, "gender")
prepare_radar_data <- function(dataset, splitby) {
  if (splitby != "No split") {
    data <-
      dataset %>% dplyr::select((!!sym(splitby)), status, ssc_p, hsc_p, degree_p, etest_p, mba_p) %>%
      dplyr::group_by(!!sym(splitby), status) %>%
      dplyr::summarise_all(median) %>%
      dplyr::arrange(dplyr::desc(ssc_p))
  } else{
    data <-
      dataset %>% dplyr::select(status, ssc_p, hsc_p, degree_p, etest_p, mba_p) %>%
      dplyr::group_by(status) %>%
      dplyr::summarise_all(median) %>%
      dplyr::arrange(dplyr::desc(ssc_p))
  }

  return(data)
}


#' create a radar plot
#'
#' create a radar plot using the data from prepare_radar_data()
#' grouping by the split_by var and filtering on value
#' It has an option 'no_split' in case the used want a general profil
#' @param dataset An object of class "data.frame", the dataset to plot
#' @param split_by An object of class "string", name of the variable used in prepare_radar_data (must be categorical)
#' @param value to An object of class "string", filter on a category in the split_by column
#' @return Returns an object of class "plotly". the radar plot for the split_by category
#'
#' @examples
#' plot_radar(dataset, "gender", "M")
plot_radar <- function(data, splitby, value, plot_height = "200px") {
  if (splitby != "No split") {
    data <- data %>% filter((!!sym(splitby)) == value)
    col_num <- 2
  } else {
    col_num <- 1
  }


  fig <- plotly::plot_ly(
    type = 'scatterpolar',
    mode = "markers",
    r = as.numeric(data[1, -c(1:col_num)]),
    theta = colnames(data[, -c(1:col_num)]),
    fill = "toself",
    name = data[1, col_num],
    height = plot_height
  )

  for (row in 2:nrow(data)) {
    fig <-
      fig %>% plotly::add_trace(
        r = as.numeric(data[row, -c(1:col_num)]),
        theta = colnames(data[, -c(1:col_num)]),
        fill = "toself",
        name = data[row, col_num]
      )

  }

  m <- 15
  fig <- fig %>% plotly::layout(
    margin = list(
      l = m,
      r = m,
      t = m,
      b = 40
    ),
    polar = list(radialaxis = list(
      visible = T,
      range = c(0, 100),
      title = "Score"
    )),
    showlegend = T,
    legend = list(
      orientation = "h",
      # show entries horizontally
      xanchor = "center",
      # use center of legend as anchor
      x = 0.5
    )
  )


  return(fig)

}

#' compute the mean salary
#'
#' compute the mean salary for the category given by value in split_by
#' It has an option 'no_split' in case the used want a general profil
#' @param dataset An object of class "data.frame", the dataset to plot
#' @param split_by An object of class "string", name of the variable to group_by before computing (must be categorical)
#' @param value to An object of class "string", filter on a category in the split_by column
#' @return Returns an object of class "string", with the mean salary in it
#'
#' @examples
#' mean_salary(dataset, "gender", "M")
mean_salary <- function(dataset, split_by, value) {
  if (split_by != 'No split') {
    data <- dataset %>%
      dplyr::filter((!!sym(split_by)) %in% value)

  } else {
    data <- dataset
  }

  salary <- data %>%
    dplyr::select(salary) %>%
    tidyr::drop_na() %>%
    dplyr::summarise_at(c('salary'), c(mean))

  return(sprintf("Mean salary: %sk₹", round(salary / 1000)))
}


#' compute the max salary
#'
#' compute the max salary for the category given by value in split_by
#' It has an option 'no_split' in case the used want a general profil
#' @param dataset An object of class "dataframe", the dataset to plot
#' @param split_by An object of class "string", name of the variable to group_by before computing (must be categorical)
#' @param value to An object of class "string", filter on a category in the split_by column
#' @return Returns an object of class "string", with the max salary in it
#'
#' @examples
#' max_salary(dataset, "gender", "M")
max_salary <- function(dataset, split_by, value) {
  if (split_by != 'No split') {
    data <- dataset %>%
      dplyr::filter((!!sym(split_by)) %in% value)

  } else {
    data <- dataset
  }

  salary <- data %>%
    dplyr::select(salary) %>%
    tidyr::drop_na() %>%
    dplyr::summarise_at(c('salary'), c(max))

  return(sprintf("Max salary: %sk₹", round(salary / 1000)))
}

#' compute the min salary
#'
#' compute the min salary for the category given by value in split_by
#' It has an option 'no_split' in case the used want a general profil
#' @param dataset An object of class "data.frame", the dataset to plot
#' @param split_by An object of class "string", name of the variable to group_by before computing (must be categorical)
#' @param value to An object of class "string", filter on a category in the split_by column
#' @return Returns an object of class "string", with the min salary in it
#'
#' @examples
#' min_salary(dataset, "gender", "M")
min_salary <- function(dataset, split_by, value) {
  if (split_by != 'No split') {
    data <- dataset %>%
      dplyr::filter((!!sym(split_by)) %in% value)

  } else {
    data <- dataset
  }

  salary <- data %>%
    dplyr::select(salary) %>%
    tidyr::drop_na() %>%
    dplyr::summarise_at(c('salary'), c(min))

  return(sprintf("Min salary: %sk₹", round(salary / 1000)))

}

#' compute the employment rate
#'
#' compute the employment rate for the category given by value in split_by
#' It has an option 'no_split' in case the used want a general profil
#' @param dataset An object of class "data.frame", the dataset to plot
#' @param split_by An object of class "string", name of the variable to group_by before computing (must be categorical)
#' @param value An object of class "string", to filter on a category in the split_by column
#' @return Returns an object of class "string", with the employment rate in it
#'
#' @examples
#' employ_rate(dataset, "gender", "M")
employ_rate <- function(dataset, split_by, value) {
  if (split_by != 'No split') {
    data <- dataset %>%
      dplyr::filter((!!sym(split_by)) %in% value)

  } else {
    data <- dataset
  }

  subset_df <- data %>%
    dplyr::select(salary)

  not_employed <-  subset_df %>% is.na() %>% sum()
  all <- subset_df %>% nrow()
  return(paste(sprintf("Employment rate: %s", round((all - not_employed) / all * 100
  )), "%"))

}


# UTILS for TRACK module
#' generate id for each categories
#'
#' generate id for each categories used in the sankey plot, id start from 0 to n-1 categories
#' the function can take a data.frame containing existing id in order to not add them twice
#' @param data An object of class "data.frame", the dataset to plot
#' @param where_var An object of class "string", variable to place on the left side of the sankey plot
#' @param to_var An object of class "string", variable to place on the right side of the sankey plot
#' @param existing_ids An object of class "data.frame", containing existing id in order to not add them twice
#' @return Returns an object of class "dataframe", a matching table between
#' categories in where_var, to_var and the given ids
#'
#' @examples
#' gen_sankey_id(dataset, "hsc_b", "status", existing_ids)
gen_sankey_id <-
  function(data,
           where_var,
           to_var,
           existing_ids = data.frame(name = c())) {
    count_where_to <-
      data %>% dplyr::group_by((!!sym(where_var)), (!!sym(to_var))) %>% count()

    where_to_id <-
      data.frame(name = c(unique(count_where_to[[where_var]]),
                          unique(count_where_to[[to_var]])))
    where_to_id <-
      where_to_id %>% filter(!name %in% existing_ids$name)

    where_to_id$id <-
      seq.int(nrow(where_to_id)) - 1 + nrow(existing_ids)

    return(rbind(where_to_id, existing_ids) %>% dplyr::arrange(id))
  }


#' generate the data use by the sankey plot
#'
#' generate id for each categories used in the sankey plot, id start from 0 to n-1 categories
#' the function can take a data.frame containing the data for the sankey plot
#' @param data An object of class "data.frame", the dataset to plot
#' @param where_var An object of class "string", variable to place on the left side of the sankey plot
#' @param to_var An object of class "string",  variable to place on the right side of the sankey plot
#' @param where_to_id An object of class "data.frame", the matching table from gen_sankey_id
#' @param existing_data An object of class "data.frame", generated by a previous call of this function to be stacked with new data
#' @return Returns an object of class "dataframe", the data to be used in sankey plot
#'
#' @examples
#' gen_sankey_data(dataset, "hsc_b", "status", where_to_id, existing_data)
gen_sankey_data <-
  function(data,
           where_var,
           to_var,
           where_to_id,
           existing_data = NULL) {
    count_where_to <-
      data %>% dplyr::group_by((!!sym(where_var)), (!!sym(to_var))) %>% count()


    sankey_data <- count_where_to %>%
      dplyr::left_join(where_to_id, by = dplyr::join_by((!!sym(where_var)) == name)) %>%
      dplyr::rename(where_id = id) %>%
      dplyr::rename(where_name = !!sym(where_var)) %>%
      dplyr::left_join(where_to_id, by = dplyr::join_by((!!sym(to_var)) == name)) %>%
      dplyr::rename(to_id = id) %>%
      dplyr::rename(to_name = !!sym(to_var))


    if (is.null(existing_data)) {
      return(sankey_data)
    } else {
      rbind(existing_data, sankey_data)
    }
  }


#' generate the sankey plot
#'
#' generate the sankey plot based on the data and the ids generated by gen_sankey_data
#' and gen_sankey_ids
#' @param sankey_data An object of class "data.frame", the dataset to plot
#' @param sankey_ids An object of class "data.frame", the matching table between categories and ids
#' @return Returns an object of class "plotly", the sankey plot
#'
#' @examples
#' plot_sankey(sankey_data, sankey_ids)
plot_sankey <-
  function(sankey_data, sankey_ids, plot_height = "470px") {
    fig <- plotly::plot_ly(
      type = "sankey",
      orientation = "h",

      node = list(
        label = sankey_ids$name,

        pad = 15,
        thickness = 20,
        line = list(color = "black",
                    width = 0.5)
      ),

      link = list(
        source = sankey_data$where_id,
        target = sankey_data$to_id,
        value =  sankey_data$n
      )
    )
    fig <- fig %>% plotly::layout(title = "",
                                  font = list(size = 10),
                                  height = plot_height)

    return(fig)

  }

#' generate the sankey plot on multiple steps
#'
#' combine gen_sankey_ids, gen_sankey_data and plot_sankey to compute a sankey plot
#' on multiple stages
#' @param dataset An object of class "data.frame", the dataset to plot
#' @param stages An object of class "vector", all the stage to use (where and to variables)
#' @return Returns an object of class "plotly", the sankey plot for multiple stages
#'
#' @examples
#' plot_sankey_recursive(dataset, c("hsc_b", "workex", "status"))
plot_sankey_recursive <-
  function(dataset, stages, plot_height = "470px") {
    student_stages <- c('hsc_s', 'degree_t', 'specialisation', 'status')
    sankey_ids = data.frame(name = c())
    sankey_data <- NULL

    validate(need(length(stages) > 1, 'Choose at least two stages to visualize'),)

    for (i in seq(length(stages) - 1)) {
      where_name <- stages[i]
      to_name <- stages[i + 1]

      sankey_ids <-
        gen_sankey_id(dataset, where_name, to_name, sankey_ids)
      sankey_data <-
        gen_sankey_data(dataset, where_name, to_name, sankey_ids, sankey_data)
    }

    fig <-
      plot_sankey(sankey_data, sankey_ids, plot_height = plot_height)
    return(fig)



  }
