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
distribution_plot <-
  function(dataset,
           var,
           type = "count",
           colsplit_by = "No split",
           rowsplit_by = "No split",
           colorsplit_by = "No split") {

    ## compute median and mean
    if ((colsplit_by != "No split") & (rowsplit_by != "No split")) {
      STATS <-
        dataset %>% group_by((!!sym(colsplit_by)), (!!sym(rowsplit_by))) %>%
        dplyr::summarize(Avg = mean((!!sym(var)),na.rm=TRUE), Median = median((!!sym(var)),na.rm=TRUE)) %>%
        tidyr::pivot_longer(Avg:Median, names_to = "Stat", values_to = "Value")

    } else if ((colsplit_by != "No split") &
               (rowsplit_by == "No split")) {
      STATS <- dataset %>% group_by((!!sym(colsplit_by))) %>%
        dplyr::summarize(Avg = mean((!!sym(var)),na.rm=TRUE), Median = median((!!sym(var)),na.rm=TRUE)) %>%
        tidyr::pivot_longer(Avg:Median, names_to = "Stat", values_to = "Value")

    }
    else if ((colsplit_by == "No split") &
             (rowsplit_by != "No split")) {
      STATS <- dataset %>% group_by((!!sym(rowsplit_by))) %>%
        dplyr::summarize(Avg = mean((!!sym(var)),na.rm=TRUE), Median = median((!!sym(var)),na.rm=TRUE)) %>%
        tidyr::pivot_longer(Avg:Median, names_to = "Stat", values_to = "Value")

    } else{
      STATS <- dataset %>%
        dplyr::summarize(Avg = mean((!!sym(var)),na.rm=TRUE), Median = median((!!sym(var)),na.rm=TRUE)) %>%
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
                     height = 470)

    return(gg)

  }


# bar plot for discrete variable
bar_plot <- function(subset_df,
                     var,
                     colsplit_by = "No split",
                     rowsplit_by = "No split",
                     colorsplit_by = "No split") {
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
    ggplot2::theme(legend.title = ggplot2::element_blank())+
    ggplot2::scale_fill_brewer(palette = "Accent")

  # convert to a plotly object
  gg <- plotly::ggplotly(gg) %>%
    plotly::layout(plot_bgcolor = '#e5ecf6',
                   height = 470)

  return(gg)
}


scatter_plot <-
  function(subset_df,
           var_x,
           var_y,
           colsplit_by = "No split",
           rowsplit_by = "No split",
           colorsplit_by = "No split"){




    # split by color
    if (colorsplit_by != "No split") {
      gg <-
        ggplot2::ggplot(subset_df, ggplot2::aes(x = (!!sym(var_x)), y = (!!sym(var_y)))) +
        ggplot2::geom_point(alpha = 0.7, ggplot2::aes(fill=factor((!!sym(colorsplit_by))),
                                                      color=factor((!!sym(colorsplit_by)))))
    } else {
      gg <-
        ggplot2::ggplot(subset_df, ggplot2::aes(x = (!!sym(var_x)), y = (!!sym(var_y)))) +
        ggplot2::geom_point(fill = '#67B7D1',
                            alpha = 0.7)
    }

    # add geom smooth, color scale and remove title
    gg <- gg +
      ggplot2::geom_smooth() +
      ggplot2::theme(legend.title = ggplot2::element_blank())+
      ggplot2::scale_fill_brewer(palette = "Accent")+
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
                     height = 470)

    return(gg)
  }


## UTILS for PROFIL module
prepare_radar_data <- function(dataset, splitby){

  if(splitby != "No split"){
    data <- dataset %>% dplyr::select((!!sym(splitby)),status, ssc_p, hsc_p, degree_p, etest_p, mba_p) %>%
      dplyr::group_by(!!sym(splitby), status) %>%
      dplyr::summarise_all(median)%>%
      dplyr::arrange(dplyr::desc(ssc_p))
  }else{
    data <- dataset %>% dplyr::select(status, ssc_p, hsc_p, degree_p, etest_p, mba_p) %>%
      dplyr::group_by(status) %>%
      dplyr::summarise_all(median)%>%
      dplyr::arrange(dplyr::desc(ssc_p))
  }

  return(data)
}

plot_radar2 <- function(data, splitby, value){

  if(splitby != "No split"){
    data <- data %>% filter((!!sym(splitby)) == value)
    col_num <- 2
  }else {
    col_num <- 1
  }


  fig <- plotly::plot_ly(
    type = 'scatterpolar',
    mode = "markers",
    r = as.numeric(data[1, -c(1:col_num)]),
    theta = colnames(data[,-c(1:col_num)]),
    fill = "toself",
    name = data[1, col_num]
  )

  for (row in 2:nrow(data)){
    fig <-
      fig %>% plotly::add_trace(
        r = as.numeric(data[row, -c(1:col_num)]),
        theta = colnames(data[,-c(1:col_num)]),
        fill = "toself",
        name = data[row, col_num]
      )

  }

  m <- 80
  fig <- fig %>% plotly::layout(
    margin = list(
      l = m,
      r = m,
      t = m,
      b = m
    ),
    polar = list(radialaxis = list(
      visible = T,
      range = c(0, 100),
      title = "Score"
    )),
    showlegend = F
  )


  return(fig)

}


mean_salary <- function(dataset, var, value) {
  if(var != 'No split'){
    data <- dataset %>%
      dplyr::filter((!!sym(var)) %in% value)

  } else {
    data <- dataset
  }

  salary <- data %>%
    dplyr::select(salary) %>%
    tidyr::drop_na() %>%
    dplyr::summarise_at(c('salary'), c(mean))

  return(sprintf("Mean salary: %sk₹", round(salary / 1000)))
}

max_salary <- function(dataset, var, value) {
  if(var != 'No split'){
    data <- dataset %>%
      dplyr::filter((!!sym(var)) %in% value)

  } else {
    data <- dataset
  }

  salary <- data %>%
    dplyr::select(salary) %>%
    tidyr::drop_na() %>%
    dplyr::summarise_at(c('salary'), c(max))

  return(sprintf("Max salary: %sk₹", round(salary / 1000)))
}

min_salary <- function(dataset, var, value) {
  if(var != 'No split'){
    data <- dataset %>%
      dplyr::filter((!!sym(var)) %in% value)

  } else {
    data <- dataset
  }

  salary <- data %>%
    dplyr::select(salary) %>%
    tidyr::drop_na() %>%
    dplyr::summarise_at(c('salary'), c(min))

  return(sprintf("Min salary: %sk₹", round(salary / 1000)))

}


employ_rate <- function(dataset, var, value) {

  if(var != 'No split'){
    data <- dataset %>%
      dplyr::filter((!!sym(var)) %in% value)

  } else {
    data <- dataset
  }

  subset_df <-data %>%
    dplyr::select(salary)

  not_employed <-  subset_df %>% is.na() %>% sum()
  all <- subset_df %>% nrow()
  return(paste(sprintf("Employment rate: %s", round((all - not_employed) / all * 100
  )), "%"))

}


# UTILS for TRACK module
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


plot_sankey <- function(sankey_data, sankey_ids) {
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
                                font = list(size = 10))

  return(fig)

}


plot_sankey_recursive <- function(dataset, stages) {
  student_stages <- c('hsc_s', 'degree_t', 'specialisation', 'status')
  sankey_ids = data.frame(name = c())
  sankey_data <- NULL
  if (length(stages) > 1) {
    for (i in seq(length(stages) - 1)) {
      where_name <- stages[i]
      to_name <- stages[i + 1]

      sankey_ids <-
        gen_sankey_id(dataset, where_name, to_name, sankey_ids)
      sankey_data <-
        gen_sankey_data(dataset, where_name, to_name, sankey_ids, sankey_data)
    }

    fig <- plot_sankey(sankey_data, sankey_ids)
    return(fig)
  }


}
