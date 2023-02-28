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
salary_distribution_plot <- function(dataset, subset_df) {
  max_salary = max(dataset$salary, na.rm = T)
  upper = max_salary - max_salary %% 100000 + 100000
  mean_salary = mean(subset_df$salary, na.rm = T)
  median_salary = median(subset_df$salary, na.rm = T)

  gg <- ggplot2::ggplot(subset_df, ggplot2::aes(x = salary)) +
    ggplot2::geom_histogram(
      aes(y = ..count..),
      bins = 20,
      binwidth = 100000 / 2,
      fill = '#67B7D1',
      alpha = 0.5
    ) +
    ggplot2::geom_rug(color = '#67B7D1') +
    ggplot2::geom_vline(
      xintercept = mean_salary,
      size = 0.3,
      color = "red",
      linetype = "twodash"
    ) +
    ggplot2::geom_vline(
      xintercept = median_salary,
      size = 0.5,
      color = "red",
      linetype = "longdash"
    ) +
    ggplot2::ylab("") +
    ggplot2::xlab("")  + ggplot2::theme(legend.title = ggplot2::element_blank()) +
    ggplot2::scale_color_manual(values = c('density' = '#67B7D1')) +
    ggplot2::scale_y_continuous(breaks = seq(0, 70, 5)) +
    ggplot2::scale_x_continuous(breaks = seq(0, upper, 100000))


  gg <- plotly::ggplotly(gg) %>%
    plotly::layout(
      plot_bgcolor = '#e5ecf6',
      xaxis = list(
        title = 'Salary (in ₹)',
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        gridcolor = 'ffff',
        range = c('0', upper)
      ),

      yaxis = list(
        tickformat = ".2e",
        title = 'count',
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        gridcolor = 'ffff',
        range = c('0', '70')
      )
    )
  return(gg)
}


workex_plot <- function(subset_df) {
  gg <- ggplot2::ggplot(subset_df, ggplot2::aes(x = status)) +
    ggplot2::geom_bar(ggplot2::aes(y = ..count..),
                      fill = '#67B7D1',
                      alpha = 0.5) +
    ggplot2::ylab("") +
    ggplot2::xlab("") + ggplot2::theme(legend.title = ggplot2::element_blank()) +
    ggplot2::scale_color_manual(values = c('density' = '#67B7D1'))


  gg <- plotly::ggplotly(gg) %>%
    plotly::layout(
      plot_bgcolor = '#e5ecf6',
      xaxis = list(
        title = 'Found a job',
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        gridcolor = 'ffff'
      ),

      yaxis = list(
        tickformat = ".2e",
        title = 'count',
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        gridcolor = 'ffff'
      )
    )
  return(gg)
}



etest_distribution_plot <- function(subset_df) {
  mean_score = mean(subset_df$etest_p, na.rm = T)
  median_score = median(subset_df$etest_p, na.rm = T)



  gg <- ggplot2::ggplot(subset_df, ggplot2::aes(x = etest_p)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ..count..),
      bins = 10,
      binwidth = 10,
      fill = '#67B7D1',
      alpha = 0.5
    ) +
    ggplot2::geom_rug(color = '#67B7D1') +
    ggplot2::geom_vline(
      xintercept = mean_score,
      size = 0.3,
      color = "red",
      linetype = "twodash"
    ) +
    ggplot2::geom_vline(
      xintercept = median_score,
      size = 0.5,
      color = "red",
      linetype = "longdash"
    ) +
    ggplot2::ylab("") +
    ggplot2::xlab("")  + ggplot2::theme(legend.title = ggplot2::element_blank()) +
    ggplot2::scale_color_manual(values = c('density' = '#67B7D1')) +
    ggplot2::scale_y_continuous(breaks = seq(0, 70, 5)) +
    ggplot2::scale_x_continuous(breaks = seq(0, 100, 10))


  gg <- plotly::ggplotly(gg) %>%
    plotly::layout(
      plot_bgcolor = '#e5ecf6',
      xaxis = list(
        title = 'Employability score',
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        gridcolor = 'ffff',
        range = c('0', '100')
      ),

      yaxis = list(
        tickformat = ".2e",
        title = 'count',
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        gridcolor = 'ffff',
        range = c('0', '70')
      )
    )
  return(gg)
}


## UTILS for PROFIL module
plot_radar <- function(stud_prof, edu) {
  stud_prof_filt <- stud_prof %>%
    dplyr::filter(hsc_s == edu,
                  stat == 'median') %>%
    dplyr::ungroup() %>%
    dplyr::arrange(status) %>%
    dplyr::select(-c(hsc_s, stat)) %>%
    dplyr::group_by(gender, status) %>%
    dplyr::summarise_all(mean) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(status, gender, salary))

  fig <- plotly::plot_ly(
    type = 'scatterpolar',
    r = as.numeric(stud_prof_filt[2, ]),
    theta = colnames(stud_prof_filt),
    fill = 'toself',
    name = "Place"
  )

  fig <-
    fig %>% plotly::add_trace(
      r = as.numeric(stud_prof_filt[1, ]),
      theta = colnames(stud_prof_filt),
      fill = 'toself',
      name = "Not place"
    )

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


mean_salary <- function(dataset, edu) {
  salary <- dataset %>%
    dplyr::filter(hsc_s == edu) %>%
    dplyr::select(salary) %>%
    tidyr::drop_na() %>%
    dplyr::summarise_at(c('salary'), c(mean))

  return(sprintf("Mean salary: %sk₹", round(salary / 1000)))
}

max_salary <- function(dataset, edu) {
  salary <- dataset %>%
    dplyr::filter(hsc_s == edu) %>%
    dplyr::select(salary) %>%
    tidyr::drop_na() %>%
    dplyr::summarise_at(c('salary'), c(max))

  return(sprintf("Max salary: %sk₹", round(salary / 1000)))
}

min_salary <- function(dataset, edu) {
  salary <- dataset %>%
    dplyr::filter(hsc_s == edu) %>%
    dplyr::select(salary) %>%
    tidyr::drop_na() %>%
    dplyr::summarise_at(c('salary'), c(min))

  return(sprintf("Min salary: %sk₹", round(salary / 1000)))

}


employ_rate <- function(dataset, edu) {
  subset_df <- dataset %>%
    dplyr::filter(hsc_s == edu) %>%
    dplyr::select(salary)

  not_employed <-  subset_df %>% is.na() %>% sum()
  all <- subset_df %>% nrow()
  return(paste(sprintf("Employment rate: %s", round((all - not_employed) / all * 100
  )), "%"))

}


# UTILS for TRACK module
gen_sankey_id <- function(data, where_var, to_var, existing_ids = data.frame(name=c())) {
  count_where_to <-
    data %>% dplyr::group_by((!!sym(where_var)), (!!sym(to_var))) %>% count()

  where_to_id <-
    data.frame(name = c(unique(count_where_to[[where_var]]),
                        unique(count_where_to[[to_var]])))
  where_to_id <- where_to_id %>% filter(!name %in% existing_ids$name)

  where_to_id$id <- seq.int(nrow(where_to_id)) - 1 + nrow(existing_ids)

  return(rbind(where_to_id, existing_ids) %>% dplyr::arrange(id))
}





gen_sankey_data <- function(data, where_var, to_var, where_to_id, existing_data= NULL) {
  count_where_to <-
    data %>% dplyr::group_by((!!sym(where_var)), (!!sym(to_var))) %>% count()


  sankey_data <- count_where_to %>%
    dplyr::left_join(where_to_id, by = dplyr::join_by((!!sym(where_var)) == name)) %>%
    dplyr::rename(where_id = id) %>%
    dplyr::rename(where_name = !!sym(where_var)) %>%
    dplyr::left_join(where_to_id, by = dplyr::join_by((!!sym(to_var)) == name)) %>%
    dplyr::rename(to_id = id) %>%
    dplyr::rename(to_name = !!sym(to_var))


  if(is.null(existing_data)){
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


plot_sankey_recursive <- function(dataset, stages){

  student_stages <- c('hsc_s', 'degree_t', 'specialisation', 'status')
  sankey_ids = data.frame(name=c())
  sankey_data <- NULL
  if(length(stages)>1){
    for (i in seq(length(stages) - 1)){
      where_name <- stages[i]
      to_name <- stages[i+1]

      sankey_ids <- gen_sankey_id(dataset, where_name, to_name, sankey_ids)
      sankey_data <-  gen_sankey_data(dataset, where_name, to_name, sankey_ids, sankey_data)
    }

    fig <- plot_sankey(sankey_data, sankey_ids)
    return(fig)
  }


}




