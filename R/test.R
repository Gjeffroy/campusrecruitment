library(plotly)
library(dplyr)


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
  fig <- fig %>% plotly::layout(title = "Basic Sankey Diagram",
                        font = list(size = 10))

  return(fig)

}


plot_sankey_recursive <- function(dataset){

  student_stages <- c('hsc_s', 'degree_t', 'specialisation', 'status')
  sankey_ids = data.frame(name=c())
  sankey_data <- NULL

  for (i in seq(length(student_stages) - 1)){
    where_name <- student_stages[i]
    to_name <- student_stages[i+1]

    sankey_ids <- gen_sankey_id(dataset, where_name, to_name, sankey_ids)
    sankey_data <-  gen_sankey_data(dataset, where_name, to_name, sankey_ids, sankey_data)
  }

  fig <- plot_sankey(sankey_data, sankey_ids)
  return(fig)

}



