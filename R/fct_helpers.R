#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
pretty_box <- function(texto, cor){
  HTML(paste('<div class = "box_voronoys " style = "border:1px solid',
             cor, '; background-color: ',
             cor, ';">',
             h3(texto), '</div> '
            ))
}

pretty_tabs <- function(texto, text_desc, cor, icon, id){
  HTML(paste0('<a id="', id,'" href="#" class="action-button">
                  <div class = "voronoys-block" style = "background-color:', cor, ';">

                    <span class = "name">', texto, '</span>
                    <span class = "description_cl"> ', text_desc,' </span>
                    <div class="img_block">

                        <div class="img_block_conteiner">
                          <i class="',icon,'"></i>
                        </div>
                      </div>
                  </div>

              </a>'))
}

