#' salary types palette
#' @export
#' @return 3 length color vector
pal_salary <- function() {
  c('total' = 'seagreen',
    'base' = '#8CD17DFF',
    'bonus' = '#A0CBE8FF',
    'bonus_2' = '#4E79A7FF')
}

#' general color palette
#' @export
#' @return  17 length color vector
pal_general <- function() {
  c(
    '#F28E2BFF',
    '#499894FF',
    '#B6992DFF',
    '#E15759FF',
    '#79706EFF',
    '#D37295FF',
    '#B07AA1FF',
    '#9D7660FF',
    '#FFBE7DFF',
    '#86BCB6FF',
    '#F1CE63FF',
    '#FF9D9AFF',
    '#BAB0ACFF',
    '#FABFD2FF',
    '#D4A6C8FF',
    '#D7B5A6FF'
    )
}

#' paletteer color palette
#' @export
pal_paletteer <- function() {
  'ltc::minou'
}
