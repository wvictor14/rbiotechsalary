#' Filters, dropdowns ui
#' @export
filters_ui <- function() {
  tagList(
    shiny.fluent::Dropdown.shinyInput(
      'title',
      multiSelect = TRUE,
      value = 'Scientist',
      label = NULL,
      options = salaries |> make_grouped_options(title_category, title_general)
    ),
    shiny.fluent::Dropdown.shinyInput(
      "location_country",
      placeHolder = "Select location",
      multiSelect = FALSE,
      value = 'United States Of America',
      options =  salaries |>
        mutate(key = location_country, text = location_country) |>
        select(key, text) |>
        distinct()  |>
        arrange(key)
    ),
    shiny.fluent::Dropdown.shinyInput(
      inputId = "location_granular",
      placeHolder = "Select sub-location",
      multiSelect = TRUE
    ),
    
    shiny.fluent::Stack(
      horizontal = TRUE,
      shiny.fluent::DefaultButton.shinyInput(
        "select_all", text = "Select all"
      ),
      shiny.fluent::DefaultButton.shinyInput(
        "deselect_all", text = "Deselect all"
      )
    ),
    shiny.fluent::Stack(
      horizontal = TRUE,
      tokens = list(childrenGap = 10),
      shiny.fluent::ChoiceGroup.shinyInput(
        'date',
        options = c('All' , '2024', '2023', '2022') %>%
          tibble(key = ., text = .),
        styles = list(flexContainer = list(display = "flex", gap = 10)),
        value = 'All')
    )
  )
}