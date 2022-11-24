library(echarts4r)
library(tidyverse)
library(glue)

iris %>%
  group_by(Species) %>%
  summarise(sepal_length = mean(Sepal.Length), .groups = "drop") %>%
  mutate(
    species = as.character(Species),
    label = glue(
      "${species}$\n{bold|Показатель} - {green|ОК}",
      .open = "${", .close = "}$"
    )
  ) %>%
  e_chart(label) %>%
  e_bar(sepal_length) %>%
  e_x_axis(
    type = "category",
    axisLabel = list(
      rich = list(
        bold = list(
          fontStyle = "bold"
        ),
        green = list(
          backgroundColor = "#339911",
          color = "#fff",
          borderRadius = 15,
          padding = 5
        )
      )
    )
  )
