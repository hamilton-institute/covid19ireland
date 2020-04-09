library(shiny)
library(shinyMobile)
library(echarts4r)
library(shinyWidgets)

f7Page(
  title = "COVID-19 Ireland",
  init = f7Init(theme = "dark"),
  f7TabLayout(
    navbar = f7Navbar(
      title = "COVID-19 Ireland Visualisation",
      hairline = TRUE,
      shadow = TRUE
      #left_panel = TRUE,
      #right_panel = TRUE
    ),
    f7Tabs(
      animated = TRUE,
      #swipeable = TRUE,
      f7Tab(
        tabName = "Ireland",
        icon = f7Icon("home"), # These come from https://framework7.io/icons/
        active = TRUE,
        
        # f7Flex(
        #   prettyRadioButtons(
        #     inputId = "theme",
        #     label = "Select a theme:",
        #     thick = TRUE,
        #     inline = TRUE,
        #     selected = "md",
        #     choices = c("ios", "md"),
        #     animation = "pulse",
        #     status = "info"
        #   ),
        #   
        #   prettyRadioButtons(
        #     inputId = "color",
        #     label = "Select a color:",
        #     thick = TRUE,
        #     inline = TRUE,
        #     selected = "dark",
        #     choices = c("light", "dark"),
        #     animation = "pulse",
        #     status = "info"
        #   )
        # ),
        
        # shiny::tags$head(
        #   shiny::tags$script(
        #     'Shiny.addCustomMessageHandler("ui-tweak", function(message) {
        #         var os = message.os;
        #         var skin = message.skin;
        #         if (os === "md") {
        #           $("html").addClass("md");
        #           $("html").removeClass("ios");
        #           $(".tab-link-highlight").show();
        #         } else if (os === "ios") {
        #           $("html").addClass("ios");
        #           $("html").removeClass("md");
        #           $(".tab-link-highlight").hide();
        #         }
        # 
        #         if (skin === "dark") {
        #          $("html").addClass("theme-dark");
        #         } else {
        #           $("html").removeClass("theme-dark");
        #         }
        # 
        #        });
        #       '
        #   )
        # ),
        f7Card(
          f7ExpandableCard(
            id = "card1",
            title = "Expandable Card 1",
            color = "blue",
            subtitle = "Click on me pleaaaaase",
            "Hello"
          ),
          f7ExpandableCard(
            id = "card1",
            title = "Expandable Card 2",
            color = "red",
            subtitle = "Click on me pleaaaaase",
            "Hello"
          )
        ),
        f7Shadow(
          intensity = 5,
          hover = TRUE,
          f7Card(
            title = "Latest updates",
            sliderTextInput(
              inputId = "by",
              label = "Date Selector:",
              choices = c("day", "week", "month"),
              selected = "day"
            ),
            # br(),
            # echarts4rOutput("river"),
            footer = tagList(
              f7Button(color = "blue", label = "My button", src = "https://www.google.com"),
              f7Badge("Badge", color = "green")
            )
          )
        )
      ),
      f7Tab(
        tabName = "World",
        icon = f7Icon("home"),
        active = FALSE,
        f7Shadow(
          intensity = 10,
          hover = TRUE,
          f7Card(
            title = "Card header",
            prettySwitch(
              inputId = "show",
              label = "Show Plot",
              status = "danger"
            ),
            echarts4rOutput("network"),
            footer = tagList(
              f7Button(color = "blue", label = "My button", src = "https://www.google.com"),
              f7Badge("Badge", color = "green")
            )
          )
        )
      ),
      f7Tab(
        tabName = "Graphs",
        icon = f7Icon("cloud_upload"),
        active = FALSE,
        f7Shadow(
          intensity = 10,
          hover = TRUE,
          f7Card(
            title = "Card header",
            prettyCheckboxGroup(
              "variable",
              "Variables to show:",
              c("Cylinders" = "cyl",
                "Transmission" = "am",
                "Gears" = "gear"),
              inline = TRUE,
              status = "danger",
              animation = "pulse"
            ),
            tableOutput("data"),
            footer = tagList(
              f7Button(color = "blue", label = "My button", src = "https://www.google.com"),
              f7Badge("Badge", color = "green")
            )
          )
        )
      ),
      f7Tab(
        tabName = "Animations",
        icon = f7Icon("cloud_upload"),
        active = FALSE,
        f7Shadow(
          intensity = 10,
          hover = TRUE,
          f7Card(
            title = "Card header",
            prettyCheckboxGroup(
              "variable",
              "Variables to show:",
              c("Cylinders" = "cyl",
                "Transmission" = "am",
                "Gears" = "gear"),
              inline = TRUE,
              status = "danger",
              animation = "pulse"
            ),
            tableOutput("data2"),
            footer = tagList(
              f7Button(color = "blue", label = "My button", src = "https://www.google.com"),
              f7Badge("Badge", color = "green")
            )
          )
        )
      )
    )
  )
)