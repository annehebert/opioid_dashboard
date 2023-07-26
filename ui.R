library(shiny)
library(ggplot2)
library(plotly)
library(hexbin)

###### ----- create dynamic radiobutton side panel -----
parameter_tabs <- tabsetPanel(
  id = "params",
  type = "hidden",
  tabPanel(
    "Number of deaths",
    radioButtons(
      "total_deaths_select",
      "Select cause of death to display",
      choices = c(
        "All causes of death",
        "Drug overdose deaths",
        "Opioid overdose deaths"
      ),
      selected = "Opioid overdose deaths"
    )
  ),
  tabPanel(
    "Crude death rate",
    radioButtons(
      "crude_rate_select",
      "Select cause of death to display",
      choices = c(
        "All causes of death",
        "Drug overdose deaths",
        "Opioid overdose deaths"
      ),
      selected = "Opioid overdose deaths"
    )
  ),
  tabPanel(
    "Age-adjusted death rate",
    radioButtons(
      "aa_rate_select",
      "Select cause of death to display",
      choices = c(
        "All causes of death",
        "Drug overdose deaths",
        "Opioid overdose deaths"
      ),
      selected = "Opioid overdose deaths"
    )
  ),
  tabPanel(
    "Opioid overdoses as % of all deaths, by age",
    radioButtons(
      "perc_select",
      "Select ages to display",
      choices = c(
        "All ages",
        "15-24 years",
        "25-34 years",
        "35-44 years",
        "45-54 years",
        "55-64 years",
        "65-74 years",
        "75+ years"
      )
    )
  ),
  tabPanel(
    "Years of life lost, total",
    radioButtons(
      "yll_tot_select",
      "Select cause of death to display",
      choices = c("All drug overdose deaths",
                  "Opioid overdose deaths"),
      selected = "Opioid overdose deaths"
    )
  ),
  tabPanel(
    "Years of life lost, per capita",
    radioButtons(
      "yll_pc_select",
      "Select cause of death to display",
      choices = c("All drug overdose deaths",
                  "Opioid overdose deaths"),
      selected = "Opioid overdose deaths"
    )
  )
)


fluidPage(
  #Set title of whole page
  titlePanel("The burden of opioid use in the United States"),
  headerPanel(
    h5(
    'The United States is in the midst of a serious epidemic of
    opioid abuse. The death toll is increasing every year, with nearly 70,000
    opioid overdose deaths in 2020. However the burden of this crisis varies
    drastically between different groups of people within the United States. This
    dashboard provides a tool for visualizing how this burden varies across
    geographical locations, ages, genders and racial/ethnic groups, as well as
    how this epidemic is evolving in time.'
    )
  ),
  headerPanel(h5('')),
  tabsetPanel(
    tabPanel(
      "State-level death maps",
      sidebarPanel(
        width = 3,
        p(
        "In this tab, we look at several measures of deaths, drug overdose deaths,
        and opioid overdose deaths, at the state level."
        ),
        radioButtons(
          inputId = 'state_map_year_select',
          label = 'Select year to display',
          choices = c('2019', '2020')
        ),
        radioButtons(
          inputId = "state_map_metric",
          label = 'Select death metric to display',
          choices = c(
            "Number of deaths",
            "Crude death rate",
            "Age-adjusted death rate",
            "Years of life lost, total",
            "Years of life lost, per capita",
            "Opioid overdoses as % of all deaths, by age"
          )
        ),
        textOutput('death_metric_definitions'),
        p(),
        parameter_tabs,
        textOutput('all_vs_opioid')
      ),
      
      mainPanel(
        br(),
        br(),
        plotlyOutput("state_map_plot",
                     width = "750px",
                     height = "400px"),
        p(
          "There is a huge variation in the severity of the opioid crisis at
              the state level. There is also a big difference in how age groups
              are hit by this epidemic, as can be seen when selecting
              'Opioid overdoses as % of all deaths, by age'.
             "
        ),
        p(
          "Because of privacy constraints, the CDC doesn't report deaths
            for numbers less than 10. For this reason, here, a missing value simply means
            that fewer than 10 deaths happened within the group and time period in question.
            Data throughout this dashboard is from the Center for Disease Control,
            unless otherwise specified. Data on deaths from",
          tags$a(href = "http://wonder.cdc.gov",
                 "CDC Wonder"),
          " and life tables from the",
          tags$a(href = "https://www.cdc.gov/nchs/nvss/life-expectancy.htm",
                 "National Vital Statistics System")
        )
      )
    ),
    
    tabPanel(
      "County-level deaths map",
      sidebarPanel(
        width = 3,
        p(
          "Here we look at death rates across the United States,
                              at the county level."
        ),
        radioButtons(
          inputId = 'county_map_select',
          label = 'Quantity to display on map',
          choices = c(
            'State outlines',
            'Total death rate by county',
            'Drug overdose death rate by county',
            'Opioid overdose death rate by county'
          )
        ),
        radioButtons(
          inputId = 'county_map_time_select',
          label = "Time span to display on map",
          choices = c('1 year (2019)',
                      '1 year (2020)',
                      '5 years (2015-2019)')
        ),
        p('')
      ),
      mainPanel(
        br(),
        plotlyOutput("county_map",
                     width = '700px', height = '400px'),
        p(
          "Above is a county-level tile map of the United States. Each
                    county is represented by circle of equal area.
                    Since in reality the surface area of counties in the contiguous
                    US range from 2 to 20,000 square miles, the resulting tile map
                    is heavily distorted. For this
                    reason, a state outline map is included here (select 'State outlines'
                    in 'Quantity to display on map' menu to the left),
                    showing where the state outlines lie on this tile map.
                    The purpose of this map is to visually give all counties equal
                    importance.
                    When a death rate is selected, the fill color
                    indicates the value of the selected death rate in that county."
        ),
        p(
          "From the 'Opioid overdose death rate by couty' map, it is
                    obvious that even within the same state there is a spread in
                    the impact of the opioid crisis at the county level."
        ),
        p(
          "The coordinates for this tile map are from",
          tags$a(href = "https://public.tableau.com/app/profile/neil.richards",
                 "Neil Richards."),
          "Because of confidentiality constraints,
                      the CDC doesn't report deaths
                      for numbers less than 10. For this reason, here, a missing
                      value simply means
                      that fewer than 10 deaths happened within the group and
                      time period in question."
        )
      )
    ),
    
    tabPanel(
      "Death by age/sex/race",
      sidebarPanel(
        width = 3,
        p(
          "Here we examine the distribution of deaths by age (left)
                         and by race/ethnicity group (right)."
        ),
        radioButtons(
          inputId = 'death_distribution_year_select',
          label = "Select year to display",
          choices = c('2019', '2020')
        ),
        radioButtons(
          inputId = 'dist_value',
          label = 'Select quantity to plot',
          choices = c(
            "Number of all deaths",
            "Number of all overdose deaths",
            "Number of opioid overdose deaths",
            "% of deaths due to opioid overdoses"
          )
        )
      ),
      mainPanel(br(),
                br(),
                fluidRow(
                  #h5('test blah di bloo'),
                  splitLayout(
                    cellWidths = c("43%", "57%"),
                    plotlyOutput(
                      "death_distribution_plot",
                      width = "100%",
                      height = "300px"
                    ),
                    plotlyOutput("deaths_by_race_plot",
                                 height = "300px",
                                 width = "100%")
                  ),
                  br(),
                  br(),
                  p(
                    "Opioid overdose deaths fall predominantly on young people, and in
          particular, young males. Looking at the race and ethnicity distribution,
          it is apparent that a majority of the number of opioid overdose deaths
          are among white, non-Hispanic people. However, opioid overdose deaths
          represent a larger percentage of deaths among the Black, Native and
          Hispanic groups. Across all race and ethnicity groups, there are more
            opioid overdose deaths among males than females."
                  ),
                  p(
                    "We have combined the race and ethnicity groups into 5 categories.
            The first group is labeled 'Hispanic', containing all individuals of
            'Hispanic or Latino' ethnicity, regardless of race. Then we have 'white',
            containing
            all individuals of 'white' race and not 'Hispanic or Latino' ethnicity,
            'Black',containing all individuals of 'Black or African-American' race
            and not 'Hispanic or Latino' ethnicity, and 'Asian', containing all
            individuals of 'Asian or Pacific Islander' race
            and not 'Hispanic or Latino' ethnicity, and 'Native', containing all
            individuals of 'American Indian or Native Alaskan' race and not
            'Hispanic or Latino' ethnicity."
                  ),
                  p(
                    "Because of the way the deaths are reported, we are only able to
          separate the death distributions by binary gender."
                  )
                ))
    ),
    tabPanel(
      "Deaths over time",
      sidebarPanel(
        p(
          "Here we look at the number of opioid overdose deaths over time,
                   since 1999."
        ),
        radioButtons(
          inputId = 'time_rate_select',
          label = 'Select measure of opioid overdoses to display',
          choices = c("Total deaths over time",
                      "Crude rate over time")#,
          #"Age-adjusted rate over time")
        ),
        radioButtons(
          inputId = 'time_rate_group_select',
          label = 'Select variable to group by',
          choices = c('Type of drug', 'Gender',
                      'Age', 'Race/ethnicity')
        ),
        textOutput('race_eth_definition')
      ),
      mainPanel(
        br(),
        br(),
        plotlyOutput('ods_over_time',
                     width = "600px",
                     height = "400px"),
        br(),
        p(
          "The opioid overdose death rate has increased dramatically over
            the last 20 years.  In particular, the deaths due to heroin and
            synthetic opioids, such
            as fentanyl, have increased 5-10x since 2010.
            Looking at the death rates grouped by race and ethnicity, it
            seems the crisis has be worsening the most for Black people, as well
            as Hispanic people, whereas it appears to be perhaps be stabilizing
              among white people."
        )
      )
    ),
    
    tabPanel(
      "Correlates of opioid burden",
      sidebarPanel(
        p(
          "Here we examine potential correlations between opioid
               overdose death rates and several socioeconomic factors, at the
               county level.
                 "
        ),
        p(
          "For each factor, we fit a linear model and find the Pearson and
                   Spearman coefficients. The regression line is shown in grey
                   on each plot."
        ),
        radioButtons(
          inputId = 'correlates_year_select',
          label = 'Select opioid overdose rates year',
          choices = c('2019', '2020')
        ),
        radioButtons(
          inputId = 'correlates_select',
          label = 'Select factor to display',
          choices = c(
            'Unemployment',
            'Education',
            'Population density',
            'Migration rate',
            'Per capita income',
            'Average household size'
          )
        ),
        textOutput("correlator_descriptions")
      ),
      mainPanel(
        br(),
        plotlyOutput('correlators_plot',
                     width = "600px",
                     height = "450px"),
        br(),
        br(),
        p(
          "Socioeconomic factor data is from the",
          tags$a(href = "https://www.ers.usda.gov/data-products/atlas-of-rural-and-small-town-america/",
                 "US Department of Agriculture
                    Economic Research Service."),
        )
      )
    ),
    
    
    tabPanel(
      "State-level prescription map",
      sidebarPanel(
        p(
          "Here we display retail opioid prescriptions dispensed per 100
               people in one year, by state."
        ),
        sliderInput(
          inputId = "select_presc_year",
          label = "Select year to display",
          min = 2006,
          max = 2020,
          value = 2020,
          step = 1,
          sep =  "",
          ticks = FALSE
        )
      ),
      mainPanel(
        plotlyOutput('prescription_map',
                     width = "750px",
                     height = "400px"),
        p(
          "Prescription rates vary between states, even in the same
                         year. We can also see that different states peaked in
                         prescription rates in different years. This data
                         includes prescriptions for an opioid supply
                         for anywhere from 1 to 365 days."
        ),
        p(
          "State level opioid prescription from the",
          tags$a(href = "https://www.cdc.gov/drugoverdose/rxrate-maps/index.html",
                 "CDC."),
        )
      )
    ),
    
    tabPanel(
      'Prescriptions over time',
      sidebarPanel(
        p(
          "Here we plot the retail opioid prescriptions dispensed per 100
               people each year in the United States. Nationwide, the prescription
                 rates peaked in 2012."
        )
      ),
      mainPanel(
        br(),
        plotlyOutput(
          'prescription_evolution',
          width = "500px",
          height = "350px"
        ),
        br(),
        p(
          "Due to difficulty in finding a single data source over all years,
              the plot above shows two different data sources."
        ),
        p(
          "Prescription data is from the",
          tags$a(href = " https://www.cdc.gov/drugoverdose/rxrate-maps/index.html",
                 " CDC"),
          "and from the",
          tags$a(href = "https://www.painphysicianjournal.com/current/pdf?article=NDIwMg%3D%3D&journal=103",
                 " Pain Physician Journal.")
        )
      )
    ),
  
  tabPanel(
    'Resources',
    
    mainPanel(
      br(),
      br(),
      h2("Resources"),
      h4("General information on opioids and opioid use disorder:"),
      p(tags$a(href = "https://www.cdc.gov/opioids/basics/terms.html", 
               "Definitions of common terms (CDC)")),
      p(tags$a(href = "https://www.hopkinsmedicine.org/health/treatment-tests-and-therapies/opioids",
               "Opioid types and uses (Johns Hopkins School of Medicine)"),
        ),
      p(tags$a(href = "https://www.hopkinsmedicine.org/health/conditions-and-diseases/opioid-use-disorder",
               "Opioid use disorder (Johns Hopkins School of Medicine)"),
      ),
      p(tags$a(href = "https://www.cdc.gov/drugoverdose/pdf/patients/Preventing-an-Opioid-Overdose-Tip-Card-a.pdf",
               "Opioid overdose tips card (CDC)"), "how to recognize an opioid overdose and what to do"),
      p(tags$a(href = "https://www.cdc.gov/opioids/index.html", 
               "Index of CDC's opioid resources")),
      h4("Information on the opioid epidemic and prevention efforts"),
      p(tags$a(href = "https://www.cdc.gov/drugoverdose/featured-topics/evidence-based-strategies.html",
               "Evidence-based strategies for combatting the opioid crisis (CDC)")),
      p(tags$a(href = "https://www.nature.com/articles/d41586-019-02686-2",
               "A history of the opioid crisis (Nature)")),
      # p("Other resources:"),
      # p(tags$a(href = "https://harmreduction.org/resource-center/",
      #          "The National Harm Reduction Coalition resource center"))
    )
    
  )
  ),
  
  headerPanel(
    h6("This dashboard was made by Anne Hébert and Alison Hill. Last updated October 2022.")
  )
)
