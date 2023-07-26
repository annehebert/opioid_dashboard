library(shiny)
library(ggplot2)
library(plotly)
library(plyr)
library(urbnmapr)
library(tidyverse)
library(hexbin)
library(stringr)
#setwd("~/Opioid_project/opioid_app/github_version")
source("data.R")
source("plotting.R")
source("functions.R")
#source("YLL.R")


function(input, output) {
  observeEvent(input$state_map_metric, {
    updateTabsetPanel(inputId = "params",
                      selected = input$state_map_metric)
  })
  
  output$text_header <- renderUI({
    HTML(
      paste(
        'The United States is in the middle of a serious epidemic of
    opioid abuse. The death toll is increasing every year, with nearly 70,000
    opioid overdose deaths in 2021. However the burden of this crisis varies
    drastically between different groups of people within the United States.
    This dashboard provides a tool for visualizing how this burden varies across
    geographical locations, ages, genders and racial/ethnic groups, as well as
    how this epidemic is evolving in time.',
        'test',
        sep = '<br>'
      )
    )
  })
  
  #######         STATE MAPS plotly       ----------------------------------------
  output$death_metric_definitions <- renderText({
    if (input$state_map_metric == "Number of deaths") {
      "Number of deaths is simply the sum of deaths in each state, due to the
    selected mode of death below."
    } else if (input$state_map_metric == "Crude death rate") {
      "Crude death rate is the death rate per 100,000 people, due
    to the selected mode of death below."
    } else if (input$state_map_metric == "Age-adjusted death rate") {
      "Age-adjusted death rate is a death rate per 100,000 people, that takes
      the age distribution of the population into account. "
    } else if (input$state_map_metric == "Years of life lost, total") {
      "Total years of life lost in each state, due to selected mode of death
      below. This is estimated using the age at time of death and the
      life expectancy."
    } else if (input$state_map_metric == "Years of life lost, per capita") {
      "Years of life lost per 100,000 people, due to selected mode of death below.
    This is estimated using the age at time of death and the life expectancy."
    } else if (input$state_map_metric == "Opioid overdoses as % of all deaths, by age") {
      "Percent of all deaths in each age group that are opioid overdose deaths."
    }
  })
  
  
  output$state_map_plot <- renderPlotly({
    if (input$state_map_year_select == '2019') {
      df.state.map.year <- df.state.map.2019
    }
    else if (input$state_map_year_select == '2020') {
      df.state.map.year <- df.state.map.2020
    }
    if (input$state_map_year_select == '2019') {
      year <- '2019'
    }
    else if (input$state_map_year_select == '2020') {
      year <- '2020'
    }
    g <- list(
      scope = "usa",
      projection = list(type = 'albers usa'),
      showlakes = FALSE,
      lakecolor = toRGB('white')
    )
    fig <- plot_geo(df.state.map.year,
                    locationmode = 'USA-states')
    
    if (input$state_map_metric == "Number of deaths") {
      if (input$total_deaths_select == "All causes of death") {
        fig <- fig %>% add_trace(
          z = ~ round(Deaths.all, digits = 1),
          color = ~ Deaths.all,
          text = ~ State,
          locations = ~ State.abbv,
          hovertemplate =
            paste0(
              "Total deaths: ",
              format(
                df.state.map.year$Deaths.all,
                format = 'd',
                big.mark = ',',
                digits = 2
              ),
              "<br>State: %{text} <extra></extra>"
            ),
          name = '',
          colors = 'Blues'
        )
        fig <-
          fig %>% layout(title = paste('Total number of deaths by state in', 
                                       year),
                         geo = g)  %>%
          colorbar(title = "Deaths",
                   limits = c(0, round_any(
                     max(df.state.map.year$Deaths.all),
                     100000, f = ceiling
                   )))
        fig
      }
      else if (input$total_deaths_select == "Drug overdose deaths") {
        fig <- fig %>% add_trace(
          z = ~ Deaths.all.od,
          color = ~ Deaths.all.od,
          text = ~ State,
          locations = ~ State.abbv,
          hovertemplate = paste(
            "Drug overdose deaths:",
            format(
              df.state.map.year$Deaths.all.od,
              format = 'd',
              big.mark = ',',
              digits = 1
            ),
            "<br>State: %{text} <extra></extra>"
          ),
          name = '',
          colors = 'Blues'
        )
        fig <-
          fig %>% layout(
            title = paste('Number of drug overdose deaths by state in', year),
            geo = g
          )  %>%
          colorbar(title = "Deaths",
                   limits = c(0, round_any(
                     max(df.state.map.year$Deaths.all.od), 1000, f = ceiling
                   )))
        fig
      }
      else if (input$total_deaths_select == "Opioid overdose deaths") {
        fig <- fig %>% add_trace(
          z = ~ Deaths.opioid.od,
          color = ~ Deaths.opioid.od,
          text = ~ State,
          locations = ~ State.abbv,
          hovertemplate = paste(
            "Opioid overdose deaths:",
            format(
              df.state.map.year$Deaths.opioid.od,
              format = 'd',
              big.mark = ',',
              digits = 2
            ),
            "<br>State: %{text} <extra></extra>"
          ),
          name = '',
          colors = 'Blues'
        )
        fig <-
          fig %>% layout(
            title = paste('Number of opioid overdose deaths by state in', year),
            geo = g
          ) %>%
          colorbar(title = "Deaths",
                   limits = c(0, round_any(
                     max(df.state.map.year$Deaths.opioid.od), 1000, f = ceiling
                   )))
        fig
      }
    }
    
    else if (input$state_map_metric == "Crude death rate") {
      if (input$crude_rate_select == "All causes of death") {
        fig <- fig %>% add_trace(
          z = ~ Crude.Rate.all,
          color = ~ Crude.Rate.all,
          text = ~ State,
          locations = ~ State.abbv,
          hovertemplate = paste0(
            "Total death rate: ",
            format(
              df.state.map.year$Crude.Rate.all,
              format = 'd',
              big.mark = ',',
              digits = 2
            ),
            " per 100,000 <br>State: %{text} <extra></extra>"
          ),
          name = '',
          colors = 'Blues'
        )
        fig <-
          fig %>% layout(title = paste('Total death rate in', year),
                         geo = g) %>%
          colorbar(title = "Death rate \n(per 100,000)",
                   limits = c(0, round_any(
                     max(df.state.map.year$Crude.Rate.all), 100, f = ceiling
                   )))
        fig
      } 
      else if (input$crude_rate_select == "Drug overdose deaths") {
        fig <- fig %>% add_trace(
          z = ~ Crude.Rate.all.od,
          color = ~ Crude.Rate.all.od,
          text = ~ State,
          locations = ~ State.abbv,
          hovertemplate = paste0(
            "Drug overdose death rate: ",
            round(df.state.map.year$Crude.Rate.all.od, digits =
                    0),
            " per 100,000 <br>State: %{text} <extra></extra>"
          ),
          name = '',
          colors = 'Blues'
        )
        fig <-
          fig %>% layout(title = paste('Drug overdose death rate in', year),
                         geo = g) %>%
          colorbar(title = "Death rate \n(per 100,000)",
                   limits = c(0, round_any(
                     max(df.state.map.year$Crude.Rate.all.od), 10, f = ceiling
                   )))
        fig
      } 
      else if (input$crude_rate_select == "Opioid overdose deaths") {
        fig <- fig %>% add_trace(
          z = ~ Crude.Rate.opioid.od,
          color = ~ Crude.Rate.opioid.od,
          text = ~ State,
          locations = ~ State.abbv,
          hovertemplate = paste0(
            "Opioid overdose death rate: ",
            round(df.state.map.year$Crude.Rate.opioid.od, digits =
                    0),
            " per 100,000 <br>State: %{text} <extra></extra>"
          ),
          name = '',
          colors = 'Blues'
        )
        fig <-
          fig %>% layout(title = paste('Opioid overdose death rate in', year),
                         geo = g) %>%
          colorbar(title = "Death rate \n(per 100,000)",
                   limits = c(0, round_any(
                     max(df.state.map.year$Crude.Rate.opioid.od), 10, f = ceiling
                   )))
        fig
      }
    }
    else if (input$state_map_metric == "Age-adjusted death rate") {
      if (input$aa_rate_select == "All causes of death") {
        fig <- fig %>% add_trace(
          z = ~ AA.all,
          color = ~ AA.all,
          text = ~ State,
          locations = ~ State.abbv,
          hovertemplate = paste(
            "Age-adjusted total death rate: ",
            round(df.state.map.year$AA.all, digits = 0),
            " <br>State: %{text} <extra></extra>"
          ),
          name = '',
          colors = 'Blues'
        )
        fig <-
          fig %>% layout(title = paste('Total age-adjusted death rate in', year),
                         geo = g) %>%
          colorbar(title = "Age-adjusted \ndeath rate \n(per 100,000)",
                   limits = c(0, round_any(
                     max(df.state.map.year$AA.all), 100, f = ceiling
                   )))
        fig
      } 
      else if (input$aa_rate_select == "Drug overdose deaths") {
        fig <- fig %>% add_trace(
          z = ~ AA.all.od,
          color = ~ AA.all.od,
          text = ~ State,
          locations = ~ State.abbv,
          hovertemplate = paste(
            "Age-adjusted drug overdose death rate: ",
            round(df.state.map.year$AA.all.od, digits = 1),
            "<br>State: %{text} <extra></extra>"
          ),
          name = '',
          colors = 'Blues'
        )
        fig <-
          fig %>% layout(
            title = paste('Drug overdose age-adjusted death rate in', year),
            geo = g
          ) %>%
          colorbar(title = "Age-adjusted \ndeath rate \n(per 100,000)",
                   limits = c(0, round_any(
                     max(df.state.map.year$AA.all.od), 10, f = ceiling
                   )))
        fig
      } 
      else if (input$aa_rate_select == "Opioid overdose deaths") {
        fig <- fig %>% add_trace(
          z = ~ AA.opioid.od,
          color = ~ AA.opioid.od,
          text = ~ State,
          locations = ~ State.abbv,
          hovertemplate = paste(
            "Age-adjusted opioid overdose death rate: %{z} <br>State: %{text} 
            <extra></extra>"
          ),
          name = '',
          colors = 'Blues'
        )
        fig <-
          fig %>% layout(
            title = paste('Opioid overdose age-adjusted death rate in', year),
            geo = g
          ) %>%
          colorbar(title = "Age-adjusted \ndeath rate \n(per 100,000)",
                   limits =  c(0, round_any(
                     max(df.state.map.year$AA.opioid.od), 10, f = ceiling
                   )))
        fig
      }
    }
    else if (input$state_map_metric == "Opioid overdoses as % of all deaths, by age") {
      if (input$perc_select == "All ages") {
        fig <- fig %>% add_trace(
          z = ~ Perc.opioid.od.deaths,
          color = ~ Perc.opioid.od.deaths,
          text = ~ State,
          locations = ~ State.abbv,
          colors = "Blues",
          hovertemplate = paste(
            round(df.state.map.year$Perc.opioid.od.deaths, digits = 1),
            "% of deaths in 15-24 year olds are due to opioid 
            overdoses <br>State: %{text} <extra></extra>"
          ),
          name = ''
        )
        fig <-
          fig %>% layout(
            title = paste('Percent of deaths due to opioid overdoses in', year),
            geo = g
          ) %>%
          colorbar(title = "Deaths (%)",
                   limits = c(0, 6))
        fig
      } else if (input$perc_select == "15-24 years") {
        fig <- fig %>% add_trace(
          z = ~ Od.percent.15,
          color = ~ Od.percent.15,
          text = ~ State,
          locations = ~ State.abbv,
          colors = "Blues",
          hovertemplate = paste(
            round(df.state.map.year$Od.percent.15, digits = 1),
            "% of deaths in 15-24 year olds are due to opioid overdoses 
            <br>State: %{text} <extra></extra>"
          ),
          name = ''
        )
        fig <-
          fig %>% layout(
            title = paste(
              'Percent of deaths in 15-24y age group due to opioid overdoses in',
              year
            ),
            geo = g
          ) %>%
          colorbar(title = "Deaths (%)",
                   limits = c(0, 40))
        fig
      }
      else  if (input$perc_select == "25-34 years") {
        fig <- fig %>% add_trace(
          z = ~ Od.percent.25,
          color = ~ Od.percent.25,
          text = ~ State,
          locations = ~ State.abbv,
          colors = "Blues",
          hovertemplate = paste(
            round(df.state.map.year$Od.percent.25, digits = 1),
            "% of deaths in 25-34 year olds are due to opioid overdoses <br> 
            State: %{text} <extra></extra>"
          ),
          name = ''
        )
        fig <-
          fig %>% layout(
            title = paste(
              'Percent of deaths in 25-34y age group due to opioid overdoses in',
              year
            ),
            geo = g
          ) %>%
          colorbar(title = "Deaths (%)",
                   limits = c(0, 60))
        fig
      }
      else  if (input$perc_select == "35-44 years") {
        fig <- fig %>% add_trace(
          z = ~ Od.percent.35,
          color = ~ Od.percent.35,
          text = ~ State,
          locations = ~ State.abbv,
          colors = "Blues",
          hovertemplate = paste(
            round(df.state.map.year$Od.percent.35, digits = 1),
            "% of deaths in 35-44 year olds are due to opioid overdoses <br>
            State: %{text} <extra></extra>"
          ),
          name = ''
        )
        fig <-
          fig %>% layout(
            title = paste(
              'Percent of deaths in 35-44y age group due to opioid overdoses in',
              year
            ),
            geo = g
          ) %>%
          colorbar(
            title = "Deaths (%)",
            limits = c(0, 52),
            breaks = c(0, 25, 50)
          )
        fig
      }
      else  if (input$perc_select == "45-54 years") {
        fig <- fig %>% add_trace(
          z = ~ Od.percent.45,
          color = ~ Od.percent.45,
          text = ~ State,
          locations = ~ State.abbv,
          colors = "Blues",
          hovertemplate = paste(
            round(df.state.map.year$Od.percent.45, digits = 1),
            "% of deaths in 45-54 year olds are due to opioid overdoses <br>
            State: %{text}<extra></extra>"
          ),
          name = ''
        )
        fig <-
          fig %>% layout(
            title = paste(
              'Percent of deaths in 45-54y age group due to opioid overdoses in',
              year
            ),
            geo = g
          ) %>%
          colorbar(
            title = "Deaths (%)",
            limits = c(0, 52),
            breaks = c(0, 25, 50)
          )
        fig
      }
      else  if (input$perc_select == "55-64 years") {
        fig <- fig %>% add_trace(
          z = ~ Od.percent.55,
          color = ~ Od.percent.55,
          text = ~ State,
          locations = ~ State.abbv,
          colors = "Blues",
          hovertemplate = paste(
            round(df.state.map.year$Od.percent.55, digits = 1),
            "% of deaths in 55-64 year olds are due to opioid overdoses <br>
            State: %{text}<extra></extra>"
          ),
          name = ''
        )
        fig <-
          fig %>% layout(
            title = paste(
              'Percent of deaths in 55-64y age group due to opioid overdoses in',
              year
            ),
            geo = g
          ) %>%
          colorbar(
            title = "Deaths (%)",
            limits = c(0, 52),
            breaks = c(0, 25, 50)
          )
        fig
      }
      else if (input$perc_select == "65-74 years") {
        fig <- fig %>% add_trace(
          z = ~ Od.percent.65,
          color = ~ Od.percent.65,
          text = ~ State,
          locations = ~ State.abbv,
          colors = "Blues",
          hovertemplate = paste(
            round(df.state.map.year$Od.percent.65, digits = 1),
            "%of deaths in 65-74 year olds are due to opioid overdoses <br>
            State: %{text} <extra></extra>"
          ),
          name = ''
        )
        fig <-
          fig %>% layout(
            title = paste(
              'Percent of deaths in 65-74y age group due to opioid overdoses in',
              year
            ),
            geo = g
          ) %>%
          colorbar(
            title = "Deaths (%)",
            limits = c(0, 52),
            breaks = c(0, 25, 50)
          )
        fig
      }
      else if (input$perc_select == "75+ years") {
        fig <- fig %>% add_trace(
          z = ~ Od.percent.75plus,
          color = ~ Od.percent.75plus,
          text = ~ State,
          locations = ~ State.abbv,
          colors = "Blues",
          hovertemplate = paste(
            format(
              df.state.map.year$Od.percent.75plus,
              format = 'd',
              big.mark = ',' ,
              digits = 1
            ),
            "% of deaths in 75+ year olds are due to opioid overdoses <br>
            State: %{text}<extra></extra>"
          ),
          name = ''
        )
        fig <-
          fig %>% layout(
            title = paste(
              'Percent of deaths in 75+ y age group due to opioid overdoses in',
              year
            ),
            geo = g
          ) %>%
          colorbar(
            title = "Deaths (%)",
            limits = c(0, 52),
            breaks = c(0, 25, 50)
          )
        fig
      }
    } else if (input$state_map_metric == "Years of life lost, total") {
      if (input$yll_tot_select == "Opioid overdose deaths") {
        fig <- fig %>% add_trace(
          z = ~ YLL,
          color = ~ YLL,
          text = ~ State,
          locations = ~ State.abbv,
          hovertemplate = paste(
            "Years of life lost: ",
            format(
              df.state.map.year$YLL,
              format = "d",
              big.mark = ",",
              digits = 2
            ),
            "<br>State: %{text} <extra></extra>"
          ),
          name = '',
          colors = 'Blues'
        )
        fig <-
          fig %>% layout(
            title = paste('Years of life lost to opioid overdose deaths in', year),
            geo = g
          ) %>%
          colorbar(title = 'Years of life lost',
                   limits = c(0, 350000))
        fig
      } else if (input$yll_tot_select == "All drug overdose deaths") {
        fig <- fig %>% add_trace(
          z = ~ YLL.all,
          color = ~ YLL.all,
          text = ~ State,
          locations = ~ State.abbv,
          hovertemplate = paste(
            "Years of life lost: ",
            format(
              df.state.map.year$YLL.all,
              format = 'd',
              big.mark = ',',
              digits = 2
            ) ,
            "<br>State: %{text} <extra></extra>"
          ),
          name = '',
          colors = 'Blues'
        )
        fig <-
          fig %>% layout(
            title = paste('Years of life lost to drug overdose deaths in', year),
            geo = g
          ) %>%
          colorbar(title = 'Years of life lost',
                   limits = c(0, 350000))
        fig
      }
      
    } else if (input$state_map_metric == "Years of life lost, per capita") {
      if (input$yll_pc_select == "Opioid overdose deaths") {
        fig <- fig %>% add_trace(
          z = ~ YLL.per.pop * 100000,
          color = ~ YLL.per.pop * 100000,
          text = ~ State,
          locations = ~ State.abbv,
          hovertemplate = paste(
            "Years of life lost (per 100,000 people): " ,
            format(
              df.state.map.year$YLL.per.pop * 100000,
              format = 'd',
              big.mark = ',',
              digits = 1
            ),
            "<br>State: %{text} <extra></extra>"
          ),
          name = '',
          colors = 'Blues'
        )
        fig <-
          fig %>% layout(
            title = paste(
              'Years of life lost to opioid overdose deaths per capita in',
              year
            ),
            geo = g
          ) %>%
          colorbar(title = 'Years of life lost \n(per 100,000)',
                   limits = c(0, 2500))
        fig
      } else if (input$yll_pc_select == "All drug overdose deaths") {
        fig <- fig %>% add_trace(
          z = ~ YLL.all.per.pop * 100000,
          color = ~ YLL.all.per.pop * 100000,
          text = ~ State,
          locations = ~ State.abbv,
          hovertemplate = paste0(
            "Years of life lost (per 100,000): ",
            format(
              df.state.map.year$YLL.all.per.pop * 100000,
              format = 'd',
              big.mark = ',',
              digits = 2
            ),
            "<br>State: %{text} <extra></extra>"
          ),
          name = '',
          colors = 'Blues'
        )
        fig <-
          fig %>% layout(
            title = paste(
              'Years of life lost to drug overdose deaths per capita in',
              year
            ),
            geo = g
          ) %>%
          colorbar(title = 'Years of life lost \n(per 100,000)',
                   limits = c(0, 2500))
        fig
      }
    }
  })
  
  
  #######         COUNTY MAPS         --------------------------------------------
  output$county_map <- renderPlotly({
    if (input$county_map_time_select == '1 year (2019)' |
        input$county_map_time_select == '1 year (2020)') {
      if (input$county_map_time_select == '1 year (2019)') {
        df.county.hex.map <- df.county.hex.map.2019
      }
      else if (input$county_map_time_select == '1 year (2020)') {
        df.county.hex.map <- df.county.hex.map.2020
      }
      if (input$county_map_time_select == '1 year (2019)') {
        year <- '2019'
      }
      else if (input$county_map_time_select == '1 year (2020)') {
        year <- '2020'
      }
      if (input$county_map_select == 'Opioid overdose death rate by county') {
        p <- ggplot(data = df.county.hex.map,
                    aes(
                      x = x.use,
                      y = -y.use,
                      color = Crude.Rate.opioid.ods,
                      text = paste(
                        format(
                          Crude.Rate.opioid.ods,
                          format = 'd',
                          big.mark = ',',
                          digits = 2
                        ),
                        'deaths in',
                        str_to_title(County),
                        'County,',
                        state.name
                      )
                    )) +
          geom_point(size = 1) +
          labs(
            title = paste('Opioid overdose death rates by county in', year),
            x = '',
            y = ''
          ) +
          theme(
            panel.background = element_rect(
              fill = "white",
              color = "white",
              size = 0.5,
              linetype = "solid"
            ),
            axis.ticks = element_blank(),
            axis.text = element_blank()
          ) +
          scale_color_gradientn(
            "Death rate \n(per 100,000)",
            colours = rainbow(8),
            limits = c(0, 200),
            breaks = c(NA, 0, 100, 200),
            labels = c("NA", 0, "100", '200'),
            na.value = 'grey'
          )
        ggplotly(p, tooltip = "text") %>% layout(legend = list(x = 0., y = 0))
        
      } else if (input$county_map_select == 'Drug overdose death rate by county') {
        p <- ggplot(data = df.county.hex.map,
                    aes(
                      x = x.use,
                      y = -y.use,
                      color = Crude.Rate.all.ods,
                      text = paste(
                        format(
                          Crude.Rate.all.ods,
                          format = 'd',
                          big.mark = ',',
                          digits = 2
                        ),
                        'deaths in',
                        str_to_title(County),
                        'County,',
                        state.name
                      )
                    )) +
          geom_point(size = 1) +
          labs(
            title = paste('Drug overdose death rates by county in', year),
            x = '',
            y = ''
          ) +
          theme(
            panel.background = element_rect(
              fill = "white",
              color = "white",
              size = 0.5,
              linetype = "solid"
            ),
            axis.ticks = element_blank(),
            axis.text = element_blank()
          ) +
          scale_color_gradientn(
            "Death rate \n(per 100,000)",
            colours = rainbow(8),
            limits = c(0, 200),
            breaks = c(NA, 0, 100, 200),
            labels = c("NA", 0, "100", '200'),
            na.value = 'grey'
          )
        ggplotly(p, tooltip = "text")
        
      }
      else if (input$county_map_select == 'Total death rate by county') {
        p <- ggplot(data = df.county.hex.map,
                    aes(
                      x = x.use,
                      y = -y.use,
                      color = Crude.Rate.all,
                      text = paste(
                        format(
                          Crude.Rate.all,
                          format = 'd',
                          big.mark = ',',
                          digits = 2
                        ),
                        'deaths per 100,000 in',
                        str_to_title(County),
                        'County,',
                        state.name
                      )
                    )) +
          geom_point(size = 1) +
          labs(
            title = paste('Total death rates by county in', year),
            x = '',
            y = ''
          ) +
          theme(
            legend.position = 'bottom',
            panel.background = element_rect(
              fill = "white",
              color = "white",
              size = 0.5,
              linetype = "solid"
            ),
            axis.ticks = element_blank(),
            axis.text = element_blank()
          ) +
          scale_color_gradientn(
            "Death rate \n(per 100,000)",
            colours = rainbow(8),
            limits = c(0, 4000),
            breaks = c(0, 2000, 4000),
            labels = c(0, "2,000", "4,000"),
            na.value = 'grey'
          )
        ggplotly(p, tooltip = "text") #%>% layout(height = 300, width = 600)
        
      } else if (input$county_map_select == 'State outlines') {
        p <- ggplot(data = df.county.hex.map,
                    aes(
                      x = x.use,
                      y = -y.use,
                      color = state.name,
                      text = paste(state.name)
                    )) +
          geom_point(size = 1) +
          labs(
            title = 'State outlines',
            x = '',
            y = '',
            colour = 'State name'
          ) +
          theme(
            legend.position = 'bottom',
            panel.background = element_rect(
              fill = "white",
              color = "white",
              size = 0.5,
              linetype = "solid"
            ),
            axis.ticks = element_blank(),
            axis.text = element_blank()
          )
        ggplotly(p, tooltip = "text")
        
      }
    }
    else if (input$county_map_time_select == '5 years (2015-2019)') {
      if (input$county_map_select == 'Opioid overdose death rate by county') {
        p <- ggplot(data = df.county.hex.map.2019,
                    aes(
                      x = x.use,
                      y = -y.use,
                      color = Crude.Rate.opioid.ods.5y,
                      text = paste(
                        format(
                          Crude.Rate.opioid.ods.5y,
                          format = 'd',
                          big.mark = ',',
                          digits = 2
                        ),
                        'deaths in',
                        str_to_title(County),
                        'County,',
                        state.name
                      )
                    )) +
          geom_point(size = 1) +
          labs(title = 'Opioid overdose death rate by county, averaged over 2015-2019',
               x = '',
               y = '') +
          theme(
            legend.position = 'bottom',
            panel.background = element_rect(
              fill = "white",
              color = "white",
              size = 0.5,
              linetype = "solid"
            ),
            axis.ticks = element_blank(),
            axis.text = element_blank()
          ) +
          scale_color_gradientn(
            "Death rate \n(per 100,000)",
            colours = rainbow(8),
            limits = c(0, 120),
            breaks = c(0, 20, 40, 60, 80, 100, 120),
            #labels = c(0, 50, 100),
            na.value = 'grey'
          )
        ggplotly(p, tooltip = "text")
        
      } else if (input$county_map_select == 'Drug overdose death rate by county') {
        p <- ggplot(data = df.county.hex.map.2019,
                    aes(
                      x = x.use,
                      y = -y.use,
                      color = Crude.Rate.all.ods.5y,
                      text = paste(
                        format(
                          Crude.Rate.all.ods.5y,
                          format = 'd',
                          big.mark = ',',
                          digits = 2
                        ),
                        'deaths in',
                        str_to_title(County),
                        'County,',
                        state.name
                      )
                    )) +
          geom_point(size = 1) +
          labs(title = 'Drug overdose death rate by county, averaged over 2015-2019',
               x = '',
               y = '') +
          theme(
            legend.position = 'bottom',
            panel.background = element_rect(
              fill = "white",
              color = "white",
              size = 0.5,
              linetype = "solid"
            ),
            axis.ticks = element_blank(),
            axis.text = element_blank()
          ) +
          scale_color_gradientn(
            "Death rate \n(per 100,000)",
            colours = rainbow(8),
            limits = c(0, 120),
            breaks = c(0, 20 , 40 , 60 , 80, 100, 120),
            #labels = c(0, "120"),
            na.value = 'grey'
          )
        ggplotly(p, tooltip = "text")
        
      }
      else if (input$county_map_select == 'Total death rate by county') {
        p <- ggplot(data = df.county.hex.map.2019,
                    aes(
                      x = x.use,
                      y = -y.use,
                      color = Crude.Rate.all.5y,
                      text = paste(
                        format(
                          Crude.Rate.all.5y,
                          format = 'd',
                          big.mark = ',',
                          digits = 2
                        ),
                        'deaths in',
                        str_to_title(County),
                        'County,',
                        state.name
                      )
                    )) +
          geom_point(size = 1) +
          labs(title = 'Total death rate by county, averaged over 2015-2019',
               x = '',
               y = '') +
          theme(
            legend.position = 'bottom',
            panel.background = element_rect(
              fill = "white",
              color = "white",
              size = 0.5,
              linetype = "solid"
            ),
            axis.ticks = element_blank(),
            axis.text = element_blank()
          ) +
          scale_color_gradientn(
            "Death rate \n(per 100,000)",
            colours = rainbow(8),
            limits = c(0, 2551),
            breaks = c(0, 500, 1000, 1500, 2000, 2500),
            labels = c(0, 500, "1,000", "1,500", "2,000", "2,500"),
            na.value = 'grey'
          )
        ggplotly(p, tooltip = "text")
        
      } else if (input$county_map_select == 'State outlines') {
        p <- ggplot(data = df.county.hex.map.2019,
                    aes(
                      x = x.use,
                      y = -y.use,
                      color = state.name,
                      text = paste(state.name)
                    )) +
          geom_point(size = 1) +
          labs(
            title = 'State outlines',
            x = '',
            y = '',
            colour = 'State name'
          ) +
          theme(
            legend.position = 'bottom',
            panel.background = element_rect(
              fill = "white",
              color = "white",
              size = 0.5,
              linetype = "solid"
            ),
            axis.ticks = element_blank(),
            axis.text = element_blank()
          )
        ggplotly(p, tooltip = "text")
        
      }
    }
    
  })
  
  #######         DEATHS by gender/age/race --------------------------------------
  output$death_distribution_plot <- renderPlotly({
    if (input$death_distribution_year_select == '2019') {
      df.death.dist <- df.death.dist2019
      year <- '2019'
    } else if (input$death_distribution_year_select == '2020') {
      df.death.dist <- df.death.dist2020
      year <- '2020'
    }
    if (input$dist_value == "Number of all deaths") {
      yvar = df.death.dist$Deaths.all
      title.lab = paste("Age distribution of \nall deaths in", year)
      y.lab = 'Deaths'
      ylim = 6e4 * c(-1, 1)
      breaks = 1e3 * c(-60, -30, 0, 30, 60)
      xtick.labs = c('60,000', '30,000', '0', '30,000', '60,000')
      df.death.dist$text = paste0(
        format(yvar, format = 'd', big.mark = ','),
        " total deaths in ",
        df.death.dist$Ages,
        " year old ",
        df.death.dist$Gender,
        's'
      )
    }
    else if (input$dist_value == "Number of opioid overdose deaths") {
      yvar = df.death.dist$Deaths.od
      title.lab = paste("Age distribution of \nopioid overdose deaths in", year)
      y.lab = 'Deaths'
      ylim = 2e3 * c(-1, 1)
      breaks = c(-2000, -1000, 0, 1000, 2000)
      xtick.labs = c('2,000', '1,000', '0', '1,000', '2,000')
      df.death.dist$text = paste0(
        format(yvar, format = 'd', big.mark = ','),
        " opioid overdose deaths in ",
        df.death.dist$Ages,
        " year old ",
        df.death.dist$Gender,
        's'
      )
    }
    else if (input$dist_value == "Number of all overdose deaths") {
      yvar = df.death.dist$Deaths.all.ods
      title.lab = paste("Age distribution of \ndrug overdose deaths in", year)
      y.lab = 'Deaths'
      ylim = 2e3 * c(-1, 1)
      breaks = 2e3 * c(-1, -0.5, 0, 0.5, 1)
      xtick.labs = c('2,000', '1,000', '0', '1,000', '2,000')
      df.death.dist$text = paste0(
        format(yvar, format = 'd', big.mark = ','),
        " drug overdose deaths in ",
        df.death.dist$Ages,
        " year old ",
        df.death.dist$Gender,
        's'
      )
    } else if (input$dist_value == "% of deaths due to opioid overdoses") {
      yvar = df.death.dist$Od.perc
      title.lab = paste("Opioid overdose deaths in", year, "\n(% of all deaths)")
      y.lab = "Opioid overdose deaths \n(% of deaths in each age group)"
      ylim = 30 * c(-1, 1) #change if needed
      xlim = c(0, 102)
      breaks = c(-30, -15, 0, 15, 30)
      xtick.labs = c('30%', '15%', '0%', '15%', '30%')
      df.death.dist$text = paste0(
        round(yvar, digits = 1),
        "% of deaths in ",
        df.death.dist$Ages,
        " year old ",
        df.death.dist$Gender,
        's',
        " are due to opioid overdoses"
      )
    }
    p <- ggplot(data = df.death.dist,
                mapping = aes(
                  x = Ages,
                  fill = Gender,
                  y = ifelse(
                    test = Gender == "male",
                    yes = -1 * yvar,
                    no = yvar
                  ),
                  text = text
                )) +
      geom_bar(stat = "identity") +
      scale_y_continuous(labels = xtick.labs,
                         limits = ylim,
                         breaks = breaks) +
      scale_fill_manual(values = c("male" = "paleturquoise3",
                                   "female" = "salmon2")) +
      guides(color = guide_legend(override.aes = list(size = 10))) +
      labs(y = y.lab, title = title.lab) +
      coord_flip() +
      theme(
        title = element_text(size = 8),
        legend.position = 'none',
        legend.title = element_text(size = 8)
      )
    ggplotly(p, tooltip = 'text')
  })
  
  
  
  output$deaths_by_race_plot <- renderPlotly({
    if (input$death_distribution_year_select == '2019') {
      df.death.hist <- df.death.hist2019
      year <- '2019'
    } else if (input$death_distribution_year_select == '2020') {
      df.death.hist <- df.death.hist2020
      year <- '2020'
    }
    if (input$dist_value == "Number of all deaths") {
      yvar <- df.death.hist$Deaths.all
      title.lab <- 'Racial and ethnic distribution \nof all deaths'
      y.lab <- "Deaths"
      x.lab <- 'Racial and ethnic distribution \nof all deaths'
      ylim = c(0, 1.5 * 10 ^ 6)
      breaks = c(0, .5 * 10 ^ 6, 1. * 10 ^ 6, 1.5 * 10 ^ 6)
      df.death.hist$text = paste0(
        format(yvar, format = 'd', big.mark = ','),
        " total deaths in ",
        df.death.hist$Race.Eth,
        ' ',
        df.death.hist$Gender,
        's'
      )
    } else if (input$dist_value == "Number of opioid overdose deaths") {
      yvar = df.death.hist$Deaths.opioids
      title.lab <-
        'Racial and ethnic distribution \nof opioid overdose deaths'
      y.lab = "Deaths"
      x.lab <-
        'Racial and ethnic distribution \nof opioid overdose deaths'
      ylim = c(0, 4.5e4)
      breaks = c(0, 15000, 30000, 45000)
      df.death.hist$text = paste0(
        format(yvar, format = 'd', big.mark = ','),
        " opioid overdose deaths in ",
        df.death.hist$Race.Eth,
        ' ',
        df.death.hist$Gender,
        's'
      ) 
    }
    else if (input$dist_value == "Number of all overdose deaths") {
      yvar = df.death.hist$Deaths.all.ods
      title.lab <-
        'Racial and ethnic distribution \nof drug overdose deaths'
      y.lab <- "Deaths"
      x.lab <-
        'Racial and ethnic distribution \nof drug overdose deaths'
      ylim = c(0, 4.5e4)
      breaks = c(0, 15000, 30000, 45000)
      df.death.hist$text = paste0(
        format(yvar, format = 'd', big.mark = ','),
        " drug overdose deaths in ",
        df.death.hist$Race.Eth,
        ' ',
        df.death.hist$Gender,
        's'
      )
    }
    else if (input$dist_value == "% of deaths due to opioid overdoses") {
      yvar <- df.death.hist$Opioid.perc
      title.lab <-
        'Opioid overdose deaths \nas % of deaths in each racial/ethnic group'
      y.lab = "Opioid overdose deaths \n(% of deaths in each racial/ethnic group)"
      x.lab <-
        'Opioid overdose deaths \n(% of deaths in each racial/ethnic group)'
      ylim = c(0, 4)
      breaks = c(0, 1, 2, 3, 4)
      df.death.hist$text = paste0(
        round(yvar, digits = 1),
        "% of deaths in ",
        df.death.hist$Race.Eth,
        ' ',
        df.death.hist$Gender,
        's',
        " are due to opioid overdoses"
      )
    }
    p <- ggplot(data = df.death.hist,
                aes(
                  x = Race.Eth,
                  y = yvar ,
                  fill = Gender,
                  text = text
                )) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(y = paste(y.lab), title = title.lab) + 
      scale_y_continuous(labels = scales::comma,
                         limits = ylim,
                         breaks = breaks) +
      scale_x_discrete(labels = c("Asian", "Black", "Hispanic", "Native", "White")) +
      scale_fill_manual(values = c("male" = "paleturquoise3",
                                   "female" = "salmon2")) +
      theme(
        title = element_text(size = 8),
        #legend.position = 'left',
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(angle = 0),
        axis.title.y = element_blank()
      ) +
      coord_flip()
    ggplotly(p, tooltip = 'text')
  })
  
  #######         DEATHS OVER TIME    --------------------------------------------
  output$race_eth_definition <- renderText({
    if (input$time_rate_group_select == "Race/ethnicity")
      "See 'Deaths by age/sex/race' tab for definition of race and ethnicity groups
    used here."
  })
  output$ods_over_time <- renderPlotly({
    if (input$time_rate_select == "Total deaths over time") {
      if (input$time_rate_group_select == 'Type of drug') {
        data = df.od.over.time
        y_var = df.od.over.time$Deaths
        group_var = df.od.over.time$Multiple.Cause.of.death
        title = 'Opioid overdose deaths over time, by substance'
        color = "Cause of death"
        y = "Number of deaths"
        ylim = c(0, 60000)
        breaks = c(0, 20000, 40000, 60000)
        labels = c('0', '20,000', '40,000', '60,000')
      } else if (input$time_rate_group_select == 'Gender') {
        data = df.od.over.time.gender
        y_var = df.od.over.time.gender$Deaths
        group_var = df.od.over.time.gender$Gender
        title = 'Opioid overdose deaths over time, by gender'
        color = "Gender"
        y = "Number of deaths"
        ylim = c(0, 50000)
        breaks = c(0, 10000, 20000, 30000, 40000, 50000)
        labels = c('0', '10,000', '20,000', '30,000', '40,000', '50,000')
      } else if (input$time_rate_group_select == 'Age') {
        data = df.od.over.time.age
        y_var = df.od.over.time.age$Deaths
        group_var = df.od.over.time.age$Ten.Year.Age.Groups.Code
        title = 'Opioid overdose deaths over time, by age'
        color = "Age groups"
        y = "Number of deaths"
        ylim = c(0, 20000)#c(0,26000)
        breaks = c(0, 5000, 10000, 15000, 20000)
        labels = c('0', '5,000', '10,000', '15,000', '20,000')
      } else if (input$time_rate_group_select == 'Race/ethnicity') {
        data = df.od.over.time.race
        y_var = df.od.over.time.race$Deaths
        group_var = df.od.over.time.race$Race.Eth
        title = 'Opioid overdose deaths over time, by race/ethnicity'
        color = "Race/ethnicity"
        y = "Number of deaths"
        ylim = c(0, 50000)
        breaks = c(0, 10000, 20000, 30000, 40000, 50000)
        labels = c('0', '10,000', '20,000', '30,000', '40,000', '50,000')
      }
      p <- ggplot(
        data,
        aes(
          x = Year,
          y = as.numeric(y_var),
          group = as.factor(group_var),
          color = group_var,
          text = paste(
            group_var,
            "<br>",
            as.numeric(y_var),
            "overdose deaths in",
            Year
          )
        )
      ) +
        geom_line() +
        time_plot_theme +
        guides(color = guide_legend(override.aes = list(size = 10))) +
        labs(title = title,
             color = color,
             y = y) +
        scale_y_continuous(breaks = breaks,
                           labels = labels,
                           limits = ylim)
      ggplotly(p, tooltip = 'text') %>%
        layout(legend = list(
          x = 0.0,
          y = 1.,
          font = list(size = 8)
        ))
    } else if (input$time_rate_select == "Crude rate over time") {
      if (input$time_rate_group_select == "Type of drug") {
        data = df.od.over.time
        y_var = df.od.over.time$Crude.Rate
        group_var = df.od.over.time$Multiple.Cause.of.death
        title = 'Rate of opioid overdose deaths over time, by substance'
        color = "Cause of death"
        y = "Rate of deaths (per 100,000)"
        ylim = c(0, 20)
        breaks = c(0, 10, 20)
        p <- ggplot(
          data,
          aes(
            x = Year,
            y = as.numeric(y_var),
            group = as.factor(group_var),
            color = group_var,
            text = paste(
              group_var,
              "<br>",
              as.numeric(y_var),
              "overdose deaths per 100,000 in",
              Year
            )
          )
        ) +
          geom_line(stat = 'identity') +
          time_plot_theme +
          scale_y_continuous(limits = ylim, breaks = breaks) +
          labs(title = title,
               color = color,
               y = y)
        ggplotly(p, tooltip = 'text') %>%
          layout(legend = list(x = 0.0, y = 1))
      } else if (input$time_rate_group_select == "Gender") {
        data = df.od.over.time.gender
        y_var = df.od.over.time.gender$Crude.Rate
        group_var = df.od.over.time.gender$Gender
        title = 'Rate of opioid overdose deaths over time, by gender'
        color = "Gender"
        y = "Rate of deaths (per 100,000)"
        ylim = c(0, 40)
        breaks = c(0, 10, 20, 30, 40)
        p <- ggplot(
          data,
          aes(
            x = Year,
            y = as.numeric(y_var),
            group = as.factor(group_var),
            color = group_var,
            #linetype = Gender,
            text = paste(
              group_var,
              "<br>",
              as.numeric(y_var),
              "overdose deaths per 100,000 in",
              Year
            )
          )
        ) +
          geom_line(stat = 'identity') +
          time_plot_theme +
          guides(color = guide_legend(override.aes = list(size = 10))) +
          labs(title = title,
               color = color,
               y = y) +
          scale_y_continuous(limits = ylim, breaks = breaks)
        ggplotly(p, tooltip = 'text') %>%
          layout(legend = list(x = 0.0, y = 1))
      } else if (input$time_rate_group_select == "Age") {
        data = df.od.over.time.age
        y_var = df.od.over.time.age$Crude.Rate
        group_var = df.od.over.time.age$Ten.Year.Age.Groups.Code
        title = 'Rate of opioid overdose deaths over time, by age'
        color = "Age groups"
        y = "Rate of deaths (per 100,000)"
        ylim = c(0, 50)
        breaks = c(0, 10, 20, 30, 40, 50)
        p <- ggplot(
          data,
          aes(
            x = Year,
            y = as.numeric(y_var),
            group = as.factor(group_var),
            color = group_var,
            text = paste(
              group_var,
              "<br>",
              as.numeric(y_var),
              "overdose deaths per 100,000 in",
              Year
            )
          )
        ) +
          geom_line(stat = 'identity') +
          time_plot_theme +
          guides(color = guide_legend(override.aes = list(size = 10))) +
          labs(title = title,
               color = color,
               y = y) +
          scale_y_continuous(limits = ylim, breaks = breaks)
        ggplotly(p, tooltip = 'text') %>%
          layout(legend = list(x = 0.0, y = 1))
      } else if (input$time_rate_group_select == "Race/ethnicity") {
        data = df.od.over.time.race
        y_var = df.od.over.time.race$Crude.Rate
        group_var = df.od.over.time.race$Race.Eth
        title = 'Rate of opioid overdose deaths over time, by race/ethnicity'
        color = "Race/ethnicity"
        y = "Rate of deaths (per 100,000)"
        ylim = c(0, 30)
        breaks = c(0, 10, 20, 30)
        p <- ggplot(
          data,
          aes(
            x = Year,
            y = as.numeric(y_var),
            group = as.factor(group_var),
            color = as.factor(group_var),
            #linetype = Gender,
            text = paste(
              group_var,
              "<br>",
              as.numeric(y_var),
              "overdose deaths per 100,000 in",
              Year
            )
          )
        ) +
          geom_line(stat = 'identity') +
          time_plot_theme +
          guides(color = guide_legend(override.aes = list(size = 10))) +
          labs(title = title,
               color = color,
               y = y) +
          scale_y_continuous(breaks = breaks, limits = ylim)
        ggplotly(p, tooltip = 'text') %>%
          layout(legend = list(x = 0.0, y = 1))
      }
      
    }
  })
  
  
  
  
  #######         CORRELATES OF BURDEN   -----------------------------------------
  output$correlator_descriptions <- renderText({
    if (input$correlates_year_select == '2019') {
      year <- '2019'
    } else if (input$correlates_year_select == '2020') {
      year <- '2020'
    }
    if (input$correlates_select == "Unemployment") {
      paste(
        "Unemployment measure used here is the percent of each county's",
        year,
        "civilian labor force (16 years and older) that are unemployed."
      )
    } else if (input$correlates_select == "Education") {
      paste(
        "Education measure used here is the percent of each county's
      population 25 years
      old or older, whose highest completed level of schooling was 12th grade and
      who have a high school diploma, from 2015-2019."
      )
    } else if (input$correlates_select == "Population density") {
      paste(
        "2010 population density is the county's population divided by size of county
    land area in square miles"
      )
    } else if (input$correlates_select == "Migration rate") {
      paste(
        "Migration rate used here is the change in each county's population in the
      10 year period from July 1, 2010 to July 1, 2019, due to net migration
      (including both domestic migration and immigration), as a percentage of the
      county's initial population. A negative rate means a net outflux of people
      into the county during the 10 year period, and a positive migration rate means
      a net influx."
      )
    } else if (input$correlates_select == "Per capita income")
    {
      paste(
        "Per capita income here is the yearly household income, in 2019 
        inflation-adjusted
      dollars, divided by the county population."
      )
    } else if (input$correlates_select == "Average household size") {
      paste(
        "Average household size is simply the county's population divided by the
      number of households in the county. from 2015-2019."
      )
    }
  })
  
  output$correlators_plot <- renderPlotly({
    if (input$correlates_year_select == '2019') {
      year <- '2019'
      df.county.correlates <- df.county.correlates2019
    } else if (input$correlates_year_select == '2020') {
      year <- '2020'
      df.county.correlates <- df.county.correlates2020
    }
    if (input$correlates_select == 'Migration rate') {
      xvar = df.county.correlates$NetMigrationRate1019
      yvar = df.county.correlates$Crude.Rate
      res1 = cor.test(
        df.county.correlates$Crude.Rate,
        df.county.correlates$NetMigrationRate1019,
        method = 'pearson'
      )
      res2 = cor.test(
        df.county.correlates$Crude.Rate,
        df.county.correlates$NetMigrationRate1019,
        method = 'spearman'
      )
      title = paste('Correlation between',
                    year,
                    'opioid death rate and migration rate')
      xlab = "Net migration rate between 2010-2019 (% of initial population)"
      ylab = paste(year, "Death rate by opioid overdoses (per 100,000)")
      #color = ""
      annotation.pearson = paste(
        'pearson coef:',
        round(res1$estimate, digits = 3),
        "with p-value:",
        signif(res1$p.value, digits = 3)
      )
      annotation.spearman = paste(
        'spearman coef:',
        round(res2$estimate, digits = 3),
        'with p-value:',
        signif(res2$p.value, digits = 3)
      )
      xpos1 = 30
      ypos1 = 82
      xpos2 = 30
      ypos2 = 72
      lm <-
        lm(formula = Crude.Rate ~ NetMigrationRate1019,
           data = df.county.correlates)
      slope = lm$coefficients[2]
      intercept = lm$coefficients[1]
    }
    else if (input$correlates_select == 'Education') {
      xvar = df.county.correlates$Ed2HSDiplomaOnlyPct
      yvar = df.county.correlates$Crude.Rate
      res1 = cor.test(
        df.county.correlates$Crude.Rate,
        df.county.correlates$Ed2HSDiplomaOnlyPct,
        method = 'pearson'
      )
      res2 = cor.test(
        df.county.correlates$Crude.Rate,
        df.county.correlates$Ed2HSDiplomaOnlyPct,
        method = 'spearman'
      )
      title = 'Correlation between opioid death rate and education'
      xlab = "Education level up to high school diploma (% of county population)"
      ylab = paste(year, "Death rate by opioid overdoses (per 100,000)")
      color = ""
      annotation.pearson = paste(
        'pearson coef:',
        round(res1$estimate, digits = 3),
        "with p-value:",
        signif(res1$p.value, digits = 3)
      )
      annotation.spearman = paste(
        'spearman coef:',
        round(res2$estimate, digits = 3),
        'with p-value:',
        signif(res2$p.value, digits = 3)
      )
      xpos1 = 25
      ypos1 = 92
      xpos2 = 25
      ypos2 = 82
      lm <-
        lm(formula = Crude.Rate ~ Ed2HSDiplomaOnlyPct,
           data = df.county.correlates)
      slope = lm$coefficients[2]
      intercept = lm$coefficients[1]
    }
    else if (input$correlates_select == 'Unemployment') {
      xvar = df.county.correlates$UnempRate
      yvar = df.county.correlates$Crude.Rate
      res1 = cor.test(df.county.correlates$Crude.Rate,
                      df.county.correlates$UnempRate,
                      method = 'pearson')
      res2 = cor.test(df.county.correlates$Crude.Rate,
                      df.county.correlates$UnempRate,
                      method = 'spearman')
      title = 'Correlation between opioid death rate and unemployment'
      xlab = paste(year, "unemployment rate (% of labor force)")
      ylab = paste(year, "Death rate by opioid overdoses (per 100,000)")
      color = ""
      annotation.pearson = paste(
        'pearson coef:',
        round(res1$estimate, digits = 3),
        "with p-value:",
        signif(res1$p.value, digits = 3)
      )
      annotation.spearman = paste(
        'spearman coef:',
        round(res2$estimate, digits = 3),
        'with p-value:',
        signif(res2$p.value, digits = 3)
      )
      xpos1 = 15
      ypos1 = 92
      xpos2 = 15
      ypos2 = 82
      lm <-
        lm(formula = Crude.Rate ~ UnempRate, data = df.county.correlates)
      slope = lm$coefficients[2]
      intercept = lm$coefficients[1]
    }
    else if (input$correlates_select == 'Per capita income') {
      xvar = df.county.correlates$PerCapitaInc
      yvar = df.county.correlates$Crude.Rate
      res1 = cor.test(
        df.county.correlates$Crude.Rate,
        df.county.correlates$PerCapitaInc,
        method = 'pearson'
      )
      res2 = cor.test(
        df.county.correlates$Crude.Rate,
        df.county.correlates$PerCapitaInc,
        method = 'spearman'
      )
      title = 'Correlation between opioid death rate and income'
      xlab = "Per capita yearly income (in 2019 inflation-adjusted $)"
      ylab = paste(year, "Death rate by opioid overdoses (per 100,000)")
      color = ""
      annotation.pearson = paste(
        'pearson coef:',
        round(res1$estimate, digits = 3),
        "with p-value:",
        signif(res1$p.value, digits = 3)
      )
      annotation.spearman = paste(
        'spearman coef:',
        round(res2$estimate, digits = 3),
        'with p-value:',
        signif(res2$p.value, digits = 3)
      )
      xpos1 = 55000
      ypos1 = 82
      xpos2 = 55000
      ypos2 = 72
      lm <-
        lm(formula = Crude.Rate ~ PerCapitaInc, data = df.county.correlates)
      slope = lm$coefficients[2]
      intercept = lm$coefficients[1]
    }
    else if (input$correlates_select == 'Population density') {
      xvar = log(df.county.correlates$PopDensity2010)
      yvar = df.county.correlates$Crude.Rate
      res1 = cor.test(
        df.county.correlates$Crude.Rate,
        log(df.county.correlates$PopDensity2010),
        method = 'pearson'
      )
      res2 = cor.test(
        df.county.correlates$Crude.Rate,
        log(df.county.correlates$PopDensity2010),
        method = 'spearman'
      )
      title = 'Correlation between opioid death rate and 2010 population density'
      xlab = "Population density (population per square mile)"
      ylab = paste(year, "Death rate by opioid overdoses (per 100,000)")
      color = ""
      annotation.pearson = paste(
        'pearson coef:',
        round(res1$estimate, digits = 3),
        "with p-value:",
        signif(res1$p.value, digits = 3)
      )
      annotation.spearman = paste(
        'spearman coef:',
        round(res2$estimate, digits = 3),
        'with p-value:',
        signif(res2$p.value, digits = 3)
      )
      xpos1 = 7.5
      ypos1 = 85
      xpos2 = 7.5
      ypos2 = 75
      lm <-
        lm(formula = Crude.Rate ~ PopDensity2010, data = df.county.correlates)
      slope = lm$coefficients[2]
      intercept = lm$coefficients[1]
    }
    else if (input$correlates_select == 'Average household size') {
      xvar = df.county.correlates$AvgHHSize
      yvar = df.county.correlates$Crude.Rate
      res1 = cor.test(df.county.correlates$Crude.Rate,
                      df.county.correlates$AvgHHSize,
                      method = 'pearson')
      res2 = cor.test(df.county.correlates$Crude.Rate,
                      df.county.correlates$AvgHHSize,
                      method = 'spearman')
      title = 'Correlation between opioid death rate and average household size'
      xlab = "Average household size (# of people per household)"
      ylab = paste(year, "Death rate by opioid overdoses (per 100,000)")
      color = ""
      annotation.pearson = paste(
        'pearson coef:',
        round(res1$estimate, digits = 3),
        "with p-value:",
        signif(res1$p.value, digits = 3)
      )
      annotation.spearman = paste(
        'spearman coef:',
        round(res2$estimate, digits = 3),
        'with p-value:',
        signif(res2$p.value, digits = 3)
      )
      xpos1 = 3.25
      ypos1 = 82
      xpos2 = 3.25
      ypos2 = 72
      lm <-
        lm(formula = Crude.Rate ~ AvgHHSize, data = df.county.correlates)
      slope = lm$coefficients[2]
      intercept = lm$coefficients[1]
    }
    p <- ggplot(df.county.correlates, aes(
      x = xvar,
      y = yvar,
      text = paste(county_name)
    )) +
      ylim(0, 100) +
      geom_point(alpha = 0.5,
                 colour = "paleturquoise3",
                 label = "Correlate") +
      geom_abline(
        colour = 'grey',
        size = 0.8,
        alpha = 0.7,
        slope = slope,
        intercept = intercept,
        label = "Regression line"
      ) +
      labs(x = xlab, y = ylab) +
      theme(
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 8)
      ) +
      theme_light() +
      annotate(
        geom = "text",
        x = xpos1,
        y = ypos1,
        label = annotation.pearson,
        color = "black"
      ) +
      annotate(
        geom = "text",
        x = xpos2,
        y = ypos2,
        label = annotation.spearman,
        color = "black"
      )
    ggplotly(p, tooltip = 'text') %>%
      layout(legend = list())
  })
  
  
  
  #######         PRESCRIPTION STATE MAP   ---------------------------------------
  
  output$prescription_map <- renderPlotly({
    g <- list(
      scope = "usa",
      projection = list(type = 'albers usa'),
      showlakes = FALSE,
      lakecolor = toRGB('white')
    )
    fig <- plot_geo(prescriptions,
                    locationmode = 'USA-states')
    
    if (input$select_presc_year == "2006") {
      var.year = prescriptions$Presc.2006
    } else if (input$select_presc_year == "2007") {
      var.year = prescriptions$Presc.2007
    }
    else if (input$select_presc_year == "2008") {
      var.year = prescriptions$Presc.2008
    }
    else if (input$select_presc_year == "2009") {
      var.year = prescriptions$Presc.2009
    }
    else if (input$select_presc_year == "2010") {
      var.year = prescriptions$Presc.2010
    }
    else if (input$select_presc_year == "2011") {
      var.year = prescriptions$Presc.2011
    }
    else if (input$select_presc_year == "2012") {
      var.year = prescriptions$Presc.2012
    }
    else if (input$select_presc_year == "2013") {
      var.year = prescriptions$Presc.2013
    }
    else if (input$select_presc_year == "2014") {
      var.year = prescriptions$Presc.2014
    }
    else if (input$select_presc_year == "2015") {
      var.year = prescriptions$Presc.2015
    }
    else if (input$select_presc_year == "2016") {
      var.year = prescriptions$Presc.2016
    }
    else if (input$select_presc_year == "2017") {
      var.year = prescriptions$Presc.2017
    }
    else if (input$select_presc_year == "2018") {
      var.year = prescriptions$Presc.2018
    }
    else if (input$select_presc_year == "2019") {
      var.year = prescriptions$Presc.2019
    }
    else if (input$select_presc_year == "2020") {
      var.year = prescriptions$Presc.2020
    }
    fig <- fig %>% add_trace(
      z = ~ var.year,
      color = ~ var.year,
      text = ~ State,
      locations = ~ State.abbv,
      hovertemplate = paste(
        "Prescription rate: ",
        round(var.year, digits = 1),
        " <br>State: %{text} <extra></extra>"
      ),
      name = '',
      colors = 'Blues'
    )
    fig <- fig %>% layout(
      geo = g) %>%
      colorbar(title = "Prescription rate \n(per 100 people)",
               limits = c(0, 150))
    fig
  })
  
  
  
  #######         PRESCRIPTIONS OVER TIME   --------------------------------------
  
  output$prescription_evolution <- renderPlotly({
    p <- ggplot(
      data = prescriptions.evolution,
      mapping = aes(
        x = Year,
        y = Presc.per100,
        color = Source,
        text = paste(
          Year,
          "rate: ",
          round(Presc.per100, digits = 0),
          "<br>",
          "Data source: ",
          Source
        )
      )
    ) +
      geom_point(size = 2) +
      scale_color_manual(values = c("Pain physician journal" = "paleturquoise3",
                                    "CDC" = "salmon2")) +
      scale_x_continuous(limits = c(1990, 2020),
                         breaks = c(1990, 2000, 2010, 2020)) +
      labs(title = "Evolution of opioid dispensing rate", x = "Year",
           y = "Prescriptions dispensed per 100 people") +
      time_plot_theme +
      guides(color = guide_legend(override.aes = list(size = 10))) +
      scale_y_continuous(limits = c(0, 100))
    ggplotly(p, tooltip = 'text') %>%
      layout(legend = list(
        x = 0.0,
        y = 1.,
        font = list(size = 8)
      ))
  })

  
}
