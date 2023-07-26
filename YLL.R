
### Generate lifetables by state and gender ------------------------------------

calc.Yll.State <- function(){
  # Calculates the years of life lost through opioid overdose deaths
  # required files: "Ages.Rda"
  #                 df.all.deaths.age.gender.state
  #                 df.all.deaths.state.aa
  #                 df.od.deaths.age.gender.state
  # both NVSS lifetables Lifetable_M and Lifetable_F
  
  # Create vector with row for each age
  age.data <- load("Ages.Rda")
  
  # Create vector with names of all states
  states.vec <- unique(df.all.deaths.age.gender.state$State.Text)
  
  # Create empty data frame
  df.YLL.State <- aggregate(Deaths ~ State, df.all.deaths.state.aa, sum)
  
  # for separating gender
  for (gender in c("F", "M")){
    j = 0
    
    # for separating by state
    for (state in sort(states.vec)){
      j = j + 1
      
      lifetable <- subset(df.all.deaths.age.gender.state, Gender == gender &
                            State.Text == state, 
                          select = c("Ages.Text", "Ages", "Deaths", "Population",
                                     "State.Text"))
      lifetable <- lifetable %>%
        mutate(Crude.Rate = Deaths/Population*1e5) %>%
        arrange(Ages)
      
      lifetable.od <- subset(df.od.deaths.age.gender.state, 
                             Gender == gender & State.Text == state, 
                             select = c("Ages.Text", "Ages", "Deaths", 
                                        "Population", "State.Text"))
      lifetable.od <- lifetable.od %>%
        mutate(Crude.Rate = Deaths/Population*1e5) %>%
        arrange(Ages)
      
      
      lifetable.nvss <- read.csv(paste("NVSS Lifetables/Lifetable_",
                                       gender,".csv", sep = ""), header = TRUE)
      #remove commas from NVSS data
      for (i in 3:ncol(lifetable.nvss)){
        lifetable.nvss[,i] <- as.numeric(as.character(gsub(",","", lifetable.nvss[,i])))
      }
      
      lifetable <- lifetable %>%
        right_join(Age.data, by = c("Ages.Text","Ages")) %>%
        replace_na(list(Crude.Rate = 0, Deaths = 0)) %>%
        arrange(Ages)
      lifetable.od <- lifetable.od %>%
        right_join(Age.data, by = c("Ages.Text","Ages")) %>%
        replace_na(list(Crude.Rate = 0, Deaths = 0)) %>%
        arrange(Ages)
      
      lifetable.all <- lifetable %>%
        inner_join(lifetable.od, by = c("Ages", "Ages.Text"), 
                   suffix = c(".all", ".od")) %>%
        filter(Ages.Text != "Not Stated") %>%
        mutate(Od.Perc = Deaths.od/Deaths.all*100)
      
      lifetable.all <- lifetable.all %>%
        mutate(Crude.Rate.no.cause = Crude.Rate.all - Crude.Rate.od)
      
      
      #create lifetables for ALL deaths and ALL deaths minus OD deaths
      lifetable.final.OD <- make.lifetable(cause = TRUE, lifetable.all, 
                                           lifetable.nvss)
      lifetable.final.no.OD <- make.lifetable(cause = FALSE, lifetable.all,
                                              lifetable.nvss)
      
      # if is.age.filter = TRUE, only keeps rows within age range of age filter
      if (is.age.filter){
        lifetable.final.OD <- lifetable.final.OD %>%
          filter(Ages > ages.filter[1] & Ages < ages.filter[2])
        
        lifetable.final.no.OD <- lifetable.final.no.OD %>%
          filter(Ages > ages.filter[1] & Ages < ages.filter[2])
      }
      
      # calculate YLL
      YLL.table <- lifetable.all # create empty dataframe with rows for each age
      if (is.age.filter){
        YLL.table <- lifetable.all %>%
          filter(Ages > ages.filter[1] & Ages < ages.filter[2])
      }
      
      YLL.table$LE.OD <- lifetable.final.OD$e
      YLL.table$LE.no.OD <- lifetable.final.no.OD$e
      YLL.table <- YLL.table %>%
        mutate(LE.diff = LE.no.OD - LE.OD)
      YLL.table <- YLL.table %>%
        mutate(YLL = Deaths.od*(LE.no.OD-0.5))
      
      
      if (gender == 'M') {
        df.YLL.State$Deaths.OD.M[j] <- sum(YLL.table$Deaths.od)
        df.YLL.State$YLL.M[j] <- sum(YLL.table$YLL)
        df.YLL.State$YLLpp.M[j] <- sum(YLL.table$YLL)/sum(YLL.table$Deaths.od)
      }
      else{
        df.YLL.State$Deaths.OD.F[j] <- sum(YLL.table$Deaths.od)
        df.YLL.State$YLL.F[j] <- sum(YLL.table$YLL)
        df.YLL.State$YLLpp.F[j] <- sum(YLL.table$YLL)/sum(YLL.table$Deaths.od)
      }
    }
    
  }
  
  df.YLL.State$Population <- aggregate(Population ~ State, 
                                       df.od.deaths.gender.state, 
                                       sum)$Population
  
  df.YLL.State <- df.YLL.State %>%
    mutate(Deaths.both = Deaths.OD.F + Deaths.OD.M,
           YLL.both = YLL.F + YLL.M,
           YLL.per.pop = YLL.both / Population)
  
  
  # create map dataframe with YLL data
  df.YLL.map <- merge(df.YLL.State,urbnmapr::states,
                      by.x="State",by.y="state_name",all=TRUE)
  
  return(df.YLL.map)
}

df.YLL.map.all.ages <- calc.Yll.State() 

