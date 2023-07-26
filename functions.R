
#these functions are needed for generating data files from raw data

Clean.Mortality.Data <- function(data){
  
  data$Notes=NULL
  
  if ("Gender" %in% colnames(data)) {
    data$Gender=NULL
    colnames(data)[colnames(data)=="Gender.Code"] <- "Gender"
  }
  
  if ("Race" %in% colnames(data)){
    data$Race.Code=NULL
  }
  if ("Hispanic.Origin.Code" %in% colnames(data)){
    data$Hispanic.Origin.Code=NULL
    colnames(data)[colnames(data)=="Hispanic.Origin"] <- "Ethnicity"
  } 
  
  if ("State.Code" %in% colnames(data)){
    colnames(data)[colnames(data)=="State.Code"] <- "State.Num"
    colnames(data)[colnames(data)=="State"] <- "State.Text"
  }
  
  if ("Single.Year.Ages" %in% colnames(data)){
    colnames(data)[colnames(data)=="Single.Year.Ages"] <- "Ages.Text"
    colnames(data)[colnames(data)=="Single.Year.Ages.Code"] <- "Ages"
  }
  
  if (("Race" %in% colnames(data)) & ("Ethnicity" %in% colnames(data))){
    #re-group the race and ethnicity categories into 5 combined categories
    #print(aggregate(Deaths ~ Race + Ethnicity, data, sum))
    data$Race.Eth=data$Race
    data$Race.Eth="Not Stated" #if either category missing
    data$Race.Eth[data$Race=="White" & data$Ethnicity=="Not Hispanic or Latino"]="White_NonHisp"
    data$Race.Eth[data$Race=="Black or African American" & data$Ethnicity=="Not Hispanic or Latino"]="Black_NonHisp"
    data$Race.Eth[data$Race=="Asian or Pacific Islander" & data$Ethnicity=="Not Hispanic or Latino"]="Asian_NonHisp"
    data$Race.Eth[data$Race=="American Indian or Alaska Native" & data$Ethnicity=="Not Hispanic or Latino"]="Native_NonHisp"
    data$Race.Eth[data$Ethnicity=="Hispanic or Latino"]="Hisp"
    
    if ("State.Num" %in% colnames(data)){
      data=data[c("Ages.Text", "Ages", "Gender","Ethnicity","Race","Race.Eth","State.Text","State.Num","Deaths","Population","Crude.Rate")]
    }else{
      data=data[c("Ages.Text", "Ages", "Gender","Ethnicity","Race","Race.Eth","Deaths","Population","Crude.Rate")]
    }
  }
  
  
  if (("Race" %in% colnames(data)) & (!"Ethnicity" %in% colnames(data))){
    #print(aggregate(Deaths ~ Race, data, sum))
    data$Race.Eth=data$Race
    data$Race.Eth="Not Stated" #if either category missing
    data$Race.Eth[data$Race=="White"]="White"
    data$Race.Eth[data$Race=="Black or African American"]="Black"
    data$Race.Eth[data$Race=="Asian or Pacific Islander"]="Asian"
    data$Race.Eth[data$Race=="American Indian or Alaska Native"]="Native"
    
    if ("State.Num" %in% colnames(data)){
      data=data[c("Ages.Text", "Ages", "Gender","Race","Race.Eth","State.Text","State.Num","Deaths","Population","Crude.Rate")]   
    }else{
      data=data[c("Ages.Text", "Ages", "Gender","Race","Race.Eth","Deaths","Population","Crude.Rate")]   
    }
  }
  
  if ((!"Race" %in% colnames(data)) & ("Ethnicity" %in% colnames(data))){
    #print(aggregate(Deaths ~ Ethnicity, data, sum))
    data$Race.Eth=data$Ethnicity
    data$Race.Eth="Not Stated" #if either category missing
    data$Race.Eth[data$Ethnicity=="Hispanic or Latino"]="Hisp"
    data$Race.Eth[data$Ethnicity=="Not Hispanic or Latino"]="NonHisp"
    
    if ("State.Num" %in% colnames(data)){
      data=data[c("Ages.Text", "Ages", "Gender","Ethnicity","Race.Eth","State.Text","State.Num","Deaths","Population","Crude.Rate")]
    } else{
      data=data[c("Ages.Text", "Ages", "Gender","Ethnicity","Race.Eth","Deaths","Population","Crude.Rate")]
    }
  }
  
  if ((!"Race" %in% colnames(data)) & ("!Ethnicity" %in% colnames(data))){
    data$Race.Eth=Ages.Text
    data$Race.Eth="Not Stated" #if either category missing
    
    if ("State.Num" %in% colnames(data)){
      data=data[c("Ages.Text", "Ages", "Gender","State.Text","State.Num","Deaths","Population","Crude.Rate")]
    }else{
      data=data[c("Ages.Text", "Ages", "Gender","Deaths","Population","Crude.Rate")]
    }
  }
  
  #print(sapply(data, class)) # check data types
  
  if ("Ages" %in% colnames(data)) {
    #remove extra rows that have empty data (can be identified by empty values in any column)
    data=data[data$Ages!="",]
    data=data[is.na(data$Ages)==FALSE,]
    #fix data types of variables that got imported incorrectly as factors
    
    #convert ages into numbers
    levels(data$Ages)[levels(data$Ages)=="NS"] = "101"
    data$Ages=as.numeric(as.character(data$Ages))
  }
  
  #convert population into numbers
  #NOTE: many population values say "not applicable", for all ages >85
  levels(data$Population)[levels(data$Population)=="Not Applicable"] = "0"
  data$Population=as.numeric(as.character(data$Population))
  
  #print(sapply(data, class)) # check data types
  
  #re-calculate crude death rate
  data$Crude.Rate=100000*data$Deaths/data$Population
  
  return(data)
}

make.lifetable <- function(cause = TRUE, df.data, lifetable){
  
  # df.data should be a data frame containing a column "Crude.Rate.all" 
  # (OPTIONAL) and a column "Crude.Rate.cause" which is the rate of deaths due to cause
  # and a column "Crude.Rate.no.cause" which is the rate of (all deaths - cause deaths)
  # this last column is the estimate total death rate, if cause were not present
  
  end.index = length(lifetable$Ages)
  mid.index = length(df.data$Crude.Rate.all[df.data$Crude.Rate.all > 0]) - 1 
  #match(TRUE, df.data$Crude.Rate.all == 0) - 1 #85
  
  #make empty vectors which will be the columns of the lifetable matrix
  a = rep(0.5, end.index) # for one year intervals, a = 1/2. deaths on average occur midway through the interval
  q = rep(0, end.index) # q is the prob of dying
  l = rep(0, end.index) # l is the number of people surviving
  d = rep(0, end.index) # d is the number of people dying
  L = rep(0, end.index) # L is the total number of person-years
  m = rep(0, end.index) # vector of mortality rates
  Tot = rep(0, end.index) # T is Total number of person–years lived above age x
  e = rep(0, end.index) # e is Expectation of life at age x
  
  
  # set initial values for first row
  l0 = 1e5  
  a[1] = 0.25 #for infants
  if (cause){
    m[1] = df.data$Crude.Rate.all[1]/l0 #crude death rate per 100,000
    
  } else{
    m[1] = df.data$Crude.Rate.no.cause[1]/l0 #crude death rate per 100,000
  }
  q[1] = m[1] / (1+(1-a[1])*m[1])
  l[1] = l0
  d[1] = q[1] * l[1]
  
  # fill in all rows from lifetable matrix
  for (i in 2:mid.index){
    if (cause){
      m[i] = df.data$Crude.Rate.all[i]/l0 #crude death rate per 100,000
      
    } else{
      m[i] = df.data$Crude.Rate.no.cause[i]/l0 #crude death rate per 100,000
    }
    
    q[i] = m[i] / (1+(1-a[i])*m[i]) # probability of surviving to age x
    l[i] = l[i-1] * (1-q[i-1]) # number of people surviving to age x
    d[i] = q[i] * l[i] # number of people dying between age x and x+1
    L[i-1] = l[i] + d[i-1] * a[i-1] #total person-years above x
    
  }
  
  # fill in from nvss table
  for (i in (mid.index+1):(end.index-1)){
    
    m[i] = 0 # crude death rate unavailable past this point
    q[i] = lifetable$q[i] * (q[mid.index]/lifetable$q[mid.index]) # probability of surviving to age x
    l[i] = l[i-1] * (1-q[i-1]) # number of people surviving to age x
    d[i] = q[i] * l[i] # number of people dying between age x and x+1
    L[i-1] = l[i] + d[i-1] * a[i-1] #total person-years above x
  }
  
  
  # final row
  i=end.index
  q[i]=1
  l[i]=l[i-1]*(1-q[i-1])
  d[i]=q[i]*l[i]
  L[i-1]=l[i]+d[i-1]*a[i-1]
  a[i]=lifetable$L[i]/lifetable$d[i] #back out what NVSS a value must have been
  L[i]=a[i]*d[i]
  
  # Calculate Tot and e
  for (i in 1:end.index) {
    
    Tot[i]=sum(L[i:end.index])
    e[i]=Tot[i]/l[i]
    
  }
  
  #return(Tot)
  final.lifetable <- lifetable
  
  final.lifetable$a = a
  final.lifetable$q = q
  final.lifetable$l = l
  final.lifetable$d = d
  final.lifetable$L = L
  final.lifetable$Tot = Tot
  final.lifetable$e = e
  final.lifetable$m = m
  
  
  return(final.lifetable)
  
} # end function