#We provide this code, but do not provide the Pakistan FIES data as we were only given permission to include Afghanistan. Therefore this cannot be fully reproduced without access to the FIES data which would
#need to be requested from the FAO. 

library(tidyverse)
library(haven)

pak_2022 <- read_dta('fies/PAK_2022_FIES_v01_EN_M_v01_A_OCS.dta')  #Again, this data is not supplied in the Nature repo for editor/peer-review 

pak_2022 <- pak_2022 %>%
  mutate(N_adults_numeric = case_when(
    N_adults == 0 ~ 1,             # Convert households with "0" adults to 1. While imperfect, it assumes that there is at least one adult in the household -- the person responding to the survey
    #N_adults == "10+" ~ 10,        # Not needed here since Kantar gives the exact number of adults -- although we could cap the weight at n_adult = 10 
    TRUE ~ as.numeric(N_adults)     # Retain all other values
  ), 
  N_child_numeric = case_when(
    TRUE ~ as.numeric(N_child)      # Convert "01", "02", etc., to 1, 2, etc.
  )
  ) %>%
  mutate(child_weight = (wt / N_adults_numeric  ) * N_child_numeric )


# This function was shared by Sara Viviani from the FAO. Since we do not have strata/PSU in the FAO FIES data available to compute directly, she recommended just making an assumption that the design effect is 2. It has been edited to extract the MOE to use more cleanly in a tibble 

moe=function(prob,rs,wt,sd=NULL,psu=NULL,strata=NULL,conf.level=0.9){
  library(survey)
  # Computes margins of error around the estimated prevalence, as the result of the
  # combination of sampling error and measurement error
  # prob = probability of being beyond the threshold for each case
  # rs = raw score for each case
  # wt = weight to be assigned to each case
  # psu = "cluster" variable (primary sampling unit)
  # strata (as there are strata with singleton PSU,need to use "survey.lonely.psu" options)
  # Compute the average probability for each rs
  n = length(prob)
  if (is.null(wt)) {wt = rep(1,n)}
  if (is.null(sd)) {sd = 1}
  if(is.null(strata) | sum(is.na(strata))==length(strata)) strata=rep(1,n)
  if(is.null(psu) | sum(is.na(psu))==length(psu)) psu=rep(1,n)
  options(survey.lonely.psu = "adjust")
  # Impute all missing in psu and strata
  if(sum(is.na((psu)))==length(psu)) psu=rep(1, length(prob))
  if(sum(is.na((strata)))==length(strata)) strata=rep(1, length(prob))
  psu[is.na(psu)]=rep(1, sum(is.na(psu)))
  strata[is.na(strata)]=rep(1, sum(is.na(strata)))
  if(sum(psu==1)==length(psu) & sum(strata==1)==length(strata)){
    svydesign=svydesign(id=~1, weights = ~ wt, lonely.psu=getOption("survey.lonely.psu"="adjust"),
                        data=data.frame(prob))
  } else{
    svydesign=svydesign(id=psu,weights = ~ wt,strata=strata,nest=T,lonely.psu=getOption("survey.lonely.psu"="adjust"),
                        data=data.frame(prob))
  }
  se_s = SE(svymean(~prob, svydesign,  deff = T,na.rm=T))*sd
  # if(length(unique(sort(prob)))!=length(unique(sort(rs)))){
  #   k=length(unique(sort(prob)))
  #   rs=rowSums(XX[,1:(k-1)])
  # }
  p = sort(unique(prob))%*%table(prob,rs)/colSums(table(prob,rs))
  
  ##as you can see, new weights are created here. Th
  wrs = NULL
  n1 = NULL
  for (i in sort(unique(rs))){
    wrs[i+1] = sum(wt[which(rs==i)])/sum(wt)*length(wt)
    n1 [i+1] = sum(rs==i)
  }
  
  wrs=wrs[!is.na(wrs)]
  n1=n1[!is.na(n1)]
  var_m = (p*(1-p)/n1)%*%(wrs/sum(wrs))^2
  se_m = sqrt(var_m)
  
  se = sqrt(se_s^2+se_m^2)
  moe = se*(-qnorm((1-conf.level)/2))
  # return(list(moe=moe, se_m=se_m,se_s=se_s)) I modify the return for ease of use in our analysis 
  return(moe[[1]])
}

in_country_prevalence <- pak_2022 %>%
  filter(!is.na(Raw_score)) %>% #the NA is in the dataset is problematic for the moe function, so filtering out here 
  group_by(as_factor(Area)) %>%
  summarise(prevalence_mod_sev = (sum(Prob_Mod_Sev*wt,na.rm=T)) / (sum(wt,na.rm=T)),
            prevalence_mod_sev_moe = moe(prob = Prob_Mod_Sev, rs=Raw_score, wt=wt, sd=sqrt(2), conf.level=.95),
            prevalence_sev = (sum(Prob_sev*wt,na.rm=T)) / (sum(wt,na.rm=T)),
            prevalence_sev_moe = moe(prob = Prob_sev, rs=Raw_score, wt=wt, sd=sqrt(2), conf.level=.95)
  ) %>%
  ungroup() %>%
  mutate(age_group = "15+") %>%
  rbind(
    pak_2022 %>%
      filter(!is.na(child_weight)) %>% 
      filter(!is.na(Raw_score)) %>%
      group_by(as_factor(Area)) %>%
      summarise(prevalence_mod_sev = (sum(Prob_Mod_Sev*child_weight,na.rm=T)) / (sum(child_weight,na.rm=T)), 
                prevalence_mod_sev_moe = moe(prob = Prob_Mod_Sev, rs=Raw_score, wt=child_weight, sd=2, conf.level=.95),
                prevalence_sev = (sum(Prob_sev*child_weight,na.rm=T)) / (sum(child_weight,na.rm=T)),
                prevalence_sev_moe = moe(prob = Prob_sev, rs=Raw_score, wt=child_weight, sd=2, conf.level=.95)
      ) %>%
      ungroup() %>%
      mutate(age_group = "14_under")
  )

# We then use https://www.pbs.gov.pk/sites/default/files/population/2023/tables/table_5_national.pdf to estimate people (first value is rural, second is urban). 
# PDF is included in /graphics/data/ for editors/reviewers but we hardcode this in as follows: 

census_pop_15_over = c(147268383-(3357790+23625438+44814833+62887908), 93189706-(1846131+12845983+23928724+34645831))

census_pop_14_under = c((3357790+23625438+44814833+62887908), (1846131+12845983+23928724+34645831))

in_country_people <- in_country_prevalence %>%
  mutate(census_pop = c((93189706-(1846131+12845983+23928724+34645831)), 147268383-(3357790+23625438+44814833+62887908), (1846131+12845983+23928724+34645831), (3357790+23625438+44814833+62887908))) %>% #flip order based on my tibble
  mutate(people_mod_sev = prevalence_mod_sev * census_pop) %>%
  mutate(people_mod_sev_moe = prevalence_mod_sev_moe * census_pop) %>%
  group_by(`as_factor(Area)`) %>%
  summarise(people_mod_sev = sum(people_mod_sev),people_mod_sev_moe = sum(people_mod_sev_moe), census_pop=sum(census_pop)) %>%
  mutate(lower_bound = people_mod_sev - people_mod_sev_moe) %>%
  mutate(upper_bound = people_mod_sev + people_mod_sev_moe) %>%
  rename(Census_UrbanRural = `as_factor(Area)`)

graphic <- in_country_people %>%
  ggplot(aes(x = factor(Census_UrbanRural, levels=c("Towns/Rural", "Urban/Suburbs")), y = people_mod_sev / 1e6, fill = Census_UrbanRural)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(people_mod_sev / 1e6, 1), "M")), vjust = -0.5, size = 2.5, fontface = "bold", family = "sans") +
  scale_fill_manual(values = c("Towns/Rural" = "#228B22", "Urban/Suburbs" = "#A0522D")) +
  geom_errorbar(aes(ymin = (lower_bound) / 1e6, ymax = (upper_bound) / 1e6 ), 
                width = 0.15, color = "gray30", linewidth = 0.5, alpha = 0.7) +
  labs(
    x = "Census Urban/Rural",
    y = "People (millions)"
    #title = "2022 Pakistan Moderate to Severe Food Insecure People by Census Definition of Urban Rural",
    #caption = "Error bars show 95% CI"
  ) +
  theme_minimal(base_size = 7, base_family = "sans") + 
  theme(plot.title = element_text(size = 8, face = "bold", family = "sans"), 
        legend.position="none")

ggsave("graphics/output/Figure_3.jpeg", plot = graphic, 
       width = 180, height = 150, units="mm", dpi = 300, device = "jpeg")