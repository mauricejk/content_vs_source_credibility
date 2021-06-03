## Libraries
library(tidyverse)
library(Hmisc)
library(lme4)
library(boot)

# Load data
base_path <- dirname(rstudioapi::getSourceEditorContext()$path)
df = droplevels(read.csv(file.path(base_path, 'responses_main_study_long.csv')))

## Snippet function: Summarize responses across groups
response_summary <- function(d, splitvars=NULL, valuevar='answer', filterval=TRUE){
  d %>% group_by_at(vars(one_of(splitvars, valuevar))) %>%
    dplyr::summarise(count = n()) %>% 
    group_by_at(vars(one_of(splitvars))) %>% 
    mutate(
      total = sum(count), 
      estimate = count/total
    ) %>% 
    rowwise() %>%
    mutate(
      lower = Hmisc::binconf(count,total)[2],
      upper = Hmisc::binconf(count,total)[3]
    ) %>% ungroup() %>% filter(!!sym(valuevar)==filterval) %>%
    arrange(vars(one_of(splitvars))) %>%
    mutate(row = 1:n())
}

## Snippet function: Summarize responses across groups for bootstrapping
response_summary_bs <- function(data, indices, long_data, splitvars) {
  data$weight <- indices
  left_join(long_data, data[,c('subject_id', 'weight')], by='subject_id') %>%
    # mutate(weight = ifelse(weight==0, 0, 1)) %>% # Without replacement
    group_by_at(vars(one_of(splitvars, 'answer'))) %>%
    dplyr::summarise(count = sum(weight), .groups = "drop") %>% 
    group_by_at(vars(one_of(splitvars))) %>% 
    mutate(mean = count/sum(count)) %>% 
    ungroup() %>% filter(answer==TRUE) %>%
    arrange(vars(one_of(splitvars))) -> r
  return(r$mean)
}


# Main manuscript
## Figure 1: Source vs. content effects
df %>% filter(relevant_for_analysis, group=='control') -> d
d %>% group_by(subject_id) %>% slice(1) %>% mutate(strata=paste(study,subject_politics)) -> d_groups
boot(d_groups, strata=factor(d_groups$strata), statistic=response_summary_bs, stype='f', R=1000, 
     long_data=d, splitvars=c('source_alignment', 'headline_alignment', 'study')) -> bs

d %>% response_summary(splitvars=c('source_alignment', 'headline_alignment','study')) %>% 
  rowwise() %>% mutate(
    lower_bs = boot.ci(bs, type="bca", index=row)['bca'][[1]][4],
    upper_bs = boot.ci(bs, type="bca", index=row)['bca'][[1]][5],
    headline_alignment = ifelse(headline_alignment, 'Favorable content', 'Unfavorable content'),
    source_alignment = ifelse(source_alignment, 'Trusted source', 'Mistrusted source')
  ) -> d_graph

pos = position_dodge(width = 0.15)
d_graph %>%
  mutate(study = recode(study, 'Study 1'='A. Strong Ideological Content', 
                        'Study 2'='B. Weak Ideological Content')) %>%
  ggplot(aes(y=estimate, x=source_alignment, color=headline_alignment, group=headline_alignment)) +
  geom_errorbar(aes(ymin=lower_bs, ymax=upper_bs), width=0.1, pos=pos) +
  geom_point(size=3, position=pos) + geom_line(pos=pos) + 
  scale_color_manual(values=c('black', '#c12b1e')) + facet_grid(~study) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) + #limits = c(0.365, 0.72)
  labs(x='Source attribution', y='Percent who believe the headline', 
       color='Content alignment') + 
  theme_bw() + theme(legend.position='right', axis.text=element_text(size=9.3),
                     axis.title.x = element_text(margin = margin(t = 6, r = 0, b = 0, l = 0))) 

## Figure 2: Incentive treatment
df %>% filter(relevant_for_analysis) -> d
d %>% group_by(subject_id) %>% slice(1) %>% mutate(strata=paste(study,subject_politics, group)) -> d_groups
boot(d_groups, strata=factor(d_groups$strata), statistic=response_summary_bs, stype='f', R=1000, 
     long_data=d, splitvars=c('group', 'headline_alignment','study')) -> bs

d %>% response_summary(splitvars=c('group', 'headline_alignment', 'study')) %>% 
  rowwise() %>% mutate(
    lower_bs = boot.ci(bs, type="bca", index=row)['bca'][[1]][4],
    upper_bs = boot.ci(bs, type="bca", index=row)['bca'][[1]][5],
    headline_alignment = ifelse(headline_alignment, 'Favorable content', 'Unfavorable content'),
    group = ifelse(group=='control', 'Without incentives', 'With incentives'),
    group = factor(group, levels=c('Without incentives', 'With incentives'))
  ) -> d_graph

d_graph %>%
  mutate(study = recode(study, 'Study 1'='A. Strong Ideological Content', 
                        'Study 2'='B. Weak Ideological Content')) %>%
  ggplot(aes(y=estimate, x=group, color=headline_alignment, group=headline_alignment)) +
  geom_errorbar(aes(ymin=lower_bs, ymax=upper_bs), width=0.1, pos=pos) +
  geom_point(size=3, position=pos) + geom_line(pos=pos) + 
  scale_color_manual(values=c('black', '#c12b1e')) + facet_grid(~study) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) + # limits = c(0.38, 0.71)
  labs(x='Accuracy incentives', y='Percent who believe the headline', color='Content alignment') + 
  theme_bw() + theme(legend.position='right', axis.text=element_text(size=9.3),
                     axis.title.x = element_text(margin = margin(t = 6, r = 0, b = 0, l = 0)))

# Supplement
## Figure 1, additional analysis
### Split news evaluations by participant ideology 
df %>% filter(relevant_for_analysis, group=='control') %>%
  response_summary(splitvars=c('source_alignment', 'headline_alignment','demo_ideology_forced')) %>% 
  rowwise() %>% mutate(
    headline_alignment = ifelse(headline_alignment, 'Favorable content', 'Unfavorable content'),
    source_alignment = ifelse(source_alignment, 'Trusted source', 'Mistrusted source'),
    demo_ideology_forced = recode(demo_ideology_forced, 'Left or center-left'='Participants leaning liberal', 
                                  'Right or center-right'='Participants leaning conservative')
    ) %>%
  ggplot(aes(y=estimate, x=source_alignment, color=headline_alignment, group=headline_alignment)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.1, pos=pos) +
  geom_point(size=3, position=pos) + geom_line(pos=pos) + 
  scale_color_manual(values=c('black', '#c12b1e')) + facet_grid(~demo_ideology_forced) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) + #limits = c(0.365, 0.72)
  labs(x='Source attribution', y='Percent who believe the headline', 
       color='Content alignment') + 
  theme_bw() + theme(legend.position='right', axis.text=element_text(size=9.3),
                     axis.title.x = element_text(margin = margin(t = 6, r = 0, b = 0, l = 0))) 

### Analyze news evaluations by participant party identification 
df %>% filter(relevant_for_analysis, group=='control') %>%
  filter(demo_party %in% c('Democrat', 'Republican')) %>%
  mutate(
    party_ideology = recode(demo_party, 'Democrat'='liberal', 'Republican'='conservative'),
    source_alignment = party_ideology == source_politics,
    headline_alignment = party_ideology == headline_politics,
  ) %>%
  response_summary(splitvars=c('source_alignment', 'headline_alignment','study')) %>% 
  rowwise() %>% mutate(
    headline_alignment = ifelse(headline_alignment, 'Favorable content', 'Unfavorable content'),
    source_alignment = ifelse(source_alignment, 'Trusted source', 'Mistrusted source'),
    # demo_party = paste(demo_party, 'participants')
  ) %>%
  mutate(study = recode(study, 'Study 1'='A. Strong Ideological Content', 
                        'Study 2'='B. Weak Ideological Content')) %>%
  ggplot(aes(y=estimate, x=source_alignment, color=headline_alignment, group=headline_alignment)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.1, pos=pos) +
  geom_point(size=3, position=pos) + geom_line(pos=pos) + 
  scale_color_manual(values=c('black', '#c12b1e')) + facet_grid(~study) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) + #limits = c(0.365, 0.72)
  labs(x='Source attribution', y='Percent who believe the headline', 
       color='Content alignment') + 
  theme_bw() + theme(legend.position='right', axis.text=element_text(size=9.3),
                     axis.title.x = element_text(margin = margin(t = 6, r = 0, b = 0, l = 0))) 

### Only show responses where the source trust manipulation succeeded
df %>% rowwise() %>%
  filter(relevant_for_analysis, group=='control') %>%
  mutate(
    source_trust_match = ifelse((source_trust_num >= 0.5) == (source_alignment), 'match', 'deviate')
  ) %>% filter(source_trust_match == 'match') %>% ungroup() %>%
  response_summary(splitvars=c('source_alignment', 'headline_alignment','study')) %>% 
  rowwise() %>% mutate(
    study = recode(study, 'Study 1'='A. Strong Ideological Content', 'Study 2'='B. Weak Ideological Content'),
    headline_alignment = ifelse(headline_alignment, 'Favorable content', 'Unfavorable content'),
    source_alignment = ifelse(source_alignment, 'Trusted source', 'Mistrusted source'),
  ) %>%
  ggplot(aes(y=estimate, x=source_alignment, color=headline_alignment, group=headline_alignment)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.1, pos=pos) +
  geom_point(size=3, position=pos) + geom_line(pos=pos) + 
  scale_color_manual(values=c('black', '#c12b1e')) + facet_grid(~study) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) + #limits = c(0.365, 0.72)
  labs(x='Source attribution', y='Percent who believe the headline', 
       color='Content alignment') + 
  theme_bw() + theme(legend.position='right', axis.text=element_text(size=9.3),
                     axis.title.x = element_text(margin = margin(t = 6, r = 0, b = 0, l = 0))) 

## Figure 2, additional analyses
### Split news evaluations by participant ideology 
df %>% filter(relevant_for_analysis) %>%
  response_summary(splitvars=c('group', 'headline_alignment','demo_ideology_forced')) %>% 
  rowwise() %>% mutate(
    headline_alignment = ifelse(headline_alignment, 'Favorable content', 'Unfavorable content'),
    demo_ideology_forced = recode(demo_ideology_forced, 'Left or center-left'='Participants leaning liberal', 
                                  'Right or center-right'='Participants leaning conservative'),
    group = recode(group, control='Without incentives', bonus='With incentives'),
    group = factor(group, levels=c('Without incentives', 'With incentives'))
  ) %>%
  ggplot(aes(y=estimate, x=group, color=headline_alignment, group=headline_alignment)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.1, pos=pos) +
  geom_point(size=3, position=pos) + geom_line(pos=pos) + 
  scale_color_manual(values=c('black', '#c12b1e')) + facet_grid(~demo_ideology_forced) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) + #limits = c(0.365, 0.72)
  labs(x='Accuracy incentives', y='Percent who believe the headline', 
       color='Content alignment') + 
  theme_bw() + theme(legend.position='right', axis.text=element_text(size=9.3),
                     axis.title.x = element_text(margin = margin(t = 6, r = 0, b = 0, l = 0))) 

### Analyze news evaluations by participant party identification 
df %>% filter(relevant_for_analysis) %>%
  filter(demo_party %in% c('Democrat', 'Republican')) %>%
  mutate(
    party_ideology = recode(demo_party, 'Democrat'='liberal', 'Republican'='conservative'),
    source_alignment = party_ideology == source_politics,
    headline_alignment = party_ideology == headline_politics,
  ) %>%
  response_summary(splitvars=c('group', 'headline_alignment','study')) %>% 
  rowwise() %>% mutate(
    headline_alignment = ifelse(headline_alignment, 'Favorable content', 'Unfavorable content'),
    group = recode(group, control='Without incentives', bonus='With incentives'),
    group = factor(group, levels=c('Without incentives', 'With incentives'))
  ) %>%
  mutate(study = recode(study, 'Study 1'='A. Strong Ideological Content', 
                        'Study 2'='B. Weak Ideological Content')) %>%
  ggplot(aes(y=estimate, x=group, color=headline_alignment, group=headline_alignment)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.1, pos=pos) +
  geom_point(size=3, position=pos) + geom_line(pos=pos) + 
  scale_color_manual(values=c('black', '#c12b1e')) + facet_grid(~study) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) + #limits = c(0.365, 0.72)
  labs(x='Accuracy incentives', y='Percent who believe the headline', 
       color='Content alignment') + 
  theme_bw() + theme(legend.position='right', axis.text=element_text(size=9.3),
                     axis.title.x = element_text(margin = margin(t = 6, r = 0, b = 0, l = 0))) 

### Only show responses where the source trust manipulation succeeded
df %>% filter(relevant_for_analysis) %>%
  response_summary(splitvars=c('group', 'headline_alignment','source_alignment')) %>% 
  rowwise() %>% mutate(
    headline_alignment = ifelse(headline_alignment, 'Favorable content', 'Unfavorable content'),
    group = recode(group, control='Without incentives', bonus='With incentives'),
    group = factor(group, levels=c('Without incentives', 'With incentives')),
    source_alignment = ifelse(source_alignment, 'Trusted source', 'Mistrusted source'),
  ) %>%
  ggplot(aes(y=estimate, x=group, color=headline_alignment, group=headline_alignment)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.1, pos=pos) +
  geom_point(size=3, position=pos) + geom_line(pos=pos) + 
  scale_color_manual(values=c('black', '#c12b1e')) + facet_grid(~source_alignment) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) + #limits = c(0.365, 0.72)
  labs(x='Accuracy incentives', y='Percent who believe the headline', 
       color='Content alignment') + 
  theme_bw() + theme(legend.position='right', axis.text=element_text(size=9.3),
                     axis.title.x = element_text(margin = margin(t = 6, r = 0, b = 0, l = 0))) 

## Source and content manipulation checks
### Source attribution ratings by source name and participant ideology
df %>% 
  filter(relevant_for_analysis, group=='control') %>%
  mutate(
    subject_politics = recode(subject_politics, liberal='Liberal', conservative='Conservative'),
    source_trust = ifelse(source_trust=='Entirely', 'A lot', source_trust),
    source_trust = ifelse(source_trust=='Barely', 'Somewhat', source_trust),
    source_trust_num = ifelse(source_trust=='A lot', 1, ifelse(source_trust=='Not at all', 0, 0.5))
  ) %>%
  group_by(source, subject_politics, study) %>%
  summarise(
    mean = Hmisc::smean.cl.normal(source_trust_num)['Mean'],
    round_mean = round(mean,2),
    lower = Hmisc::smean.cl.normal(source_trust_num)['Lower'],
    upper = Hmisc::smean.cl.normal(source_trust_num)['Upper'],
  ) %>%
  mutate(
    subject_politics = paste(subject_politics, 'participants'),
    source = recode(source, bbart='Breitbart\nNews', cnn='CNN', 
                    drudge='Drudge\nReport',fox='Fox\nNews',
                    nyt='New York\nTimes', huff='Huffington\nPost'),
    source = factor(source, levels = c('Breitbart\nNews', 'Drudge\nReport', 'Fox\nNews',
                                       'Huffington\nPost', 'New York\nTimes', 'CNN'))
  ) %>%
  ggplot(aes(x=source, y=mean, fill=subject_politics, group=subject_politics)) +
  geom_bar(stat='identity', width=0.5, color='white', position=position_dodge(), alpha=0.8) + # position="dodge"
  scale_fill_manual(values=c('#c12b1e','#020a7b')) + facet_grid(~study, scales='free_x',space = "free_x") + 
  geom_errorbar(aes(ymin=lower, ymax=upper), color='black', width=0.18, position=position_dodge(width=0.5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  labs(fill = "Participant politics", color='', x='News source', y='Trust rating') +
  theme_bw() + theme(legend.position='right', axis.text=element_text(size=9.3),
                     axis.title.x = element_text(margin = margin(t = 6, r = 0, b = 0, l = 0)))

### Correlation between source attribution in study design and post-study survey
pos = position_dodge(width = 0.15)
df %>% 
  # filter(relevant_for_analysis, group=='control') %>%
  group_by(source_trust, source_alignment, study) %>%
  summarise(count = n()) %>% group_by(source_alignment, study) %>%
  mutate(
    percentage = count/sum(count),
    # percentage_text = ifelse(source_trust != 'Entirely', paste0(round(percentage * 100), '%'), ''),
    percentage_text = ifelse(percentage > 0.1, paste0(round(percentage * 100), '%'), ''),
    # source_trust = factor(source_trust, levels=c('Low', 'Moderate','High')),
    source_trust = factor(source_trust, levels=rev(c('Not at all', 'Barely','Somewhat','A lot','Entirely'))),
    source_alignment = ifelse(source_alignment, 'Trusted source', 'Mistrusted source'),
  ) %>% 
  ggplot(aes(x=source_alignment, y=percentage, fill=source_trust, group = source_trust)) +
  geom_bar(stat='identity', width=0.5, color='white') + # position="dodge"
  geom_bar(stat='identity', width=0.5, fill='white', alpha=0.25) + facet_grid(~study) +
  geom_text(aes(label=percentage_text), position = position_stack(vjust = .5)) +
  # scale_fill_manual(values=c('#c12b1e','darkgrey', 'black')) +
  # scale_fill_manual(values=c('#c12b1e', '#ae6353', '#898989', '#434343', '#000000')) +
  scale_fill_manual(values=rev(c('#c12b1e', '#ae6353', '#898989', '#434343', '#000000'))) +
  # geom_errorbar(aes(ymin=lower, ymax=upper, y=mean, group=label), color='black', data=trust_summary, width=0.05, pos=pos) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  # geom_line(aes(y=mean, color=label, group=label), data=trust_summary) + 
  scale_color_manual(values=c('black')) + labs(fill = "Reported source trust\n(post-study survey)", color='', x='Source classification (study design)', y='Percent who trust the source') +
  theme_bw() + theme(legend.position='right', axis.text=element_text(size=9.3),
                     axis.title.x = element_text(margin = margin(t = 6, r = 0, b = 0, l = 0))) 

cor.test(as.numeric(dfx$source_trust_num >= 0.5), as.numeric(dfx$source_alignment))
agreements = sum((dfx$source_trust_num >= 0.5) == dfx$source_alignment)
random_agreement = sum(sample(dfx$source_trust_num >= 0.5) == dfx$source_alignment)
kappa = (agreements/nrow(dfx)-random_agreement/nrow(dfx))/(1-random_agreement/nrow(dfx)) # 0.37
  
### Crrelation between content alignment in study design and in the independent pre-study robustness check
droplevels(read.csv(file.path(base_path, 'responses_pretest_long.csv'))) %>%
  mutate(study = ifelse(grepl('x', headline), 'Study 1', 'Study 2')) -> df_pre

table(df_pre$ideology)

df_pre %>% 
  filter(group == 'ideology') %>%
  filter(ideology != "") %>%
  group_by(headline_politics, ideology_num, study) %>%
  summarise(count = n()) %>% group_by(headline_politics, study) %>%
  mutate(
    percentage = count/sum(count),
    percentage_text = paste0(round(percentage * 100), '%'),
    ideology_text = ifelse(ideology_num < 0, "Favorable to liberals", 
                           ifelse(ideology_num > 0, "Favorable to conservatives", 
                                  "Politically neutral")),
    headline_politics = recode(headline_politics, Dem="Favorable to\nliberals", Rep="Favorable to\nconservatives"),
    ideology_text = factor(ideology_text, levels=c("Favorable to liberals", "Politically neutral","Favorable to conservatives"))
  ) %>%
  ggplot(aes(x=headline_politics, y=percentage, fill=ideology_text, group = ideology_text)) +
  geom_bar(stat='identity', width=0.5, color='white') + # position="dodge"
  geom_bar(stat='identity', width=0.5, fill='white', alpha=0.25) +
  geom_text(aes(label=percentage_text), position = position_stack(vjust = .5)) +
  scale_fill_manual(values=c('#020a7b','grey','#c12b1e')) + facet_grid(~study) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  scale_color_manual(values=c('black')) + labs(fill = "Content alignment labels\n(independent survey)", color='', 
                          x='Content classification (study design)', y='Percent of content labels') +
  theme_bw() + theme(legend.position='right', axis.text=element_text(size=9.3),
                     axis.title.x = element_text(margin = margin(t = 6, r = 0, b = 0, l = 0))) 

df_pre %>% filter(group == 'ideology') %>% mutate(random_politics = sample(headline_politics)) -> d
cor.test(as.numeric(d$headline_politics=='Dem'), as.numeric(d$ideology_num<0))
cor.test(as.numeric(d$headline_politics=='Rep'), as.numeric(d$ideology_num>0))
agreements = sum(ifelse(d$headline_politics=='Dem'&d$ideology_num<0, 1, 
                        ifelse(d$headline_politics=='Rep'& d$ideology_num>0, 1, 0)))
random_agreement = sum(ifelse(d$random_politics=='Dem'&d$ideology_num<0, 1, 
                              ifelse(d$random_politics=='Rep'& d$ideology_num>0, 1, 0)))
kappa = (agreements/nrow(d)-random_agreement/nrow(d))/(1-random_agreement/nrow(d)) # 0.16

## Statistical modeling
### Regression model
df$study
df %>% 
  filter(relevant_for_analysis) %>% #
  filter(study == 'Study 2') %>%
  mutate(
    headline_alignment = ifelse(headline_alignment,'aligned','misaligned'),
    source_alignment = ifelse(source_alignment,'aligned','misaligned'),
    group = factor(group, levels = c('control','bonus'))
  ) %>%
  glm(formula = answer ~ headline_alignment*group + source_alignment*group, family=binomial(link='logit')) %>% summary()
  
df %>% 
  filter(relevant_for_analysis, group=='control',headline_alignment!=source_alignment) %>%
  glm(formula = answer ~ headline_alignment, family=binomial(link='logit')) %>% summary()

### Moderator analysis
df %>% 
  filter(relevant_for_analysis, group=='control') %>% #
  mutate(
    demo_gender = ifelse(demo_gender=='Other', 'Female', demo_gender),
    demo_education = recode(demo_education, 'Less than a high school diploma'=0, 'High school degree or equivalent (e.g. GED)'=1,
                            'Bachelor’s degree (e.g. BA, BS)'=2, 'Doctorate (e.g. PhD, EdD)'=4,
                            'Associate degree (e.g. AA, AS)'=2, 'Master’s degree (e.g. MA, MS)'=3, 'Professional degree (e.g. MD, JD)'=3),
    demo_ideology_strength = recode(demo_ideology, 'conservative'=1, 'liberal'=1, 'no opinion'=0, 'somewhat conservative'=0.5,
                                    'somewhat liberal'=0.5),
    headline_alignment = ifelse(headline_alignment,'aligned','misaligned'),
    source_alignment = ifelse(source_alignment,'aligned','misaligned')
  ) %>%
  # glm(formula = answer ~ headline_alignment*demo_education + source_alignment*demo_education, family=binomial(link='logit')) -> m1
  # glm(formula = answer ~ headline_alignment*demo_age + source_alignment*demo_age, family=binomial(link='logit')) -> m2
  # glm(formula = answer ~ headline_alignment*demo_gender + source_alignment*demo_gender, family=binomial(link='logit')) -> m3
  glm(formula = answer ~ headline_alignment*demo_ideology_strength + source_alignment*demo_ideology_strength, family=binomial(link='logit')) -> m4

df %>% 
  filter(relevant_for_analysis) %>%
  mutate(
    demo_education = recode(demo_education, 'Less than a high school diploma'=0, 'High school degree or equivalent (e.g. GED)'=1,
                            'Bachelor’s degree (e.g. BA, BS)'=2, 'Doctorate (e.g. PhD, EdD)'=4,
                            'Associate degree (e.g. AA, AS)'=2, 'Master’s degree (e.g. MA, MS)'=3, 'Professional degree (e.g. MD, JD)'=3),
    demo_ideology_strength = recode(demo_ideology, 'conservative'=1, 'liberal'=1, 'no opinion'=0, 'somewhat conservative'=0.5,
                                    'somewhat liberal'=0.5),
    headline_alignment = ifelse(headline_alignment,'aligned','misaligned'),
    source_alignment = ifelse(source_alignment,'aligned','misaligned'),
    group = factor(group, levels = c('control', 'bonus'))
  ) %>% 
  # glm(formula = answer ~ headline_alignment*group*demo_education, family=binomial(link='logit')) -> m6
  # glm(formula = answer ~ headline_alignment*group*demo_age, family=binomial(link='logit')) -> m7
  # glm(formula = answer ~ headline_alignment*group*submit_time, family=binomial(link='logit')) -> m8
  glm(formula = answer ~ headline_alignment*group*demo_ideology_strength, family=binomial(link='logit'))-> m9

library(stargazer)
summary(m8)
stargazer(m1, m2, m3, m4, type='html', out='~/model2.doc',
          star.cutoffs = c(.05, .01, .001))
stargazer(m6, m7, m8, m9, type='html', out='~/model3.doc',
          star.cutoffs = c(.05, .01, .001))

## Additional analyses (not included in the supplement)
## News belief by source name and participant ideology
  df %>% 
  response_summary(splitvars=c('subject_politics', 'study', 'source')) %>%
    mutate(
      source = recode(source, bbart='Breitbart\nNews', cnn='CNN', 
                      drudge='Drudge\nReport',fox='Fox\nNews',
                      nyt='New York\nTimes', huff='Huffington\nPost'),
      source = factor(source, levels = c('Breitbart\nNews', 'Drudge\nReport', 'Fox\nNews',
                                         'New York\nTimes', 'Huffington\nPost', 'CNN')),
      # source = ifelse(study == 'Study 1', paste0(' ', source), source),
    ) %>%
  ggplot(aes(y=estimate, x=source, color=subject_politics, group=subject_politics)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2, alpha=0.5, pos=pos) +
  geom_point(size=2.5, position=pos) +
  scale_color_manual(values=c('#c12b1e','#020a7b')) + facet_grid(~study, scales='free_x', space='free_x') + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L), limits = c(0, 1)) + #
  labs(x='Source', y='Percent who believe the source', 
       color='Participant\nideology') + 
  theme_bw() + theme(legend.position='right', axis.text=element_text(size=9.3),
                     axis.title.x = element_text(margin = margin(t = 6, r = 0, b = 0, l = 0))) 

  
## Disagreement on individual headlines by participant ideology
df %>% 
  filter(relevant_for_analysis) %>% 
  response_summary(splitvars=c('study', 'headline', 'headline_politics', 'headline_alignment')) %>%
  # rbind(list("Study 2","Rep_10", "conservative", FALSE, TRUE,1, 1, 0.03, 0, 0.2, 1)) %>%
  mutate(
    headline = str_replace(headline, 'Dem_', 'L' ),
    headline = str_replace(headline, 'Rep_', 'C' ),
    headline = ifelse(study=='Study 1', paste0(headline, '_S1'), paste0(headline,'_S2')),
    ci = ifelse(study=='Study 1', 0.19, 0.327)
  ) %>%
  group_by(headline, study) %>% mutate(
    # estimate = ifelse(headline_politics=='conservative', 0, estimate),
    cred_aligned = ifelse(headline_alignment==TRUE, estimate, 0),
    cred_midaligned = ifelse(headline_alignment==FALSE, estimate, 0),
    cred_diff = sum(cred_aligned) - sum(cred_midaligned),
    headline_politics = ifelse(headline_politics=='liberal', 'Liberal', 'Conservative')
  ) %>%
  filter(headline_alignment==TRUE) %>%
  ggplot(aes(y=cred_diff, x=reorder(headline, cred_diff), color=headline_politics)) +
  geom_rect(aes(ymin =0.1225, ymax=0.1788, xmin=-Inf, xmax=Inf), alpha=0.1, fill='grey', color='white') +
  geom_errorbar(aes(ymin=cred_diff-ci/2, ymax=cred_diff+ci/2), width=0.2, alpha=0.5) +
  geom_point(size=2.5) + 
  geom_hline(aes(yintercept = 0), alpha=0.5, linetype="solid") +
  geom_hline(aes(yintercept = 0.1507), alpha=0.5, linetype="dashed") +
  scale_color_manual(values=c('#c12b1e','#020a7b')) +
  # facet_grid(c(~headline_politics, ~study), scales='free_x', space='free_x') + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) + #
  labs(x='Headline', y='Content alignment effect', 
       color='Headline \nideology') + 
  theme_bw() + theme(legend.position='right', axis.text=element_text(size=9.3),
                     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                     axis.title.x = element_text(margin = margin(t = 6, r = 0, b = 0, l = 0))) 

## Reduction of headline disagreement due to accuracy treatment
df %>% 
  filter(relevant_for_analysis) %>% 
  response_summary(splitvars=c('study', 'group', 'headline', 'headline_politics', 'headline_alignment')) %>%
  rbind(list("Study 2","control", "Rep_10", "conservative", FALSE, TRUE,1, 1, 0.03, 0, 0.2, 1)) %>%
  mutate(
    headline = str_replace(headline, 'Dem_', 'L' ),
    headline = str_replace(headline, 'Rep_', 'C' ),
    headline = ifelse(study=='Study 1', paste0(headline, '_S1'), paste0(headline,'_S2')),
    ci = ifelse(study=='Study 1', 0.3365, 0.5819)
  ) %>%
  group_by(headline, study, group) %>% mutate(
    # estimate = ifelse(headline_politics=='conservative', 0, estimate),
    cred_aligned = ifelse(headline_alignment==TRUE, estimate, 0),
    cred_midaligned = ifelse(headline_alignment==FALSE, estimate, 0),
    cred_diff = sum(cred_aligned) - sum(cred_midaligned),
    headline_politics = ifelse(headline_politics=='liberal', 'Liberal', 'Conservative')
  ) %>% filter(headline_alignment==TRUE) %>% 
  group_by(headline, study) %>% mutate(
    # estimate = ifelse(headline_politics=='conservative', 0, estimate),
    diff_control = ifelse(group=='control', cred_diff, 0),
    diff_bonus = ifelse(group=='bonus', cred_diff, 0),
    treat_diff = sum(diff_bonus) - sum(diff_control)
  ) %>% filter(group=='control') %>% 

  ggplot(aes(y=treat_diff, x=reorder(headline, treat_diff), color=headline_politics)) +
  geom_rect(aes(ymin =-0.1452, ymax=-0.0299, xmin=-Inf, xmax=Inf), alpha=0.1, fill='grey', color='white') +
  geom_errorbar(aes(ymin=treat_diff-ci/2, ymax=treat_diff+ci/2), width=0.2, alpha=0.5, pos=pos) +
  geom_point(size=2.5, position=pos) + 
  geom_hline(aes(yintercept = 0), alpha=0.5, linetype="solid") +
  geom_hline(aes(yintercept = -0.0907), alpha=0.5, linetype="dashed") +
  scale_color_manual(values=c('#c12b1e','#020a7b')) +
  # facet_grid(c(~headline_politics, ~study), scales='free_x', space='free_x') + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) + #
  labs(x='Headline', y='Change in disagreement\nwith accuracy incentives', 
       color='Headline \nideology') + 
  theme_bw() + theme(legend.position='right', axis.text=element_text(size=9.3),
                     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                     axis.title.x = element_text(margin = margin(t = 6, r = 0, b = 0, l = 0))) 


  