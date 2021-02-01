## Libraries
library(tidyverse)
library(Hmisc)
library(lme4)
library(boot)

# Load data
base_path <- dirname(rstudioapi::getSourceEditorContext()$path)
dfx = droplevels(read.csv(file.path(base_path, 'responses_long_format.csv')))

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


## Plot results

# Figure 1: Source vs. content effects
dfx %>% filter(relevant_for_analysis, group=='control') -> d
d %>% group_by(subject_id) %>% slice(1) %>% mutate(strata=paste(study,subject_politics)) -> d_groups
boot(d_groups, strata=factor(d_groups$strata), statistic=response_summary_bs, stype='f', R=1000, 
     long_data=d, splitvars=c('source_alignment', 'headline_alignment', 'study')) -> bs

d %>% response_summary(splitvars=c('source_alignment', 'headline_alignment','study')) %>% 
  rowwise() %>% mutate(
    lower_bs = boot.ci(bs, type="bca", index=row)['bca'][[1]][4],
    upper_bs = boot.ci(bs, type="bca", index=row)['bca'][[1]][5],
    headline_alignment = ifelse(headline_alignment, 'Aligned content', 'Misaligned content'),
    source_alignment = ifelse(source_alignment, 'Aligned source', 'Misaligned source')
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
  labs(x='Source alignment', y='Percent who believe the headline', 
       color='Content alignment') + 
  theme_bw() + theme(legend.position='right', axis.text=element_text(size=9.3),
                     axis.title.x = element_text(margin = margin(t = 6, r = 0, b = 0, l = 0))) 



# Figure 2: Incentive treatment
dfx %>% filter(relevant_for_analysis) -> d
d %>% group_by(subject_id) %>% slice(1) %>% mutate(strata=paste(study,subject_politics, group)) -> d_groups
boot(d_groups, strata=factor(d_groups$strata), statistic=response_summary_bs, stype='f', R=1000, 
     long_data=d, splitvars=c('group', 'headline_alignment','study')) -> bs

d %>% response_summary(splitvars=c('group', 'headline_alignment', 'study')) %>% 
  rowwise() %>% mutate(
    lower_bs = boot.ci(bs, type="bca", index=row)['bca'][[1]][4],
    upper_bs = boot.ci(bs, type="bca", index=row)['bca'][[1]][5],
    headline_alignment = ifelse(headline_alignment, 'Aligned content', 'Misaligned content'),
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

# Figure 3: Source trust ratings
pos = position_dodge(width = 0.15)

dfx %>% 
  filter(relevant_for_analysis, group=='control') %>%
  group_by(source_trust, source_alignment) %>%
  mutate(
    # source_trust = recode(source_trust, 'A lot'='High', Entirely='High', Barely='Moderate',
    #                       Somewhat='Moderate', 'Not at all'='Low'),
  ) %>%
  summarise(count = n()) %>% group_by(source_alignment) %>%
  mutate(
    percentage = count/sum(count),
    percentage_text = ifelse(source_trust != 'Entirely', paste0(round(percentage * 100), '%'), ''),
    # source_trust = factor(source_trust, levels=c('Low', 'Moderate','High')),
    source_trust = factor(source_trust, levels=rev(c('Not at all', 'Barely','Somewhat','A lot','Entirely'))),
    source_alignment = ifelse(source_alignment, 'Aligned source', 'Misaligned source'),
  ) %>% 
  ggplot(aes(x=source_alignment, y=percentage, fill=source_trust, group = source_trust)) +
  geom_bar(stat='identity', width=0.5, color='white') + # position="dodge"
  geom_bar(stat='identity', width=0.5, fill='white', alpha=0.25) +
  geom_text(aes(label=percentage_text), position = position_stack(vjust = .5)) +
  # scale_fill_manual(values=c('#c12b1e','darkgrey', 'black')) +
  # scale_fill_manual(values=c('#c12b1e', '#ae6353', '#898989', '#434343', '#000000')) +
  scale_fill_manual(values=rev(c('#c12b1e', '#ae6353', '#898989', '#434343', '#000000'))) +
  # geom_errorbar(aes(ymin=lower, ymax=upper, y=mean, group=label), color='black', data=trust_summary, width=0.05, pos=pos) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  # geom_line(aes(y=mean, color=label, group=label), data=trust_summary) + 
  scale_color_manual(values=c('black')) + labs(fill = "Source trust", color='', x='Source alignment', y='Percent who trust the source') +
  theme_bw() + theme(legend.position='right', axis.text=element_text(size=9.3),
                     axis.title.x = element_text(margin = margin(t = 6, r = 0, b = 0, l = 0))) 

## Supplement: Fig 1 detail
dfx %>% filter(relevant_for_analysis) -> d
d %>% group_by(subject_id) %>% slice(1) %>% mutate(strata=paste(study,subject_politics, group)) -> d_groups
boot(d_groups, strata=factor(d_groups$strata), statistic=response_summary_bs, stype='f', R=1000, 
     long_data=d, splitvars=c('group', 'demo_ideology', 'headline_alignment')) -> bs

pos = position_dodge(width = 0.15)
d %>% response_summary(splitvars=c('group', 'demo_ideology', 'headline_alignment')) %>% 
  rowwise() %>% mutate(
    lower_bs = boot.ci(bs, type="bca", index=row)['bca'][[1]][4],
    upper_bs = boot.ci(bs, type="bca", index=row)['bca'][[1]][5],
    headline_alignment = ifelse(headline_alignment, 'Aligned content', 'Misaligned content'),
    # source_alignment = ifelse(source_alignment, 'Aligned source', 'Misaligned source'),
    # demo_party = paste(demo_party, 'participants'),
    # demo_party = factor(demo_party, levels=c('Republican participants', 'Democrat participants')),
    group = recode(group, control='Without incentives', bonus='With accuracy incentives'),
    # subject_politics = recode(subject_politics, liberal='Liberal participants', conservative='Conservative participants')
  ) %>%
  filter(demo_ideology!='no opinion') %>%
  ggplot(aes(y=estimate, x=headline_alignment, color=demo_ideology, group=demo_ideology)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.1, pos=pos) +
  geom_point(size=3, position=pos) + geom_line(pos=pos) +
  scale_color_manual(values=c('#c12b1e', '#020a7b','#d7938d', '#9ba0e7')) +
  facet_grid(~group) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) + #limits = c(0.365, 0.72)
  labs(x='Content alignment', y='Percent who believe the headline', 
       color='Participant ideology') + 
  theme_bw() + theme(legend.position='right', axis.text=element_text(size=9.3),
                     axis.title.x = element_text(margin = margin(t = 6, r = 0, b = 0, l = 0))) 

## Supplement: Fig 2 detail
dfx %>% filter(relevant_for_analysis, demo_party %in% c('Democrat','Republican')) -> d
d %>% group_by(subject_id) %>% slice(1) %>% mutate(strata=paste(study,demo_party, group)) -> d_groups
boot(d_groups, strata=factor(d_groups$strata), statistic=response_summary_bs, stype='f', R=1000, 
     long_data=d, splitvars=c('demo_party', 'group', 'headline_alignment')) -> bs

d %>% response_summary(splitvars=c('demo_party', 'group', 'headline_alignment')) %>% 
  rowwise() %>% mutate(
    lower_bs = boot.ci(bs, type="bca", index=row)['bca'][[1]][4],
    upper_bs = boot.ci(bs, type="bca", index=row)['bca'][[1]][5],
    headline_alignment = ifelse(headline_alignment, 'Aligned content', 'Misaligned content'),
    # source_alignment = ifelse(source_alignment, 'Aligned source', 'Misaligned source'),
    demo_party = paste(demo_party, 'participants'),
    demo_party = factor(demo_party, levels=c('Republican participants', 'Democrat participants')),
    group = recode(group, control='Without incentives', bonus='With incentives'),
    # subject_politics = recode(subject_politics, liberal='Liberal participants', conservative='Conservative participants')
  ) -> d_graph

ggplot(d_graph, aes(y=estimate, x=group, color=headline_alignment, group=headline_alignment)) +
  geom_errorbar(aes(ymin=lower_bs, ymax=upper_bs), width=0.1, pos=pos) +
  geom_point(size=3, position=pos) + geom_line(pos=pos) + 
  scale_color_manual(values=c('black', '#c12b1e')) + facet_grid(~demo_party) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  labs(x='Accuracy incentives', y='Percent who believe the headline', color='Content alignment') + 
  theme_bw() + theme(legend.position='right', axis.text=element_text(size=9.3),
                     axis.title.x = element_text(margin = margin(t = 6, r = 0, b = 0, l = 0)))

## Models
dfx %>% 
  filter(relevant_for_analysis, study=='Study 1') %>% #
  # filter(group == 'control') %>%
  # filter(demo_party %in% c('Democrat','Republican')) %>%
  mutate(
    headline_alignment = ifelse(headline_alignment,'aligned','misaligned'),
    source_alignment = ifelse(source_alignment,'aligned','misaligned'),
    group = factor(group, levels = c('control','bonus'))
  ) %>%
  # glmer(formula = answer ~ headline_alignment*group + source_alignment*group + (1|source), family=binomial) %>% summary()
  lm(formula = answer ~ headline_alignment*group + source_alignment*group) %>% summary

dfx %>% 
  filter(relevant_for_analysis) %>% 
  lm(formula = source_trust_num ~ source_alignment) %>% summary()

class(m1) <- "lmerMod"
class(m2) <- "lmerMod"
class(m3) <- "lmerMod"
stargazer(m1, m2, m3, type='html', out='model2.doc',
          star.cutoffs = c(.05, .01, .001))

# Moderator analysis
table(dfx$demo_ideology)
df1$time

mean(dfx$submit_time)

dfx %>% 
  filter(relevant_for_analysis) %>% #
  mutate(
    demo_education = recode(demo_education, 'Less than a high school diploma'=0, 'High school degree or equivalent (e.g. GED)'=1, 
                            'Bachelor’s degree (e.g. BA, BS)'=2, 'Doctorate (e.g. PhD, EdD)'=4,
                            'Associate degree (e.g. AA, AS) '=2, 'Master’s degree (e.g. MA, MS)'=3, 'Professional degree (e.g. MD, JD)'=3),
    demo_ideology_strength = recode(demo_ideology, 'conservative'=1, 'liberal'=1, 'no opinion'=0, 'somewhat conservative'=0.5,
                                    'somewhat liberal'=0.5),
    headline_alignment = ifelse(headline_alignment,'aligned','misaligned'),
    source_alignment = ifelse(source_alignment,'aligned','misaligned')
  ) %>%
  # response_summary(splitvars=c('headline_alignment', 'demo_ideology_strength')) %>%
  # ggplot(aes(x=headline_alignment, y=estimate, group=demo_ideology_strength, color=demo_ideology_strength)) + 
  # geom_line()
  lm(formula = submit_time ~ group) %>% summary

hist(dfx$submit_time)

# Supplement: Credibility over trust
dfx %>% 
  filter(relevant_for_analysis, group=='control') %>%
  mutate(
    subject_politics = recode(subject_politics, liberal='Liberal', conservative='Conservative'),
    source_trust = ifelse(source_trust=='Entirely', 'A lot', source_trust),
    source_trust = ifelse(source_trust=='Barely', 'Somewhat', source_trust),
    source_trust_num = ifelse(source_trust=='A lot', 1, ifelse(source_trust=='Not at all', 0, 0.5))
  ) %>%
  group_by(source, subject_politics) %>%
  summarise(
    mean = Hmisc::smean.cl.normal(source_trust_num)['Mean'],
    round_mean = round(mean,2),
    lower = Hmisc::smean.cl.normal(source_trust_num)['Lower'],
    upper = Hmisc::smean.cl.normal(source_trust_num)['Upper'],
  ) %>%
  mutate(
    
    source = recode(source, bbart='Breitbart\nNews', cnn='CNN', 
                    drudge='Drudge\nReport',fox='Fox\nNews',
                    nyt='New York\nTimes', huff='Huffington\nPost'),
    source = factor(source, levels = c('Breitbart\nNews', 'Drudge\nReport', 'Fox\nNews',
                                       'Huffington\nPost', 'New York\nTimes', 'CNN'))
  ) %>%
  ggplot(aes(x=source, y=mean, fill=subject_politics, group=subject_politics)) +
  geom_bar(stat='identity', width=0.5, color='white', position=position_dodge(), alpha=0.8) + # position="dodge"
  scale_fill_manual(values=c('#020a7b','#c12b1e')) +
  geom_errorbar(aes(ymin=lower, ymax=upper), color='black', width=0.18, position=position_dodge(width=0.5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  labs(fill = "Participant politics", color='', x='Source name', y='Mean source trust rating') +
  theme_bw() + theme(legend.position='right', axis.text=element_text(size=9.3),
                     axis.title.x = element_text(margin = margin(t = 6, r = 0, b = 0, l = 0)))


ggsave(
  '~/plot2a.png',
  plot = last_plot(),
  scale = 1,
  width = 5.38,
  height = 2.83,
  dpi = 600,
)


# Supplement: Individual headlines
pos = position_dodge(width = 0.3)
dfx %>% 
  filter(relevant_for_analysis, group!='control') %>% #
  response_summary(splitvars=c('subject_politics', 'study', 'headline', 'headline_politics')) %>%
  filter(headline_politics!='liberal') %>%
  rbind(list("conservative","Study 2",'Rep_10','conservative',1, 1, 1, 0.03, 0, 0.2, 1)) %>%
  mutate(
    headline = str_replace(headline, 'Dem_', '#' ),
    headline = str_replace(headline, 'Rep_', '#' ),
    headline = ifelse(study=='Study 1', paste0(' ', headline), headline)
  ) %>%
  group_by(headline, study, headline_politics) %>% mutate(
    # estimate = ifelse(headline_politics=='conservative', 0, estimate),
    cred_liberal = ifelse(subject_politics=='liberal', estimate, 0),
    cred_conservative = ifelse(subject_politics=='conservative', estimate, 0),
    cred_diff = sum(cred_liberal) - sum(cred_conservative),
    subject_politics = recode(subject_politics, liberal='Liberal', conservative='Conservative'),
    headline_politics = recode(headline_politics, liberal='Headlines favoring\nliberal views', conservative='Headlines favoring\nconservative views'),
  ) %>%
  group_by(study, subject_politics, headline_politics) %>%
  mutate(
    average = sum(estimate)/n()
  ) %>% 
  ggplot(aes(y=estimate, x=reorder(headline, cred_diff), color=subject_politics, group=subject_politics)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2, alpha=0.5, pos=pos) +
  geom_point(size=2.5, position=pos) +
  geom_hline(aes(yintercept = average, color=subject_politics), alpha=0.5, linetype="solid") +
  scale_color_manual(values=c('#c12b1e','#020a7b')) + 
  facet_grid(c(~headline_politics, ~study), scales='free_x', space='free_x') + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L),limits = c(0, 1)) + #
  labs(x='Headline', y='Percent who believe the headline', 
       color='Participant \nideology') + 
  theme_bw() + theme(legend.position='right', axis.text=element_text(size=9.3),
                     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                     axis.title.x = element_text(margin = margin(t = 6, r = 0, b = 0, l = 0))) 

pos = position_dodge(width = 0.3)

table(dfx$source_politics)

dfx %>% 
  filter(source_politics=='NA', source!='cnn', group!='control') %>%
  mutate(
    # source = recode(source, cnn='CNN', huff='Huffington Post', nyt='New York Times', fox='Fox News', drudge='Drudge Report', bbart='Breitbart'),
    source = ifelse(study == 'Study 1', paste0(' ', source), source),
    source = str_to_title(source),
    source = recode(source, 'Itv'='ITV', 'Cnn'='CNN', 'Nypost'='NYPost')
  ) %>%
  response_summary(splitvars=c('subject_politics', 'study', 'source')) %>%
  group_by(source, study) %>% mutate(
    cred_liberal = ifelse(subject_politics=='liberal', estimate, 0),
    cred_conservative = ifelse(subject_politics=='conservative', estimate, 0),
    subject_politics = recode(subject_politics, liberal='Liberal', conservative='Conservative'),
    cred_diff = sum(cred_liberal) - sum(cred_conservative)
  ) %>%
  ggplot(aes(y=estimate, x=reorder(source, cred_diff), color=subject_politics, group=subject_politics)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2, alpha=0.5, pos=pos) +
  geom_point(size=2.5, position=pos) +
  scale_color_manual(values=c('#c12b1e','#020a7b')) + facet_grid(~study, scales='free_x', space='free_x') + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L), limits = c(0, 1)) + #
  labs(x='Source', y='Percent who believe the source', 
       color='Participant\nideology') + 
  theme_bw() + theme(legend.position='right', axis.text=element_text(size=9.3),
                     axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                     axis.title.x = element_text(margin = margin(t = 6, r = 0, b = 0, l = 0))) 

dfx %>% 
  filter(relevant_for_analysis, group=='control') %>%
  lm(formula=answer~source*subject_politics+0) %>% summary()
chisq.test()


dfx %>% 
  filter(relevant_for_analysis) %>%
  mutate(
    source_trust_num = recode(source_trust, 'A lot'=1, Entirely=1, Barely=0.5,
                              Somewhat=0.5, 'Not at all'=0),
    source = recode(source, cnn='CNN', huff='Huffington Post', nyt='New York Times', fox='Fox News', drudge='Drudge Report', bbart='Breitbart'),
    source = ifelse(study == 'Study 1', paste0(' ', source), source)
  ) %>%
  group_by(study, source, source_politics, subject_politics) %>%
  summarise(
    mean = Hmisc::smean.cl.normal(source_trust_num)['Mean'],
    round_mean = round(mean,2),
    lower = Hmisc::smean.cl.normal(source_trust_num)['Lower'],
    upper = Hmisc::smean.cl.normal(source_trust_num)['Upper'],
  ) %>%
  group_by(source) %>% mutate(
    cred_liberal = ifelse(subject_politics=='liberal', mean, 0),
    cred_conservative = ifelse(subject_politics=='conservative', mean, 0),
    cred_diff = sum(cred_liberal) - sum(cred_conservative),
    subject_politics = recode(subject_politics, liberal='Liberal', conservative='Conservative'),
  ) %>%
  group_by(study, source_politics) %>%
  mutate(average = sum(mean)/n()) %>%
  
  ggplot(aes(y=mean, x=reorder(source, cred_diff), color=subject_politics, group=subject_politics)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2, alpha=0.5, pos=pos) +
  geom_point(size=2.5, position=pos) +
  scale_color_manual(values=c('#c12b1e','#020a7b')) + facet_grid(~study, scales='free_x', space='free_x') + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L),limits = c(0, 1)) + #
  labs(x='Source', y='Source trust rating', 
       color='Participant\nideology') + 
  # geom_hline(aes(yintercept = average, fill = subject_politics)) + 
  theme_bw() + theme(legend.position='right', axis.text=element_text(size=9.3),
                     axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                     axis.title.x = element_text(margin = margin(t = 6, r = 0, b = 0, l = 0))) 



ggplot(aes(x=source_alignment, y=percentage, fill=source_trust, group = source_trust)) +
  geom_bar(stat='identity', width=0.5, color='white') + # position="dodge"
  geom_bar(stat='identity', width=0.5, fill='white', alpha=0.25) +
  # scale_fill_manual(values=c('#c12b1e','darkgrey', 'black')) +
  scale_fill_manual(values=c('#c12b1e', '#ae6353', '#898989', '#434343', '#000000')) +
  geom_errorbar(aes(ymin=lower, ymax=upper, y=mean, group=label), color='black', data=trust_summary, width=0.05, pos=pos) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  geom_line(aes(y=mean, color=label, group=label), data=trust_summary) + 
  scale_color_manual(values=c('black')) + labs(fill = "Source trust", color='', x='Source alignment', y='Source trust rating') +
  theme_bw() + theme(legend.position='right', axis.text=element_text(size=9.3),
                     axis.title.x = element_text(margin = margin(t = 6, r = 0, b = 0, l = 0))) 

response_summary_bs_diff <- function(data, indices, long_data, splitvars) {
  d <- data[indices,]
  d %>%
    group_by_at(vars(one_of(splitvars))) %>%
    dplyr::summarise(count = n(), mean=mean(answer), .groups = "drop") %>%
    group_by(study, headline) %>%
    mutate(
      cred_aligned = ifelse(headline_alignment==TRUE, mean, 0),
      cred_misaligned = ifelse(headline_alignment==FALSE, mean, 0),
      cred_diff = sum(cred_aligned) - sum(cred_misaligned)
    ) %>%
    filter(headline_alignment==TRUE) -> r
  # print(r)
  return(r$cred_diff)
  # group_by(headline, study) %>% mutate(
  #   diff_control = ifelse(group=='control', cred_diff, 0),
  #   diff_bonus = ifelse(group=='bonus', cred_diff, 0),
  #   treat_diff = sum(diff_bonus) - sum(diff_control)
  # ) %>% ungroup() %>% filter(group=='control') -> r
  # arrange(vars(one_of(splitvars))) -> r
  # return(r$treat_diff)
}

dfx %>% filter(relevant_for_analysis, group=='control') %>%
  # mutate(study=1, headline=1, group=1, headline_politics=1) %>%
  mutate(strata = paste(study, headline, group)) %>%
  boot(., strata=factor(.$strata), statistic=response_summary_bs_diff, R=1000, 
       splitvars=c('study', 'headline', 'headline_politics', 'headline_alignment')) -> bs
boot.ci(bs, type="norm", index=8, conf = 0.9)
-0.0863-0.3964
# S1: 0.2805-0.5220 = 0.2415 / 0.19
# S2: 0.47 / 0.327
# Overall effect: 0.1562 0.1973 0.2376 / 0.1225,  0.1507,  0.1788
0.0099- 0.337
dfx$headline_politics
dfx %>% 
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


# Treatment detail
# Treatment
#S1 -0.3637+0.0272 -0.3365
#S2 -0.3616-0.2203  -0.5819
# Overall -0.1452, -0.0907 -0.0299


dfx %>% 
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
  labs(x='Headline', y='Change in content alignment effect', 
       color='Headline \nideology') + 
  theme_bw() + theme(legend.position='right', axis.text=element_text(size=9.3),
                     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                     axis.title.x = element_text(margin = margin(t = 6, r = 0, b = 0, l = 0))) 

dfx %>% filter(relevant_for_analysis) -> test



table(dfx$subject_politics)
dfx %>% 
  filter(headline_politics=='NA') %>% 
  response_summary(splitvars=c('study', 'headline', 'subject_politics')) %>%
  # rbind(list("Study 2","Rep_10", "conservative", FALSE, TRUE,1, 1, 0.03, 0, 0.2, 1)) %>%
  mutate(
    headline = str_replace(headline, 'Other_', 'Decoy_' ),
    headline = str_replace(headline, 'Ggl_1', 'Decoy_5' ),
    headline = str_replace(headline, 'Ggl_2', 'Decoy_6' ),
    headline = ifelse(study=='Study 1', paste0(headline, '_S1'), paste0(headline,'_S2')),
    ci = ifelse(study=='Study 1', 0.3365, 0.5819)
  ) %>%
  group_by(headline, study) %>% mutate(
    # estimate = ifelse(headline_politics=='conservative', 0, estimate),
    cred_aligned = ifelse(subject_politics=='liberal', estimate, 0),
    cred_midaligned = ifelse(subject_politics=='conservative', estimate, 0),
    cred_diff = sum(cred_aligned) - sum(cred_midaligned),
    # headline_politics = ifelse(headline_politics=='liberal', 'Liberal', 'Conservative')
  ) %>%
  filter(subject_politics=='liberal') %>%
  ggplot(aes(y=cred_diff, x=reorder(headline, cred_diff))) +
  geom_rect(aes(ymin=-0.032761, ymax=0.02353, xmin=-Inf, xmax=Inf), alpha=0.1, fill='grey', color='white') +
  geom_errorbar(aes(ymin=cred_diff-ci/2, ymax=cred_diff+ci/2), width=0.2, alpha=0.5) +
  geom_point(size=2.5) + 
  geom_hline(aes(yintercept = 0), alpha=0.5, linetype="solid") +
  geom_hline(aes(yintercept = -0.00461189), alpha=0.5, linetype="dashed") +
  scale_color_manual(values=c('#c12b1e','#020a7b')) +
  # facet_grid(c(~headline_politics, ~study), scales='free_x', space='free_x') + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L), limits=c(-0.4,0.73)) + #
  labs(x='Headline', y='Difference between liberal\nand conservative belief', 
       color='Headline \nideology') + 
  theme_bw() + theme(legend.position='right', axis.text=element_text(size=9.3),
                     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                     axis.title.x = element_text(margin = margin(t = 6, r = 0, b = 0, l = 0))) 

# Figure 3: Accuracy treatment
dfx %>% filter(relevant_for_analysis) -> d
d %>% group_by(subject_id) %>% slice(1) %>% mutate(strata=paste(study,subject_politics, group)) -> d_groups
boot(d_groups, strata=factor(d_groups$strata), statistic=response_summary_bs, stype='f', R=1000, 
     long_data=d, splitvars=c('group', 'source_alignment','study')) -> bs

d %>% response_summary(splitvars=c('group', 'source_alignment', 'study')) %>% 
  rowwise() %>% mutate(
    lower_bs = boot.ci(bs, type="bca", index=row)['bca'][[1]][4],
    upper_bs = boot.ci(bs, type="bca", index=row)['bca'][[1]][5],
    source_alignment = ifelse(source_alignment, 'Aligned source', 'Misaligned source'),
    group = ifelse(group=='control', 'Without incentives', 'With incentives'),
    group = factor(group, levels=c('Without incentives', 'With incentives'))
  ) -> d_graph

d_graph %>%
  mutate(study = recode(study, 'Study 1'='Blatant Partisan Content', 
                        'Study 2'='Everyday Partisan Content')) %>%
  ggplot(aes(y=estimate, x=group, color=source_alignment, group=source_alignment)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.1, pos=pos) +
  geom_point(size=3, position=pos) + geom_line(pos=pos) + 
  scale_color_manual(values=c('black', '#c12b1e')) + facet_grid(~study) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) + # limits = c(0.38, 0.71)
  labs(x='Accuracy incentives', y='Percent who believe the headline', color='Source alignment') + 
  theme_bw() + theme(legend.position='right', axis.text=element_text(size=9.3),
                     axis.title.x = element_text(margin = margin(t = 6, r = 0, b = 0, l = 0)))

## Statistical modeling
dfx %>% 
  filter(relevant_for_analysis) %>% 
  mutate(
    headline_alignment = ifelse(headline_alignment,'aligned','misaligned'),
    source_alignment = ifelse(source_alignment,'aligned','misaligned'),
    group = factor(group, levels = c('control', 'bonus'))
  ) %>%
  lm(formula = answer ~ group*headline_alignment+group*source_alignment) %>% summary()
  glm(formula = answer ~ headline_alignment*group + source_alignment*group, family=binomial) %>% summary()
