library(readr)
library(dplyr)
library(ggplot2)
library(cetcolor)
library(scales)
library(reshape2)
library(lme4)
library(htmlTable)
source('utility_functions.r')

puns = read_csv('puns.csv')
pun_dict = read_csv('pun_dictionary.csv', locale=locale(encoding='latin1'))


pun_lengths = nchar(pun_dict$Full_Text)
pun_cols = paste0('Q', 1:30)
pun_dict$QID = pun_cols
pun_dict$pun_length = pun_lengths


pun_correlations = cor(puns %>% select(Q1:Q30), method='spearman')

molten_cor = melt(pun_correlations)

# before clustering
ggplot(molten_cor) + geom_tile(aes(x=Var1,y=Var2, fill=value)) + 
  scale_fill_gradientn(colors=cet_pal( 7, 'inferno'), values=seq(0,1,length.out=7))

# using length
ggplot(molten_cor %>% fix_factors(c('Var1','Var2'), pun_cols[order(pun_lengths)])) + 
  geom_tile(aes(x=Var1,y=Var2, fill=value)) + 
  scale_fill_gradientn(colors=cet_pal( 7, 'inferno'), values=seq(0,1,length.out=7))

# cluster questions by correlation
distmat = as.dist(1-pun_correlations)

clust = hclust(distmat, method='ward.D2')
labs = cutree(clust, 4)

ggplot(molten_cor %>% fix_factors(c('Var1','Var2'), clust$labels[clust$order])) + 
  geom_tile(aes(x=Var1,y=Var2, fill=value)) + 
  scale_fill_gradientn(colors=cet_pal( 7, 'inferno'), values=seq(0,1,length.out=7))

# order questions by average score
score_summary = puns %>% select(Q1:Q30) %>% melt() %>%
  group_by(variable) %>%
  summarize(mean=mean(value), sd=sd(value), Q1=quantile(value, 0.25), Q2=quantile(value,0.5), Q3 = quantile(value, 0.75)) %>%
  ungroup() %>% 
  cbind(pun_dict) %>%
  mutate(
    pun_length = pun_lengths
  )

question_score_levels = score_summary$variable[order(score_summary$mean)] %>% as.character()

# length appears to be insignificant in this context
summary(lm(mean~pun_length, data=score_summary))

score_summary_gender = puns %>% select(Q1:Q30, gender) %>% melt(id.vars='gender') %>%
  group_by(variable, gender) %>%
  summarize(mean=mean(value), sd=sd(value)) %>%
  ungroup() %>% 
  left_join(pun_dict, by=c('variable'='QID')) 

ggplot(score_summary_gender %>% fix_factors('variable',rev(question_score_levels)) %>%
         filter(gender != "Other")) + 
  geom_bar(aes(x=variable, y=mean, fill=gender), position='dodge', stat='identity') + 
  coord_flip()

# do a lot of melting
molten_puns = puns %>% mutate(subject_id=1:n()) %>%
  select(Q1:Q30, gender, EnglishFirstLanguage, EnjoysPuns, age, subject_id, location) %>%
  melt(id.vars=c('gender','EnglishFirstLanguage','EnjoysPuns','age','subject_id','location'))

# model question
summary(
  step(lm(value ~ gender + EnglishFirstLanguage+EnjoysPuns+age+variable + variable:gender +variable:age + 
            variable:EnglishFirstLanguage + location + variable:location + variable:EnjoysPuns, data = molten_puns),
       direction='both'
       ))

#random_effects_model = lmer(value ~ gender+EnjoysPuns+age+variable+(1|subject_id), data=molten_puns, REML=TRUE)

#model_q_scores = summary(random_effects_model)$coefficients %>% as.data.frame()
#model_q_scores$question=gsub('variable','',rownames(model_q_scores))
#model_q_scores = model_q_scores %>% filter(grepl('Q\\d+',question))
#model_q_scores = rbind(model_q_scores, 
#                       data.frame(Estimate=0, "Std. Error"=0, "t value"=0, question='Q1',check.names=FALSE))


# just make some tables
score_summary$img_url = sprintf('<div id="Q%s_img_div"><img src="https://maxcandocia.com/media/articles/puns/Q%s.png"/></div>',
  score_summary$Question_Number,score_summary$Question_Number,score_summary$Question_Number)
(html = htmlTable(score_summary %>% mutate(Rank=rank(-mean)) %>%
            select(Rank, img_url, Prompt, Punchline, mean, Q1, Q3) %>%
            mutate( mean=formatC(mean, 3)) %>%
              rename(Tally=img_url, Avg = mean) %>%
            arrange(Rank),
          align=c('c','c','l','l','c','c','c'),
          rnames=FALSE))

table_css = '<style>.gmisc_table tbody tr td:nth-child(4){color:#888;}</style>'
table_css %>% write_lines('results_table.html')
'<style> .gmisc_table tbody tr td {padding:6px;}</style>' %>% write_lines('results_table.html', append=TRUE)
html %>% write_lines('results_table.html', append=TRUE)

# very simple plots to output to article
dir.create('Q')
for (i in 1:30){
  column = paste0('Q',i)
  png(sprintf('Q/%s.png',column), width=80,height=40)
  print(
  ggplot(puns) + 
    geom_bar(aes(x=!!ensym(column), fill=factor(!!ensym(column)))) + 
     guides(fill=FALSE) + 
    scale_fill_manual(values=rev(cet_pal(6, 'rainbow'))) +
    theme_void()
  )
  dev.off()
}

                        