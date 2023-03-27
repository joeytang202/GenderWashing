library(tidyverse)
library(stargazer)
library(plm)
library(jtools)
library(broom.mixed)
library(sandwich)
library(lmtest)
library(fixest)
library(modelsummary)
options("modelsummary_format_numeric_latex" = "plain")
#NOTE: TOBIN's Q, Account Followers, emp and D/E Ratio are LOGGED!!!
setwd("C:/Users/joeyt/Documents/PhD/chapter_4/data_and_analysis/Untitled Folder")

df <- read.csv('dataR.csv') %>% 
  mutate(
    Division = ifelse(twitter_name %in% c('BAMGroep_NL', 'beleefboskalis'), 'Construction',
                      ifelse(twitter_name == 'BeterBedHolding', 'Retail Trade',
                             ifelse(twitter_name == 'DSM','Manufacturing',
                                    ifelse(twitter_name %in% c('asr', 'ingnl'),  'Finance, Insurance, And Real Estate',
                                           ifelse(twitter_name == 'fugro', 'Services',
                                                  ifelse(Division == 'A', 'Agriculture, Forestry, And Fishing',
                                                         ifelse(Division == 'B', 'Mining',
                                                                ifelse(Division == 'C', 'Construction',
                                                                       ifelse(Division == 'D', 'Manufacturing',
                                                                              ifelse(Division == 'E', 'Transportation, Communications, Electric, Gas, And Sanitary Services',
                                                                                     ifelse(Division == 'F', 'Wholesale Trade',
                                                                                            ifelse(Division == 'G', 'Retail Trade',
                                                                                                   ifelse(Division == 'H', 'Finance, Insurance, And Real Estate',
                                                                                                          ifelse(Division == 'I', 'Services',
                                                                                                                 ifelse(Division == 'J', 'Public Administration', Division))))))))))))))),
    Division = relevel(factor(Division), 'Manufacturing'),
    year = factor(year),
    tobinsq = log(tobinsq),
    account_followers = log(account_followers),
    #annual_returns = annual_returns + abs(min(annual_returns)) + 0.001,
    #annual_returns = log(annual_returns),
    service = ifelse(Division %in% c("Finance, Insurance, And Real Estate" ,
                                     "Retail Trade",
                                     "Services" ,
                                     "Transportation, Communications, Electric, Gas, And Sanitary Services",
                                     "Wholesale Trade"), 1, 0)
   ) %>%
  rename(
    debt = de
  )


sum(is.na(df$ex_board_blau_imput))

sum(is.na(df$ex_board_blau))


temp <- df %>% select(tobinsq,
                        annual_returns,
                        disc_manag,
                        women_management_blau,
                        gender_tweets_dummy,
                        emp,
                        debt,
                        ex_board_blau_imput,
                        account_followers
)

stargazer(temp, 
          type = 'latex',
          covariate.labels = c("Tobin's Q (log)",
                               'Annual Returns',
                               'Management Gender Diversity Disclosure',
                               'Management Gender Diversity',
                               'Gender Equality Presentation',
                               '# of Employees (log)',
                               'Debt-to-Equity Ratio (log)',
                               'Board Gender Diversity',
                               'Twitter Followers (log)'),
          out = 'variables.tex',
          omit.summary.stat = c('n')
          )

c <- data.frame(cor(temp))
rownames(c) <-  c("Tobin's Q (log)",
                  'Annual Returns',
                  'Management Gender Diversity Disclosure',
                  'Management Gender Diversity',
                  'Gender Equality Presentation',
                  '# of Employees (log)',
                  'Debt-to-Equity Ratio (log)',
                  'Board Gender Diversity',
                  'Twitter Followers (log)')


stargazer(c, summary = F, type = 'latex',
          covariate.labels = c("Tobin's Q (log)",
                               'Annual Returns',
                               'Management Gender Diversity Disclosure',
                               'Management Gender Diversity',
                               'Gender Equality Presentation',
                               '# of Employees (log)',
                               'Debt-to-Equity Ratio (log)',
                               'Board Gender Diversity',
                               'Twitter Followers (log)'),
          out = 'cor_matrix.tex')

#for overview data:
t <- df %>% mutate(n = 1) %>% group_by(year) %>% summarise(n = sum(n))
t


t <- df[!duplicated(df$name),] %>% mutate(n = 1) %>% group_by(Division) %>% summarise(n = sum(n))
t2 <- df %>% mutate(n = 1) %>% group_by(Division) %>% summarise(n = sum(n))
t['N Observations'] <- t2$n

t %>% rename('N Unique Firms' = n)

#for testing:
# y <- 'tobinsq'
# gdd <- 'disc_manag'
# gdb <- 'women_management_blau'
# tw <- 'gender_tweets_dummy'
# title <- 'test'

df$int <- df$disc_manag * df$women_management_blau

regger <- function(y, gep){
  f <- y ~ x
  f <- update(y ~ x, paste0(y, " ~ ", paste(
    'disc_manag', 'int', gep, 'emp', 'debt', 'ex_board_blau_imput',
    'account_followers', 'year', 'Division', sep = ' + ')))
  regger_int <- function(x){
    lm(x, df)
  }
  
  if(gep == 'gender_tweets_dummy'){
  
  l <- list(f,
            update(f, paste(" ~ . + ", "disc_manag * gender_tweets_dummy" , " int * gender_tweets_dummy",  sep = ' + '))
  )
  }else{
    l <- list(f,
              update(f, paste(" ~ . + ", "disc_manag * gender_tweets_prop" , " int * gender_tweets_prop",  sep = ' + '))
    )
  }
  
  regs <- lapply(l, regger_int)
  
  return(regs)
}
modeller <- function(regs, title, output, coef_omit = NULL){
  rows <- tibble::tribble(~t0, ~t1, ~t2, ~t3, ~t4,
                          'Year Dummies', 'Yes', 'Yes', 'Yes', 'Yes',
                          'Industry Dummies', 'Yes', 'Yes', 'Yes', 'Yes')

  if(is.null(coef_omit)){
    attr(rows, 'position') <- c(41,42)
  }else{
    attr(rows, 'position') <- c(21,22)
  }
  names <- c('disc_manag' = 'MGDD',
           'int'= 'MGDD:MGD',
           'gender_tweets_dummy' = 'GEP',
           'emp' = '# of Employees',
           'debt' = 'Debt-to-equity ratio',
           'ex_board_blau_imput' = 'Board Gender Diversity',
           'account_followers' = '# of Twitter Followers'
  )
  notes = c('MGDD = Management Gender Diversity Disclosure',
          'MGD = Management Gender Diversity',
          'GEP = Gender Equality Presentation')

  modelsummary(list('MGDD, MGD & GEP'= regs[[1]],
                       'MGDD, MGD & GEP (Clus. Stand. Errors)' = regs[[1]],
                       'Gender-Washing' = regs[[2]],
                       'Gender-Washing (Clus. Stand. Errors)' = regs[[2]]),
                  vcov = c('classical', ~name, 'classical', ~name),
                  stars = c('*' = .1,  '**' = .05, '***' = .01, '****' = .001),
                  coef_omit = coef_omit,
                  add_rows = rows,
                  coef_rename = names,
                  notes = notes,
                  title = title,
                  output = output
  )
}
clusterer <- function(x){
  coeftest(x, vcov = vcovCL, cluster = ~name)
}
summer <- function(regs, model.names = NULL){
  if(is.null(model.names)){
    model.names <- c('MGDD, MGD & GEP',
                     'MGDD, MGD & GEP (Clus. Stand. Errors)',
                     'Gender-Washing',
                     'Gender-Washing (Clus. Stand. Errors)'
    )
  }else if(model.names == 'prop'){
    model.names <- c('MGDD, MGD & GEP (Prop.)',
                     'MGDD, MGD & GEP (Prop.) (Clus. Stand. Errors)',
                     'Gender-Washing',
                     'Gender-Washing (Clus. Stand. Errors)'
    )
  }
  
  plot_summs(regs,
               ci_level = .9,
               coefs = c('MGD' = 'disc_manag',
                         'MGD * MGDD' = 'int',
                         'GEP' = 'gender_tweets_dummy',
                         'GEP (Prop.)' = 'gender_tweets_prop',
                         'MGD * GEP' = 'disc_manag:gender_tweets_dummy',
                         'MGD * GEP (Prop.)' = 'disc_manag:gender_tweets_prop',
                         'MGD * MGDD * GEP' = 'int:gender_tweets_dummy',
                         'MGD * MGDD * GEP (Prop.)' = 'int:gender_tweets_prop',
                         '# of Employees (log)' = 'emp',
                         'Debt-to-Equity Ratio (log)' = 'debt',
                         'Board Gender Diversity' = 'ex_board_blau_imput',
                         'Twitter Followers (log)' = 'account_followers'
                ),
               model.names = model.names
             )

}
onetailer <- function(model){
  mod <- coeftest(model)
  a <- mod[,4] / 2
  b <- 1 - mod[,4] / 2
  
  mod <- coeftest(model, vcov. = vcovCL, cluster = ~name)
  c <- mod[,4] / 2
  d <- 1 - mod[,4] / 2
  
  l <- list(a,b,c,d)
  names(l) <- c('1', '2', 'cluster 1', 'cluster 2')
  
  return(l)
}

regs <- regger('tobinsq', 'gender_tweets_dummy')
modeller(regs, "Predicting Tobin's Q", 'tq.tex', "Division|year")
modeller(regs, "Predicting Tobin's Q", 'tq_full.tex')
regs_cl <- lapply(regs, clusterer)
total <- list(regs[[1]], regs_cl[[1]], regs[[2]], regs_cl[[2]])
jpeg('tq.jpg', height = 480, width = 600)
summer(total)
dev.off()
onetailer(regs[[1]])
onetailer(regs[[2]])

regs <- regger('annual_returns', 'gender_tweets_dummy')
modeller(regs, 'Predicting Annual Returns', 'ar.tex', "Division|year")
modeller(regs, 'Predicting Annual Returns', 'ar_full.tex')
regs_cl <- lapply(regs, clusterer)
total <- list(regs[[1]], regs_cl[[1]], regs[[2]], regs_cl[[2]])
jpeg('ar.jpg', height = 480, width = 600)
summer(total)
dev.off()
onetailer(regs[[1]])
onetailer(regs[[2]])

regs <- regger('tobinsq', 'gender_tweets_prop')
modeller(regs, "Predicting Tobin's Q", 'tq_prop.tex', "Division|year")
modeller(regs, "Predicting Tobin's Q", 'tq_prop_full.tex')
regs_cl <- lapply(regs, clusterer)
total <- list(regs[[1]], regs_cl[[1]], regs[[2]], regs_cl[[2]])
jpeg('tq_prop.jpg', height = 480, width = 600)
summer(total, 'prop')
dev.off()
onetailer(regs[[1]])
onetailer(regs[[2]])

regs <- regger('annual_returns', 'gender_tweets_prop')
modeller(regs, 'Predicting Annual Returns', 'ar_prop.tex', "Division|year")
modeller(regs, 'Predicting Annual Returns', 'ar_prop_full.tex')
regs_cl <- lapply(regs, clusterer)
total <- list(regs[[1]], regs_cl[[1]], regs[[2]], regs_cl[[2]])
jpeg('ar_prop.jpg', height = 480, width = 600)
summer(total, 'prop')
dev.off()
onetailer(regs[[1]])
onetailer(regs[[2]])






# 
# 
# 
# modelsummary(list('Model 1' = model,
#                   'Model 1-tail' = model,
#                   'Model Ind.' = model_industry,
#                   'Model Ind. 1-tail' = model_industry,
#                   'Model Clus.' = model,
#                   'Model Clus. 1-tail' = model,
#                   'Model Clus. Ind.' = model_industry,
#                   'Model Clus. Ind. 1-tail' = model_industry),
#              vcov = c('classical', 'classical', 'classical', 'classical', ~name, ~name, ~name, ~name),
#              stars = c('*' = .1,  '**' = .05, '***' = .01, '****' = .001),
#              coef_omit = "Division|year",
#              add_rows = rows,
#              coef_rename = names,
#              notes = notes,
#              title = "Predicting Tobin's Q",
#              output = 'tq_cluster.tex'
# )
# 
# 
# 
# 
# 
# 
# model <- lm(
#  tobinsq ~ disc_manag + int + gender_tweets_dummy * disc_manag +
#   int * gender_tweets_dummy + emp + debt + ex_board_blau_imput +
#   account_followers + year,
#    df
# )
# 
# model_industry <- lm(
#   tobinsq ~ disc_manag + int + gender_tweets_dummy * disc_manag + 
#     int * gender_tweets_dummy + emp + debt + ex_board_blau_imput +
#     account_followers + year + Division,
#   df
# )
# 
# model_ar <- lm(
#   annual_returns ~ disc_manag + int + gender_tweets_dummy * disc_manag +
#     int * gender_tweets_dummy + emp + debt + ex_board_blau_imput +
#     account_followers + year,
#   df
# )
# 
# model_ar_industry <- lm(
#   annual_returns ~ disc_manag + int + gender_tweets_dummy * disc_manag + 
#     int * gender_tweets_dummy + emp + debt + ex_board_blau_imput +
#     account_followers + year + Division,
#   df
# )
# 
# rows <- tibble::tribble(~t0, ~t1, ~t2, ~t3, ~t4, ~t5, ~t6, ~t7, ~t8,
#                         'Year Dummies', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 
#                         'Industry Dummies', 'No', 'No', 'Yes', 'Yes', 'No', 'No', 'Yes', 'Yes')
# 
# attr(rows, 'position') <- c(21, 22)
# 
# names <- c('disc_manag' = 'MGDD',
#            'int'= 'MGDD:MGD',
#            'gender_tweets_dummy' = 'GEP',
#            'emp' = '# of Employees',
#            'debt' = 'Debt-to-equity ratio',
#            'ex_board_blau_imput' = 'Board Gender Diversity',
#            'account_followers' = '# of Twitter Followers'
#            )
# notes = c('MGDD = Management Gender Diversity Disclosure',
#           'MGD = Management Gender Diversity',
#           'GEP = Gender Equality Presentation')
# 
# modelsummary(list('Model' = model,
#                   'Model 1-tail' = model,
#                   'Model Ind.' = model_industry,
#                   'Model Ind. 1-tail' = model_industry,
#                   'Model Clus.' = model,
#                   'Model Clus. 1-tail' = model,
#                   'Model Clus. Ind.' = model_industry,
#                   'Model Clus. Ind. 1-tail' = model_industry),
#              vcov = c('classical', 'classical', 'classical', 'classical', ~name, ~name, ~name, ~name),
#              stars = c('*' = .1,  '**' = .05, '***' = .01, '****' = .001),
#              coef_omit = "Division|year",
#              add_rows = rows,
#              coef_rename = names,
#              notes = notes,
#              title = "Predicting Tobin's Q",
#              output = 'tq_cluster.tex'
# )
# 
# mod <- coeftest(model)
# mod[,4] / 2
# 1 - mod[,4] /2
# 
# mod <- coeftest(model_industry)
# mod[,4] / 2
# 1 - mod[,4] /2
# 
# 
# mod <- coeftest(model, vcov. = vcovCL, cluster = ~name)
# mod[,4] /2
# 1 - mod[,4] /2
# 
# mod <- coeftest(model_industry, vcov. = vcovCL, cluster = ~name)
# mod[,4] /2
# 1 - mod[,4] /2
# 
# 
# modelsummary(list('Model' = model_ar,
#                   'Model 1-tail' = model_ar,
#                   'Model Ind.' = model_ar_industry,
#                   'Model Ind. 1-tail' = model_ar_industry,
#                   'Model Clus.' = model_ar,
#                   'Model Clus. 1-tail' = model_ar,
#                   'Model Clus. Ind.' = model_ar_industry,
#                   'Model Clus. Ind. 1-tail' = model_ar_industry),
#              vcov = c('classical', 'classical', 'classical', 'classical', ~name, ~name, ~name, ~name),
#              stars = c('*' = .1,  '**' = .05, '***' = .01, '****' = .001),
#              coef_omit = "Division|year",
#              add_rows = rows,
#              coef_rename = names,
#              notes = notes,
#              title = "Predicting Annual Returns",
#              output = 'ar_cluster.tex'
# )
# mod <- coeftest(model_ar)
# mod[,4] /2
# 1 - mod[,4] /2
# 
# mod <- coeftest(model_ar_industry)
# mod[,4] /2
# 1 - mod[,4] /2
# 
# 
# mod <- coeftest(model_ar, vcov. = vcovCL, cluster = ~name)
# mod[,4] /2
# 1 - mod[,4] /2
# 
# mod <- coeftest(model_ar_industry, vcov. = vcovCL, cluster = ~name)
# mod[,4] /2
# 1 - mod[,4] /2
# 
# 
# regger <- function(y, gdd, gdb, tw, title){
#   df$int <- unlist(df[gdd] * df[gdb])
#   f <- y ~ x
#   f <- update(y ~ x, paste0(y, " ~ ", paste('emp', 'debt', 'ex_board_blau_imput', 'account_followers', 'year', 'Division', sep = ' + ')))
#   
#   regger_int <- function(x){
#     lm(x, df)
#   }
#   
#   s <- df
#   df <- df[df[gdd] == 1,]
#   
#   l <- list(f,
#             update(f, paste(" ~ . + ", gdb, sep = ' + ')),
#             update(f, paste(" ~ . + ", gdb, tw, sep = ' + ')),
#             update(f, paste(" ~ . + ", paste(gdb, tw, sep = ' * ')))
#   )
#   
#   regs <- lapply(l, regger_int)
#   
#   gazer <- function(models, omit = NULL, covariate.labels = NULL, length = 'short', omit.labels = NULL, title = title){
#     stargazer(models,
#               type = 'latex',
#               omit = omit,
#               omit.labels = omit.labels,
#               star.char = c('*', '**', '***'),
#               star.cutoffs = c(0.05, 0.01, 0.001),
#               omit.stat = c('adj.rsq', 'ser', 'f'),
#               covariate.labels = covariate.labels,
#               out = paste0(paste(title, length, sep = '_'), '.tex')
#     )
#   }
#   
#   outputer <- function(regs, title_add, sample = 'small'){
#     
#     if(sample == 'small'){
#     gazer(regs, 
#           c('year', 'Division'), 
#           c('# of Employees (log)',
#             ' Debt-to-Equity Ratio (log)', 'Board Gender Diversity',
#             'Twitter Followers (log)','MGD', 'GEP', 'MGD * GEP'),
#           omit.labels = c('Year Fixed Effects', 'Industry Fixed Effects'),
#           title = paste0(title,title_add))
#     
#     gazer(regs,
#           covariate.labels = c('# of Employees (log)',
#                                'Debt-to-Equity Ratio (log)', 'Board Gender Diversity',
#                                'Twitter Followers (log)', '2018', '2019', '2020', '2021',
#                                'Construction', 'Finance, Insurance, And Real Estate',
#                                'Retail Trade', 'Services', 'Transportation, Communications, Electric, Gas, And Sanitary Services',
#                                'Wholesale Trade', 'MGD', 'GEP', 'MGD * GEP'
#           ),
#           length = 'long',
#           title = paste0(title,title_add)
#     )
#     
#     }else if(sample == 'large'){
#       
#       gazer(regs, c('year', 'Division'),
#                     c('# of Employees (log)',
#                       'Debt-to-Equity Ratio (log)',
#                       'Board Gender Diversity',
#                       'Twitter Followers (log)',
#                       'MGDD', 'MGDD * MGD', 'GEP', 'MGD * GEP', 'MGDD * GDD * GEP'),
#                     omit.labels = c('Year Fixed Effects', 'Industry Fixed Effects'),
#                     title = paste0(title,title_add))
#         gazer(regs, covariate.labels = c('# of Employees (log)',
#                                                'D/E Ratio (log)', 'Board Gender Diversity',
#                                                'Twitter Followers (log)', '2018', '2019', '2020', '2021',
#                                                'Construction', 'Finance, Insurance, And Real Estate',
#                                                'Retail Trade', 'Services', 'Transportation, Communications, Electric, Gas, And Sanitary Services',
#                                                'Wholesale Trade',  'MGDD', 'MGDD * MGD', 'GEP', 'MGD * GEP', 'MGDD * GDD * GEP'),
#                     length = 'long',
#                     title = paste0(title,title_add))
# 
# 
# 
#       
#       
#     }
#   }
#   
#   clusterer <- function(x){
#     coeftest(x, vcov = vcovCL, cluster = ~name)
#   }
#   regs_clustered <- lapply(regs, clusterer)
#   
#   outputer(regs, '1')
#   outputer(regs_clustered, '_clustered1')
#   
#   f1 <- plot_summs(regs,
#                    coefs = c('MGD' = gdb,
#                              'GEP' = tw,
#                              'MGD * GEP' = paste(gdb, tw, sep = ':'),
#                              '# of Employees (log)' = 'emp',
#                              'Debt-to-Equity Ratio (log)' = 'debt',
#                              'Board Gender Diversity' = 'ex_blard_blau_imput',
#                              'Twitter Followers (log)' = 'account_followers'
#                    ),
#                    model.names = c('Base',
#                                    'Diversity',
#                                    'Tweets',
#                                    'Interaction'))
#   
#   f1_clustered <- plot_summs(regs_clustered,
#                    coefs = c('MGD' = gdb,
#                              'GEP' = tw,
#                              'MGD * GEP' = paste(gdb, tw, sep = ':'),
#                              '# of Employees (log)' = 'emp',
#                              'Debt-to-Equity Ratio (log)' = 'debt',
#                              'Board Gender Diversity' = 'ex_blard_blau_imput',
#                              'Twitter Followers (log)' = 'account_followers'
#                    ),
#                    model.names = c('Base',
#                                    'Diversity',
#                                    'Tweets',
#                                    'Interaction'))
#   
#   df <- s
#   
#   l <- list(f,
#             update(f, paste(" ~ . + ", gdd, sep = ' + ')),
#             update(f, paste(" ~ . + ", gdd, 'int', sep = ' + ')),
#             update(f, paste(" ~ . + ", gdd, 'int', tw, sep = ' + ')),
#             update(f, paste(" ~ . + ", gdd, 'int', tw, paste(gdd, tw, sep = ' * '), paste('int', tw, sep = '*'), sep = ' + '))
#   )
#   
#   regs <- lapply(l, regger_int)
#   regs_clustered <- lapply(regs, clusterer)
#   
#   outputer(regs, '2', sample = 'large')
#   outputer(regs_clustered, '_clustered2', sample = 'large')
#   
#   f2 <- plot_summs(regs,
#                    coefs = c('MGDD' = gdd,
#                              'MGDD * MGD' = 'int',
#                              'GEP' = tw,
#                              'MGDD * GEP' = paste(gdd, tw, sep = ':'),
#                              "MGDD * MGD * GEP" = paste('int', tw, sep = ':'),
#                              '# of Employees (log)' = 'emp',
#                              'Debt-to-Equity Ratio (log)' = 'debt',
#                              'Board Gender Diversity' = 'ex_board_blau_imput',
#                              'Twitter Followers (log)' = 'account_followers'),
#                    model.names = c('Base',
#                                    'Disclosure',
#                                    'Diversity',
#                                    'Tweets',
#                                    'Interaction')
#   )
#   
#   f2_clustered <- plot_summs(regs_clustered,
#                    coefs = c('MGDD' = gdd,
#                              'MGDD * MGD' = 'int',
#                              'GEP' = tw,
#                              'MGDD * GEP' = paste(gdd, tw, sep = ':'),
#                              "MGDD * MGD * GEP" = paste('int', tw, sep = ':'),
#                              '# of Employees (log)' = 'emp',
#                              'Debt-to-Equity Ratio (log)' = 'debt',
#                              'Board Gender Diversity' = 'ex_board_blau_imput',
#                              'Twitter Followers (log)' = 'account_followers'),
#                    model.names = c('Base',
#                                    'Disclosure',
#                                    'Diversity',
#                                    'Tweets',
#                                    'Interaction')
#   )
#   
#   
#   return(list(f1,f1_clustered, f2, f2_clustered))
#   
# }
# 
# 
# 
# 
# 
# 
# 
# l1 <- regger('tobinsq', 'disc_manag', 'women_management_blau', 'gender_tweets_dummy', 'tobinsq')
# jpeg('tobinsq1.jpg')
# l1[[1]]
# dev.off()
# jpeg('tobinsq1_clustered.jpg')
# l1[2]
# dev.off()
# jpeg('tobinsq2.jpg')
# l1[3]
# dev.off()
# jpeg('tobinsq2_clustered.jpg')
# dev.off()
# l2 <- regger('annual_returns', 'disc_manag', 'women_management_blau', 'gender_tweets_dummy', 'annual_returns')
# jpeg('annual_returns1.jpg')
# l1[[1]]
# dev.off()
# jpeg('annual_returns1_clustered.jpg')
# l1[2]
# dev.off()
# jpeg('annual_returns2.jpg')
# l1[3]
# dev.off()
# jpeg('annual_returns2_clustered.jpg')
# dev.off()
# # regger <- function(y, gdd, gdb, tw, title){ #gdd: gender diversity disclosure, gdb: gender diversity blau, tw: gender tweets
# #   df$int <- unlist(df[gdd] * df[gdb])
# #   
# #   f <- y ~ x
# #   f <- update(y ~ x, paste0(y, " ~ ", paste('emp', 'debt', 'ex_board_blau_imput', 'account_followers', 'year', 'Division', sep = ' + ')))
# # 
# #   regger_int <- function(x){
# #       lm(x, df)
# #   }
# #   
# #   s <- df
# #   df <- df[df[gdd] == 1,]
# #   
# #   l <- list(f,
# #             update(f, paste(" ~ . + ", gdb, sep = ' + ')),
# #             update(f, paste(" ~ . + ", gdb, tw, sep = ' + ')),
# #             update(f, paste(" ~ . + ", paste(gdb, tw, sep = ' * ')))
# #   )
# #   
# #   regs <- lapply(l, regger_int)
# #   
# #   gazer <- function(models, omit = NULL, covariate.labels = NULL, length = 'short', omit.labels = NULL, title = title){
# #     stargazer(models,
# #               type = 'latex',
# #               omit = omit,
# #               omit.labels = omit.labels,
# #               star.char = c('*', '**', '***'),
# #               star.cutoffs = c(0.05, 0.01, 0.001),
# #               omit.stat = c('adj.rsq', 'ser', 'f'),
# #               covariate.labels = covariate.labels,
# #               out = paste0(paste(title, length, sep = '_'), '.tex')
# #     )
# #   }
# # 
# #   gazer(regs, 
# #         c('year', 'Division'), 
# #         c('# of Employees (log)',
# #           ' Debt-to-Equity Ratio (log)', 'Board Gender Diversity',
# #           'Twitter Followers (log)','MGD', 'GEP', 'MGD * GEP'),
# #         omit.labels = c('Year Fixed Effects', 'Industry Fixed Effects'),
# #         title = paste0(title,'1'))
# #   
# #   panel <- pdata.frame(df, index = c('name', 'year'))
# #   fi1 <- plm(tobinsq ~ emp + debt + ex_board_blau_imput + account_followers + 
# #                 women_management_blau + gender_tweets_dummy + 
# #                 women_management_blau:gender_tweets_dummy, panel,  model = 'within')
# #   
# #   gazer(regs,
# #         covariate.labels = c('# of Employees (log)',
# #                              'Debt-to-Equity Ratio (log)', 'Board Gender Diversity',
# #                              'Twitter Followers (log)', '2018', '2019', '2020', '2021',
# #                              'Construction', 'Finance, Insurance, And Real Estate',
# #                              'Retail Trade', 'Services', 'Transportation, Communications, Electric, Gas, And Sanitary Services',
# #                              'Wholesale Trade', 'MGD', 'GEP', 'MGD * GEP'
# #                                 ),
# #         length = 'long',
# #         title = paste0(title,'1')
# #         )
# #   
# #   f1 <- plot_summs(regs,
# #              coefs = c('MGD' = gdb,
# #                        'GEP' = tw,
# #                        'MGD * GEP' = paste(gdb, tw, sep = ':'),
# #                        '# of Employees (log)' = 'emp',
# #                        'Debt-to-Equity Ratio (log)' = 'debt',
# #                        'Board Gender Diversity' = 'ex_blard_blau_imput',
# #                        'Twitter Followers (log)' = 'account_followers'
# #                        ),
# #              model.names = c('Base',
# #                              'Diversity',
# #                              'Tweets',
# #                              'Interaction'))
# #  
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #    df <- s
# #   
# #   l <- list(f,
# #             update(f, paste(" ~ . + ", gdd, sep = ' + ')),
# #             update(f, paste(" ~ . + ", gdd, 'int', sep = ' + ')),
# #             update(f, paste(" ~ . + ", gdd, 'int', tw, sep = ' + ')),
# #             update(f, paste(" ~ . + ", gdd, 'int', tw, paste(gdd, tw, sep = ' * '), paste('int', tw, sep = '*'), sep = ' + '))
# #             )
# #   
# # 
# #   
# #   regs <- lapply(l, regger_int)
# #   
# #   gazer(regs, c('year', 'Division'),
# #         c('# of Employees (log)',
# #           'Debt-to-Equity Ratio (log)', 
# #           'Board Gender Diversity',
# #           'Twitter Followers (log)',
# #           'MGDD', 'MGDD * MGD', 'GEP', 'MGD * GEP', 'MGDD * GDD * GEP'),
# #         omit.labels = c('Year Fixed Effects', 'Industry Fixed Effects'),
# #         title = paste0(title,'2'))
# #   gazer(regs, covariate.labels = c('# of Employees (log)',
# #                                    'D/E Ratio (log)', 'Board Gender Diversity',
# #                                    'Twitter Followers (log)', '2018', '2019', '2020', '2021',
# #                                    'Construction', 'Finance, Insurance, And Real Estate',
# #                                    'Retail Trade', 'Services', 'Transportation, Communications, Electric, Gas, And Sanitary Services',
# #                                    'Wholesale Trade',  'MGDD', 'MGDD * MGD', 'GEP', 'MGD * GEP', 'MGDD * GDD * GEP'),
# #         length = 'long',
# #         title = paste0(title,'2'))
# #   
# #   panel <- pdata.frame(df, index = c('name', 'year'))
# #   fi2 <- plm(tobinsq ~ emp + debt + ex_board_blau_imput + account_followers + 
# #                 disc_manag  + int + disc_manag * gender_tweets_dummy + gender_tweets_dummy * int, panel,  model = 'within')
# #   
# #   f2 <- plot_summs(regs,
# #              coefs = c('MGDD' = gdd,
# #                        'MGDD * MGD' = 'int',
# #                        'GEP' = tw,
# #                        'MGDD * GEP' = paste(gdd, tw, sep = ':'),
# #                        "MGDD * MGD * GEP" = paste('int', tw, sep = ':'),
# #                        '# of Employees (log)' = 'emp',
# #                        'Debt-to-Equity Ratio (log)' = 'debt',
# #                        'Board Gender Diversity' = 'ex_board_blau_imput',
# #                        'Twitter Followers (log)' = 'account_followers'),
# #              model.names = c('Base',
# #                              'Disclosure',
# #                              'Diversity',
# #                              'Tweets',
# #                              'Interaction')
# #              )
# #   
# #   return(list(f1,f2, fi1, fi2))
# #   
# # }
# 
# 
# 
# 
# 
# #panel <- pdata.frame(df, index = c('name', 'year'))
# 
# #panel$int <- panel$disc_manag * panel$women_management_blau
# 
# #summary(plm(tobinsq ~ disc_manag * int * gender_tweets_dummy + emp + roa + debt + ex_board_blau_imput + account_followers, panel, model = 'within'))
# 
# 
# # df$int <- df$women_management_blau * df$disc_manag
# # 
# # reg <- lm(tobinsq ~ disc_manag + int + gender_tweets_dummy * disc_manag + gender_tweets_dummy * int + emp + debt + ex_board_blau_imput + account_followers + year, df)
# # reg_clustered <- coeftest(reg,  vcovCL, cluster = ~Division)
# # 
# # reg <- lm(annual_returns ~ disc_manag + int + gender_tweets_dummy * disc_manag + gender_tweets_dummy * int + emp + debt + ex_board_blau_imput + account_followers + year, df)
# # reg_clustered <- coeftest(reg,  vcovCL, cluster = ~Division)
# # reg_clustered
