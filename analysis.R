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
