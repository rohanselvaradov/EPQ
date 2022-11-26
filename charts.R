library(ggplot2)
library(dplyr)
library(ggthemes)
library(openxlsx)
library(scales)
library(lubridate)
library(ggrepel)
library(stringr)

# Chart 1
economics <- read.xlsx('data.xlsx', sheet='economics')
economics %>%
  ggplot(aes(reorder(subject, desc(return)), return, ymin=return-err, ymax=return+err, colour=group)) +
  geom_pointrange() +
  scale_colour_discrete(breaks=c('STEM', 'LEM', 'Other')) +
  scale_y_continuous(labels = percent) +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.title=element_blank()) +
  labs(x='Subject', y='Relative return', title='Figure 1: Relative returns of subjects')

# Chart 2
admissions <- read.xlsx('data.xlsx', sheet='admissions') %>%
  mutate(year=as.Date(ISOdate(year, 1, 1)))
start <- min(admissions$year)
end <- max(admissions$year)

admissions %>%
  ggplot(aes(year, total)) +
  geom_line() +
  scale_x_date(date_breaks='10 year', date_labels='%Y', limits=c(start, end), expand=c(0, 0)) +
  scale_y_continuous(labels = comma) +
  theme_economist() +
  theme(axis.text.y=element_text(hjust=0.95),
    plot.margin=margin(10, 20, 10, 10)) +
  labs(x='Year', y='Number of students entering first degree', title='Figure 2: Higher education admissions numbers')

# Chart 3
mobility <- read.xlsx('data.xlsx', sheet='mobility')
mobility %>%
  ggplot(aes(access, success, colour=group)) +
  geom_point() +
  geom_text_repel(data=subset(mobility, label == 1),
            aes(access, success, label=university),
            show.legend = FALSE, box.padding = unit(0.45, "lines"),
            vjust="inward", hjust="inward") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent) +
  guides(color = guide_legend(nrow = 3), text='none') +
  theme_economist() +
  labs(x='Access rate', y='Success rate', colour='University type', title='Figure 3: Social mobility by higher education institution')

# Chart 4
exams <- read.xlsx('data.xlsx', sheet='exams')
exams %>%
  ggplot(aes(reorder(subject, desc(accuracy)), accuracy)) +
  geom_point() +
  scale_y_continuous(labels = percent, limits=c(0, 1)) +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.title=element_blank()) +
  labs(x='Subject', y='Probability of definitive grade', title='Figure 4: Qualification accuracy')

# Chart 5
curriculum <- read.xlsx('data.xlsx', sheet='curriculum') %>%
  pivot_longer(!c(subject, group), names_to='year', values_to='pupils') %>%
  mutate(year=as.Date(ISOdate(year, 1, 1)))

curriculum %>%
  group_by(year, group) %>%
  summarise(total = sum(pupils)) %>%
  ggplot(aes(year, total, fill=group)) +
  geom_area() +
  scale_x_date() +
  scale_y_continuous(labels = comma) +
  theme_economist() +
  theme() +
  labs(x='Year', y='Number of candidates', title='Figure 5: AS- and A-level candidates by subject')
