library(ggplot2)
library(dplyr)
library(ggthemes)
library(openxlsx)
library(scales)
library(lubridate)
library(ggrepel)
library(stringr)
library(tidyr)

col_grid <- rgb(100, 100, 100, 100, maxColorValue = 255)

# Chart 1
economics <- read.xlsx('data.xlsx', sheet='economics')
economics %>%
  mutate(return=return / 100, err = err / 100) %>%
  ggplot(aes(reorder(subject, desc(return)), return, ymin=return-err, ymax=return+err, colour=group)) +
  geom_point(size=3) +
  geom_errorbar(width=0, linewidth=.8) +
  scale_colour_discrete(breaks=c('STEM', 'LEM', 'Other')) +
  scale_y_continuous(labels = percent) +
  expand_limits(y=c(-0.2, NA)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position=c(.9, 0.82),
        panel.grid = element_line(color = col_grid)) +
  labs(x='Subject', y='Relative return', colour='Subject group',
       title='Figure 1: Returns of degree subjects relative to History')

# Chart 2
admissions <- read.xlsx('data.xlsx', sheet='admissions') %>%
  mutate(year=as.Date(ISOdate(year, 1, 1)))
start <- min(admissions$year)
end <- max(admissions$year)
top <- max(admissions$total)

admissions %>%
  ggplot(aes(year, total)) +
  geom_area() +
  scale_x_date(date_breaks='10 year', date_labels='%Y', limits=c(start, end), expand=c(0, 0)) +
  scale_y_continuous(labels = comma, expand = c(0,0)) +
  theme_bw() +
  theme(axis.text.y=element_text(hjust=0.95),
        plot.margin=margin(10, 20, 10, 10),
        panel.grid = element_line(color = col_grid)) +
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
  scale_colour_discrete(breaks=c('Most Selective Russell', 'Other Russell Group', 'Old Universities (Pre-1992)',
                                 'Post-1992 (more selective)', 'Post-1992 (least selective)')) +
  guides(color = guide_legend(nrow = 5), text='none') +
  theme_bw() +
  theme(legend.position=c(0.85, 0.8),
        panel.grid = element_line(color = col_grid)) +
  labs(x='Access rate', y='Success rate', colour='University type',
       title='Figure 3: Social mobility by higher education institution')

# Chart 4
exams <- read.xlsx('data.xlsx', sheet='exams')
exams %>%
  ggplot(aes(reorder(subject, desc(accuracy)), accuracy, label=paste0(accuracy * 100, "%"))) +
  geom_point(size=3) +
  geom_text(vjust=-1, hjust=0.3, size=3) +
  scale_y_continuous(labels = percent, limits=c(0, 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.4),
        legend.title=element_blank(),
        panel.grid = element_line(color = col_grid)) +
  labs(x='Subject', y='Probability of definitive grade',
       title='Figure 4: Qualification accuracy by subject')

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
  theme_bw() +
  theme(legend.position=c(0.85, 0.84),
        panel.grid = element_line(color = col_grid)) +
  labs(x='Year', y='Number of candidates', fill='Subject group',
       title='Figure 5: AS- and A-level candidates by subject')
