library(ggplot2)
library(data.table)
library(ggthemes)
library(cowplot)
library(tidyverse)
library(ggExtra)

# load data files
# avg_flex <- fread("res/exp-flex_avg-ss.csv")

## plotting for exp-flex
# avg_flex$switch <- as.factor(avg_flex$switch)
# avg_flex$train_type <- as.factor(avg_flex$train_type)
# avg_flex[, train := ifelse(train_type==1, 'frequent', 'infrequent')]
# rename factor levels
avg_flex[, subses := ifelse(subses==1, 'first', 'last')]
avg_flex[, switch := ifelse(switch==0, 'non-switch', 'switch')]
avg_flex[, train_type := ifelse(train_type==1, 'variable', 'stable')]
avg_flex[, ses := ifelse(ses==2, 'training', 'testing')]


# accuracy_mean
pt1 <- ggplot(data=avg_flex[ses=='training' & switch=='non-switch',], 
              aes(x=subses, y=accuracy_mean, color=train_type)) + 
  geom_point( position = position_jitterdodge(dodge.width = 0.75,
                                              jitter.width = 0.15)) +
  geom_tufteboxplot(median.type = "line",
                    hoffset = 0, 
                    width = 2,
                    size = 1.25,
                    position = position_dodge(width = c(0.2, 1.3)),
                    show.legend=FALSE) +
  geom_rangeframe(sides='b', color='black') +
  # scale_x_discrete(breaks=c(0, 1),
  #                  labels=c('non-switch', 'switch')) + 
  theme_tufte() + 
  theme(axis.line.y.left = element_line(),
        legend.key = element_blank(),
        legend.position = "none",
        #strip.text.x = element_blank(),
        text = element_text(family = "Arial",
                            size=16)) +
  scale_colour_colorblind()

hst1 <- ggplot(data=avg_flex[ses=='testing',], 
               aes(x=accuracy_mean, fill=train_type)) +
  geom_histogram() +
  facet_wrap('switch') + 
  theme_light()

pt2 <- ggplot(data=avg_flex[ses=='training' & switch=='non-switch',], 
              aes(x=subses, y=setting_errors_mean, color=train_type)) + 
  geom_point( position = position_jitterdodge(dodge.width = 0.75,
                                              jitter.width = 0.15)) +
  geom_tufteboxplot(median.type = "line",
                    hoffset = 0, 
                    width = 2,
                    size = 1.25,
                    position = position_dodge(width = c(0.2, 1.3)),
                    show.legend=FALSE) +
  geom_rangeframe(sides='b', color='black') +
  # scale_x_discrete(breaks=c(0, 1),
  #                  labels=c('no switch', 'switch')) + 
  theme_tufte() + 
  theme(axis.line.y.left = element_line(),
        legend.key = element_blank(),
        legend.position = "none",
        #strip.text.x = element_blank(),
        text = element_text(family = "Arial",
                            size=16)) +
  scale_colour_colorblind()

pt3 <- ggplot(data=avg_flex[ses=='training' & switch=='non-switch',], 
              aes(x=subses, y=general_errors_mean, color=train_type)) + 
  geom_point( position = position_jitterdodge(dodge.width = 0.75,
                                              jitter.width = 0.15)) +
  geom_tufteboxplot(median.type = "line",
                    hoffset = 0, 
                    width = 2,
                    size = 1.25,
                    position = position_dodge(width = c(0.2, 1.3)),
                    show.legend=FALSE) +
  geom_rangeframe(sides='b', color='black') +
  # scale_x_discrete(breaks=c(0, 1),
  #                  labels=c('no switch', 'switch')) + 
  theme_tufte() + 
  theme(axis.line.y.left = element_line(),
        legend.key = element_blank(),
        legend.position = "none",
        #strip.text.x = element_blank(),
        text = element_text(family = "Arial",
                            size=16)) +
  scale_colour_colorblind()

pt4 <- ggplot(data=avg_flex[ses=='training' & switch=='non-switch',], 
              aes(x=subses, y=setting_sticks_mean, color=train_type)) + 
  geom_point( position = position_jitterdodge(dodge.width = 0.75,
                                              jitter.width = 0.15)) +
  geom_tufteboxplot(median.type = "line",
                    hoffset = 0, 
                    width = 2,
                    size = 1.25,
                    position = position_dodge(width = c(0.2, 1.3)),
                    show.legend=FALSE) +
  geom_rangeframe(sides='b', color='black') +
  # scale_x_discrete(breaks=c(0, 1),
  #                  labels=c('no switch', 'switch')) + 
  theme_tufte() + 
  theme(axis.line.y.left = element_line(),
        legend.key = element_blank(),
        legend.position = "none",
        #strip.text.x = element_blank(),
        text = element_text(family = "Arial",
                            size=16)) +
  scale_colour_colorblind()

pt5 <- ggplot(data=avg_flex[ses=='training' & switch=='non-switch',], 
              aes(x=subses, y=setting_slips_mean, color=train_type)) + 
  geom_point( position = position_jitterdodge(dodge.width = 0.75,
                                              jitter.width = 0.15)) +
  geom_tufteboxplot(median.type = "line",
                    hoffset = 0, 
                    width = 2,
                    size = 1.25,
                    position = position_dodge(width = c(0.2, 1.3)),
                    show.legend=FALSE) +
  geom_rangeframe(sides='b', color='black') +
  # scale_x_discrete(breaks=c(0, 1),
  #                  labels=c('no switch', 'switch')) + 
  theme_tufte() + 
  theme(axis.line.y.left = element_line(),
        legend.key = element_blank(),
        legend.position = c(0.8, 0.8),
        #strip.text.x = element_blank(),
        text = element_text(family = "Arial",
                            size=16)) +
  scale_colour_colorblind()

pt6 <- ggplot(data=avg_flex[ses=='training' & switch=='non-switch',], 
              aes(x=subses, y=rt_first_correct_mean, color=train_type)) + 
  geom_point( position = position_jitterdodge(dodge.width = 0.75,
                                              jitter.width = 0.15)) +
  geom_tufteboxplot(median.type = "line",
                    hoffset = 0, 
                    width = 2,
                    size = 1.25,
                    position = position_dodge(width = c(0.2, 1.3)),
                    show.legend=FALSE) +
  geom_rangeframe(sides='b', color = 'black') +
  # scale_x_discrete(breaks=c(0, 1),
  #                  labels=c('no switch', 'switch')) + 
  theme_tufte() + 
  theme(axis.line.y.left = element_line(),
        legend.key = element_blank(),
        legend.position = "none",
        #strip.text.x = element_blank(),
        text = element_text(family = "Arial",
                            size=16)) +
  scale_colour_colorblind()

pt7 <- ggplot(data=avg_flex[ses=='training' & switch=='non-switch',], 
              aes(x=subses, y=rt_subs_correct_mean, color=train_type)) + 
  geom_point( position = position_jitterdodge(dodge.width = 0.75,
                                              jitter.width = 0.15)) +
  geom_tufteboxplot(median.type = "line",
                    hoffset = 0, 
                    width = 2,
                    size = 1.25,
                    position = position_dodge(width = c(0.2, 1.3)),
                    show.legend=FALSE) +
  geom_rangeframe(sides='b', color='black') +
  # scale_x_discrete(breaks=c(0, 1),
  #                  labels=c('no switch', 'switch')) + 
  theme_tufte() + 
  theme(axis.line.y.left = element_line(),
        legend.key = element_blank(),
        legend.position = "none",
        #strip.text.x = element_blank(),
        text = element_text(family = "Arial",
                            size=16)) +
  scale_colour_colorblind()

pt8 <- ggplot(data=avg_flex[ses=='training' & switch=='non-switch',], 
              aes(x=subses, y=rt_post_error_mean, color=train_type)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.75,
                                             jitter.width = 0.15)) +
  geom_tufteboxplot(median.type = "line",
                    hoffset = 0, 
                    width = 2,
                    size = 1.25,
                    position = position_dodge(width = c(0.2, 1.3)),
                    show.legend=FALSE) +
  geom_rangeframe(sides='b', color='black') +
  # scale_x_discrete(breaks=c(0, 1),
  #                  labels=c('no switch', 'switch')) + 
  theme_tufte() + 
  theme(axis.line.y.left = element_line(),
        legend.key = element_blank(),
        legend.position = "none",
        #strip.text.x = element_blank(),
        text = element_text(family = "Arial",
                            size=16)) +
  scale_colour_colorblind()

# melt.data.table(avg_flex_wide[ses=='testing',],
#                 id.vars=c('subID', 'ses', 'train_type'),
#                 measure.vars=c('LISAS_rt_fst_cor_non-switch',
#                                'LISAS_rt_fst_cor_switch',
#                                'LISAS_rt_sub_cor_non-switch',
#                                'LISAS_rt_sub_cor_switch'),
#                 variable.name='switch',
#                 value.name='LISAS')
#                 

pt9 <- ggplot(data=avg_flex[ses=='testing'], 
              aes(x=switch, y=LISAS_rt_fst_cor, color=train_type)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.75,
                                             jitter.width = 0.15)) +
  geom_tufteboxplot(median.type = "line",
                    hoffset = 0, 
                    width = 2,
                    size = 1.25,
                    position = position_dodge(width = c(0.2, 1.3)),
                    show.legend=FALSE) +
  geom_rangeframe(sides='b', color='black') +
  # scale_x_discrete(breaks=c(0, 1),
  #                  labels=c('no switch', 'switch')) + 
  theme_tufte() + 
  theme(axis.line.y.left = element_line(),
        legend.key = element_blank(),
        legend.position = "none",
        #strip.text.x = element_blank(),
        text = element_text(family = "Arial",
                            size=16)) +
  scale_colour_colorblind()

pt10 <- ggplot(data=avg_flex[ses=='testing'], 
              aes(x=switch, y=LISAS_rt_sub_cor, color=train_type)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.75,
                                             jitter.width = 0.15)) +
  geom_tufteboxplot(median.type = "line",
                    hoffset = 0, 
                    width = 2,
                    size = 1.25,
                    position = position_dodge(width = c(0.2, 1.3)),
                    show.legend=FALSE) +
  geom_rangeframe(sides='b', color='black') +
  # scale_x_discrete(breaks=c(0, 1),
  #                  labels=c('no switch', 'switch')) + 
  theme_tufte() + 
  theme(axis.line.y.left = element_line(),
        legend.key = element_blank(),
        legend.position = "none",
        #strip.text.x = element_blank(),
        text = element_text(family = "Arial",
                            size=16)) +
  scale_colour_colorblind()

  

# combine 
fig <- ggdraw() + 
  draw_plot(pt1, x=0, y=0.66, width = 0.33, height=0.33) +
  draw_plot(pt2, x=0.33, y=0.66, width = 0.33, height=0.33) + 
  draw_plot(pt3, x=0.66, y=0.66, width = 0.33, height=0.33) +
  draw_plot(pt4, x=0, y=0.33, width = 0.33, height=0.33) + 
  draw_plot(pt5, x=0.33, y=0.33, width = 0.33, height=0.33) +
  draw_plot(pt6, x=0, y=0, width = 0.33, height=0.33) + 
  draw_plot(pt7, x=0.33, y=0, width = 0.33, height=0.33) +
  draw_plot(pt8, x=0.66, y=0, width = 0.33, height=0.33)

fig <- ggdraw() + 
  draw_plot(pt9, x=0, y=0, width = 0.5, height=1) +
  draw_plot(pt10, x=0.5, y=0, width = 0.5, height=1)


# save fig
png("exp-flex_test_LISAS.png",
    width=8, height=4, units='in', res=400)
plot(fig)
dev.off()


## Find the % of time people go to each door from the correct context on the first click following a switch
grp_data <- read_csv('res/exp-flex_evt.csv')
door_counts <- grp_data %>% 
  filter(switch==1 & lag(switch)==0 & door_cc==1 & ses==3) %>%
  group_by(sub, train_type, ses, context, door) %>% 
  summarise(counts=n()) %>% 
  ungroup() %>% 
  group_by(sub, context) %>% 
  arrange(desc(counts), .by_group = T)
door_counts %<>% 
  group_by(sub, train_type, ses, context) %>%
  mutate(door_order=row_number())
door_counts_totals <- door_counts %>% 
  group_by(sub, train_type, context) %>% 
  summarise(total_clicks=sum(counts))
door_counts <- inner_join(door_counts_totals, door_counts, 
                          by=join_by(sub, train_type, context))
door_counts %<>% 
  mutate(counts_pcent=(counts/total_clicks)*100) %>%
  group_by(sub, train_type, door_order) %>%
  summarise(counts_pcent_av=mean(counts_pcent))
door_counts$door_order <- as.factor(door_counts$door_order)
fct_rev(door_counts$door_order)
door_counts$train_type <- ifelse(door_counts$train_type==1, 'stable', 'variable')
door_counts$train_type <- as.factor(door_counts$train_type)


# test for difference from chance
x <- door_counts %>% filter(door_order==1) %>% pull(counts_pcent_av)
t.test(x, mu=50)

p <- ggplot() +
  geom_line(data=door_counts,
            aes(x=door_order,
                y=counts_pcent_av,
                group=sub),
            alpha=0.1) +
  # position=position_jitter(width=0.05, seed=12345)) +
  geom_point(data=door_counts,
             aes(x=door_order,
                 y=counts_pcent_av,
                 color=train_type,
                 # shape=train_type
                 ),
             alpha=1,
             position=position_jitter(width=0.05, seed=12345)) +
  scale_x_discrete(name='Door (most to least selected)',
                   limits=rev,
                   labels=c('4th', '3rd', '2nd', '1st')) + 
  scale_y_continuous(name='Mean no. of correct clicks after switch (%)',
                     breaks=c(0, 25, 50, 75, 100),
                     labels=c('0', '25', '50', '75', '100'),
                     limits=c(0, 100)) +
  coord_flip() +
  theme(text=element_text(family='sans',
                          face='plain',
                          color='black',
                          size=18), 
        legend.position='top',
        panel.background = element_blank()) +
  scale_color_tableau()

p_m <- ggMarginal(p,
                  type = 'density', 
                  groupColour = T, 
                  groupFill = T,
                  margin='x')

# save fig
png("exp-flex_door_selections_bygrp.png",
    width=6, height=4, units='in', res=400)
plot(p)
dev.off()


## plotting for exp-multi
avg_multi <- fread("res/exp-multi_avg-ss.csv")
mts_multi <- fread('res/exp-multi_mts.csv')

# rename factor levels
avg_multi[, subses := ifelse(subses==1, 'first', 'last')]
avg_multi[, switch := ifelse(switch==0, 'non-switch', 'switch')]
avg_multi[, train_type := ifelse(train_type==1, 'variable', 'stable')]
avg_multi[, ses := ifelse(ses==2, 'training', 'testing')]

# mts_multi[stage==4, block := ifelse(t <= 24, 'early', 'late')]
# mts_multi[stage==3, stage_3_t := seq(1, .N), by=sub]
# mts_multi[stage==3, block := ifelse(stage_3_t <= 32, 'early', 'late')]
mts_multi <- mts_multi[,.(accuracy_mean = mean(acc),
                          rt_mean = mean(rt)),
                       by = .(sub, stage, cond)]
# 
mts_multi[, session := ifelse(stage == 4, 'initial', 'test')]
mts_multi <- mts_multi[avg_multi[, .(train_type=unique(train_type)), by=sub], on='sub']

# accuracy_mean
pt1 <- ggplot(data=avg_multi[ses=='training' & switch=='non-switch',], 
              aes(x=subses, y=accuracy_mean, color=train_type)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.75,
                                             jitter.width = 0.15)) +
  geom_tufteboxplot(median.type = "line",
                    hoffset = 0, 
                    width = 2,
                    size = 1.25,
                    position = position_dodge(width = c(0.2, 1.3)),
                    show.legend=FALSE) +
  geom_rangeframe(sides='b', color='black') +
  # scale_x_discrete(breaks=c(0, 1),
  #                  labels=c('no switch', 'switch')) + 
  # facet_wrap('multi_trial') +
  theme_tufte() + 
  theme(axis.line.y.left = element_line(),
        legend.key = element_blank(),
        legend.position = "none",
        #strip.text.x = element_blank(),
        text = element_text(family = "Arial",
                            size=16)) +
  scale_colour_colorblind()

pt2 <- ggplot(data=avg_multi[ses=='training' & switch=='non-switch',], 
              aes(x=subses, y=setting_errors_mean, color=train_type)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.75,
                                             jitter.width = 0.15)) +
  geom_tufteboxplot(median.type = "line",
                    hoffset = 0, 
                    width = 2,
                    size = 1.25,
                    position = position_dodge(width = c(0.2, 1.3)),
                    show.legend=FALSE) +
  geom_rangeframe(sides='b', color='black') + 
  # scale_x_discrete(breaks=c(0, 1),
  #                  labels=c('no switch', 'switch')) + 
  # facet_wrap('multi_trial') +
  theme_tufte() + 
  theme(axis.line.y.left = element_line(),
        legend.key = element_blank(),
        legend.position = "none",
        #strip.text.x = element_blank(),
        text = element_text(family = "Arial",
                            size=16)) +
  scale_colour_colorblind()

pt3 <- ggplot(data=avg_multi[ses=='training' & switch=='non-switch',], 
              aes(x=subses, y=general_errors_mean, color=train_type)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.75,
                                             jitter.width = 0.15)) +
  geom_tufteboxplot(median.type = "line",
                    hoffset = 0, 
                    width = 2,
                    size = 1.25,
                    position = position_dodge(width = c(0.2, 1.3)),
                    show.legend=FALSE) +
  geom_rangeframe(sides='b', color='black') +
  # scale_x_discrete(breaks=c(0, 1),
  #                  labels=c('no switch', 'switch')) + 
  # facet_wrap('multi_trial') +
  theme_tufte() + 
  theme(axis.line.y.left = element_line(),
        legend.key = element_blank(),
        legend.position = c(0.8, 0.5),
        #strip.text.x = element_blank(),
        text = element_text(family = "Arial",
                            size=16)) +
  scale_colour_colorblind()


pt4 <- ggplot(data=avg_multi[ses=='training' & switch=='non-switch',], 
              aes(x=subses, y=setting_sticks_mean, color=train_type)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.75,
                                             jitter.width = 0.15)) +
  geom_tufteboxplot(median.type = "line",
                    hoffset = 0, 
                    width = 2,
                    size = 1.25,
                    position = position_dodge(width = c(0.2, 1.3)),
                    show.legend=FALSE) +
  geom_rangeframe(sides='b', color='black') +
  # scale_x_discrete(breaks=c(0, 1),
  #                  labels=c('no switch', 'switch')) + 
  # facet_wrap('multi_trial') +
  theme_tufte() + 
  theme(axis.line.y.left = element_line(),
        legend.key = element_blank(),
        legend.position = "none",
        #strip.text.x = element_blank(),
        text = element_text(family = "Arial",
                            size=16)) +
  scale_colour_colorblind()

pt5 <- ggplot(data=avg_multi[ses=='training' & switch=='non-switch',], 
              aes(x=subses, y=setting_slips_mean, color=train_type)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.75,
                                             jitter.width = 0.15)) +
  geom_tufteboxplot(median.type = "line",
                    hoffset = 0, 
                    width = 2,
                    size = 1.25,
                    position = position_dodge(width = c(0.2, 1.3)),
                    show.legend=FALSE) +
  geom_rangeframe(sides='b', color='black') +
  # scale_x_discrete(breaks=c(0, 1),
  #                  labels=c('no switch', 'switch')) + 
  # facet_wrap('multi_trial') +
  theme_tufte() + 
  theme(axis.line.y.left = element_line(),
        legend.key = element_blank(),
        legend.position = 'none',
        #strip.text.x = element_blank(),
        text = element_text(family = "Arial",
                            size=16)) +
  scale_colour_colorblind()


pt6 <- ggplot(data=avg_multi[ses=='training' & switch=='non-switch',], 
              aes(x=subses, y=rt_first_correct_mean, color=train_type)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.75,
                                             jitter.width = 0.15)) +
  geom_tufteboxplot(median.type = "line",
                    hoffset = 0, 
                    width = 2,
                    size = 1.25,
                    position = position_dodge(width = c(0.2, 1.3)),
                    show.legend=FALSE) +
  geom_rangeframe(sides='b', color='black') +
  # scale_x_discrete(breaks=c(0, 1),
  #                  labels=c('no switch', 'switch')) + 
  # facet_wrap('multi_trial') +
  theme_tufte() + 
  theme(axis.line.y.left = element_line(),
        legend.key = element_blank(),
        legend.position = "none",
        #strip.text.x = element_blank(),
        text = element_text(family = "Arial",
                            size=16)) +
  scale_colour_colorblind()

pt7 <- ggplot(data=avg_multi[ses=='training' & switch=='non-switch',], 
              aes(x=subses, y=rt_subs_correct_mean, color=train_type)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.75,
                                             jitter.width = 0.15)) +
  geom_tufteboxplot(median.type = "line",
                    hoffset = 0, 
                    width = 2,
                    size = 1.25,
                    position = position_dodge(width = c(0.2, 1.3)),
                    show.legend=FALSE) +
  geom_rangeframe(sides='b', color='black') +
  # scale_x_discrete(breaks=c(0, 1),
  #                  labels=c('no switch', 'switch')) + 
  # facet_wrap('multi_trial') +
  theme_tufte() + 
  theme(axis.line.y.left = element_line(),
        legend.key = element_blank(),
        legend.position = "none",
        #strip.text.x = element_blank(),
        text = element_text(family = "Arial",
                            size=16)) +
  scale_colour_colorblind()

pt8 <- ggplot(data=avg_multi[ses=='training' & switch=='non-switch',], 
              aes(x=subses, y=rt_post_error_mean, color=train_type)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.75,
                                             jitter.width = 0.15)) +
  geom_tufteboxplot(median.type = "line",
                    hoffset = 0, 
                    width = 2,
                    size = 1.25,
                    position = position_dodge(width = c(0.2, 1.3)),
                    show.legend=FALSE) +
  geom_rangeframe(sides='b', color='black') +
  # scale_x_discrete(breaks=c(0, 1),
  #                  labels=c('no switch', 'switch')) + 
  # facet_wrap('multi_trial') +
  theme_tufte() + 
  theme(axis.line.y.left = element_line(),
        legend.key = element_blank(),
        legend.position = "none",
        #strip.text.x = element_blank(),
        text = element_text(family = "Arial",
                            size=16)) +
  scale_colour_colorblind()

pt9 <- ggplot(data=mts_multi, aes(x=cond, y=accuracy_mean, color=train_type)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.75,
                                             jitter.width = 0.15)) +
  geom_tufteboxplot(median.type = "line",
                    hoffset = 0, 
                    width = 2,
                    size = 1.25,
                    position = position_dodge(width = c(0.2, 1.3)),
                    show.legend=FALSE) +
  geom_rangeframe(sides='b', color='black') +
  scale_x_discrete(breaks=c('nc', 'oc'),
                   labels=c('neither', 'other')) +
  facet_wrap('session') +
  theme_tufte() + 
  theme(axis.line.y.left = element_line(),
        legend.key = element_blank(),
        legend.position = "none",
        #strip.text.x = element_blank(),
        text = element_text(family = "Arial",
                            size=16)) +
  scale_colour_colorblind()

pt10 <- ggplot(data=mts_multi, aes(x=cond, y=rt_mean, color=train_type)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.75,
                                             jitter.width = 0.15)) +
  geom_tufteboxplot(median.type = "line",
                    hoffset = 0, 
                    width = 2,
                    size = 1.25,
                    position = position_dodge(width = c(0.2, 1.3)),
                    show.legend=FALSE) +
  geom_rangeframe(sides='b', color='black') +
  scale_x_discrete(breaks=c('nc', 'oc'),
                   labels=c('neither', 'other')) +
  facet_wrap('session') +
  theme_tufte() + 
  theme(axis.line.y.left = element_line(),
        legend.key = element_blank(),
        legend.position = c(0.8, 0.9),
        #strip.text.x = element_blank(),
        text = element_text(family = "Arial",
                            size=16)) +
  scale_colour_colorblind()


# pt11 <- ggplot(data=mts_multi, aes(x=cond, y=rt_mean, color=train_type)) + 
#   geom_point(position = position_jitterdodge(dodge.width = 0.75,
#                                              jitter.width = 0.15)) +
#   geom_tufteboxplot(median.type = "line",
#                     hoffset = 0, 
#                     width = 2,
#                     size = 1.25,
#                     position = position_dodge(width = c(0.2, 1.3)),
#                     show.legend=FALSE) +
#   geom_rangeframe(sides='b') +
#   scale_x_discrete(breaks=c('nc', 'oc'),
#                    labels=c('neither', 'other')) +
#   facet_grid(cols=vars(block), rows=vars(session)) +
#   theme_tufte() + 
#   theme(axis.line.y.left = element_line(),
#         legend.key = element_blank(),
#         legend.position = c(0.8, 0.9),
#         #strip.text.x = element_blank(),
#         text = element_text(family = "Arial",
#                             size=16)) +
#   scale_colour_colorblind()

# pt12 <- ggplot(data=mts_multi, aes(x=cond, y=accuracy_mean, color=train_type)) + 
#   geom_point(position = position_jitterdodge(dodge.width = 0.75,
#                                              jitter.width = 0.15)) +
#   geom_tufteboxplot(median.type = "line",
#                     hoffset = 0, 
#                     width = 2,
#                     size = 1.25,
#                     position = position_dodge(width = c(0.2, 1.3)),
#                     show.legend=FALSE) +
#   geom_rangeframe(sides='b') +
#   scale_x_discrete(breaks=c('nc', 'oc'),
#                    labels=c('neither', 'other')) +
#   facet_grid(cols=vars(block), rows=vars(session)) +
#   theme_tufte() + 
#   theme(axis.line.y.left = element_line(),
#         legend.key = element_blank(),
#         legend.position = "none",
#         #strip.text.x = element_blank(),
#         text = element_text(family = "Arial",
#                             size=16)) +
#   scale_colour_colorblind()

mts_multi_av <- mts_multi[, .(gav=mean(accuracy_mean)), by=.(session, train_type)]
mts_multi_av$session <- as.factor(mts_multi_av$session)
  
pt13 <- ggplot() + 
  geom_point(data=mts_multi,
             aes(x=session, y=accuracy_mean, color=train_type),
             position=position_jitter(width=0.05, seed=123),
             alpha=0.2) +
  geom_line(data=mts_multi,
            aes(x=session, y=accuracy_mean, group=sub, color=train_type),
            position=position_jitter(width=0.05, seed=123),
            alpha=0.2) +
  geom_point(data=mts_multi_av,
             aes(x=session, y=gav, color=train_type),
             size=2,
             alpha=1) +
  geom_line(data=mts_multi_av,
            aes(x=session, y=gav, group=train_type, color=train_type),
            size=1,
            alpha=1) +
  geom_rangeframe(sides='b') +
  theme_tufte() + 
  theme(axis.line.y.left = element_line(),
        legend.key = element_blank(),
        legend.position = "top",
        #strip.text.x = element_blank(),
        text = element_text(family = "Arial",
                            size=16)) +
  scale_colour_colorblind()

p14 <- ggplot(data=mts_multi, aes(x=accuracy_mean)) + 
  # geom_boxplot() +
  geom_histogram() +
  facet_wrap('session') + 
  # facet_grid(rows=vars(train_type), cols=vars(session)) + 
  scale_color_colorblind() + 
  theme_classic()

# combine 
fig <- ggdraw() + 
  draw_plot(pt1, x=0, y=0.66, width = 0.33, height=0.33) +
  draw_plot(pt2, x=0.33, y=0.66, width = 0.33, height=0.33) + 
  draw_plot(pt3, x=0.66, y=0.66, width = 0.33, height=0.33) +
  draw_plot(pt4, x=0, y=0.33, width = 0.33, height=0.33) + 
  draw_plot(pt5, x=0.33, y=0.33, width = 0.33, height=0.33) +
  draw_plot(pt6, x=0, y=0, width = 0.33, height=0.33) + 
  draw_plot(pt7, x=0.33, y=0, width = 0.33, height=0.33) +
  draw_plot(pt8, x=0.66, y=0, width = 0.33, height=0.33)

fig <- ggdraw() + 
  draw_plot(pt9, x=0, y=0, width = 0.5, height=1) +
  draw_plot(pt10, x=0.5, y=0, width = 0.5, height=1)

fig <- ggdraw() + 
  draw_plot(pt11, x=0, y=0, width = 0.5, height=1) +
  draw_plot(pt12, x=0.5, y=0, width = 0.5, height=1)


# save fig
svg("exp-multi_train_fullSample.svg",
    width=12, height=12)
plot(fig)
dev.off()


png("exp-multi_wmt_hist.png",
    width=8, height=4, units='in', res=400)
plot(p14)
dev.off()
