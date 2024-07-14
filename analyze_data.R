#' This script analyzes data from popular CBS game shows: Amazing Race, Big 
#' Brother, and Survivor. It performs parametric and non-parametric tests to 
#' determine if black players have significantly different standings in the
#' games compared to players that are not black.
#'
#' @import tidyverse, ggplot2
#' @author George Gruenhagen

# Load Libraries
library("tidyverse")
library("ggplot2")

# Read in Data
setwd("/mnt/c/Users/ggruenhagen/Downloads/game_show/")
dfs = list()
dfs[["Amazing Race"]] = read.csv("amazing_race_standings.csv")
dfs[["Big Brother"]]  = read.csv("big_brother_standings.csv")
dfs[["Survivor"]]     = read.csv("survivor_standings.csv")

col_pal = c("#583101", "#ddb892")
col_pal2 = c(col_pal[1], "gray50")

all_res = data.frame()  # stores the results (including statistical tests)
all_data = data.frame() # stores the cleaned data
p1_list = list()
p2_list = list()
for (df_idx in 1:length(dfs)) {
    df = dfs[[df_idx]]
    df = df %>% group_by(season) %>% mutate(lose_quantile = (lose_rank/n())-(1/n())/2) %>% as.data.frame()
    game_show_name = names(dfs)[df_idx]
    p1_list[[df_idx]] = ggplot(df, aes(x = race, y = lose_quantile*100, color = race, fill = race)) + xlab("") + ylab("Quantile") + geom_violin(alpha=0.2, color=NA) + geom_boxplot(alpha=0.85, width=0.5, fill="white", lwd=1) + geom_point(size=0.8, shape=21, fill=NA, alpha=0.3, position=position_jitter(width=0.2)) + theme_bw() + guides(color=F, fill=F, alpha=F) + scale_color_manual(values=col_pal) + scale_fill_manual(values=col_pal) + scale_x_discrete(labels = c("Black", "Not Black")) + ggtitle(paste0(game_show_name, " Standings")) + theme(axis.text=element_text(size=12), plot.title=element_text(hjust=0.5))
    ggsave(paste0(game_show_name, "_race_boxplot.png"), p1_list[[df_idx]], width = 4, height = 5, dpi=300)

    # Separate out the standings of black and not black players
    black_standings     = df[which(df$race == "black"), "lose_quantile"]
    not_black_standings = df[which(df$race != "black"), "lose_quantile"]

    # Perform statistical tests to see if there's significant differences
    t_test      = t.test(black_standings,      not_black_standings)
    wilcox_test = wilcox.test(black_standings, not_black_standings)
    mean_black     = mean(black_standings)
    mean_not_black = mean(not_black_standings)
    race_fc = mean_not_black/mean_black

    # Perform a permutation test by shuffling the race labels
    nperms = 1000
    set.seed(123)
    perms = lapply(1:nperms, function(x) {
        shuffled_race = sample(df$race)
        perm_black_standings = df[which(shuffled_race == "black"), "lose_quantile"]
        perm_not_black_standings = df[which(shuffled_race != "black"), "lose_quantile"]
        perm_t_p      = t.test(     perm_black_standings, perm_not_black_standings)$p.value
        perm_wilcox_p = wilcox.test(perm_black_standings, perm_not_black_standings)$p.value
        perm_fc = mean(perm_not_black_standings)/mean(perm_black_standings)
        return(data.frame(perm=x, perm_black_standings=perm_black_standings, perm_fc=perm_fc, perm_t_p=perm_t_p, perm_wilcox_p=perm_wilcox_p))
    })

    # See how often the differences are greater in the permutations
    perm_df = do.call('rbind', perms)
    perm_df_stats = perm_df %>% group_by(perm) %>% slice(1)
    perm_fc_p      = length(which(perm_df_stats$perm_fc       >= race_fc))             / nperms
    perm_t_p       = length(which(perm_df_stats$perm_t_p      <= t_test$p.value))      / nperms
    perm_wilcox_p  = length(which(perm_df_stats$perm_wilcox_p <= wilcox_test$p.value)) / nperms

    # Plot black standings if things were equal
    df_equal = data.frame()
    df_equal = rbind(data.frame(lose_quantile=black_standings, perm="Real"), data.frame(lose_quantile=perm_df$perm_black_standings[which(perm_df$perm == 1)], perm="Equal"))
    df_equal$perm = factor(df_equal$perm, levels=c("Real", "Equal"))
    p2_list[[df_idx]] = ggplot(df_equal, aes(x = perm, y = lose_quantile*100, color = perm, fill = perm)) + xlab("") + ylab("Quantile") + geom_violin(alpha=0.2, color=NA) + geom_boxplot(alpha=0.85, width=0.5, fill="white", lwd=1) + geom_point(size=0.8, shape=21, fill=NA, alpha=0.3, position=position_jitter(width=0.2)) + theme_bw() + guides(color=F, fill=F, alpha=F) + scale_color_manual(values=col_pal2) + scale_fill_manual(values=col_pal2) + scale_alpha_manual(values=c(1, 0)) + scale_x_discrete(labels = c("Real", "Equal")) + ggtitle(paste0(game_show_name, " if Equal")) + theme(axis.text=element_text(size=12), plot.title=element_text(hjust=0.5))
    ggsave(paste0(game_show_name, "_race_boxplot_equal.png"), p2_list[[df_idx]], width = 4, height = 5, dpi=300)

    # Save the results
    all_res = rbind(all_res, data.frame(game_show=game_show_name, mean_black=mean_black, mean_not_black=mean_not_black, 
                                        t_p=t_test$p.value, wilcox_p=wilcox_test$p.value, perm_fc_p=perm_fc_p, perm_t_p=perm_t_p, perm_wilcox_p=perm_wilcox_p))
    df$show = game_show_name
    all_data = rbind(all_data, df[,c("show", "season", "player_name", "race", "lose_rank", "lose_quantile")])
}

write.csv(all_res, paste0("all_cbs_shows_stats.csv"))
write.csv(all_data, paste0("all_cbs_shows_data.csv"))
