library(ggplot2)
library("ggpubr")
theme_set(
  theme_bw() +
    theme(legend.position = "top")
)

data("ToothGrowth")
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
# Box plot
p <- ggplot(ToothGrowth, aes(x = dose, y = len)) + 
  geom_boxplot(aes(fill = supp), position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))
p

p + facet_grid(rows = vars(supp))

# Split in horizontal direction
p + facet_grid(cols = vars(supp))

p + facet_grid(rows = vars(dose), cols = vars(supp))

p + facet_wrap(vars(dose))

p + facet_wrap(vars(dose), ncol=2)

p + facet_grid(rows = vars(dose), cols = vars(supp), scales = "free")
# 0. Define custom color palette and prepare the data
my3cols <- c("#E7B800", "#2E9FDF", "#FC4E07")
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

# 1. Create a box plot (bp)
p <- ggplot(ToothGrowth, aes(x = dose, y = len))
bxp <- p + geom_boxplot(aes(color = dose)) +
  scale_color_manual(values = my3cols)

# 2. Create a dot plot (dp)
dp <- p + geom_dotplot(aes(color = dose, fill = dose), 
                       binaxis='y', stackdir='center') +
  scale_color_manual(values = my3cols) + 
  scale_fill_manual(values = my3cols)

# 3. Create a line plot
lp <- ggplot(economics, aes(x = date, y = psavert)) + 
  geom_line(color = "#E46726") 

figure <- ggarrange(bxp, dp, lp,
                    labels = c("A", "B", "C"),
                    ncol = 2, nrow = 2)
figure
