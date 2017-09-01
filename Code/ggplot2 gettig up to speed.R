library(ggplot2)
library(Hmisc)
library(dplyr)
library(reshape)
library(lme4)
library(nlme)


head(Milk)
ggplot(data=Milk, aes(x=Time, y=protein)) +
  geom_point() +
  geom_boxplot(aes(color=Diet))

ggplot(data=Milk, aes(x=Time, y=protein)) +
  geom_point(aes(color=Diet))

ggplot(data=Milk, aes(x=Time, y=protein)) +
  geom_point(color="green")

p <- ggplot(data=Milk, aes(x=Time, y=protein)) +
  geom_point()

p + geom_smooth()

p + facet_wrap(~Diet)

pa <- ggplot(Milk, aes(x=protein))
pa + geom_histogram()
pa + geom_freqpoly()
pa + geom_density()

pb <- ggplot(Milk, aes(x=Diet))
pb + geom_bar()

p + geom_smooth()

pc <- p + geom_line(aes(group=Cow)) + geom_smooth()


ggsave("junk.png", plot=pc, width=10, height=5, units="in")
getwd()


p + geom_jitter(size=2, alpha=1/2) + theme_gray()

p + geom_jitter(size=2, alpha=1/2) + theme_bw()
p + geom_jitter(size=2, alpha=1/2) + theme_classic()
p + geom_jitter(size=2, alpha=1/2) + theme_minimal()

ggplot(data=Milk, aes(x=Time, y=protein)) +
  geom_point() +
  geom_violin(aes(color=Diet))


qplot(Sepal.Length, Petal.Length, data=iris)

qplot(Sepal.Length, Petal.Length, data=iris, color = Species)

pc <- p + geom_line(aes(group=Cow)) +geom_smooth()

ggplot(data=Milk, aes(x=Time, y=protein)) + geom_smooth()
ggplot(data=Milk, aes(x=Time, y=protein)) + geom_jitter() + geom_smooth()



