setwd("~/Desktop/Big Data/Project/Project/")
rm(list=ls())

require(MASS)
require(glmnet)
require(ggplot2)
require(dplyr)
require(gridExtra)
require(lme4)

green <- read.table("green_buckets", sep=",", stringsAsFactors = F)
uber <- read.table("uber_buckets", sep=",", stringsAsFactors = F)
yellow <- read.table("yellow_buckets", sep=",", stringsAsFactors = F)

split.col1 <- function(unsplit) {
  split_caret <- strsplit(unsplit$V1, split="^", fixed=T)
  weekhour <- as.numeric(sapply(split_caret, "[[", 1))
  weekday <- as.numeric(sapply(split_caret, "[[", 2))
  e3 <- sapply(split_caret, "[[", 3)
  e4 <- strsplit(e3, "\t")
  neighborhood <- as.factor(sapply(e4, "[[", 1))
  count <- as.numeric(sapply(e4, "[[", 2))
  data.frame(weekhour, weekday, neighborhood, count)
}

green2 <- split.col1(green)
yellow2 <- split.col1(yellow)
uber2 <- split.col1(uber)

combine.data <- function(d1, d2) {
  d2$avg.distance.traveled <- d1$V2
  d2$avg.fare.amount <- d1$V3
  d2$avg.total.amount <- d1$V4
  d2$avg.num.passengers <- d1$V5
  d2$avg.pickup.dist.subway <- d1$V6
  d2$avg.dropoff.dist.subway <- d1$V7
  d2
}

combine.uber <- function(d1, d2) {
  d2$avg.pickup.dist.subway <- d1$V2
  d2
}

green <- combine.data(green, green2)
yellow <- combine.data(yellow, yellow2)
uber <- combine.uber(uber, uber2)

green$type <- factor("green", levels = c("yellow", "green", "uber"))
yellow$type <- factor("yellow", levels = c("yellow", "green", "uber"))
uber$type <- factor("uber", levels = c("yellow", "green", "uber"))

convert.time <- function(data) {
  data$weekday <- factor(data$weekday, labels = 
                  c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
  data$hour.sin <- sin(data$weekhour*pi/12)
  data$hour.cos <- cos(data$weekhour*pi/12)
  data$is.weekend <- as.factor(ifelse(data$weekday == "Sun" | data$weekday == "Sat" | 
                  (data$weekday == "Fri" & data$weekhour >= 20), "weekend", "weekday"))
  data
}

green <- convert.time(green)
yellow <- convert.time(yellow)
uber <- convert.time(uber)

all(levels(uber$neighborhood) == levels(yellow$neighborhood))
add.green <- which(!levels(yellow$neighborhood) %in% levels(green$neighborhood))
levels(green$neighborhood) <- c(levels(green$neighborhood), levels(yellow$neighborhood)[add.green])
green$neighborhood <- relevel(green$neighborhood, "LaGuardia Airport")
yellow$neighborhood <- relevel(yellow$neighborhood, "LaGuardia Airport")
uber$neighborhood <- relevel(uber$neighborhood, "LaGuardia Airport")

borough.neighborhoods <- list(Bronx = c("Central Bronx", "Bronx Park and Fordham", 
            "High Bridge and Morrisania", "Hunts Point and Mott Haven", "Kingsbridge and Riverdale",
            "Northeast Bronx", "Southeast Bronx"), Brooklyn = c("Central Brooklyn", "Southwest Brooklyn",
            "Borough Park", "Canarsie and Flatlands", "Southern Brooklyn", "Northwest Brooklyn",
            "Flatbush", "East New York and New Lots", "Greenpoint", "Sunset Park", 
            "Bushwick and Williamsburg", "Navy Yard"), Manhattan = c("Central Harlem", 
            "East Harlem", "Gramercy Park and Murray Hill", "Greenwich Village and Soho", 
            "Lower Manhattan", "Lower East Side", "Upper East Side", "Upper West Side",
            "Inwood and Washington Heights", "Battery Park City", "Central Park", "Midtown",
            "Morningside Heights", "Chelsea and Clinton", "Tribeca"), Queens = c("Northeast Queens", 
            "North Queens", "Floral Parl", "LaGuardia Airport", "Long Island City", "New Hyde Park", 
            "Central Queens", "Jamaica", "Northwest Queens", "West Central Queens", "Rockaways",
            "Southeast Queens", "Southwest Queens", "West Queens"), Staten.Island = c("Port Richmond",
            "South Shore", "Stapleton and St. George", "Mid-Island"))

get.borough <- function(data) {
  boro.matrix <- sapply(borough.neighborhoods, function(b) data$neighborhood %in% b)
  boroughs <- apply(boro.matrix, 1, which)
  data$borough <- as.factor(names(borough.neighborhoods)[boroughs])
  data
}

green$borough <- yellow$borough <- uber$borough <- NULL

green <- get.borough(green)
yellow <- get.borough(yellow)
uber <- get.borough(uber)
green$borough <- relevel(green$borough, "Manhattan")
yellow$borough <- relevel(yellow$borough, "Manhattan")
uber$borough <- relevel(uber$borough, "Manhattan")

View(green); View(yellow); View(uber)
str(green); str(yellow); str(uber)
dim(green); dim(yellow); dim(uber)
nlevels(green$neighborhood); nlevels(yellow$neighborhood); nlevels(uber$neighborhood)
table(green$borough); table(yellow$borough); table(uber$borough)

yellow.poisson2 <- glm(count ~ weekday + borough + avg.pickup.dist.subway + avg.dropoff.dist.subway +
                        hour.sin + hour.cos + avg.num.passengers + avg.distance.traveled +
                        is.weekend, data = yellow, family = poisson())
green.poisson2 <- glm(count ~ weekday + borough + avg.pickup.dist.subway + avg.dropoff.dist.subway +
                        hour.sin + hour.cos + avg.num.passengers + avg.distance.traveled +
                        is.weekend, data = green, family = poisson())
uber.poisson2 <- glm(count ~ weekday + borough + avg.pickup.dist.subway + hour.sin + hour.cos +
                       is.weekend, data = uber, family = poisson())

yellow.poisson.stepwise <- stepAIC(yellow.poisson2)
green.poisson.stepwise <- stepAIC(green.poisson2)
uber.poisson.stepwise <- stepAIC(uber.poisson2)
summary(yellow.poisson.stepwise)
summary(green.poisson.stepwise)
summary(uber.poisson.stepwise)

yellow.poisson3 <- glm(count ~ weekday + borough + avg.pickup.dist.subway +
                         hour.sin + hour.cos + avg.num.passengers + avg.distance.traveled +
                         is.weekend, data = yellow, family = poisson())
yellow.poisson4 <- glm(count ~ weekday + borough +
                         hour.sin + hour.cos + avg.num.passengers + avg.distance.traveled +
                         is.weekend, data = yellow, family = poisson())
green.poisson3 <- glm(count ~ weekday + borough + avg.pickup.dist.subway +
                        hour.sin + hour.cos + avg.num.passengers + avg.distance.traveled +
                        is.weekend, data = green, family = poisson())
green.poisson4 <- glm(count ~ weekday + borough +
                        hour.sin + hour.cos + avg.num.passengers + avg.distance.traveled +
                        is.weekend, data = green, family = poisson())
uber.poisson3 <- glm(count ~ weekday + borough + hour.sin + hour.cos +
                        is.weekend, data = uber, family = poisson())

anova(yellow.poisson4, yellow.poisson3, yellow.poisson2, test = "Chisq")
anova(green.poisson4, green.poisson3, green.poisson2, test = "Chisq")
anova(uber.poisson3, uber.poisson2, test = "Chisq")

yellow.poisson <- glm(count ~ weekday + neighborhood + avg.pickup.dist.subway + hour.sin + hour.cos + 
                        is.weekend, data = yellow, family = poisson())
green.poisson <- glm(count ~ weekday + neighborhood + avg.pickup.dist.subway + hour.sin + hour.cos + 
                        is.weekend, data = green, family = poisson())
uber.poisson <- glm(count ~ weekday + neighborhood + avg.pickup.dist.subway + hour.sin + hour.cos + 
                        is.weekend, data = uber, family = poisson())

summary(yellow.poisson)
summary(green.poisson)
summary(uber.poisson)

anova(yellow.poisson, test = "Chisq")
anova(green.poisson, test = "Chisq")
anova(uber.poisson, test = "Chisq")

qplot(data = green, x = weekhour, y = hour.sin, color = "sine") + geom_smooth() +
  geom_point(aes(y = hour.cos, color = "cos")) + geom_smooth(aes(x = weekhour, y = hour.cos, 
    color = "cos")) + labs(color = "function", y = "transformed", x = "hour")

make.dummies <- function(data) {
  factor.vars <- names(which(sapply(data, is.factor)))
  dummy.vars <- sapply(factor.vars, function(a) dummy(data[,a]))
  do.call("cbind", dummy.vars)
}

green.dummies <- make.dummies(green)
yellow.dummies <- make.dummies(yellow)
uber.dummies <- make.dummies(uber)

make.dummies.interact <- function(dummies, x) {
  interact.terms <- colnames(dummies)[!(colnames(dummies) %in% c("uber", "green"))]
  interact.terms <- c(interact.terms, "avg.pickup.dist.subway", "hour.sin", "hour.cos")
  green.interact <- sapply(interact.terms, function(i) x[,i]*x[,"green"])
  uber.interact <- sapply(interact.terms, function(i) x[,i]*x[,"uber"])
  colnames(green.interact) <- paste("green", colnames(green.interact), sep=":")
  colnames(uber.interact) <- paste("uber", colnames(uber.interact), sep=":")
  x <- cbind(x, green.interact, uber.interact)
  x
}

green.x <- cbind(green.dummies, avg.pickup.dist.subway = green$avg.pickup.dist.subway, 
                 hour.sin = green$hour.sin, hour.cos = green$hour.cos)
yellow.x <- cbind(yellow.dummies, avg.pickup.dist.subway = yellow$avg.pickup.dist.subway, 
                  hour.sin = yellow$hour.sin, hour.cos = yellow$hour.cos)
uber.x <- cbind(uber.dummies, avg.pickup.dist.subway = uber$avg.pickup.dist.subway, 
                hour.sin = uber$hour.sin, hour.cos = uber$hour.cos)

green.cv.poisson <- cv.glmnet(x = green.x, y = green$count, family = "poisson", alpha = 1, nfolds = 10)
yellow.cv.poisson <- cv.glmnet(x = yellow.x, y = yellow$count, family = "poisson", alpha = 1, nfolds = 10)
uber.cv.poisson <- cv.glmnet(x = uber.x, y = uber$count, family = "poisson", alpha = 1, nfolds = 10)

min(green.cv.poisson$cvm)
coef(green.cv.poisson, s="lambda.min")
min(yellow.cv.poisson$cvm)
coef(yellow.cv.poisson, s="lambda.min")
min(uber.cv.poisson$cvm)
coef(uber.cv.poisson, s="lambda.min")

predicted.counts.green <- predict(green.cv.poisson, green.x, s="lambda.min", type="response")
predicted.counts.yellow <- predict(yellow.cv.poisson, yellow.x, s="lambda.min", type="response")
predicted.counts.uber <- predict(uber.cv.poisson, uber.x, s="lambda.min", type="response")

standardized.res <- function(data, predictions) {
  y <- data$count
  (y - predictions)/sqrt(predictions)
}

residuals.green <- standardized.res(green, predicted.counts.green)
residuals.yellow <- standardized.res(yellow, predicted.counts.yellow)
residuals.uber <- standardized.res(uber, predicted.counts.uber)
unusual.green <- quantile(residuals.green, c(.005, .995))
unusual.yellow <- quantile(residuals.yellow, c(.005, .995))
unusual.uber <- quantile(residuals.uber, c(.005, .995))

low.green.buckets <- green[which(residuals.green <= unusual.green[1]),1:3]
high.green.buckets <- green[which(residuals.green >= unusual.green[2]),1:3]
low.yellow.buckets <- yellow[which(residuals.yellow <= unusual.yellow[1]),1:3]
high.yellow.buckets <- yellow[which(residuals.yellow >= unusual.yellow[2]),1:3]
low.uber.buckets <- uber[which(residuals.uber <= unusual.uber[1]),1:3]
high.uber.buckets <- uber[which(residuals.uber >= unusual.uber[2]),1:3]

add.observed.expected <- function(buckets, predicted, data) {
  index.buckets <- as.numeric(rownames(buckets))
  observed <- data$count[index.buckets]
  expected <- predicted[index.buckets]
  buckets$observed <- observed
  buckets$expected <- expected
  buckets$type <- data$type[index.buckets]
  buckets[with(buckets, order(neighborhood, weekday, weekhour)),]
}

low.green.buckets <- add.observed.expected(low.green.buckets, predicted.counts.green, green)
high.green.buckets <- add.observed.expected(high.green.buckets, predicted.counts.green, green)
low.yellow.buckets <- add.observed.expected(low.yellow.buckets, predicted.counts.yellow, yellow)
high.yellow.buckets <- add.observed.expected(high.yellow.buckets, predicted.counts.yellow, yellow)
low.uber.buckets <- add.observed.expected(low.uber.buckets, predicted.counts.uber, uber)
high.uber.buckets <- add.observed.expected(high.uber.buckets, predicted.counts.uber, uber)

View(low.green.buckets); View(high.green.buckets)
View(low.yellow.buckets); View(high.yellow.buckets)
View(low.uber.buckets); View(high.uber.buckets)

write.csv(low.green.buckets, "low_green_buckets", quote = F, row.names = F)
write.csv(high.green.buckets, "high_green_buckets", quote = F, row.names = F)
write.csv(low.yellow.buckets, "low_yellow_buckets", quote = F, row.names = F)
write.csv(high.yellow.buckets, "high_yellow_buckets", quote = F, row.names = F)
write.csv(low.uber.buckets, "low_uber_buckets", quote = F, row.names = F)
write.csv(high.uber.buckets, "high_uber_buckets", quote = F, row.names = F)

green.x.interact <- make.dummies.interact(green.dummies, green.x)
yellow.x.interact <- make.dummies.interact(yellow.dummies, yellow.x)
uber.x.interact <- make.dummies.interact(uber.dummies, uber.x)
combined.x.interact <- rbind(green.x.interact, yellow.x.interact, uber.x.interact)

combined.cv.poisson <- cv.glmnet(x = combined.x.interact, 
                                 y = c(green$count, yellow$count, uber$count), 
                                 family = "poisson", alpha = 1, nfolds = 10)
min(combined.cv.poisson$cvm)
coef(combined.cv.poisson, s="lambda.min")
write.table(as.matrix(coef(green.cv.poisson, s="lambda.min")), "regression_coefficients_green",
            quote = F, row.names = T, col.names = F)
write.table(as.matrix(coef(yellow.cv.poisson, s="lambda.min")), "regression_coefficients_yellow",
            quote = F, row.names = T, col.names = F)
write.table(as.matrix(coef(uber.cv.poisson, s="lambda.min")), "regression_coefficients_uber",
            quote = F, row.names = T, col.names = F)
write.table(as.matrix(coef(combined.cv.poisson, s="lambda.min")), "regression_coefficients_interaction",
          quote = F, row.names = T, col.names = F)

common.columns <- colnames(uber)
common.data <- rbind(green[,common.columns], yellow[,common.columns], uber)
green.index <- which(common.data$type == "green")
yellow.index <- which(common.data$type == "yellow")
uber.index <- which(common.data$type == "uber")

predicted.counts.combined <- predict(combined.cv.poisson, combined.x.interact, 
                                     s="lambda.min", type="response")
residuals.combined <- standardized.res(common.data, predicted.counts.combined)
green.unusual <- quantile(residuals.combined[green.index], c(.005, .995))
yellow.unusual <- quantile(residuals.combined[yellow.index], c(.005, .995))
uber.unusual <- quantile(residuals.combined[uber.index], c(.005, .995))
low.green.combined <- green.index[which(residuals.combined[green.index] <= green.unusual[1])]
high.green.combined <- green.index[which(residuals.combined[green.index] >= green.unusual[2])]
low.yellow.combined <- yellow.index[which(residuals.combined[yellow.index] <= yellow.unusual[1])]
high.yellow.combined <- yellow.index[which(residuals.combined[yellow.index] >= yellow.unusual[2])]
low.uber.combined <- uber.index[which(residuals.combined[uber.index] <= uber.unusual[1])]
high.uber.combined <- uber.index[which(residuals.combined[uber.index] >= uber.unusual[2])]

low.combined.buckets <- common.data[c(low.green.combined, low.yellow.combined, 
                                      low.uber.combined),1:3]
high.combined.buckets <- common.data[c(high.green.combined, high.yellow.combined, 
                                       high.uber.combined),1:3]
low.combined.buckets <- add.observed.expected(low.combined.buckets, predicted.counts.combined, 
                                              data = common.data)
high.combined.buckets <- add.observed.expected(high.combined.buckets, predicted.counts.combined, 
                                               data = common.data)

write.csv(low.combined.buckets, "low_combined_buckets", quote = F, row.names = F)
write.csv(high.combined.buckets, "high_combined_buckets", quote = F, row.names = F)
table(low.combined.buckets$type); table(high.combined.buckets$type)
View(low.combined.buckets); View(high.combined.buckets)

all.combined.buckets <- add.observed.expected(common.data[c(green.index, yellow.index, uber.index), 1:3],
                              predicted.counts.combined, data = common.data)
all.combined.buckets$residual <- (all.combined.buckets$observed - all.combined.buckets$expected) /
                                    sqrt( all.combined.buckets$expected)
View(all.combined.buckets)
write.csv(all.combined.buckets, "all_combined_buckets", quote = F, row.names = F)

monday.morning.yellow <- filter(all.combined.buckets, type == "yellow" & weekday == "Mon" & weekhour == 9)
sunday.midnight.yellow <- filter(all.combined.buckets, type == "yellow" & weekday == "Sun" & weekhour == 0)
write.csv(monday.morning.yellow, "monday_9am_yellow", quote = F, row.names = F)
write.csv(sunday.midnight.yellow, "sunday_midnight_yellow", quote = F, row.names = F)

plot.neighborhoods.color <- function(neighborhoods.interest, colorcab = "yellow") {
  neighborhoods_use <- filter(all.combined.buckets, type == colorcab & 
                                   neighborhood %in% neighborhoods.interest)
  neighborhoods_use$num_hour <- with(neighborhoods_use, (as.numeric(weekday)-1)*24 + weekhour)
  nb.plot <- ggplot(neighborhoods_use, aes(x=num_hour, y = observed, 
                  color = neighborhood, linetype = "Observed"))
  nb.plot <- nb.plot + geom_smooth(fill=NA) + geom_smooth(aes(y = expected, 
                  color = neighborhood, linetype = "Expected"), fill=NA) 
  nb.plot <- nb.plot + labs(color = "Neighborhood", linetype = "Number of Pickups", 
                  x = "Hour of Week", y = "Total Number of Pickups") + 
    scale_linetype_manual(values = c(4, 1)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_x_discrete(breaks = 24*0:6, labels = levels(neighborhoods_use$weekday))
  nb.plot
}

neighborhoods.interest <- c("LaGuardia Airport", "Bushwick and Williamsburg", "East Harlem", 
                            "Greenpoint", "Jamaica")
neighborhoods.interest2 <- c("Chelsea and Clinton", "Upper East Side", "Greenwich Village and Soho",
                             "Lower East Side")

q1 <- plot.neighborhoods.color(neighborhoods.interest) + ggtitle("Yellow Cabs")
q2 <- plot.neighborhoods.color(neighborhoods.interest2) + ggtitle("Yellow Cabs")
q1
q2

yellow.weekday.counts <- as.data.frame(summarize(group_by(yellow, weekday), mean(count)))
green.weekday.counts <- as.data.frame(summarize(group_by(green, weekday), mean(count)))
uber.weekday.counts <- as.data.frame(summarize(group_by(uber, weekday), mean(count)))

yellow.hourly.counts <- as.data.frame(summarize(group_by(yellow, weekhour), sum(count)))
green.hourly.counts <- as.data.frame(summarize(group_by(green, weekhour), sum(count)))
uber.hourly.counts <- as.data.frame(summarize(group_by(uber, weekhour), sum(count)))
yellow.hourly.counts$type <- "yellow"
green.hourly.counts$type <- "green"
uber.hourly.counts$type <- "uber"
hourly.counts <- rbind(yellow.hourly.counts, green.hourly.counts, uber.hourly.counts)


p1 <- ggplot(hourly.counts, aes(x=weekhour, y=`sum(count)`)) + geom_smooth(fill=NA, aes(color = type)) +
  ylab("Number of Pickups") + xlab("Hour of Day") + theme(legend.position="none") +
  scale_color_manual(values=c("green4", "gray32", "yellow3"))
p1
p2 <- ggplot(hourly.counts, aes(x=weekhour, y=log(`sum(count)`))) + geom_smooth(fill=NA, aes(color = type)) +
  ylab("Log Number of Pickups") + xlab("Hour of Day") + 
  scale_color_manual(values=c("green4", "gray32", "yellow3"))
p2

grid.arrange(p1,p2, nrow=1, widths = c(600, 700))

sum(combined.cv.poisson$glmnet.fit$beta[,100] != 0)
combined.cv.poisson$glmnet.fit$beta[,100]

save.image("parse_buckets.RData")




