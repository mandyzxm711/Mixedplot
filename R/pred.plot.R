##' Generate prediction and observation plots.
##'
##' This function plots predicted values and observed response values against time variable associated with repeated measures from a linear mixed effect model for visual comparison of model fit.
##' @param mod a lmer model
##' @param data the dataset used in the object
##' @param subject name of the independent sampling unit
##' @param time name of the time variable associated with repeated measures
##' @param response name of the response variable
##' @examples
##' data(sleepstudy)
##' mod<-lmer(Reaction ~ Days + (Days|Subject), data=sleepstudy)
##' pred.plot(mod, sleepstudy, subject='Subject', time='Days', response='Reaction')
##' @author Yian Guo, Yuting Yang, Xiaoman Zhai
##' @export

pred.plot <- function(mod, data, subject, time, response) {
    pred <- predict(mod)
    actual <- data[, response]
    sub <- data[, subject]
    tim <- data[, time]
    dat <- within(data, {
        Predicted <- pred
        Observed <- actual
        Subject <- sub
        Time <- tim
    })
    dat2 <- gather(dat, key = Class, value = Value, Observed, Predicted)
    ggplot(dat2) + aes(Time, Value, colour = Subject) + geom_line() + facet_wrap(~Class) + theme_bw()
}
