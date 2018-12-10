##' Plotting pearson residuals for linear mixed effect models with outliers denotation
##'
##' This function plots the pearson type residuals against fitted values from a linear mixed effect model fitted by lmer function, with optional denotation of possible outliers.
##' @param mod a lmer model
##' @param denote gives the index with top 5 percent pearson type residuals
##' @examples
##' data(sleepstudy)
##' mod<-lmer(Reaction ~ Days + (Days|Subject), data=sleepstudy)
##' resid.plot(mod, denote=TRUE)
##' @author Yian Guo, Yuting Yang, Xiaoman Zhai
##' @export


resid.plot <- function(mod, denote = TRUE) {
    red <- data.frame(residual = resid(mod, type = "pearson"), fitted = fitted(mod))
    cut <- quantile(abs(red$residual), prob = 0.975)
    if (denote == TRUE) {
        print(ggplot(red, aes(fitted, residual)) + geom_point() + geom_text(aes(label = ifelse(abs(residual) >= cut, rownames(red), "")), 
            hjust = -0.3, vjust = -0.3, size = 3) + theme_bw() + geom_hline(yintercept = 0, color = "blue"))
    } else {
        print(ggplot(red, aes(fitted, residual)) + geom_point() + theme_bw() + geom_hline(yintercept = 0, color = "blue"))
    }
}
