##' Plotting cook's distance for linear mixed effect models.
##'
##' This function plots the cook's distance (calculated by HLMdiag ::cooks.distance) a linear mixed effect model fitted by lmer function.
##' @param mod a lmer model
##' @param data the dataset used in the object
##' @param cut cutoff value, unless specified, equals 4/n where n equals the observation number in the dataset.
##' @examples
##' data(sleepstudy)
##' mod<-lmer(Reaction ~ Days + (Days|Subject), data=sleepstudy)
##' cooks.plot(mod, data=sleepstudy)
##' @author Yian Guo, Yuting Yang, Xiaoman Zhai
##' @export


cooks.plot <- function(mod, data, cut) {
    cook <- cooks.distance(mod)
    cook.df <- data.frame(Index = 1:dim(data)[1], cooks.distance = cook[1:dim(data)[1]])
    if (missing(cut)) {
        ggplot(cook.df, aes(Index, cooks.distance)) + geom_point() + geom_text(aes(label = ifelse(cooks.distance >= 4/dim(data)[1], rownames(cook.df), 
            "")), hjust = -0.3, vjust = -0.3, size = 3) + theme_bw() + geom_hline(yintercept = 4/dim(data)[1], color = "blue")
    } else {
        ggplot(cook.df, aes(Index, cooks.distance)) + geom_point() + geom_text(aes(label = ifelse(cooks.distance >= cut, rownames(cook.df), 
            "")), hjust = -0.3, vjust = -0.3, size = 3) + theme_bw() + geom_hline(yintercept = cut, color = "blue")
    }
}
