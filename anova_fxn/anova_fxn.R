library(patchwork)
library(ggplot2)
library(tidyverse)

plot_resids_anova<-function(formula, data,dec=3,withsd = TRUE){
    IV<-all.vars(formula)[2]
    DV<-all.vars(formula)[1]
    sds<-tapply(data[[DV]],data[[IV]],sd)
    levs<-levels(data[[IV]])
    aov.model<-aov(formula(paste(DV,IV,sep="~")), data = data)
    M<-max(aov.model$residuals)
    m<-min(aov.model$residuals)
    if (withsd){
        ggplot(data = data.frame(x=data[[IV]],
                                 y=aov.model$residuals),
               aes(x=x,
                   y=y,
                   color=x))+
            geom_hline(yintercept=0)+
            geom_boxplot(aes(fill=x),alpha=.3, color=NA)+
            # geom_stripchart()
            geom_point(position=position_jitter(width=.1), alpha=.6)+theme_bw()+labs(y="residuals",x="fitted values", title=paste("Residual Versus Fitted Plot,",DV,"~",IV),                                                                    subtitle="(Boxes represent interquartile ranges)")+ scale_y_continuous(limits=c(m-.1,M+.12))+
            
            scale_color_discrete(IV,labels=paste(levs,"\n(stdev = ",round(sds,dec),")\n",sep=""))+
            scale_fill_discrete(guide="none")
    } else {
        ggplot(data = data.frame(x=data[[IV]],
                                 y=aov.model$residuals),
               aes(x=x,
                   y=y,
                   color=x))+
            geom_hline(yintercept=0)+
            geom_boxplot(aes(fill=x),alpha=.3, color=NA)+
            # geom_stripchart()
            geom_point(position=position_jitter(width=.1), alpha=.6)+theme_bw()+
            labs(y="residuals",x="fitted values", 
                 title=paste("Residual Versus Fitted Plot,",DV,"~",IV),                                                                    
                 subtitle="(Boxes represent interquartile ranges)")+ 
            scale_y_continuous(limits=c(m-.1,M+.12))+
            scale_color_discrete(guide="none")+
            scale_fill_discrete(guide="none")
    }
}

plot_qq_anova<-function(formula, data){
    IV<-all.vars(formula)[2]
    DV<-all.vars(formula)[1]
    aov.model<-aov(formula(paste(DV,IV,sep="~")), data = data)
    ggplot()+
        stat_qq(aes(sample=aov.model$residuals),color='blue',alpha=.5)+
        stat_qq_line(aes(sample=aov.model$residuals),color='red')+
        labs(title=paste("QQ-plot for ANOVA residuals:\n",paste(DV,IV,sep=" ~ ")),
             x="Theoretical Quantiles",
             y="Residuals")+
        theme_bw() 
}

# plot_qq_anova(weight~feed, data = chickwts)
# plot_qq_anova(Sepal.Width~Species, data = iris)

plot_hist_anova<-function(formula, data, bins=10){
    IV<-all.vars(formula)[2]
    DV<-all.vars(formula)[1]
    levs<-levels(data[[IV]])
    # aov.model<-aov(formula(paste(DV,IV,sep="~")), data = data)
    ggplot(data = data.frame(x = data[[DV]],
                             IV = data[[IV]]))+
        geom_histogram(aes(x=x,y=after_stat(density)),
                       color='gray4',fill='dodgerblue',alpha=.4,
                       bins = bins)+
        labs(title=paste("Histograms by",IV),
             x=DV,
             y="")+
        facet_wrap(vars(IV),ncol=length(levs),scales="free_y")+
        theme_bw()
}

plot_hist_anova(Sepal.Width~Species, data = iris)

library(dplyr)
library(kableExtra)
library(gridExtra)
summary_table<-function(formula, data,dec=3){
    IV<-all.vars(formula)[2]
    DV<-all.vars(formula)[1]
    df<-data.frame(DV =data[[DV]],
                   Groups = data[[IV]])
    df %>% 
        group_by(Groups) %>%
        summarise(means = mean(DV),
                     stddevs = sd(DV),
                     size = n()) -> df1
    colnames(df1)<-c("Group",
                     "Sample Means",
                     "Sample Std. Deviations",
                     "Sample Sizes (n)")
        # kable(col.names = c("Sample Means",
        #                     "Sample Std. Deviations",
        #                     "Sample Sizes (n)"),
        #       row.names=FALSE) %>%
        df1 %>% gridExtra::tableGrob(rows=NULL,
                                     theme=ttheme_minimal())
        
}
summary_table(Sepal.Width~Species, data = iris)
test_vals<-function(formula, data,dec=5){
    IV<-all.vars(formula)[2]
    DV<-all.vars(formula)[1]
    aov.model<-aov(formula(paste(DV,IV,sep="~")), data = data)
    Levenes.P.Value<-broom::tidy(leveneTest(formula(paste(DV,IV,sep="~")),
                           center="mean", data = data))$p.value
    Shapiro.Wilkes.P.Value<-shapiro.test(aov.model$residuals)$p.value
    
    df1<-data.frame(Levenes.P.Value = round(Levenes.P.Value,dec),
               Shapiro.Wilks.P.Value=round(Shapiro.Wilkes.P.Value,dec)) 
    colnames(df1)<-c("Levene's Test",
                     "Shapiro-Wilk Test")
    
    df1%>%
        gridExtra::tableGrob(rows="P-values: ",
                             theme=ttheme_minimal())
}

test_vals(Sepal.Width~Species, data = iris)


plot_anova_stuff<-function(formula, data, bins=10,dec=3,dec2=5,withsd=FALSE){
    p1<-plot_resids_anova(formula=formula, data = data, dec=dec,withsd=withsd)
    t1<-summary_table(formula=formula, data = data)
    t2<-test_vals(formula=formula,data =data,dec=dec2)
    p2<-plot_qq_anova(formula=formula, data = data)
    p3<-plot_hist_anova(formula=formula, data = data, bins=bins)
    
    layout1<-"
    EEEFF#
    EEEFF#
    EEEFF#
    AAACCC
    AAACCC
    AAACCC
    AAACCC
    DDDDDD
    DDDDDD
    "
    
    p1+p2+p3+t1+t2+plot_layout(design=layout1)
}

plot_anova_stuff(Sepal.Width~Species, data = iris, bins = 10,
                 dec2=8)

