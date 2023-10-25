library(patchwork)
library(ggplot2)

plot_resids_anova<-function(formula, data,dec=3){
    IV<-all.vars(formula)[2]
    DV<-all.vars(formula)[1]
    sds<-tapply(data[[DV]],data[[IV]],sd)
    levs<-levels(data[[IV]])
    aov.model<-aov(formula(paste(DV,IV,sep="~")), data = data)
    M<-max(aov.model$residuals)
    m<-min(aov.model$residuals)
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
}

plot_qq_anova<-function(formula, data){
    IV<-all.vars(formula)[2]
    DV<-all.vars(formula)[1]
    aov.model<-aov(formula(paste(DV,IV,sep="~")), data = data)
    ggplot()+
        stat_qq(aes(sample=aov.model$residuals),color='blue',alpha=.5)+
        stat_qq_line(aes(sample=aov.model$residuals),color='red')+
        labs(title="QQ-plot",
             x="Theoretical Quantiles",
             y=paste("Residuals\n","ANOVA: ",paste(DV,IV,sep=" ~ ")))+
        theme_bw() 
}

# plot_qq_anova(weight~feed, data = chickwts)
# plot_qq_anova(Sepal.Width~Species, data = iris)

plot_hist_anova<-function(formula, data, bins=10){
    IV<-all.vars(formula)[2]
    DV<-all.vars(formula)[1]
    print(DV)
    levs<-levels(data[[IV]])
    print(levs)
    # aov.model<-aov(formula(paste(DV,IV,sep="~")), data = data)
    ggplot(data = data.frame(x = data[[DV]],
                             IV = data[[IV]]))+
        geom_histogram(aes(x=x,y=after_stat(density)),
                       color='gray4',fill='dodgerblue',alpha=.4,
                       bins = bins)+
        labs(title="",
             x=DV,
             y="")+
        facet_wrap(vars(IV),ncol=length(levs),scales="free_y")+
        theme_bw()
}

plot_hist_anova(Sepal.Width~Species, data = iris)

plot_anova_stuff<-function(formula, data, bins=10,dec=3){
    p1<-plot_resids_anova(formula=formula, data = data, dec=dec)
    p2<-plot_qq_anova(formula=formula, data = data)
    p3<-plot_hist_anova(formula=formula, data = data, bins=bins)
    
    layout1<-"
    AA##
    AA##
    AA##
    ##BB
    ##BB
    CCCC
    CCCC
    "
    
    p1+p2+p3+plot_layout(design=layout1)
}

plot_anova_stuff(weight~feed, data = chickwts, bins = 5)
