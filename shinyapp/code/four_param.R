four_param<-function(conc,y1,y2,y3,nreplicates,nconc){
  conc=as.numeric(as.character(unlist(strsplit(conc,"\t"))))
  y1=as.numeric(as.character(unlist(strsplit(y1,"\t"))))
  y2=as.numeric(as.character(unlist(strsplit(y2,"\t"))))
  y3=as.numeric(as.character(unlist(strsplit(y3,"\t"))))
  # a="10	5	2.5	1.25	0.625	0.3125	0.15625	0.078125	0.0390625	0.01953125	Vehicle"
  # strsplit(a,"\t")
  # a="c(10,5,2.5,1.25,.625,.3125,.15625,.078125,.0390625,.01953125,0)"
  # as.numeric(as.character(unlist(strsplit(a,","))))

  # rm(list=ls())
    # nconc=11 #includes Vehicle control
    # nreplicates=3
    # conc=c(10,5,2.5,1.25,.625,.3125,.15625,.078125,.0390625,.01953125,0)
    # y1=c(3.09E-04,1.86E-03,4.26E-02,4.71E-01,1.12E+00,1.18E+00,9.27E-01,1.10E+00,1.22E+00,1.20E+00,1.00E+00)
    # y2=c(3.78E-04,	3.06E-03,	1.01E-01,	9.21E-01,	9.89E-01,	1.30E+00,	1.17E+00,	1.45E+00,	1.26E+00,	1.40E+00,	1.00E+00)
    # y3= c(5.82E-04,	2.56E-03,	5.65E-02,	5.11E-01,	6.25E-01,	8.79E-01,	1.05E+00,	1.05E+00,	1.40E+00,	1.26E+00,	1.00E+00)
    #Skipping a step for later: normalize the data and remove the Vehicle concentration
    y_mean=data.frame(conc)
  y_mean$y1=y1
  y_mean$y2=y2
  y_mean$y3=y3
  # y_mean=y_mean%>%mutate(ymean=(y1+y2+y3)/nreplicates)
  y_mean=y_mean%>%rowwise()%>%dplyr::mutate(y.mean=mean(c(y1,y2,y3)),y.sd=sd(c(y1,y2,y3)))

  ####### 4 Parameter fit#######
    x=conc
    y=y_mean$y.mean
    #Next: Appproximating Response from dose (inverse of the prediction)
    ic50.ll4=drm(y.mean~conc,data=y_mean,fct=LL.3(fixed=c(NA,1,NA)))
    b=coef(ic50.ll4)[1]
    c=0
    d=1
    e=coef(ic50.ll4)[2]
    ###Getting predictions
    y_mean=y_mean%>%group_by(conc)%>%mutate(y_model=c+((d-c)/(1+exp(b*(log(conc)-log(e))))))
    y_mean=data.frame(y_mean) #idk why I have to end up doing this

    ####### Output 1#######
    # y_mean$y.model
    y_mean
    ####### Output 2#######
    # plotly=ggplot(y_mean,aes(conc,y.mean))+geom_point()+geom_errorbar(aes(ymin=y.mean-y.sd,ymax=y.mean+y.sd))+geom_line(aes(y=y_model),linetype="dashed")+geom_ribbon(aes(ymin=y_model-y.sd,ymax=y_model+y.sd,alpha=0.3))+scale_x_continuous(trans="log10")
    # ggplotly(plotly)
  }
#4param.R
