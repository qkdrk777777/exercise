#' generate exercise data
#'
#' @return exercise data
#' @examples
#' data<-exe(ex='kettlebell_swing',input=c(30,30,30,30))
#' exdata<-exe('kettlebell_swing',c(30,30,30),rawdata=exdata)
#' @export
exe<-function(ex,input,rawdata=NULL,summary=F){
  if(!require(plyr)){install.packages('plyr')
    Sys.sleep(0.01)
    library(plyr)}

    ex1<-data.frame(type=ex,day=Sys.Date(),count=input)
    ex2<-ddply(ex1,~day,summarise,sum_count=sum(count))
        if(is.null(rawdata)){output<-merge(ex1,ex2,by='day')}else if(is.data.frame(rawdata)){
      output<-rbind(rawdata,merge(ex1,ex2,by='day'))} else stop("Rawdata type only data.frame")
    if(!is.null(rawdata)){
    if(unique(ex1$day)%in%unique(rawdata[,1]))

   {temp<-output[output$day==unique(ex1$day),]
    output[output$day==unique(ex1$day),]<-merge(temp[,-4],ddply(temp,~day+type,summarise,sum_count=sum(count)),by=c('day','type'))}
    }

    output<-output[order(output$day),]
    if(summary) output<-ddply(output,~day,summarise,sum_count=sum(count))
    return(output)
    }
#devtools::use_data(exdata, internal = F,overwrite=T)

