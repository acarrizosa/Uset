#========================================================
#
#
# BEGIN FUNCTION
#
#
#========================================================
uset<-function(
     dat,
     uset.coarse=TRUE,
     uset.fine=FALSE,
     uint.coarse=.1,
     uint.fine=.01,
     threshold=0.5,
     yolk=FALSE,
     yolkint.coarse=.05,
     yolkint.fine=.01,
     points.on.line=1000,
     plotuset=TRUE,
     clusters="default"){

        #===================
        # Check for any specification mistakes
        #===================
        if(!is.data.frame(dat)){
                stop("Input data should be a data frame")
        }
        if(any(names(dat)[1:2]!=c("x", "y"))){
                stop("First two variables of input data should be named 'x' and 'y'")
        }
        if(clusters=="default"){
                clusters<-1
        } else if(clusters=="optimal"){
                clusters<-detectCores()-1
        } else {
                clusters<-clusters
        }
        if(!is.numeric(clusters) | clusters>detectCores()){
                stop("Clusters must either be 'default', 'optimal' or a numeric value that is less than the number of cores of your computer.")
        }

        #===================
        # Start function
        #===================
        startall<-Sys.time()
        #get necessary packages
          require(foreach)
          require(doParallel)
          require(ggplot2)
          require(MASS)
          require(stringr)

          workers<-makeCluster(clusters-1)
          registerDoParallel(workers)

          circleFun <- function(center = c(0,0),diameter = .2, npoints = 100){
               r = diameter / 2
               tt <- seq(0,2*pi,length.out = npoints)
               xx <- center[1] + r * cos(tt)
               yy <- center[2] + r * sin(tt)
               return(data.frame(x = xx, y = yy))
          }

          #==========================================================================================
          #
          #
          #                        FIND UNCOVERED SET
          #
          #
          #==========================================================================================
          if(uset.coarse){
               #===================
               # create grid for search
               #===================
               x<-seq(min(dat$x),max(dat$x), by=uint.coarse)
               y<-seq(min(dat$y),max(dat$y), by=uint.coarse)
               bootstraps<-lapply(1:length(y), function(num) data.frame(x, y[num]))
               bootstraps2<-bootstraps[[1]]
               for(i in 2:length(bootstraps)){
                    bootstraps2<-rbind(bootstraps2, bootstraps[[i]])
               }
               bootstraps<-bootstraps2

               #===================================
               #
               #    FIND UNCOVERED SET
               #
               #===================================

               print("Begin Coarse Search (Uncovered Set)")
               start<-Sys.time()
               results<-foreach(count=1:nrow(bootstraps))%dopar%{
                              covered<-FALSE
                              a<-count
                              b<-(1:nrow(bootstraps))[-a]
                              for(i in 1:length(b)){
                                   BbeatA<-FALSE
                                   BbeatA<-sum(sqrt(((bootstraps[a,1]-dat[,1])^2)+((bootstraps[a,2]-dat[,2])^2))> #distance to A larger than Distance to B is bad for A
                                                    sqrt(((bootstraps[b[i],1]-dat[,1])^2)+((bootstraps[b[i],2]-dat[,2])^2)))>
                                        nrow(dat)*threshold

                                   CbeatB<-FALSE
                                   CbeatA<-FALSE
                                   if(BbeatA){
                                        c<-(1:nrow(bootstraps))[c(-a,-b[i])]
                                        for(j in 1:length(c)){
                                             CbeatB[j]<-sum(sqrt(((bootstraps[b[i],1]-dat[,1])^2)+((bootstraps[b[i],2]-dat[,2])^2))> #distance to A larger than Distance to B is bad for A
                                                                 sqrt(((bootstraps[c[j],1]-dat[,1])^2)+((bootstraps[c[j],2]-dat[,2])^2)))>
                                                  nrow(dat)*threshold
                                             CbeatA[j]<-sum(sqrt(((bootstraps[a,1]-dat[,1])^2)+((bootstraps[a,2]-dat[,2])^2))> #distance to A larger than Distance to B is bad for A
                                                                 sqrt(((bootstraps[c[j],1]-dat[,1])^2)+((bootstraps[c[j],2]-dat[,2])^2)))>
                                                  nrow(dat)*threshold
                                        }
                                        if(all(CbeatA[CbeatB]) & length(CbeatA[CbeatB])>0){
                                             covered<-TRUE
                                             break
                                        }
                                   }
                              }
                              return(covered)
                         }
                    end<-Sys.time()
                    print("End Coarse Search")
                    message(paste("Elapsed time:", round(difftime(end, start), 3), attr(difftime(end, start), "units")))

                    #save uncovered set
                    bootstraps<-bootstraps[!unlist(results),]
                    names(bootstraps)<-c("x","y")

                    uset<-bootstraps

                    #==========================================================================================
                    #
                    #
                    #                        Fine SEARCH FOR UNCOVERED SET
                    #
                    #
                    #==========================================================================================
                    if(uset.fine){
                            #===================
                            # create grid for search
                            #===================
                            x<-seq(min(bootstraps$x)-(uint.coarse*3),max(bootstraps$x)+(uint.coarse*3), by=uint.fine)
                            y<-seq(min(bootstraps$y)-(uint.coarse*3),max(bootstraps$y)+(uint.coarse*3), by=uint.fine)
                            bootstraps<-lapply(1:length(y), function(num) data.frame(x, y[num]))
                            bootstraps2<-bootstraps[[1]]
                            for(i in 2:length(bootstraps)){
                                    bootstraps2<-rbind(bootstraps2, bootstraps[[i]])
                            }
                            bootstraps<-bootstraps2

                            #===================================
                            #
                            #    FIND UNCOVERED SET
                            #
                            #===================================

                            print("Begin Fine Search (Uncovered Set)")
                            start<-Sys.time()
                            results<-foreach(count=1:nrow(bootstraps))%dopar%{
                                    covered<-FALSE
                                    a<-count
                                    b<-(1:nrow(bootstraps))[-a]
                                    for(i in 1:length(b)){
                                            BbeatA<-FALSE
                                            BbeatA<-sum(sqrt(((bootstraps[a,1]-dat[,1])^2)+((bootstraps[a,2]-dat[,2])^2))> #distance to A larger than Distance to B is bad for A
                                                                sqrt(((bootstraps[b[i],1]-dat[,1])^2)+((bootstraps[b[i],2]-dat[,2])^2)))>
                                                    nrow(dat)*threshold

                                            CbeatB<-FALSE
                                            CbeatA<-FALSE
                                            if(BbeatA){
                                                    c<-(1:nrow(bootstraps))[c(-a,-b[i])]
                                                    for(j in 1:length(c)){
                                                            CbeatB[j]<-sum(sqrt(((bootstraps[b[i],1]-dat[,1])^2)+((bootstraps[b[i],2]-dat[,2])^2))> #distance to A larger than Distance to B is bad for A
                                                                                   sqrt(((bootstraps[c[j],1]-dat[,1])^2)+((bootstraps[c[j],2]-dat[,2])^2)))>
                                                                    nrow(dat)*threshold
                                                            CbeatA[j]<-sum(sqrt(((bootstraps[a,1]-dat[,1])^2)+((bootstraps[a,2]-dat[,2])^2))> #distance to A larger than Distance to B is bad for A
                                                                                   sqrt(((bootstraps[c[j],1]-dat[,1])^2)+((bootstraps[c[j],2]-dat[,2])^2)))>
                                                                    nrow(dat)*threshold
                                                    }
                                                    if(all(CbeatA[CbeatB]) & length(CbeatA[CbeatB])>0){
                                                            covered<-TRUE
                                                            break
                                                    }
                                            }
                                    }
                                    return(covered)
                            }
                            end<-Sys.time()

                            print("End Fine Search")
                            message(paste("Elapsed time:", round(difftime(end, start), 3), attr(difftime(end, start), "units")))

                            #save uncovered set
                            bootstraps<-bootstraps[!unlist(results),]
                            names(bootstraps)<-c("x","y")

                            uset<-bootstraps
                    }
          }

          #==========================================================================
          #
          #
          #              FIND YOLK
          #
          #
          #==========================================================================
          if(yolk){
                  #=======================================
                  # Get slopes and intercepts of lines given all permutations of points possible
                  #=======================================
                  m<-list()
                  b<-list()
                  x<-list()
                  y<-list()
                  count<-0
                  perm<-combn(1:nrow(dat), m=2)
                  for(i in 1:(ncol(perm))){
                          count<-count+1
                          y1<-dat[perm[1,i],2]
                          y2<-dat[perm[2,i],2]

                          x1<-dat[perm[1,i],1]
                          x2<-dat[perm[2,i],1]

                          m[[count]]<-(y2-y1)/ (x2-x1)
                          b[[count]]<-y1-(m[[count]]*x1)

                          if(x2-x1==0) {
                                  m[[count]]<-0
                                  b[[count]]<-0
                          }

                          x[[count]]<-c(x1,x2)
                          y[[count]]<-c(y1,y2)
                  }

                  m<-unlist(m)
                  b<-unlist(b)
                  x<-unlist(x)
                  y<-unlist(y)

                  x<-matrix(x, ncol=2, nrow=length(x)/2, byrow=T)
                  y<-matrix(y, ncol=2, nrow=length(y)/2, byrow=T)
                  colnames(x)<-c("x1", "x2")
                  colnames(y)<-c("y1", "y2")

                  lines<-data.frame(m,b,x,y)
                  lines<-unique(lines)

                  #======================================
                  # Only keep lines that meet condition of "median" lines
                  #======================================
                  print("Retrieving Median Lines")
                  start<-Sys.time()
                  thresh<-ceiling(nrow(dat)*threshold)
                  medianline<-foreach(n=1:nrow(lines))%dopar%{
                          resid<-vector("numeric", length=nrow(dat))
                          for(i in 1:nrow(dat)){
                                  x1<-dat[i,1]
                                  y1<-dat[i,2]

                                  resid[i]<-lines[n,]$m*x1+lines[n,]$b
                                  resid[i]<-round(resid[i]-y1, 10)

                          }
                          onabove<-sum(resid==0 | resid>0)
                          onbelow<-sum(resid==0 | resid<0)

                          medianline<-onabove==thresh | onbelow==thresh
                          return(medianline)
                  }
                  medianline<-unlist(medianline)
                  end<-Sys.time()

                  lines<-lines[medianline,]
                  lines<-lines[lines$m!=0,]

                  print(paste0(nrow(lines), " Median Lines Retrieved"))
                  message(paste("Elapsed time:", round(difftime(end, start), 3), attr(difftime(end, start), "units")))

                  #========================================================================
                  #
                  #                       BEGIN Coarse Yolk SEARCH
                  #
                  #========================================================================

                  #===========
                  # discrete interval for lines
                  #===========

                  drawlines<-lapply(1:nrow(lines), function(n){
                          x<-seq(min(dat$x),max(dat$x), length.out=points.on.line)
                          y<-(lines[n,]$m*x)+(lines[n,]$b)
                          result<-cbind(x,y)
                          return(result)
                  })

                  #===========
                  # Make Coarse Grid
                  #===========
                  x<-seq(min(dat$x),max(dat$x), by=yolkint.coarse)
                  y<-seq(min(dat$y),max(dat$y), by=yolkint.coarse)

                  if(uset.coarse | uset.fine){
                          x<-seq(min(bootstraps$x)-(uint.coarse),max(bootstraps$x)+(uint.coarse), by=yolkint.coarse)
                          y<-seq(min(bootstraps$y)-(uint.coarse),max(bootstraps$y)+(uint.coarse), by=yolkint.coarse)
                  }

                  bootstraps<-lapply(1:length(y), function(num) data.frame(x, y[num]))
                  bootstraps2<-bootstraps[[1]]
                  for(i in 2:length(bootstraps)){
                          bootstraps2<-rbind(bootstraps2, bootstraps[[i]])
                  }
                  bootstraps<-bootstraps2

                  #======================================
                  #
                  # Search for point with closest distance to all lines. (find smallest distance from each line.
                  # Then calculate sqrt of sum of squared distances for each line.
                  # sum this for each line. Find point with smallest sum.)
                  # For chosen point, distance of point to furthest line as radius
                  #======================================
                  print("Begin Coarse Yolk Search")

                  start<-Sys.time()
                  mindist<-foreach(m=1:nrow(bootstraps))%dopar%{
                          mindist<-vector()
                          for(n in 1:length(drawlines)){
                                  onedist<-vector("numeric", length=nrow(drawlines[[n]]))
                                  for(x in 1:nrow(drawlines[[n]])){
                                          onedist[x]<-sqrt((((bootstraps[m,1]-drawlines[[n]][x,1])^2)+
                                                                    ((bootstraps[m,2]-drawlines[[n]][x,2])^2)))
                                  }

                                  mindist[n]<-min(onedist)
                          }
                          return(mindist)
                  }
                  end<-Sys.time()
                  print("End Coarse Yolk Search")
                  message(paste("Elapsed time:", round(difftime(end, start), 3), attr(difftime(end, start), "units")))

                  radius<-lapply(mindist, max)

                  #===========================
                  # save center
                  #===========================
                  center<-bootstraps[which(unlist(radius)==min(unlist(radius))),]

                  #========================================================================
                  #
                  #                       BEGIN Fine Yolk SEARCH
                  #
                  #========================================================================
                  # Make Coarse Grid

                  x<-seq(center[,1]-(yolkint.coarse),center[,1]+(yolkint.coarse), by=yolkint.fine)
                  y<-seq(center[,2]-(yolkint.coarse),center[,2]+(yolkint.coarse), by=yolkint.fine)

                  bootstraps<-lapply(1:length(y), function(num) data.frame(x, y[num]))
                  bootstraps2<-bootstraps[[1]]
                  for(i in 2:length(bootstraps)){
                          bootstraps2<-rbind(bootstraps2, bootstraps[[i]])
                  }
                  bootstraps<-bootstraps2

                  #======================================
                  #
                  # Search for point with closest distance to all lines. (find smallest distance from each line.
                  # Then calculate sqrt of sum of squared distances for each line.
                  # sum this for each line. Find point with smallest sum.)
                  # For chosen point, distance of point to furthest line as radius
                  #======================================
                  print("Begin Fine Yolk Search")

                  start<-Sys.time()
                  mindist<-foreach(m=1:nrow(bootstraps))%dopar%{
                          mindist<-vector()
                          for(n in 1:length(drawlines)){
                                  onedist<-vector("numeric", length=nrow(drawlines[[n]]))
                                  for(x in 1:nrow(drawlines[[n]])){
                                          onedist[x]<-sqrt((((bootstraps[m,1]-drawlines[[n]][x,1])^2)+
                                                                    ((bootstraps[m,2]-drawlines[[n]][x,2])^2)))
                                  }

                                  mindist[n]<-min(onedist)
                          }
                          return(mindist)
                  }
                  end<-Sys.time()
                  print("End Fine Yolk Search")
                  message(paste("Elapsed time:", round(difftime(end, start), 3), attr(difftime(end, start), "units")))

                  radius<-lapply(mindist, max)

                  #===========================
                  # save circle
                  #===========================
                  center<-bootstraps[which(unlist(radius)==min(unlist(radius))),]

                  radius<-min(unlist(radius))
                  diameter<-radius*2
                  circle<-circleFun(as.vector(unlist(center)), diameter=diameter, npoints=1000)
                  names(center)<-c("x","y")

                  circle2<-circleFun(as.vector(unlist(center)), diameter=diameter*4, npoints=1000)
          }

          #========================================================================
          #
          #                       Draw Uncovered set, median lines, circles
          #
          #========================================================================
          if(plotuset & !yolk){
                  g<-qplot(dat[,1], dat[,2], geom="point")+geom_point(col="black")+
                          geom_point(aes(uset[,1], uset[,2]),pch=8, alpha=.5)+
                          geom_point(aes(dat[,1], dat[,2]))+
                          coord_equal()+
                          theme_classic()+
                          theme(axis.line = element_blank(),
                                panel.border = element_rect(colour = "black", fill=NA, size=1))+
                          xlim(-1.1,1.1)+
                          ylim(-1.1,1.1)+
                          xlab("First Dimension")+
                          ylab("Second Dimension")
          }

          if(plotuset & yolk){
                  g<-qplot(dat[,1], dat[,2], geom="point")+geom_point(col="black")

                  #plot median lines

                  g<-g+geom_segment(aes(x=lines$x1,
                                   xend=lines$x2,
                                   y=lines$y1,
                                   yend=lines$y2))

                  #plot uset
                  g<-g+geom_point(aes(uset[,1], uset[,2]),pch=8, col="gray")

                  #Plot yolk and 4r circle
                  g<-g+geom_point(aes(dat[,1], dat[,2]))+ #set points over uncovered set if they overlap
                          geom_point(aes(center[,1], center[,2]),col="black", pch=2)+
                          geom_path(aes(circle[,1], circle[,2]),col="black", lty=2,)+
                          geom_path(aes(circle2[,1], circle2[,2]),col="black", lty=3,)+
                          coord_equal()+
                          theme_classic()+
                          theme(axis.line = element_blank(),
                                panel.border = element_rect(colour = "black", fill=NA, size=1))+
                          xlim(-1.1,1.1)+
                          ylim(-1.1,1.1)+
                          xlab("First Dimension")+
                          ylab("Second Dimension")
          }

          #====================================================
          #
          #         CREATE OUTPUT
          #
          #====================================================

          endall<-Sys.time()
          message(paste("Overall elapsed time:", round(difftime(endall, startall), 3), attr(difftime(endall, startall), "units")))

          if(uset.coarse & plotuset & yolk){

               return(list(dat=dat,
                           yolkcenter=unlist(center),
                           yolkradius=radius,
                           yolkdiameter=diameter,
                           medianlines=lines,
                           uset=uset,
                           g))

          }else if(uset.coarse & !yolk & !plotuset){
               return(list(dat=dat,
                           uset=uset))

          }else if(uset.coarse & yolk & !plotuset){

               return(list(dat=dat,
                           yolkcenter=unlist(center),
                           yolkradius=radius,
                           yolkdiameter=diameter,
                           medianlines=lines,
                           uset=uset))

          }else if(uset.coarse & plotuset & !yolk){
                  return(list(dat=dat,
                              uset=uset,
                              g))
          }
         gc()
}




























































