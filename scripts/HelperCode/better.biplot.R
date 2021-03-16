
better.biplot=function(pca,groups=rep("default",nrow(pca$scores)),pcx=1,pcy=2,col.lu=NULL,pch.lu=NULL,cex.lu=NULL,legpos='bottomright',leglist=unique(groups),PLOTLOADS=TRUE, loadthreshold=0.01,main=NULL,arrowlabpos=2){
	#default plot info	
	if (is.null(col.lu)) {col.lu="black";names(col.lu)="default"} 
	if (is.null(pch.lu)) {pch.lu =1;names(pch.lu)="default"} 
	if (is.null(cex.lu)) {cex.lu =1;names(cex.lu)="default"} 

	#sort out colors, shapes, and sizes of markers
	plotcol=mapply(pca.col.lookup,lu_val=groups,MoreArgs=list(lu_table=col.lu))
	legcol=mapply(pca.col.lookup,lu_val=leglist,MoreArgs=list(lu_table=col.lu))
	plotpch=mapply(pca.pch.lookup,lu_val=groups,MoreArgs=list(lu_table=pch.lu))
	legpch=mapply(pca.pch.lookup,lu_val=leglist,MoreArgs=list(lu_table=pch.lu))
	plotcex=mapply(pca.cex.lookup,lu_val=groups,MoreArgs=list(lu_table=cex.lu))
	legcex=mapply(pca.cex.lookup,lu_val=leglist,MoreArgs=list(lu_table=cex.lu))

	#get relevant loadings to plot
	loadx=pca$loadings[,pcx]
	loady=pca$loadings[,pcy]
	goodload=abs(loadx)>=loadthreshold|abs(loady)>=loadthreshold
	loadx=loadx[goodload]
	loady=loady[goodload]

	 plot(pca$score[,pcx], pca$score[,pcy], col=plotcol,pch=plotpch,cex=plotcex,xlab=paste("Principal Component",pcx),ylab=paste("Principal Component",pcy),main=main)
	
	#plot variable loadings
	if (PLOTLOADS){
		axislims=par('usr')
		yrng=axislims[4]-axislims[3]
		xrng=axislims[2]-axislims[1]
		ycenter=axislims[4]-yrng/2
		xcenter=axislims[2]-xrng/2
		xlines=cbind(xcenter,xcenter+xrng/2*loadx)
		ylines=cbind(ycenter,ycenter+yrng/2*loady)
		for (i_load in 1:nrow(xlines)){
			arrows(x0=xlines[i_load,1],x1=xlines[i_load,2],y0=ylines[i_load,1],y1=ylines[i_load,2],col="red",len=.1)
		}
		text(x=xlines[,2]*.975,y=ylines[,2]*.975,labels=names(loadx),cex=.5,pos=arrowlabpos,col="red")
	}
		if (!is.null(legpos))legend(legpos,legend=leglist,col=legcol,pch=legpch,pt.cex=legcex,bg = 'white')

}


better.biplot.4gradientdisplay=function(pca,dispdata=NULL,groups=NULL,pcx=1,pcy=2,pch.lu=NULL,legpos='bottomright',leglist=unique(groups),PLOTLOADS=TRUE, loadthreshold=0.01,main=NULL){
	#Here "dispdata" has 4 columns, 1:3 encode rgb colors, 4 cex function
	
	#default plot info	
	if (is.null(pch.lu)) {pch.lu =1;names(pch.lu)="default"} 

	#sort out colors, shapes, and sizes of markers
	plotcol=rgb(red=dispdata[,1],green=dispdata[,2],blue=dispdata[,3],maxColorValue=max(dispdata[,1:3]))
	plotpch=mapply(pca.pch.lookup,lu_val=groups,MoreArgs=list(lu_table=pch.lu))
	plotcex=dispdata[,4]
	legcol='black'
	legpch=mapply(pca.pch.lookup,lu_val=leglist,MoreArgs=list(lu_table=pch.lu))
	legcex=1
	
	#get relevant loadings to plot
	loadx=pca$loadings[,pcx]
	loady=pca$loadings[,pcy]
	goodload=abs(loadx)>=loadthreshold|abs(loady)>=loadthreshold
	loadx=loadx[goodload]
	loady=loady[goodload]

	 plot(pca$score[,pcx], pca$score[,pcy], col=plotcol,pch=plotpch,cex=plotcex,xlab=paste("Principal Component",pcx),ylab=paste("Principal Component",pcy),main=main)
	if (!is.null(legpos))legend(legpos,legend=leglist,col=legcol,pch=legpch,pt.cex=legcex)
	
	#plot variable loadings
	if (PLOTLOADS){
		axislims=par('usr')
		yrng=axislims[4]-axislims[3]
		xrng=axislims[2]-axislims[1]
		ycenter=axislims[4]-yrng/2
		xcenter=axislims[2]-xrng/2
		xlines=cbind(xcenter,xcenter+xrng/2*loadx)
		ylines=cbind(ycenter,ycenter+yrng/2*loady)
		for (i_load in 1:nrow(xlines)){
			arrows(x0=xlines[i_load,1],x1=xlines[i_load,2],y0=ylines[i_load,1],y1=ylines[i_load,2],col="red",len=.1)
		}
		text(x=xlines[,2]*.975,y=ylines[,2]*.975,labels=names(loadx),cex=.5,pos=2,col="red")
	}
}

pca.col.lookup=function(lu_val,lu_table){
	retval=lu_table[as.character(lu_val)]
	if (!is.na(retval)){
		return(retval)
	}else{
		retval=lu_table["default"]
		if (!is.na(retval)){
			return(retval)
		}else{
			return("gray")
		}
	}
}
pca.pch.lookup=function(lu_val,lu_table){
	retval=lu_table[as.character(lu_val)]
	if (!is.na(retval)){
		return(retval)
	}else{
		retval=lu_table["default"]
		if (!is.na(retval)){
			return(retval)
		}else{
			return(1)
		}
	}
}
pca.cex.lookup=function(lu_val,lu_table){
	retval=lu_table[as.character(lu_val)]
	if (!is.na(retval)){
		return(retval)
	}else{
		retval=lu_table["default"]
		if (!is.na(retval)){
			return(retval)
		}else{
			return(1)
		}
	}
}

bin.proportion=function(prop,steps){
	labels=rep("NA",length(prop))
	labels[prop==1]="100%"
	for (s in steps:1){
		target=s/steps		
		labels[prop<target]=paste("<",round(100*target,0),"%",sep="")
	}	
	labels[prop==0]="NONE"
	return(labels)
}


pca.point.color = function(model.id) {
        if (model.id == "null") {
            return("black")
        } else if (model.id == "Persian Gulf") {
            return("orange")
        } else if (model.id == "Pacific") {
            return("blue")
        } else if (model.id == "Indian") {
            return("Purple")
        } else if (model.id == "Persian Gulf") {
            return("orange")
        } else if (model.id == "Red Sea") {
            return("red")
        } else {
        	  return("gray")
            # unrecognized model id:
            # raise an exception (good), report an error (bad), and/or happily ignore the problem
            # and return some default value (BAD and UGLY; sadly, this approach is seen in many
            # R libraries)
        }
    }


pca.point.character = function(model.id) {
        if (model.id == "null") {
            return(1)
        } else if (model.id == "Red Sea") {
            return(4)
        } else {
        	  return(1)
            # unrecognized model id:
            # raise an exception (good), report an error (bad), and/or happily ignore the problem
            # and return some default value (BAD and UGLY; sadly, this approach is seen in many
            # R libraries)
        }
    }
plot.propcolorlogisitic=function(x){
	#return a nice logistic
	return((1/(1+1000*exp(-12.5*x))))
}
