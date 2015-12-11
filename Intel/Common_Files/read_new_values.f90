	subroutine read_new_values(xdata,njset,ncols,niobs,xval,yval,ndatset,ndimd,ndv1,&
			w,kwi,kwj,n1,hdisp,nplot,iplot,Data_list,newfile,xvalold,yvalold)

	logical hdisp,newfile
	real*4 XVAL(n1:ndv1,ndimd),YVAL(n1:ndv1,ndimd)
	real*4 w(kwi,kwj)
	real*4 XVALold(n1:ndv1,ndimd),YVALold(n1:ndv1,ndimd)

	real :: xdata(nplot,njset,ncols,niobs)
	integer ndatset(200,20),data_list(50,50)

	do j=1,ndimd
		do I=1,ndatset(iplot,j)
		   xvalold(i,j)=xval(i,j)
	       xval(i,j)=xdata(iplot,j,1,i)
	       if(.not.hdisp) then		!X only for histograms
		       yvalold(i,j)=yval(i,j)
		       yval(i,j)=xdata(iplot,j,2,i)
			   
		       w(i,j)=xdata(iplot,j,4,i)
		       if(w(i,j).gt.1.e-37) then
		         sd=sqrt(1.0/w(i,j))
		       else
		         sd=1.e36
		       endif
		       sd=xdata(iplot,j,3,i)
	        endif
	     enddo
		 
		 
	enddo	 
	end