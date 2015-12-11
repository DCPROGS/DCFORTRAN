subroutine format_list(ifiltype,ixd,iyd,xobs,yobs,w,njj,xnum,niobs,njset,Main,iData_list,&
		   j,iplot,nplot,xdata,ncols,nrows,fstitle,hdisp,icallasc,n1,button6,ValArray6,&
		   nset,gfile,newfile,iDataMainPanel)

	integer :: iData_list
	integer :: Button6(10)
	real :: xdata(nplot,njset,ncols,niobs)
	real*4 xobs(n1:niobs,njset),yobs(n1:niobs,njset),w(niobs,njset)
	character*60 fstitle,gfile
	character*70 mtitle
	integer :: ValArray6(50,50)
	real*4 Xnum(20,3)
	logical hdisp,newfile,discprt
	common/dp/discprt
	if(ifiltype.eq.1) then
	    ncol=4
	    if(hdisp) ncol=1
	    nd1=njj+20	!extra rows in case lines added in inwindv
	    
		if(discprt) write(7,1) 
		do I=1,njj
	           xdata(iplot,j,1,i)=xobs(i,j)
	           if(.not.hdisp) then		!X only for histograms
		       xdata(iplot,j,2,i)=yobs(i,j)
		       xdata(iplot,j,4,i)=w(i,j)
		       if(w(i,j).gt.1.e-37) then
		         sd=sqrt(1.0/w(i,j))
		       else
		         sd=1.e36
		       endif
		       xdata(iplot,j,3,i)=sd
	           endif
			   if(discprt) write(7,2) i,xobs(i,j),yobs(i,j),w(i,j),sd
	     enddo
		 do i=njj+1,njj+20
	           xdata(iplot,j,1,i)=0.
	           xdata(iplot,j,2,i)=0.
	           xdata(iplot,j,3,i)=0.
	           xdata(iplot,j,4,i)=0.
		 enddo
		 
		 NROWS=NJJ
		 NCOLS=NCOL
		 call values_list(Main,iData_list,ixd,iyd,ValArray6,button6,j,iplot,ncols,nrows,xdata,fstitle,&
		 niobs,njset,nplot,mtitle,istatic6,nset,gfile,newfile,iDataMainPanel)	
	else if(ifiltype.eq.2) then
		  j=1
		  ncol=4
	    if(hdisp) ncol=1
	    nd1=njj+20	!extra rows in case lines added in inwindv
	    
		if(discprt) write(7,1) 
		do I=1,njj
	           xdata(iplot,j,1,i)=xobs(i,j)
	           if(.not.hdisp) then		!X only for histograms
		       xdata(iplot,j,2,i)=yobs(i,j)
		       xdata(iplot,j,4,i)=w(i,j)
		       if(w(i,j).gt.1.e-37) then
		         sd=sqrt(1.0/w(i,j))
		       else
		         sd=1.e36
		       endif
		       xdata(iplot,j,3,i)=sd
	           endif
			   if(discprt) write(7,2) i,xobs(i,j),yobs(i,j),w(i,j),sd
	     enddo
		 
		 
		 NROWS=NJJ
		 NCOLS=NCOL
          call values_list(Main,iData_list,ixd,iyd,ValArray6,button6,j,iplot,ncols,nrows,xdata,fstitle,&
		  niobs,njset,nplot,mtitle,istatic6,nset,gfile,newfile,iDataMainPanel)

    else if(ifiltype.eq.3) then
		  
		  

	      
	      IF (ICALLasc.EQ.201) THEN
			NSET=1
			NCOLS=4

	       
	        do I=1,NROWS
	          xdata(1,1,1,i)=XNUM(i,1)
	          xdata(1,1,2,i)=0.
	          xdata(1,1,3,i)=1.
	          xdata(1,1,4,i)=1.
			enddo
	        
			
		  ELSE IF (ICALLasc.EQ.202) THEN
			NSET=1
			NCOLS=4
		
		
			do I=1,NROWS
	          xdata(1,1,1,i)=0.
	          xdata(1,1,2,i)=XNUM(i,2)
	          xdata(1,1,3,i)=1.
	          xdata(1,1,4,i)=1.
			enddo
	        
		 
	      ELSE IF (ICALLasc.EQ.203) THEN
			NSET=1
			NCOLS=4
	        
			do I=1,NROWS
	          xdata(1,1,1,i)=0.
	          xdata(1,1,2,i)=0.
	          xdata(1,1,3,i)=xnum(i,3)
			  SD=xDATA(1,1,3,I)
		      if(sd.lt.1.e18) then
				wt=1.0/(sd*sd)
		      else
				wt=0.0
		      endif
		      xDATA(1,1,4,I)=WT
	        
		    enddo
	      ELSE IF (ICALLasc.EQ.204) THEN
			NSET=1
			NCOLS=4
	        
	        do I=1,NROWS
	           xdata(1,1,1,i)=XNUM(i,1)
	           xdata(1,1,2,i)=XNUM(i,2)
	           xdata(1,1,3,i)=1.
	           xdata(1,1,4,i)=1.
	       enddo
	       
		  
	      ELSE IF (ICALLasc.EQ.205) THEN
			NSET=1
			NCOLS=4
	        
			do I=1,NROWS
	           xdata(1,1,1,i)=XNUM(i,1)
	           xdata(1,1,2,i)=XNUM(i,2)
	           xdata(1,1,3,i)=XNUM(i,3)
			   SD=xDATA(1,1,3,I)
		       if(sd.lt.1.e18) then
				wt=1.0/(sd*sd)
		       else
				wt=0.0
		       endif
		       xDATA(1,1,4,I)=WT
	        enddo
	        
		 
	      ELSE IF (ICALLasc.EQ.206) THEN
			NSET=1
			NCOLS=4
	        
	        do I=1,NROWS
	           xdata(1,1,1,i)=0.
	           xdata(1,1,2,i)=XNUM(i,2)
	           xdata(1,1,3,i)=XNUM(i,3)
		       SD=xDATA(1,1,3,I)
		       if(sd.lt.1.e18) then
				wt=1.0/(sd*sd)
		       else
				wt=0.0
		       endif
		       xDATA(1,1,4,I)=WT
	        enddo
	       
		 
	      ELSE IF (ICALLasc.EQ.207) THEN
			NSET=1
			NCOLS=4
	        
	        do I=1,NROWS
	           xdata(1,1,1,i)=Xnum(i,1)
	           xdata(1,1,2,i)=0.
	           xdata(1,1,3,i)=XNUM(i,3)
		       SD=xDATA(1,1,3,I)
		     if(sd.lt.1.e18) then
				wt=1.0/(sd*sd)
		     else
				wt=0.0
		     endif
		     xDATA(1,1,4,I)=WT
	        enddo
	        
		 
	      ELSE IF (ICALLasc.EQ.208) THEN
			NSET=3
			NCOLS=4

	        
	        do I=1,NROWS
	           xdata(1,1,1,i)=XNUM(i,1)
	           xdata(1,1,2,i)=0.
	           xdata(1,1,3,i)=1.
	           xdata(1,1,4,i)=1.
	        enddo
	        
	      ENDIF
		
		  call values_list(Main,iData_list,ixd,iyd,ValArray6,button6,j,iplot,ncols,nrows,xdata,fstitle,&
		  niobs,njset,1,mtitle,istaatic6,nset,gfile,newfile,iDataMainPanel)	 
	     
    endif
1	format(/,'  Obs   X value        Y value         s(Y)           Weight')
2	FORMAT(i4,4(2x,g13.6))
end
