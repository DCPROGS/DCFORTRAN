subroutine fitting_mode(ifitmode,nset,niobs,njset,ndimc,xobs,yobs,w,ncalc,printed,iflag,&
dcurve,nodata,shortprt,inkset,ncurvc,ncal,icurvc,jfix,noguess,fixset,fitted,sameq,sep,&
nj,juse,ikset,iver,titlef,nsfit,jmiss,norm1)

	real xobs(NIOBS,NJSET),yobs(NIOBS,NJSET),w(NIOBS,NJSET)
	integer ikset(20),nj(NJSET),jmiss(njset),juse(njset),ndat(njset),ncal(ndimc)
	logical noguess(100),sep,sameq,discprt,nodata,norm1,use1,printed,shortprt,norm,logyfit
	LOGICAL CONSTR,FLINE,SETVAR,defolt1,fitted,dcurve,logt,badset,fixset,neweq
	real setx(njset)
	integer njbase(njset),icurvd(njset),icurvw(njset),icurvc(njset),jfix(njset)
	integer ijoin(njset),iline(njset),ilinesav(njset),icuse(njset)
	ALLOCATABLE::xgrp,ybar,sdy,sdm,ngrp,nxval
	real*4 xgrp(:,:),ybar(:,:),sdy(:,:),sdm(:,:)
	integer ngrp(:,:),nxval(:)
	character*80 text,stringf
	CHARACTER*60  TITLEF,TITLED(20)
	CHARACTER*22 CNUM(10),CNUM1

	common/potrat/jfirst,iset,kmax1	
	common/dp/discprt
	COMMON/BLOCK3/logyfit,norm,xnorm
	ALLOCATE(xgrp(niobs,njset),ybar(niobs,njset),&
     sdy(niobs,njset),sdm(niobs,njset),ngrp(niobs,njset))
	ALLOCATE(nxval(njset))

	
	fixset=.false.
	fitted=.false.
	do i=1,njset
		 jfix(i)=0
		 noguess(i)=.false.
	enddo
	
	   
	kset=-1
	if(nset.eq.1) then
	       ifitmode=1	!if only one set
	       jset=1	!fit set 1 if only one set
	       ncurvc=1
	       ncal(1)=ncalc
	       icurvc(1)=1
	       iline(1)=0	!continuous
	       nsfit=1
	       juse(1)=1
	 endif
	 norm1=.false.
	 if(nset.gt.1) then
	     if(ifitmode.eq.6) then
			guesdone=.false.
			use1=.false.
			do m=1,nsfit	!check if set 1 is among those used
				j=juse(m)
				if(j.eq.1) use1=.true.
			enddo
			if(use1) then	!add rest of data to set 1
			n=nj(1)
			do m=1,nsfit
				j=juse(m)
				if(j.ne.1) then
					do i=1,nj(j)
					n=n+1
					Xobs(n,1)=Xobs(i,j)
					Yobs(n,1)=Yobs(i,j)
					w(n,1)=w(i,j)
					enddo
				endif
			enddo
			else		!copy all data to set 1
			n=0
			do m=1,nsfit
				j=juse(m)
				do i=1,nj(j)
				n=n+1
				Xobs(n,1)=Xobs(i,j)
				Yobs(n,1)=Yobs(i,j)
				w(n,1)=w(i,j)
				enddo
			enddo
			endif
            if(discprt) write(7,38)
            if(discprt) write(7,39) (juse(i),i=1,nsfit)
			nj(1)=n
			jset=1
			nsfit=1
			ifitmode=1
!!!! not clear here !!!
			do i=1,nset
				jmiss(i)=1	!miss all
			enddo
			jmiss(jset)=0	!fit set i
			nmiss=nset-1
			juse(1)=1
			norm1=.true.	!so means displayed in cvdisp
			call SORTr3(Xobs,Yobs,w,1,nj(1),.true.,&
     		niobs,njset,niobs,njset)
	     endif
	 endif
	 if(ifitmode.ge.1.and.ifitmode.le.5) then
	       do m=1,nsfit	!check if set 1 is among those used
		   j=juse(m)
	       call GETGROUP(xobs,yobs,w,j,nj,niobs,njset,nxv,&
     	   xgrp,ybar,sdy,sdm,ngrp,SSwg,ndf,SStot,SScol,ymean,n0)
		   nxval(j)=nxv
		   if(ndf.ge.1) then
			Serrwg=sqrt(SSwg/float(ndf))
			if(discprt) write(7,421) j
			if(discprt) write(7,42) nj(j),nxval(j),SStot,SScol,&
     			SSwg,ndf,Serrwg
			do k=1,nxval(j)
			if(discprt) write(7,43)k,ngrp(k,j),xgrp(k,j),&
     			 ybar(k,j),sdy(k,j),sdm(k,j)
			enddo
			if(n0.gt.0) then
		   		if(discprt) write(7,8) n0
			endif
		   endif
	       enddo
	 endif
! normalize  missing
	if(norm) goto 11		!already normalised to Y at given X
	if(norm1) goto 11
 11    if(ifitmode.eq.1) then
	        if(nsfit.eq.1.and.nset.gt.1) jset=juse(1)
			ncurvc=1
	        icurvc(1)=jset
		
	        iline(jset)=0	!continuous
	       
	 else
	        ncurvc=0
	   		do j=1,nset
	   			if(jmiss(j).ne.1) then !set omitted
	   			ncurvc=ncurvc+1
	   			icurvc(ncurvc)=j
				endif
			enddo
	 endif
	 do j1=1,ncurvc
	 		j=icurvc(j1)

	 		iline(j)=0			!all curves continuous
			ilinesav(j)=0
	 		if(ifitmode.eq.4.or.ifitmode.eq.5) iline(j)=0	!all continuous
	 		ncal(j)=ncalc
	 enddo
	 if(.not.printed) then
			if(iver.ge.1002) then
	          if(discprt) write(7,322) titlef
			endif
			do j=1,nset
				if(jmiss(j).ne.1) then
			!	if(.not.shortprt) call PRINTJ(j,titled,setx,hdisp,setvar,prt,nj,w,xobs,yobs,&
     		!	niobs,njset,norm1)
				endif
			enddo
	 endif
	     
	 if(neweq) then
			neweq=.false.
     else
		 
			if(ifitmode.ge.2) then
				mset=1		!to count sets for ifitmode=2
				j=juse(1)
				jset=j
			endif
			jfirst=jset
			if(ifitmode.ge.3.and.ifitmode.le.5) then	   !print titles of all sets
	   		n=1
	        stringf='                                                '
	        do j=1,nset
				if(jmiss(j).ne.1) then
				stringf(n:n+1)=char(j+48)//','
				n=n+2
				endif
	        enddo
	        n=nblank1(stringf)
	        stringf(n:n)=' '		!remove final comma
              if(discprt) write(7,321)
	        if(iver.ge.1002) then
	           if(discprt) write(7,322) titlef
	        endif
	        do j=1,nset
	           if(jmiss(j).ne.1) then
	           text='FIT TO DATA SET # '
                 if(discprt) write(7,1092) text,j,titled(j)
			   endif
			enddo
			endif
			do i=1,njset
				icuse(i)=0
			enddo
			SSDsep=0.0		!initialise for case where summed for each set
			nfittot=0		!ditto
			if(ifitmode.eq.1.or.ifitmode.eq.2) then
	  			if(iver.ge.1002) then
				 if(discprt) write(7,322) titlef
				endif
				text='FIT TO DATA SET # '
				if(norm1) text='FIT TO POOLED DATA IN SET #'
				if(dcurve) text='CURVE + DATA SET #'
				if(nodata) text='CALCULATED CURVE #'
				if(ifitmode.eq.1) then
					kset=jset
				else
					kset=ikset(1)
					if(kset.lt.1) kset=1
				endif
				call INTCONV(kset,cnum(1))
				stringf=titled(kset)
				 if(discprt) write(7,1092) text,kset,titled(kset)
				if(ifitmode.eq.2) then			
				nt=nblank1(text)
				call guimsg(' ',text(1:nt)//cnum(1),3,0,istatg)
				if(istatg.eq.6) then
				jset=1
				else 
				jset=2
				endif
				endif
			endif
	 endif
	 imin=1
	 imax=1
	 if(iflag.ne.2) then
	          j1=juse(1)
	          xmax2=-1.e37
	          xmin2=1.e37
	          do i=1,nj(j1)
	            if(Xobs(i,j1).lt.xmin2) then
	              xmin2=Xobs(i,j1)
	              imin=i
	            else if(Xobs(i,j1).gt.xmax2) then
	              xmax2=Xobs(i,j1)
	              imax=i
	            endif
	          enddo
	          if(Yobs(imin,j1).lt.Yobs(imax,j1)) then
				iequiv=0       !increasing
			  else
				iequiv=1		!decreasing
			  endif

	 endif

38	format(' Data from following sets pooled into set 1')
39	format(30(2x,i3))
1092	FORMAT(/,' ',a27,i3,/,3x,a60)
421		   format(1x,/,' Analysis of replicates for set ',i3)
42		   format(2x,i4,' observations at ',i4,' different X values',/,&
     	   ' Weighted sums of squares are:',/,&
     	   ' Total SSD = ',g13.6,/,&
     	   ' SSD between groups (X values) = ',g13.6,/,&
     	   ' Within group SSD = ',g12.5,' with ',i3,' df.',/,&
     	   ' s(err) = sqrt(SSD/df) = ',g13.6,/,/,&
           ' Group  n(j)  X value      mean           sd          sdm')
43			format(i3,i8,4g13.6)
8			format(/,' ',i3,&
     		' values omitted because they have zero weight')
322	   format(/,' File: ',a60)
321	        format(/,' SIMULTANEOUS FIT TO THE FOLLOWING SETS')
	   
end