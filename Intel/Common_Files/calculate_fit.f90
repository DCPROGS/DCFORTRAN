subroutine calculate_fit(Main,Xobs,Yobs,w,nj,juse,niobs,njset,iparsav,&
          theta,ndth,thetgues,nset1,ndimc,setx,jfix,nfix,sepmax,kfix,&
		  nodata,sameq,sep,jcset,mset,ikset,iflag,hillang,isepfit,autosep,&
		  titlep,ndc1,ncalc,xcal,ycal,iw,titled,ncurvc,icurvc,ncal,jmiss)
	
	
	use menu_f90
	allocatable::cormat
	real*4 xcal(ndc1,ndimc),ycal(ndc1,ndimc)
	real setx(njset)
	real*4 ymaxj(100)
	REAL SDEV(100),SDEVSAV(100)
	real xobs(NIOBS,NJSET),yobs(NIOBS,NJSET),w(NIOBS,NJSET)
	real cormat(:,:)
	real theta(100)
	real thetsav(100),thetgues(100,nset)
	real*4 xgrp(niobs,njset),ybar(niobs,njset),sdm(niobs,njset)
	integer ngrp(niobs,njset)
	integer ikset(20),nj(NJSET),jmiss(njset),juse(njset),ndat(njset),ncal(ndimc)
	integer isline(ndimc),iparsav(10),jfixsav(njset),nxval(njset)
	integer njbase(njset),icurvd(njset),icurvw(njset),icurvc(njset),jfix(njset)
	integer ijoin(njset),iline(njset),ilinesav(njset),icuse(njset)
	logical logx,xgt0
	logical sep,sameq,discprt,nodata,norm1,use1,printed,shortprt,skipq
	LOGICAL CONSTR,FLINE,SETVAR,defolt1,fitted,dcurve,logt,badset,fixset,sepmax
	logical hillang,allmode,autosep,noerr
	
	character*20 TITLEP(100),ptitle(100)	
	character*10 t1(100),titlep1(100)
	CHARACTER*22 CNUM(10),CNUM1
	character*80 mtext(10),param(100)
	character*40 message
	character*60 wtext(100),thetasc(100),sdasc(100)
	character*60 titled(njset)

	COMMON/BLOCK1/constr,nset,nfit,nsfit,Xv,kmax,ncomp,&
	nmod,fline,nomit,jomit(20),jset,ifitmode
	COMMON/BLOCK2/castar,X1,X2,iequiv,ip1,ip2
	COMMON/BLOCK3/logyfit,norm,xnorm
	common/fix/fixratio,ir1,ir2,rval	!to fix ratio of 2 parameters
	common/potrat/jfirst,iset,kmax1	
	common/dp/discprt
	common/abt/abort
	common/pwrfunc/ybarp
	common/limit/wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
	xmin,xmax,ymin,ymax
	common/logval/logx,logy,sqrty,logity

	fixset=.true.
	fitted=.true.
	dcurve=.false.
	ndisp=20
	skipq=.true.
	negpar=.false.
	do i=1,kmax
		   if(theta(i).lt.0.0) negpar=.true.
	enddo
	if(negpar.or.nmod.eq.21) then
		   constr=.false.
	else
		   constr=.true.
	endif
	badset=.false.
	if(ifitmode.le.2) then
		  nfix=1
		  
		  jfix(nfix)=1
	   	  kfit=kmax-nfix    !in case jump to 524
          if(discprt) write(7,45) kfit,nxval(jset)
	      if(nxval(jset).lt.kfit) then
          if(discprt) write(7,46) jset,kfit,nxval(jset)
		   badset=.true.
	      endif
	else
		   if(nfix.lt.1) nfix=1
		   jfix(1)=1
	endif
	irestrt=3
	errfac=1.e-4
	ndisp=0
	kfit=kmax-nfix    !in case jump to 524
	nevm=-200000		!so iterations not printed to disc
	confac=0.5		!parameter for simplex3
	errfac=1.e-4
			!do not use delmin for convergence
	

!note
	smin=SSDCV(kmax,theta,Xobs,yobs,w,nj,juse,setx,niobs,njset)

    if(discprt) write(7,20) smin,(theta(i),i=1,kmax)
!	skipq=.false.
!	jfix(1)=0
	stpfac=0.1
!	note	delmin=-1.
	delmin=-1.
	call SIMPLEXv(kmax,THETA,stpfac,errfac,nev,nevm,&
          smin,SSDCV,Ndisp,jfix,delmin,confac,irestrt,iconv,&
          Xobs,yobs,w,nj,juse,setx,niobs,njset,ndth,message)
	      sres=-1.
	if(nfit.gt.kfit) then
	        var=smin/float(nfit-kfit)		!error var
	        if(var.gt.1.e-30) sres=sqrt(var)
	else if(kfit.eq.nfit) then
	   		var=0.0
	   		sres=0.0
	endif
	ndf=nfit-kfit		!degrees of freedom for error
    if(discprt) write(7,5234) nfit,kfit,Smin,sres,ndf
	j=jset
	if(ifitmode.eq.4.or.ifitmode.eq.5) then
	      do i=1,kmax
		  thetsav(i)=theta(i)
		  jfixsav(i)=jfix(i)
	      enddo
	      sminsav=smin
	   	iparsav(1)=ifitmode 	!save for lik intervals on sim fit
	   	iparsav(2)=nmod
	   	iparsav(3)=kmax
	   	iparsav(4)=kmax1
	   	iparsav(5)=kfit
	   	iparsav(6)=nfit
	   	iparsav(7)=ip1
	   	iparsav(8)=ip2
	   	if(nmod.eq.28) then
		 thetgues(1,1)=theta(1)		!x0 for 1st set
		 do j=2,nsfit
	         thetgues(1,j)=thetgues(1,1)*theta(j+1)
		 enddo
	   	endif
	endif
	if((nmod.eq.26.or.nmod.eq.27).and.ifitmode.eq.2.and.iflag.ne.3) then
	   	do i=1,kmax
		thetgues(i,jset)=theta(i)
	   	enddo
	endif
    ALLOCATE(cormat(kmax,kmax))
	ndf=nfit-kfit		!degrees of freedom for error
	if(dcurve.or.nodata.or.ndf.eq.0) then
	   	   noerr=.true.
    else if(.not.(autosep.and.mset.gt.1)) then
		  call guimsg(' ','Calculate approximate errors',1,3,istatus)
		  if(allocated(cormat)) deallocate(cormat)
	      ALLOCATE(cormat(kmax,kmax))	!for correlation matrix
		  if(istatus.eq.6) then
		    kfit=kmax-nfix		!redefine (in case kmax redef for ifitmode=4,5)
	        if(iw.eq.2.or.iw.eq.3.or.iw.eq.4) then
	   			serr=1.0		!error SD if weights used for errors
		    else if(iw.eq.1.or.iw.eq.5) then
	   			serr=sres	!error SD if residuals used for errors
				call HESMAT3(theta,sdev,nfix,jfix,SMIN,SSDCV,kmax,kfit,&
      			ndth,Xobs,yobs,w,nj,juse,setx,niobs,njset,cormat,serr)
		   endif
          endif
	endif
	
	if(ifitmode.eq.4.or.ifitmode.eq.5) then
	    do i=1,kmax
		    sdevsav(i)=sdev(i)	!save for likint
	   	enddo
	endif
	if(iflag.eq.2) then
	   	  if(isepfit.eq.1.or.isepfit.eq.2) then  !one simplex for each set
	      	SSDsep=SSDsep+smin		!add values for each set
			nfittot=nfittot+nfit
	      	kfitsep=nsfit*(kmax-nfix) !save number of param fitted for ditto
	   	  else if(isepfit.eq.3) then		!only one call to simplex
		      SSDsep=smin
			nfittot=nfit
			kfitsep=kmax-nfix
	   	  endif
	endif
	km2=kmax	!copy for call
	call CVOUT1w(theta,noerr,iw,nev,smin,jfix,fitted,titlep,&
      	sres,Sdev,titled,yobs,xobs,w,nj,jmiss,setx,niobs,njset,&
       	nodata,dcurve,ndth,km2,cormat,kfit,&
      	nxval,xgrp,ybar,sdm,ngrp,SSwg,ndf,wtext,thetasc,sdasc)
		
		if(allocated(cormat)) deallocate(cormat)
   if(ifitmode.eq.1) then		!one pass only
	   	  if(nmod.eq.26.or.nmod.eq.27) then
		  	iset=1		!needed in commom for ycalcv
	        endif
	   	    call DEFYcal(xmin,xmax,ncalc,logx,theta,Xcal,Ycal,jset,&
         	  xgt0,setx,njset,ndimc,ndc1)
	else if(ifitmode.ge.3.and.ifitmode.le.5) then !one pass only -calc all curves
	   	  iset=0
	   	  do m=1,nsfit		!go through all fitted sets
	    	   j=juse(m)
	    	   iset=iset+1	!for common -needed for ifitmode=4,5 if sets omitted
	    	   jset=j		!for COMMON
	    	   jval=j
	         if(iflag.eq.2) then	!for sep fits (with common max) after ifit=4,5
			jval=j+nset		!sep fits in j=nset+1, nset+2,....,2*nset
            ncurvc=ncurvc+1
			icurvc(ncurvc)=jval
			iline(jval)=2
			ncal(jval)=ncalc
	         endif
	    	   call DEFYcal(xmin,xmax,ncalc,logx,theta,Xcal,Ycal,&
     	   jval,xgt0,setx,njset,ndimc,ndc1)
		  enddo
	        if((nmod.eq.26.or.nmod.eq.27).and.ifitmode.eq.3.and.&
     		ip1.eq.1) then
			iset=0
			do m=1,nsfit
		   	j=juse(m)
		   	iset=iset+1
		   	j0=1+(iset-1)*ncomp	!=1,1+ncomp,1+2*ncomp for set 1,2,3...
		   	ymaxj(j)=0.0	!total ymax for all components in set j
		   	do i=1,ncomp
		      ymaxj(j)=ymaxj(j)+theta(j0+i)	!Ymax for component #1, set #iset
		   	enddo
			enddo
	   	endif
	    if(ifitmode.gt.3) goto 99
	else if(ifitmode.eq.2.and.sameq.and.mset.le.nsfit) then
	        if(nmod.eq.26.or.nmod.eq.27) then
		 	iset=1		!needed in commom for ycalcv
	   	    endif
	        jval=jset
	        if(iflag.eq.2) then	!for separate fits
		  	jval=jset+nset	!sep fits in j=nset+1, nset+2,....,2*nset
			iline(jval)=2
			ncal(jval)=ncalc
			ncurvc=ncurvc+1
			icurvc(ncurvc)=jval
	   	    endif
	   	    call DEFYcal(xmin,xmax,ncalc,logx,theta,Xcal,Ycal,jval,&
         	  xgt0,setx,njset,ndimc,ndc1)
	   	    mset=mset+1         !Go on to the next set iflag=3 :done all
	        if(mset.gt.nsfit) then
			iflag=3
			goto 99
		    endif
	        j=juse(mset)		!next set # to be fitted
	        jset=j
	        if(iflag.eq.0) iflag=1		!to tell GETEQN this is not 1st set
	        do i=1,kmax
			theta(i)=thetgues(i,jset)
	        enddo
	   	  if(hillang.and.isepfit.eq.2) then
			theta(2)=thetsav(2)	!value from simultaneous fit must be used
	        endif

	endif

	redo=.true.
	if(sep) then
		   do m=1,nsfit
                  ilinesav(m)=iline(m)
		      if(isline(m).eq.-1) then
                    iline(m)=isline(m)
			 endif
		   enddo
	else
		   do m=1,nsfit
                  iline(m)=ilinesav(m)
		   enddo
	endif
	iwid=14
	iheight=kmax+kfit+4
	itextable = gmCreateComplexDialogueBox(Main, 23, 1, iwid, iheight, GALL, 'Least Square Fit', &
              	gmVpos=Gtop,gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmCallBack=-21,gmSelect=-22)


    itextPanel=gmCreatePanel(itextable, 0, 0, iwid,iheight, &
              	gmHpos=GCENTRE, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)

	
	istit=gmcreatetextentry(itextpanel,1,kmax+kfit+3,iwid-2,1,wtext(100),60,&
	gdisplay,gmtextcol=1,gmHpos=Gleft, gmVpos=Gbottom, gmExpand=GON)

istit=gmcreatetextentry(itextpanel,1,1,iwid-2,1,wtext(1),60,&
	gdisplay,gmtextcol=1,gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON)
!!	do i=1,kmax+1
	do i=1,kmax
	    iscot=gmcreatetextentry(itextpanel,1,kmax+kfit+3-i,4,1,titlep(i),60,&
		gdisplay,gmtextcol=1,gmHpos=Gleft, gmVpos=Gbottom, gmExpand=GON)
		iscot=gmcreatetextentry(itextpanel,5,kmax+kfit+3-i,4,1,thetasc(i),60,&
		gdisplay,gmtextcol=1,gmHpos=Gleft, gmVpos=Gbottom, gmExpand=GON)
		if(jfix(i).eq.1) then
			iscot=gmcreatetextentry(itextpanel,9,kmax+kfit+3-i,4,1,'fixed',60,&
		gdisplay,gmtextcol=1,gmHpos=Gleft, gmVpos=Gbottom, gmExpand=GON)
		else
		iscot=gmcreatetextentry(itextpanel,9,kmax+kfit+3-i,4,1,'sd='//sdasc(i),60,&
		gdisplay,gmtextcol=1,gmHpos=Gleft, gmVpos=Gbottom, gmExpand=GON)
		endif
	enddo
	
	istit1=gmcreatetextentry(itextpanel,1,kfit+1,iwid-2,1,wtext(kmax+2),60,&
	display=gdisplay,gmtextcol=1,gmHpos=Gleft, gmVpos=Gbottom, gmExpand=GON)

	
	do i=1,kfit
	     iscot1=gmcreatetextentry(itextpanel,1,kfit+1-i,iwid-2,1,wtext(kmax+2+i),60,&
		 gdisplay,gmtextcol=1,gmHpos=Gleft, gmVpos=Gbottom, gmExpand=GON)

	enddo
	call gmdrawwindow(itextable)	  
99  continue	



20	format(/,' Initial guesses: SSD = ',g12.5,/,&
       10g10.3,/,10g10.3,/,10g10.3,/,10g10.3,/,10g10.3,/)
45	   format(' Number of free parameters = ',i3,/,&
     	' Number of different x values = ',i4)
46	      format(' Not enough points for fit to set ',i3,/,&
     	' Number of free parameters = ',i3,/,&
     	' Number of different x values = ',i4)
5234	format(/,' Number of points fitted = ',i8,/,&
      ' Number of parameters estimated = ',i8,/,&
      ' Minimum SSD (Smin) = ',g13.6,/,&
      ' Error S.D.= sqrt(Smin/df) = ',g13.6,&
     '  (',i5,' degrees of freedom)',/)
end
