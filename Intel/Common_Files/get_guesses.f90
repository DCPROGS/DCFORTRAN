subroutine get_guesses(Main,ITOGGLE,itogglepanel,iopt,Xobs,Yobs,nj,juse,niobs,njset,&
          theta,ndth,thetgues,nset1,ybarp,ndimc,&
          setx,jfix,nfix,sepmax,guesdone,noguess,kfix,&
		  nodata,sameq,sep,jcset,mset,ikset,iflag,icall,&
		  xcal,ycal,ncal,ndc1,ncalc,idat,pdata,intoggle,itogbutton,&
		  autosep,hillang,titlep,idev,iuse,thetsav)
	
	
	use menu_f90
    real*8 stpfac,confac,resfac,errfac,delmin,smin
    logical logyfit, norm, guesdone, logx, xgt0 
	real Xcal(ndc1,ndimc),Ycal(ndc1,ndimc)
	integer ncal(ndimc)
	real xobs(NIOBS,NJSET),yobs(NIOBS,NJSET),w(NIOBS,NJSET)
	integer ikset(20),nj(NJSET),jmiss(njset),juse(njset),ndat(njset)
	logical noguess(100),sep,sameq,discprt,nodata,norm1,use1,printed,shortprt
	LOGICAL CONSTR,FLINE,SETVAR,defolt1,fitted,dcurve,logt,badset,fixset,sepmax
	logical hillang,allmode,autosep
	real setx(njset)
	integer njbase(njset),icurvd(njset),icurvw(njset),icurvc(njset),jfix(njset)
	integer ijoin(njset),icuse(njset),iuse(100)
	character*20 TITLEP(100),ptitle(100)	
	character*10 t1(100),titlep1(100)
	CHARACTER*22 CNUM(10),CNUM1
	character*80 mtext(10),param(100)
	allocatable::ymaxsep
	real ymaxsep(:),pdata(100),theta(100)
	real thetsav(100),thetgues(100,nset)
	integer isline(ndimc),intoggle(100),idat(100)
	integer :: textButton

	common/logval/logx,logy,sqrty,logity
	common/limit/wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
	xmin,xmax,ymin,ymax
	COMMON/BLOCK1/constr,nset,nfit,nsfit,&
     Xv,kmax,ncomp,nmod,fline,nomit,jomit(20),jset,ifitmode
	COMMON/BLOCK2/castar,X1,X2,iequiv,ip1,ip2
	COMMON/BLOCK3/logyfit,norm,xnorm		!CVIN2,YCALCV,CVDISP,GETEQN
	common/potrat/jfirst,iset,kmax1	
		common/dp/discprt

!	allmode=.false.
	itcall=6091
	call guimsg(' ','Y increasing with X ?',1,3,istatus)
	if(istatus.eq.6) then
		 iequiv=0
    else if(istatus.eq.3) then
		 iequiv=1
	endif 
	if(jset.eq.0) jset=1  
	if(nset.le.1) then
		jset=nset
		nsfit=nset
    endif
	if(nmod.eq.1.or.nmod.eq.-1) then
	   
		if(ncomp.lt.1) ncomp=1
		if(iopt.lt.4) then
	     ptitle(1)='Y(0) ' 			    !same for all
	     if(iequiv.eq.1) ptitle(1)='Y(inf) '  !same for all
	     lmax=1
	     do i=1,ncomp
		 call INTCONV(i,cnum1)
		 n1=nblank1(cnum1)
	       lmax=lmax+1
		 ptitle(lmax)='Ymax'//cnum1(1:2)
		 lmax=lmax+1
		 ptitle(lmax)='k'//cnum1(1:2)
	     enddo
	    endif
		if(iopt.eq.1) then
		if(discprt) write(7,735)
			ip1=-1
			titlep(1)='Y(0) ' 			    !same for all
			if(iequiv.eq.1) titlep(1)='Y(inf) '  !same for all
			k=1
			do j=1,nsfit
				j1=juse(j)		!set number used
				call INTCONV(j1,cnum(1))
				n=nblank1(cnum(1))
				do i=1,ncomp
					call INTCONV(i,cnum1)
					n1=nblank1(cnum1)
					k=k+1
					titlep(k)='K'//cnum1(1:2)//'(set '//cnum(1)(1:2)//')'
					icuse(k)=iuse(j1)		!curve # for set j1
					k=k+1
					titlep(k)='Ymax'//cnum1(1:2)//'(set '//cnum(1)(1:2)//')'
					icuse(k)=iuse(j1)		!curve # for set j1
				enddo
			enddo
		
			goto 400
	     else if(iopt.eq.2) then
			if(discprt) write(7,35)
			ip1=0
			titlep(1)='Y(0) ' 			    !same for all
			if(iequiv.eq.1) titlep(1)='Y(inf) '  !same for all
			k=1
			do i=1,ncomp	!ymax(i) for component #i (same for all sets)
				call INTCONV(i,cnum(1))
				n=nblank1(cnum(1))
				k=k+1
				titlep(k)='Ymax'//cnum(1)(1:2)//' '	!='Ymax1','Ymax2',...
			enddo
			do j=1,nsfit
				j1=juse(j)		!set number used
				call INTCONV(j1,cnum(1))
				n=nblank1(cnum(1))
				do i=1,ncomp
					call INTCONV(i,cnum1)
					n1=nblank1(cnum1)
					k=k+1
					titlep(k)='K'//cnum1(1:2)//'(set '//cnum(1)(1:2)//')'
					icuse(k)=iuse(j1)		!curve # for set j1
				enddo
			enddo
			
			goto 400
	     else if(iopt.eq.3) then
			if(discprt) write(7,36)
			ip1=1
			titlep(1)='Y(0) '  !same for all
			if(iequiv.eq.1) titlep(1)='Y(inf) '  !same for all
			k=1
			do j=1,nsfit
				j1=juse(j)		!set number used
				call INTCONV(j1,cnum(1))
				n=nblank1(cnum(1))
				do i=1,ncomp
					call INTCONV(i,cnum1)
					n1=nblank1(cnum1)
					k=k+1
					titlep(k)='Ymax'//cnum1(1:2)//' (set '//cnum(1)(1:2)//')'
					icuse(k)=iuse(j1)		!curve # for set j1
				enddo
			enddo
			do i=1,ncomp	!K(i) for component #i (same for all sets)
				call INTCONV(i,cnum1)
				n1=nblank1(cnum1)
				k=k+1
				titlep(k)='K'//cnum1(1:2)//' '	!='K1','K2',...
			enddo
			
			goto 400
	     else if(iopt.eq.4) then
			if(ifitmode.eq.3) then
				if(ncomp.eq.1) then	!common K, sep max by fitting log(y)
				if(discprt) write(7,37)
					ip1=1
					titlep(1)='Y(0) '  !same for all
					if(iequiv.eq.1) titlep(1)='Y(inf) '  !same for all
					logyfit=.true.
					titlep(1)='K1 '
					k=1
					
					goto 400
				else if(ncomp.eq.2) then
					mtext(1)='Estimate common fraction of component'
		   
				endif
				goto 99
			
			endif
	     endif
	   endif
! end of nmod=1
400	continue

	if(nmod.eq.99) then
		if(ip2.eq.-1.or.ip2.eq.-2) then
	         kmax=1
		   kmax1=kmax
	         titlep(1)='KB '
		else
		   titlep(1)='Y(0) '  !same for all
		   if(iequiv.eq.1) titlep(1)='Y(inf) '  !same for all
		   ptitle(1)=titlep(1)
		   lmax=1
		   ind=jset
		   titlep(2)='a1 '  !same for all
		   ptitle(2)=titlep(2)
		   if(ifitmode.eq.3) then
		    do l=2,nset
			  call intconv(l,cnum(1))
			  titlep(l+1)='a1 (set '//cnum(1)(1:2)//')'
		    enddo
		    kmax=1+2*nset
		   endif
		   kmax1=kmax
		endif
	 endif
	 !-------------------------------------
	 if(nmod.eq.1.or.nmod.eq.-1) then
	     if(iopt.eq.4.and.ifitmode.ne.3) then	!for ifitmode=1, 2: no common params
			if(discprt) write(7,39)
			if(nmod.eq.1) then
				nmod=26	!Langmuir
			else
				nmod=27	!Hill
			endif
			titlep(1)='Y(0) '  !same for all
			if(iequiv.eq.1) titlep(1)='Y(inf) '  !same for all
			ptitle(1)=titlep(1)
			lmax=1
			ind=jset
			if(ifitmode.eq.2) then
				ind=ikset(1)
				ind=jcset
				if(ind.lt.1) ind=1
			endif
			call INTCONV(ind,cnum(1))
			n=nblank1(cnum(1))
			k=1
			do i=1,ncomp
				call INTCONV(i,cnum1)
				n1=nblank1(cnum1)
				k=k+1
				titlep(k)='Ymax'//cnum1(1:2)//'(set '//cnum(1)(1:2)//')'
				icuse(k)=iuse(jset)		!curve # for set jset
				k=k+1
				titlep(k)='K'//cnum1(1:2)//'(set '//cnum(1)(1:2)//')'
				icuse(k)=iuse(jset)		!curve # for set jset
				if(nmod.eq.27) then
					k=k+1
					titlep(k)='nH'//cnum1(1:2)//'(set '//cnum(1)(1:2)//')'
					icuse(k)=iuse(jset)		!curve # for set jset
				 endif
			enddo
			do i=1,ncomp
				lmax=lmax+1
				ptitle(lmax)='Ymax '!1 Ymax for each set
				lmax=lmax+1
				ptitle(lmax)='K'
				if(nmod.eq.27) then
					lmax=lmax+1
					ptitle(lmax)='nH'
				endif
			enddo
			if(fline) then
				k=k+1
				lmax=lmax+1
				titlep(k)='slope (set '//cnum(1)(1:2)//')'
				ptitle(lmax)='slope'
			icuse(k)=iuse(jset)		!curve # for set jset
			endif
			kmax=k
			goto 316
		 else
			if(nmod.eq.1) then
				nmod=26	!Langmuir
				kmax=k
			else if(nmod.eq.-1) then
				nmod=27	!Hill
				if(ip2.eq.1) then		!common nH (1 for each component)     		!separate nH
					do i=1,ncomp	!ymax(i) for component #i (same for all sets)
						call INTCONV(i,cnum(1))
						n=nblank1(cnum(1))
						k=k+1
						titlep(k)='nH'//cnum(1)(1:2)//' '	!='nH1','nH2',...
					enddo
				else				!sep nH for each set
					do j=1,nsfit
						j1=juse(j)		!set number used
						call INTCONV(j1,cnum(1))
						n=nblank1(cnum(1))
						do i=1,ncomp
							call INTCONV(i,cnum1)
							n1=nblank1(cnum1)
							k=k+1
							titlep(k)='nH'//cnum1(1:2)//' (set '//cnum(1)(1:2)//')'
							icuse(k)=iuse(j1)		!curve # for set j1
						enddo
					enddo
				endif
				kmax=k
			endif
		endif
		if(ifitmode.eq.3.and.ncomp.eq.2.and.ip1.eq.2) then
				kmax=kmax+1
				titlep(kmax)='fraction of comp 1 '
			else if(ifitmode.eq.3.and.ncomp.eq.2.and.ip1.eq.3) then
				kmax=kmax+1
				titlep(kmax)='fraction of comp 2 '
			endif
			if(fline) then
				k=kmax	!add more to kmax!
				! sort out titlep !!!!!!!!!!!!!!!!!!!!!!
				do j=1,nsfit
					j1=juse(j)		!set number used
					call INTCONV(j1,cnum(1))
					n=nblank1(cnum(1))
					do i=1,ncomp
						call INTCONV(i,cnum1)
						n1=nblank1(cnum1)
						k=k+1
						titlep(k)='slope'//cnum1(1:2)//' (set '//cnum(1)(1:2)//')'
						icuse(k)=iuse(j1)		!curve # for set j1
					enddo
				enddo
				kmax=k	!new final value
			endif
	    endif !end of nmod=1 
! ------------------------------------------------------------------
	
316 continue     
	nset1=nset	!copy for call
	errfac=1.e-4
	delmin=-1.		!do not use delmin for convergence
	stpfac=0.1
	skipq=.false.
	if(ifitmode.eq.2) then
		if(mset.gt.1.or.iflag.eq.2) goto 1	!keep jfix() after mode 4,5
	endif   
	if(.not.fixset) then 				!keep jfix(); already set
			nfix=0		!no param fixed
			do i=1,njset
				jfix(i)=0
			enddo
			fixset=.false.
	endif
!------------------------------------------------
1	negpar=.false.
	  
	if((iabs(nmod).eq.1.or.(nmod.eq.26.and.(.not.fline))&
        .or.(nmod.eq.27.and.(.not.fline)).or.nmod.eq.28.or.(nmod.eq.99.and.ip1.eq.1))&
        .and.iflag.eq.0.and.ncomp.eq.1.and.(.not.(nodata.or.dcurve))) then
		 allmode=.true.
	     if(allocated(ymaxsep)) deallocate(ymaxsep)
	     ALLOCATE(ymaxsep(nset))	!for guesses for maxima (nmod=27)
	     call GETGUESS(nmod,Xobs,Yobs,nj,juse,niobs,njset,nsfit,&
          theta,ndth,thetgues,nset1,kmax,ybarp,ncomp,ifitmode,jset,&
          setx,jfix,nfix,logyfit,norm,sepmax,ymaxsep,guesdone,noguess,kfix)
		    iset=0
			
			do m=1,nsfit
				jval=juse(m)
				iset=iset+1	!for common -needed for ifitmode=4,5 if sets omitted
				jset=jval		!for COMMON
				if(ifitmode.le.2) then
				    indsav=kmax*(jset-1)
					do i=1,kmax
						theta(i)=thetgues(i,jset)
						thetsav(i+indsav)=theta(i)
					enddo
					if(m.eq.jcset) then
				   		noguess(m)=.false.
						isline(m)=10
					else
						noguess(m)=.true.
						if(isline(m).ne.10) isline(m)=-1
					endif
				endif
		
				if(.not.noguess(jval)) then
					call DEFYcal(xmin,xmax,ncalc,logx,theta,Xcal,Ycal,&
     				jval,xgt0,setx,njset,ndimc,ndc1)
				endif
			enddo

			call DRAWGUES(ncalc,y0,yinf,&
     		Xcal,Ycal,nsfit,juse,noguess,njset,ndc1,ndimc,38,idev,jcset,ifitmode)
			 if(ifitmode.le.2) then
		jset=juse(1)
		do i=1,kmax
		   theta(i)=thetgues(i,jset)	!params for current set
		enddo
	   endif
     		mtext(1)='Use these orange curves as initial guess for fit'
			mtext(2)='Try a different equation'
			istatus=gmDisplayMessageBox(' ', mtext(1),Gquestion,Gyesno)
			
			if(istatus.eq.6) then !yes
				autosep=.true.
			call DRAWGUES(ncalc,y0,yinf,&
     		Xcal,Ycal,nsfit,juse,noguess,njset,ndc1,ndimc,ibk,idev,jcset,ifitmode)	!delete
				if(ifitmode.eq.3.and.nmod.eq.27.and.ip1.eq.1.and.sepmax) then
					mtext(1)='Maxima are very different-suggest weighting sets'
					istatus=gmDisplayMessageBox(' ', mtext(1),Gquestion,Gyesno)
					
					if(istatus.eq.6) then !yes
						do m=1,nsfit
						j=juse(m)
						ym=ymaxsep(j)
						do i=1,nj(j)
							w(i,j)=w(i,j)/ym**2
						enddo
						enddo
					endif
      				if(discprt) write(7,110)
		
				endif
				
			else
				call DRAWGUES(ncalc,y0,yinf,&
     			Xcal,Ycal,nsfit,juse,noguess,njset,ndc1,ndimc,ibk,idev,jcset,ifitmode)	!delete
				goto 99
			endif
	     
	endif	
		

	if(allmode) then
	     if(allocated(ymaxsep)) DEALLOCATE(ymaxsep)
	     if(autosep) then
			
			if(nmod.eq.-1.or.nmod.eq.1.or.nmod.eq.26.or.nmod.eq.27) then
				
				nrow1=kmax+1
				do j=1,kmax
					pdata(j)=theta(j)
					ptitle(j)=titlep(j)
				enddo
				if(ifitmode.eq.2) then
				indsav=kmax*(jcset-1)
				do i=1,kmax
					pdata(i)=thetsav(i+indsav)
					
					!	theta(i)=thetgues(i,jcset)	!params for current set
				enddo
				
				endif
			else
				
				if(nmod.eq.99.and.ip2.lt.0) kmax=1
				do j=1,kmax
					pdata(j)=theta(j)
					ptitle(j)=titlep(j)
				enddo
				nrow1=kmax+1
			endif
			ntog=kmax
			if(nfix.eq.1) itcall=6200
			call toggle_panel(main,ITOGGLE,itogglepanel,ntog,ptitle,intoggle,itcall,pdata,idat,itogbutton)
			
			goto 99
		 else
			fixset=.false.
	     endif	!end of section for initial guesses
	endif
	hillang=nmod.le.4.or.nmod.eq.26.or.nmod.eq.27
	if(hillang.or.nmod.eq.28.or.nmod.eq.99) then
		fixmax=hillang.and.jfix(2).eq.1   !Ymax fixed for Lang/Hill
		fix0=hillang.and.jfix(1).eq.1		!Y(0) fixed for Hill
		iset=1
		do i=1,kmax1
		   theta(i)=thetgues(i,jset)
		enddo
	!	titlep(3)='       K1' 	!remove 'set 1' from title
	else if(nmod.eq.28) then		!power function
		do i=1,kmax1
		   theta(i)=thetgues(i,jset)
		enddo
	else if(nmod.eq.99.and.ip1.eq.1) then		!polynomial -straight line case
		do i=1,2
		   theta(i)=thetgues(i,jset)
		enddo
	endif
	hillang=nmod.eq.-1.or.nmod.eq.1.or.nmod.eq.26.or.nmod.eq.27 
	if(nmod.le.4.or.hillang.or.nmod.eq.99.or.&
        nmod.eq.23.or.nmod.eq.24.or.nmod.eq.25.or.nmod.eq.28) then
	    j=jset
!		if(ifitmode.eq.2) j=jcset
	    if(autosep.and.(.not.noguess(j))) then
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
		icall=6090
		goto 99	!and skip questions
	    endif
	endif
	mtext(1)='Specify initial guesses:'
	dcurve=.false.
	if(nodata) then
	      mtext(1)='Specify parameter values:'
	      do i=1,ndth
		   jfix(i)=1 	!for printout in cvout1
	      enddo
		
	else
      	mtext(1)='Use parameters to plot curve without fitting'
	
	endif
	istatus=gmDisplayMessageBox(' ', mtext(1),Gquestion,Gyesno)
				
	
	if(istatus.eq.6)then !yes
		fitted=.false.
		dcurve=.true.
		mtext(1)='Specify parameter values:'
	else
		mtext(1)='Specify initial guesses:'
	endif
	do i=1,kmax
	     if(ifitmode.eq.2) then
	        do m=1,18
	         if(titlep(i)(m:m+2).eq.'set') then
				call INTCONV(jset,cnum(1))
				n=nblank1(cnum(1))
				n2=m+3+n
				if(n2.gt.20) n2=20
				titlep(i)(m:n2)='set '//cnum(1)(1:n)
				goto 2
	         endif
	        enddo
	     endif
2	 continue
	     i1=i
	     if(NMOD.eq.7.and.i.gt.3) i1=3

	     if(icuse(i).ne.0) then
	        call INTCONV(icuse(i),cnum1)
	     endif
		 nc=nblank1(cnum1)
		 nt=nblank1(titlep)
	     if(icuse(i).eq.0) then
	         param(i)='Param('//cnum1(1:nc)//'): '//titlep(i1)(1:nt)
	     else
	         param(i)='Param('//cnum1(1:nc)//'): '//titlep(i1)(1:nt)//&
     ' (curve '//cnum1(1:nc)//')'
	     endif
	enddo
	if(ifitmode.eq.3.and.ip1.eq.2.and.i.eq.kmax) then  !common K, sep Ymax for each set
                mtext(1)='Fraction of component 1 must be between 0 and 1'
	endif
	
	do i=1,kmax
			pdata(i)=0.0
			ptitle(i)=titlep(i)
    enddo
	if(nmod.eq.99.and.ip2.lt.0) kmax=1
	nrow1=kmax+1
	ntog=kmax
	call toggle_panel(main,ITOGGLE,itogglepanel,ntog,ptitle,intoggle,itcall,pdata,idat,itogbutton)

35    format(' Common maximum, separate K values for each set',/)
36	format(' Common K value, separate maxima for each set',/)
37    format(' Common K value, separate maxima for each set',/,&
     ' Fit to log(y) values with equal weight for log values',/)
39	format(30(2x,i3))
735	format(' Separate maximum, separate K values for each set',/)

	
	
110   format(' Each set weighted with guessed Ymax to',/,&
     	     ' compensate for different maxima',/)
	
99 end
