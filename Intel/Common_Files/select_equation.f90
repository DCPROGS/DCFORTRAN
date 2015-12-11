	subroutine select_equation(main,ITOGGLE,ittoglepanel,intoggle,itogbutton,imod,nmod,norm,&
	titlep,ptitle,iuse,icuse,kmax,&
	ncomp,ip1,ip2,logyfit,fline,nodata,icall,njset,ifitmode,nsfit,iw,nset,&
	mset,t1,titmod,titw,juse,jmiss)
	
	use menu_f90

	integer intoggle(100),idat(100),textbutton
	logical logyfit,fline,discprt,norm,nodata
	
	real valdat(100)
	character*60 text,mtext(100),title
	integer jmiss(njset),juse(njset),iuse(100),icuse(100)
	character*20 TITLEP(100),ptitle(100)	
	character*10 t1(100),titlep1(100)
	CHARACTER*22 CNUM(10),CNUM1
	character*60 titw(5),titmod(40)
	common/dp/discprt
	if(ifitmode.eq.2) then
		  do m=1,kmax
		   if(icuse(m).ne.0) icuse(m)=mset
		  enddo
	endif
	
	nmod=0
	
	   

		if(nodata) then
		  if(discprt) write(7,131) titmod(imod)
		endif

	   logyfit=.false.
	   fline=.false.
	   if(imod.eq.1) then
	     nmod=99			!polynomial
	   else if(imod.eq.2) then
	     nmod=1				!Langmuir hyperbola
	     fline=.false.
	   else if(imod.eq.3) then
	     nmod=-1			!Hill eqn
	     fline=.false.
	   else if(imod.eq.4) then
	     nmod=1	!later 26         !Langmuir+line
	     fline=.true.
	   else if(imod.eq.5) then
	     nmod=-1	!later 27   !Hill +line
	     fline=.true.
	   else if(imod.eq.6) then
	     nmod=28	!power function  !power
	   ELSE IF(IMOD.EQ.7) THEN
	     nmod=5                      !binding inhibitation
	   else if(imod.eq.8) then       !old imod=8,9; nmod=6,7; exponential onset
	     nmod=6
		 nmod1=7                      !expon
	   ELSE IF(IMOD.EQ.9) THEN
	     nmod=19                      !expon
	   ELSE IF(IMOD.EQ.10) THEN
	     nmod=20  				!geometrics
	   ELSE IF(IMOD.EQ.11) THEN
	     				!Popen
	   ELSE IF(IMOD.EQ.12) THEN
	     nmod=22
	   else if(imod.ge.21.and.imod.le.24) then
	     nmod=imod-13
	   else if(imod.ge.25.and.imod.le.27) then
	     nmod=imod-12
	   else if(imod.eq.28) then
	     nmod=30      !n bindings +opening (nmod=30)
	   else if(imod.eq.29) then
	     nmod=31      !MWC(n) (nmod=31)
	   ELSE IF(IMOD.EQ.31) THEN
	     nmod=12
	   else IF(imod.ge.32.and.imod.le.34) then
	     nmod=imod-16
	   ELSE IF(IMOD.EQ.35) THEN
	     nmod=21
	   endif
	   
	   if(imod.eq.40) then
	     nmod=100
	     kmax=3
	     titlep(1)='a    '
	     titlep(2)='b    '
	     titlep(3)='c    '

	   endif
		if(norm.and.(nmod.ne.-1.and.nmod.ne.1.and.nmod.ne.28)) then
	     mtext(1)= 'This option is available only for (a) Langmuir/Hill with one'//&
     'component per curve, or power function fit at present:-please restart CVFIT'
	 istatus=gmDisplayMessageBox(' ', mtext(1),GSTOP,GOK)
	goto 99
	endif
	if(nmod.eq.1.or.nmod.eq.-1.or.nmod.eq.19.or.nmod.eq.20) then
	 if((nmod.eq.1.or.nmod.eq.-1).and.norm) then
	      ncomp=1
		if(nmod.eq.1) then
		   nmod=26	!Langmuir
		else
		   nmod=27	!Hill
		endif
		titlep(1)='Y(0) '  !same for all
		ptitle(1)='Y(0) '  !same for all
		k=1
		do j=1,nsfit
		   j1=juse(j)		!set number used
		   call INTCONV(j1,cnum1)
		   n=nblank1(cnum1)
		   k=k+1
		   titlep(k)='K (set '//cnum1(1:n)//')'
		   icuse(k)=iuse(1)
		   if(nmod.eq.27) then
			k=k+1
			titlep(k)='nH (set '//cnum1(1:n)//')'
			icuse(k)=iuse(1)
		   endif
		enddo
		ptitle(2)='K  '
		if(nmod.eq.27) ptitle(3)='nH'
		lmax=3
		kmax=k
		goto 400
	 endif
	 if(nmod.eq.19) then
	       mtext(1)='Number of exponential components to be fitted:'
		   mtext(2)='Not available for this version'
	  istatus=gmDisplayMessageBox(' ', mtext(2),GSTOP,GOK)
        
	 goto 99
	 else if(nmod.eq.20) then
	       mtext(1)='Number of geometrical components to be fitted:'
		   mtext(2)='Not available for this version'
	  istatus=gmDisplayMessageBox(' ', mtext(2),GSTOP,GOK)
        
	 goto 99
	 else
	       mtext(1)='Number of components for each set:'
		   goto 400
		   ! pe urma componenti
	 endif
	  	   
	else if(nmod.eq.28) then
	     if(norm) then
			kmax=1
			titlep(1)='n (=log-log slope)'
	     else
			kmax=2		!no of parameters
			titlep(1)='x0 (=x at y=ybar) '
			titlep(2)='n (=log-log slope)'
	     endif
	     goto 400
	    
	else
	     ip1=1
	     ip2=1
	     if(iw.ge.1.and.iw.le.5) then
	        if(discprt) write(7,1301) titmod(imod),titw(iw)
	        titprint=.true.
	     endif
	     if(nmod.eq.23.or.nmod.eq.24) then
	        if(discprt) write(7,1302)
	     endif
		 
	endif
	if(nmod.eq.21) then
	      kmax=7
		titlep(1)=t1(29)
		titlep(2)=t1(30)
		titlep(3)=t1(31)
		titlep(4)=t1(1)
		titlep(5)=t1(3)
		titlep(6)=t1(32)
		titlep(7)=t1(33)
		 mtext(3)='Not available for this version'
	      istatus=gmDisplayMessageBox(' ', mtext(3),GSTOP,GOK)
		goto 400
	endif
	if(nmod.eq.22) then
	      kmax=2
	      titlep(1)=t1(28)  !tau
	      titlep(2)=t1(24)  !H (mV)
		   mtext(3)='Not available for this version'
	      istatus=gmDisplayMessageBox(' ', mtext(3),GSTOP,GOK)
		  goto 400
	endif
	if(nmod.eq.16.or.nmod.eq.17.or.nmod.eq.18) then
		kmax=3
		titlep(1)=t1(22)
		titlep(2)=t1(23)
		titlep(3)=t1(24)
		if(nmod.Ne.16) THEN
		kmax=5		!nmod=17
		titlep(2)=t1(26)
		titlep(4)=t1(27)
		titlep(5)=t1(25)
		if(nmod.eq.18) titlep(5)=t1(11)
		ENDIF
		 mtext(3)='Not available for this version'
	      istatus=gmDisplayMessageBox(' ', mtext(3),GSTOP,GOK)
		goto 400
	endif
	if(nmod.eq.13) then
	     ip1=0
	     ip2=0
	     titlep(1)=t1(11)
	     titlep(2)=t1(1)
	     kmax=2
           Mtext(1)=' Same affinity for both subunits [Y] ? '
	     Mtext(2)=' Same beta/alpha for both subunits [Y] ? '
		  mtext(3)='Not available for this version'
	      istatus=gmDisplayMessageBox(' ', mtext(3),GSTOP,GOK)
         goto 99
	endif
	if(nmod.eq.14.or.nmod.eq.15) then
	     kmax=5
	     titlep(1)=t1(1)
	     titlep(2)=t1(20)
	     titlep(3)=t1(21)
	     if(nmod.eq.14) titlep(4)=t1(11)
	     if(nmod.eq.15) titlep(4)=t1(12)
	     titlep(5)=t1(13)
	     if(nmod.eq.14) then
		 icall=400
           endif
	     x2=-1.		!k+2 not to be estimated
	     mtext(1)=' Mean burst length (ms)= '
           mtext(2)=' k(+2) ( = -1 as a parameter to be estimated) = '
		   mtext(3)='Not available for this version'
	      istatus=gmDisplayMessageBox(' ', mtext(3),GSTOP,GOK)
         goto 99
           
        
	endif
	if(nmod.eq.5) then	!binding inhibition -1 langmuir only now
	     ncomp=1		!only possibility
	     titlep(1)='Y(inf)   '
	     titlep(2)='Y(0)     '
	     titlep(3)='KB       '
	     castar=1.
           mtext(1)='Normalized conc of labelled ligand ='
	     kmax=3
		 mtext(3)='Not available for this version'
	      istatus=gmDisplayMessageBox(' ', mtext(3),GSTOP,GOK)
         goto 99
             
	endif
	if(nmod.le.4) then
	     kmax=nmod+1		!for nmod=1-4
	     ncomp=kmax/2	!for nmod=1-4
	     if(nmod.eq.-1) then
	        ncomp=1
	        kmax=4		!Hill equation
	     endif
	     FLINE=.false.
	     if(nmod.eq.2.or.nmod.eq.4) fline=.true.	!for nmod=2,4 ie imod=4,6
	     titlep(1)=t1(1)
	     titlep(2)=t1(2)
	     titlep(3)=t1(5)
	     IF(NMOD.Eq.-1) then
	     titlep(3)=t1(14)
	     titlep(4)=t1(15)
		 endif
	     IF(NCOMP.Eq.2) then
	       titlep(3)=t1(3)
	       titlep(4)=t1(4)
	       titlep(5)=t1(5)
	     endif
	     goto 400
	    
	endif
	if(nmod.eq.6.or.nmod.eq.7) then
	     KMAX=3
	     IF(NMOD.EQ.7) KMAX=2+NSET	!K1,K2 + NSET VALUES OF BMAX
	     titlep(1)=t1(8)
	     titlep(2)=t1(9)
	     titlep(3)=t1(7)
		  mtext(3)='Not available for this version'
	      istatus=gmDisplayMessageBox(' ', mtext(3),GSTOP,GOK)
	     goto 400
	endif
	if(nmod.eq.10.or.nmod.eq.11) then
           Mtext(1)='Parameters:'
	     Mtext(2)='Fit for non-equiv subunits (no cooperativity)'
		 mtext(3)='Not available for this version'
	      istatus=gmDisplayMessageBox(' ', mtext(3),GSTOP,GOK)
         goto 99
           
	endif
	if(nmod.eq.8.or.nmod.eq.9.or.nmod.eq.12) then
	     KMAX=3
	     titlep(1)=t1(11)
	     IF(NMOD.EQ.9) titlep(1)=t1(12)
	     titlep(2)=t1(1)
	     titlep(3)=t1(13)
	     if(nmod.eq.12) titlep(4)=t1(2)
	     if(nmod.eq.12) KMAX=4
	     if(nmod.eq.8.or.nmod.eq.12) then
		  mtext(3)='Not available for this version'
	      istatus=gmDisplayMessageBox(' ', mtext(3),GSTOP,GOK)
		 goto 400
		 
		 else	
	     MText(1)='Mean burst length (ms)= '
           Mtext(2)='k(+2) ( = -1 as a parameter to be estimated) = '
	     Mtext(3)='Use K2 (Y), [or k(-2), (N)] as parameter [Y] ? '
		mtext(4)='Not available for this version'
	      istatus=gmDisplayMessageBox(' ', mtext(4),GSTOP,GOK)
         goto 99
           endif
	   endif
	if(nmod.eq.99) then		!not polynomial
		kmax=2
		kmax1=kmax
		if (ifitmode.eq.3) then
		    kmax=1+nset
		    goto 400
        else
			call guimsg(' ',&
           'Fit Schild equation, i.e. parameter=KB (slope=1/KB)',&
            1,3,istatus)
			if(istatus.eq.6) then
				kmax=1
				kmax1=1
     			call guimsg(' ','Are the Y values = dose ratio-1',1,3,istatus)
				if(istatus.eq.6) then
					ip2=-2
				else
     	            call guimsg(' ','Are the Y values = dose ratio',&
     				1,3,istatus)
					if(istatus.eq.6) then
						ip2=-1
					else
						kmax=2
						kmax1=kmax
     					call guimsg(' ','Invalid Schild data',3,0,istatus)
					goto 400
                    
					endif
				endif
				goto 400
             
			else
				goto 400
               
			endif
		endif 
	endif
	
400	continue


	
	ncomp=1
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
		icall=6089
	endif

	if(nmod.eq.1.or.nmod.eq.-1) then
	!	imess=gmdisplaymessagebox('','No components for each set: YES=2, NO=1',gquestion,gyesno)
		ncomp=1
	!	if(imess.eq.gyesbutton) ncomp=2
		if(iw.ge.1.and.iw.le.5) then
	      if(discprt) write(7,1301) titmod(imod),titw(iw)
		  titprint=.true.
	    endif
	    if(ifitmode.eq.4.or.ifitmode.eq.5) then
			if(nmod.eq.1) then
				nmod=26	!Langmuirelse
			
				nmod=27	!Hill
			endif
			titlep(1)='Y(0) '  !same for all
			call INTCONV(jset,cnum(1))
			n=nblank1(cnum(1))
			ptitle(1)='Y(0) '  !same for all
			lmax=1
			k=1
 		    !for ifitmode=4,5 Ymax and nH same for all sets
			do i=1,ncomp
				call INTCONV(i,cnum1)
				n1=nblank1(cnum1)
				k=k+1
				titlep(k)='Ymax'//cnum1(1:2)//' (set '//cnum(1)(1:2)//')'
				lmax=lmax+1
				ptitle(lmax)='Ymax'//cnum1(1:2)
				icuse(k)=iuse(1)
				lmax=lmax+1
				k=k+1
				titlep(k)='K'//cnum1(1:2)//' (set '//cnum(1)(1:2)//')'
				ptitle(lmax)='K'//cnum1(1:2)//' (set '//cnum(1)(1:2)//')'
				icuse(k)=iuse(1)
				if(nmod.eq.27) then
					k=k+1
					titlep(k)='nH'//cnum1(1:2)//'(set '//cnum(1)(1:2)//')'
					lmax=lmax+1
					ptitle(lmax)='nH'//cnum1(1:2)//'(set '//cnum(1)(1:2)//')'
				endif
			enddo
			kmax=k
	        iequiv=0		!increasing
			iopt=4
			NVAL=1
			icall=6089
		 else if(ifitmode.eq.1.or.ifitmode.eq.2.or.ifitmode.eq.3) then
			if(ifitmode.eq.3) then
				title='Fit options'
				mtext(1)='Fit separate maximum, separate K values'
				mtext(2)='Fit common maximum, separate K values'
				mtext(3)='Fit common K value, separate maxima'
				if(ncomp.eq.1.and.(.not.fline)) then
					mtext(4)='ditto using fast fit to log(y)'
					nval=4
				else if(ncomp.eq.2) then
					mtext(4)='Separate K and Ymax, but common % Ymax1'
					nval=4
				else
					nval=3
				endif
				iequiv=0		     		!increasing  

				if(nmod.eq.-1) then
					ip2=0			!separate nH
				endif
				itcall=6085
			
			call toggle_panel(main,ITOGGLE,ittoglepanel,nval,mtext,intoggle,itcall,valdat,idat,itogbutton)
			
			else
		    
				iequiv=0		!increasing
				iopt=4
				NVAL=1
				icall=6089
			endif
		  endif
	
		 endif
!	endif
1301	     format(/,' FITTING:',/,' Equation = ',a60,/,' Weighting = ',&
          a60)
1302	        format(' (common maximum for all sets fitted)')
131	   format(/,' Calculated curves:',/,' Equation = ',a60)


99 end	
	


	