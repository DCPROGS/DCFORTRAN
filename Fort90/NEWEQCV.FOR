	subroutine NEWEQCV(nmod,kmax,kmax1,titlep,nsfit,juse,ifitmode,
     & thetsav,setx)
c To alter equation to be fitted to nmod=23 or 24 in CVFIT2 in case of
c isepfit=2, following simultaneous fit with ifitmode=4 or 5.  Model
c is redefined here, ifitmode changed to 3, and initial guesses, based
c on the simultaneous fit results, put in thetsav(i,1) ready for use
c in CVSIMP (for these models fit done in one pass -only one set of init
c guesses so put in thetsav(i,1))
c In this case nmod=1 (Langmuir) or -1 (Hill) and want to have
c separate K1 ( and for Hill, separate NHill) for each curve but a
c common maximum for all, ie nmod=23,24 (see GETEQN)
c
	real*4 thetsav(20,11),setx(10)
	integer juse(10)
	character*10 TITLEP(20)		!names of params for fit
c
c Put initial guesses into thetsav(i,1) before kmax etc altered
c (order of params as specified when titlep() defined below)
	aka=thetsav(1,11)		!value of K1 from parallel fit
	Ymax=thetsav(2,11)	!value of Ymax from parallel fit
	Y0=thetsav(3,11) 		!Y(0) (Hill only)
	hill=thetsav(4,11) 		!n(Hill) (Hill only)
	do 10 iset=1,nsfit
	 j=juse(iset)
	 ratio=1.0
	 if(ifitmode.eq.4.and.iset.gt.1) then
	   ratio=thetsav(kmax1+iset-1,11)		!from parallel fit
	 else if(ifitmode.eq.5) then
	   akb=thetsav(kmax,11)		!=KB (see GETEQN) -value from parallel fit
	   xB=setx(j)		!antag conc
	   ratio=(1.0 + xB/aKB)	!mult by dose ratio
	 endif
	 if(nmod.eq.1) then
	   thetsav(1,1)=Ymax		!init guess for nmod=23 fit
	   thetsav(iset+1,1)=aka*ratio
	 else if(nmod.eq.-1) then
	   thetsav(1,1)=Ymax		!init guess for nmod=24 fit
	   thetsav(2,1)=Y0		!init guess for nmod=24 fit
	   m1=2*iset+1
	   thetsav(m1,1)=aka*ratio
	   thetsav(m1+1,1)=hill
	 endif
10	continue
c
c Redefine model
	if(nmod.eq.1) then
	   nmod=23		!Langmuir with common maximum
	   kmax=nsfit+1	!K for each, + common max
	   titlep(1)='     Ymax '
	   do 55 m=1,nsfit
		j=juse(m)		!set number used
	      titlep(m+1)='       K'//char(j+48)//' '	!='K3','K5',...
	      if(j.eq.10) titlep(m+1)='      K10 '
55	   continue
	endif
	if(nmod.eq.-1) then
	   nmod=24		!Hill with common maximum
	   kmax=2*nsfit+2	!K,nH for each + common max and min
	   titlep(1)='     Ymax '
	   titlep(2)='     Y(0) '
	   do 56 m=1,nsfit
		j=juse(m)		!set number used
		m1=2*m+1
	      titlep(m1)='       K'//char(j+48)//' '	!='K3','K5',...
	      titlep(m1+1)='      nH'//char(j+48)//' '	!='nH3','nH5',...
	      if(j.ge.10.and.j.le.19) then
	         titlep(m1)='      K1'//char(j-10+48)//' '
	         titlep(m1+1)='     nH1'//char(j-10+48)//' '
		endif
56	   continue
	endif
c
c Redefine ifitmode,kmax1
	ifitmode=3				!change ifitmode if not already 3
	kmax1=kmax
c
	RETURN
	end

