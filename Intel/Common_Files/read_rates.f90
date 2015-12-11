!     Last change:  D    15 Jan 104    2:21 pm
subroutine read_rates(Main,form1,Form1_TxtArray1,irecm0,imod,qmec,nrows,indk,&
ratcons,models,ic,ipbar_Progress2,jgraph,nmr,conc,iformText)

use menu_f90

integer :: Form1_TxtArray1(25,4),form1(25,4),iformText(25,4)
real*4 conc(10,10)
	allocatable text
	integer nlink(100),jfix(200)
	character*10 text(:,:),choices(100,10)
	REAL*8 dgamma(200),theta0(200)
	character*2 charmod(25,40)	!to print model
	character*2 cmodnew(25,40)	!for NEWMOD
	real*4 pstar(4)
	character*8 titles(100)
	logical nopath
	character qpath*30,qname*8,suffix*3,ndev*2,cnum*11,conex*30,xtext*100,cnum1*11
	character rfile*40,qmec*60,qfilmsav*40
	character*10 titlep(200),titlep1(200)
	integer IQ(100,100),imatrix(25,40),IC(2,200),iconc(200)
	integer inew(100)
	real*8 QT(100,100),temp(100),vknd
	integer jstart(500)
	character*20 ligname(10)
	character*10 statname(100),qij(100)
	character*74 rtitle,rtitles(500)
	CHARACTER*80 titlem	!title to describe rate constants
	integer IQQ(100,100)
	integer irate(200),jrate(200)
	logical boundef,chardef,indmod,repeat,samefil,altmod
	logical readp
	logical kmfast,readini,useini,present,renum,alter,newrate
	character*1 ans,UC,ans1,ch1,ch2
	character*74 mtitle	!title for model
	character*74 mtits(100)	!to read all ditto from disc
	integer ijmod(500),iwid(2),iwcell(10)
	character*20 namelink(100)
		logical discprt,automr
	common/dp/discprt
	!Addition for getbound
	integer nbound(100,10)
	common/nbnd/nbound
	logical monot
	common/ec2/monot,nlvar,vhold,vref,cfacd,dgammas 
	character*11 cnum0
	!For 'true' model used for simulation -used in getqd2
	logical autosim,first
	
	logical statdef	!state names defined

	real*8 dgammas(100),cfacd
	character*60 :: qdir='.'
	character*120 :: qfilt='*.mec'//char(124)//'Mechanism Files(MEC)'//&
     	char(124)//'*.dat'//char(124)//'Old Files (DAT)'//&
     	char(124)//'*.*'//char(124)//'All Files'

	integer::Button7(10),RECORDS
	character*10 label7(2)
    common/cpar/ncdep,IX,JX,x
	logical obeymr(50),allmr

	character*74 mtitles	!title for sim model
	character*2 charmods(25,40)	!to print model

	!character*40 mtitle1*40,filnam*32,prtport*4	!for WINPRINT

	PARAMETER  N_STATES=200
	TYPE MODEL
		integer irecm
	    INTEGER N
		integer ka
		integer kb
		integer kstat
		integer kstat0
		integer nsub
		integer kcon
		integer ncon
		integer npar
		logical indmod
		logical chardef
		integer ix
		integer iy
		integer model
		character*80 title_model
		real X(N_STATES)
		real Y(N_STATES)
		INTEGER COLOUR(N_STATES)
		LOGICAL STATUS(N_STATES)
		LOGICAL DRAW(N_STATES)
		CHARACTER*3 NAME(N_STATES)
		character*15 statname(n_states)
		character*10 con_states(20)
		character*10 sub_states(10,20)
		integer link(n_states,n_states)
		integer inter_link(n_states)
		character*40 name_link(n_states)
		character*20 ligname(10)
		integer nlig
		integer nbound(n_states,10)
		integer nchan
		real vref
		real*8 dgamma(n_states)
		integer nwidth
		integer nheight
		character*2 charmod(25,40)
		integer ilast
		integer jlast
		integer ic(2,200)
		integer index(n_states)
	END TYPE MODEL

	TYPE (MODEL) MODELS(25)

	TYPE rate_constant 
		integer irec
		integer imod
		character*74 title
		character*10 titlep(200)
		real*8 value(200)
		character*15 qij(200)
		integer iconc(200)
		character*20 ligant(200)
		integer nsetq
		integer ieq(200)
		integer jeq(200)
		integer ifq(200)
		integer jfq(200)
		real efacq(200)
		real*8 qt(100,100)
		character*20 micro(200)
		integer ncyc
		integer nsc(50)
		integer im(50,50)
		integer jm(50,50)
	END TYPE rate_constant
	
	type(rate_constant) ratcons(500)
	logical dcmodel,fixec50,prtec50
		integer IE(200),JE(200),IF(200),JF(200)
	real EFAC(200)
	real*8 ec50,xqlo,xqhi,dround,ec501
	common/sm/nsmax		!for getqd/charq
	COMMON/EBLK/NEQ,IE,JE,IF,JF,EFAC
	common/ec/fixec50,nmod9,ec501,i50,j50,m50,prtec50,xqlo,xqhi,dcmodel	
	integer KMCON(9)
	integer IV(100),JV(100)
	real HPAR(100)
	common/LIG/nligsav,IL
	integer IX(100),JX(100)
	integer NSC(50),IM(50,100),JM(50,100)	!up to 50 cycles
!	integer IE(200),JE(200),IF(200),JF(200)
!	real EFAC(200)
	integer ieq(200),jeq(200),ifq(200),jfq(200)
	real efacq(200)
	integer IL(100)
	common/mr/obeymr,automr
	integer isetmr(50)
	common/mr1/isetmr
	common/mpar1/NCYC,NSC,IM,JM
	common/ir/irate,jrate
	do i=1,200
		titlep(i)='          '
	enddo
	OPEN(unit=15,file=qmec,status='UNKNOWN',access='DIRECT', form='BINARY', RECL=1)
		read(unit=15,rec=1) iver,nrecs,nextrec,ireclast,jstart
	iver1=0
     	imod=0
		mtitle='     '
		k=0
		kA=0
		kB=0 
		kC=0 
		kD=0
		rtitle='     '
		ilast=0
		jlast=0
		nrateq=0
     	ncon=0
		ncdep=0
		nlig=0
		chardef=.false.
		boundef=.false.
		ncyc=0
		vref=0
		nvdep=0
		kmfast=.false.
     	indmod=.false.
		npar=0
		nsetq=0
		kstat=0
		cnum=titlem(1:3)
		call gDefineRGB(50,1.,0.80,0.8)	!pink
    call gDefineRGB(51,0.8,1.0,0.8)	!green
	call gDefineRGB(48,0.,0.,0.)
	ired=50
	igreen=51
		call chtoint(cnum,imod)
		
	
		
		irecm=irecm0
		
10		irecq=irecm
		
		irec=jstart(irecq)	!start byte for data in record #irecq
		irecsav=irec
		irecqsav=irecq
		read(unit=15,rec=irec) iver1
		if(iver1.eq.200) then
		read(unit=15,rec=irec) iver1,&
     	imod,mtitle,k, kA, kB, kC, kD,rtitle,ilast,jlast,nrateq,&
     	ncon,ncdep,nlig,chardef,boundef,ncyc,vref,nvdep,kmfast,&
     	indmod,npar,nsetq,kstat,((charmod(i,j),i=1,ilast),j=1,jlast),&
     	(irate(i),i=1,nrateq),(jrate(i),i=1,nrateq),&
		(QT(irate(i),jrate(i)),i=1,nrateq),(titlep(i),i=1,npar),&
        (ligname(i),i=1,nlig),((nbound(i,j),i=1,k),j=1,nlig),&
     	(IX(i),i=1,ncdep),(JX(i),i=1,ncdep),(IL(i),i=1,ncdep),&
     	(dgamma(i),i=1,kA),(nsc(i),i=1,ncyc),((im(i,j),j=1,nsc(i)),i=1,ncyc),&
     	((jm(i,j),j=1,nsc(i)),i=1,ncyc),(iv(i),i=1,nvdep),(jv(i),i=1,nvdep),&
     	(hpar(i),i=1,nvdep),(pstar(i),i=1,4),(kmcon(i),i=1,9),&
	    (ieq(i),i=1,nsetq),(jeq(i),i=1,nsetq),(ifq(i),i=1,nsetq),(jfq(i),i=1,nsetq),&
     	(efacq(i),i=1,nsetq),(statname(i),i=1,kstat),&
		nsub,kstat0,npar0,kcon,npar1,ncyc0,&
		((ic(i,j),i=1,2),j=1,ncon),(models(jgraph)%name(i),i=1,kstat),&
		(models(jgraph)%colour(i),i=1,kstat),(models(jgraph)%x(i),i=1,kstat),&
		(models(jgraph)%y(i),i=1,kstat),nxmodel,nymodel
		models(jgraph)%n=k
do mc=1,k
			if(models(jgraph)%colour(mc).eq.12) models(jgraph)%colour(mc)=ired
			if(models(jgraph)%colour(mc).eq.2) models(jgraph)%colour(mc)=igreen
		enddo
		else if(iver.eq.201) then
		
     	read(unit=15,rec=irec) iver1,&
     	imod,mtitle,k, kA, kB, kC, kD,rtitle,ilast,jlast,nrateq,&
     	ncon,ncdep,nlig,chardef,boundef,ncyc,vref,nvdep,kmfast,&
     	indmod,npar,nsetq,kstat,&
     	((charmod(i,j),i=1,ilast),j=1,jlast),&
		(irate(i),i=1,nrateq),(jrate(i),i=1,nrateq),&
		(QT(irate(i),jrate(i)),i=1,nrateq),(titlep(i),i=1,npar),&
        (ligname(i),i=1,nlig),((nbound(i,j),i=1,k),j=1,nlig),&
     	(IX(i),i=1,ncdep),(JX(i),i=1,ncdep),(IL(i),i=1,ncdep),&
     	(dgamma(i),i=1,kA),(nsc(i),i=1,ncyc),((im(i,j),j=1,nsc(i)),i=1,ncyc),&
     	((jm(i,j),j=1,nsc(i)),i=1,ncyc),(iv(i),i=1,nvdep),(jv(i),i=1,nvdep),&
     	(hpar(i),i=1,nvdep),(pstar(i),i=1,4),(kmcon(i),i=1,9),&
	    (ieq(i),i=1,nsetq),(jeq(i),i=1,nsetq),(ifq(i),i=1,nsetq),(jfq(i),i=1,nsetq),&
     	(efacq(i),i=1,nsetq),(statname(i),i=1,kstat),&
      	nsub,kstat0,npar0,kcon,npar1,ncyc0,((ic(i,j),i=1,2),j=1,ncon)
		models(jgraph)%n=k
		else
		read(unit=15,rec=irec) iver1,&
     	imod,mtitle,k, kA, kB, kC, kD,rtitle,ilast,jlast,nrateq,&
     	ncon,ncdep,nlig,chardef,boundef,ncyc,vref,nvdep,kmfast,&
     	indmod,npar,nsetq,kstat,&
     	((charmod(i,j),i=1,ilast),j=1,jlast),&
     	(irate(i),i=1,nrateq),(jrate(i),i=1,nrateq),&
		(QT(irate(i),jrate(i)),i=1,nrateq),(titlep(i),i=1,npar),&
        (ligname(i),i=1,nlig),((nbound(i,j),i=1,k),j=1,nlig),&
     	(IX(i),i=1,ncdep),(JX(i),i=1,ncdep),(IL(i),i=1,ncdep),&
     	(dgamma(i),i=1,kA),(nsc(i),i=1,ncyc),((im(i,j),j=1,nsc(i)),i=1,ncyc),&
     	((jm(i,j),j=1,nsc(i)),i=1,ncyc),(iv(i),i=1,nvdep),(jv(i),i=1,nvdep),&
     	(hpar(i),i=1,nvdep),(pstar(i),i=1,4),(kmcon(i),i=1,9),&
	    (ieq(i),i=1,nsetq),(jeq(i),i=1,nsetq),(ifq(i),i=1,nsetq),(jfq(i),i=1,nsetq),&
     	(efacq(i),i=1,nsetq),(statname(i),i=1,kstat),&
      	nsub,kstat0,npar0,kcon,npar1,ncyc0
			
		!call CQLAST(charmod,ilast,jlast)
		!call CHARQ(charmod,ilast,jlast,kA,kF,ncon,ic)
		models(jgraph)%n=k
		endif
		close(unit=15)
	nchan=1
	vkin=-80.
	vknd=dble(vkin*float(nchan))
	vknd=DROUND(vknd,0)	!exact double precision as long as vkin is integer
	cfacd=vknd*1.d-3*1.d12
		call intconv(imod,cnum)
		titlem=cnum(1:3)//mtitle
		imodel=imod
		ncols=5
	!	models(jgraph)%n=kstat
		!if(npar.gt.100) npar=100
		nrows=npar
		!	if(indmod) nrows=2*ncon
		isw=2
		ihcell=24
		do i=1,5
			iwcell(i)=100
		enddo
		do i=6,10
			iwcell(i)=100
		enddo
		do i=1,ncyc		!if nsc(i) is neg, set obeymr(i)=F and restore nsc
		if(nsc(i).lt.0) then
		   obeymr(i)=.false.
		   nsc(i)=iabs(nsc(i))
		else
		   obeymr(i)=.false.
		endif
	    enddo
		if(ncyc.gt.0) then
			allmr=.true.
			do i=1,ncyc
				if(.not.obeymr(i)) allmr=.false.
			enddo
		endif
		do k=1,npar
		    nb=len_trim(titlep(k))
			do jn=nb+1,10
				titlep(k)(jn:jn)=' '
			enddo
			ratcons(indk)%titlep(k)=titlep(k)
				
			ratcons(indk)%value(k)=qt(irate(k),jrate(k))
			ratcons(indk)%qt(irate(k),jrate(k))=qt(irate(k),jrate(k))
			i=irate(k)
			j=jrate(k)
			if(ncyc.gt.0) then
				
				do n=1,ncyc
				if(nsc(n).gt.0) obeymr(n)=.true.
				!if(obeymr(n).and.im(n,k).eq.i.and.jm(n,k).eq.j) then
				if(obeymr(n).and.im(n,1).eq.i.and.jm(n,1).eq.j) then
				ratcons(indk)%micro(k)='MR'	!dagger sign '=q(1,2)' indicates micro rev route
				endif
				enddo
			endif
				
		enddo
		ratcons(indk)%ncyc=ncyc
		do i=1,ncyc	
		ratcons(indk)%nsc(i)=nsc(i)
		enddo
		do l=1,ncyc
		do m=1,nsc(l)
		
			ratcons(indk)%im(l,m)=im(l,m)
			ratcons(indk)%jm(l,m)=jm(l,m)
		enddo
		enddo
		do i=1,nrateq
				ratcons(indk)%value(i)=qt(irate(i),jrate(i))
				ratcons(indk)%qt(irate(i),jrate(i))=qt(irate(i),jrate(i))
		enddo
		if(indmod) then
			do m=1,npar
				call intconv(m,cnum)
				call intconv(irate(m),cnum)
				call intconv(jrate(m),cnum1)
				ratcons(indk)%qij(m)='q('//cnum(1:2)//','//cnum1(1:2)//')'
				ratcons(indk)%ligant(m)='none'	
				if(ncdep.gt.0) then
				do n=1,ncdep
					i=ix(n)
					j=jx(n)
					
					if(i.eq.irate(m).and.j.eq.jrate(m)) then
					il1=IL(n)
					ratcons(indk)%iconc(m)=il1
					ratcons(indk)%ligant(m)=ligname(il1)		!ligand number
				
					endif
				enddo
				endif
			enddo
			k=1
			do m=1,ncon
				ic(1,m)=irate(k)
				ic(2,m)=jrate(k)
				call intconv(ic(1,m),cnum)
				call intconv(ic(2,m),cnum1)
				models(jgraph)%ic(1,m)=ic(1,m)
				models(jgraph)%ic(2,m)=ic(2,m)
				ratcons(indk)%qij(k)='q('//cnum(1:2)//','//cnum1(1:2)//')'
				ratcons(indk)%qij(k+1)='q('//cnum1(1:2)//','//cnum(1:2)//')'
				k=k+2	
			
			enddo
			 m=0
			 do i=2,models(jgraph)%n
			 do j=1,i	!look at lower trangular part of QT only
				if(QT(i,j).gt.1.d-30) then
					m=m+1
				    ratcons(indk)%qt(i,j)=qt(i,j)
				endif
			enddo
			enddo
			mtot=m
		else
			k=1
			i1=0
			do m=1,ncon
				ic(1,m)=irate(k)
				ic(2,m)=jrate(k)
				models(jgraph)%ic(1,m)=ic(1,m)
				models(jgraph)%ic(2,m)=ic(2,m)
				i=IC(1,m)
				j=IC(2,m)
				i1=i1+1
				iQ(i,j)=i1
				i1=i1+1
				IQ(j,i)=i1
				call intconv(i,cnum)
				call intconv(j,cnum1)
				ratcons(indk)%qij(k)='q('//cnum(1:2)//','//cnum1(1:2)//')'
				ratcons(indk)%ligant(k)='none'	
				do l=1,ncdep
					if(ix(l).eq.i.and.jx(l).eq.j) then
						ratcons(indk)%iconc(k)=il(l)
						ratcons(indk)%ligant(k)=ligname(il(l))
					endif
				enddo
				ratcons(indk)%qij(k+1)='q('//cnum1(1:2)//','//cnum(1:2)//')'
				ratcons(indk)%ligant(k+1)='none'	
				do l=1,ncdep
					if(ix(l).eq.j.and.jx(l).eq.i) then
						ratcons(indk)%iconc(k+1)=il(l)
						ratcons(indk)%ligant(k+1)=ligname(il(l))
					endif
				enddo
				k=k+2
				
				
			enddo
			
		endif
		nchan=1
		if(nsetq.gt.0) then
				ratcons(indk)%nsetq=nsetq
				do i=1,nsetq
					ratcons(indk)%ieq(i)=ieq(i)
					ratcons(indk)%jeq(i)=jeq(i)
					ratcons(indk)%ifq(i)=ifq(i)
					ratcons(indk)%jfq(i)=jfq(i)
					ratcons(indk)%efacq(i)=efacq(i)
				enddo
		endif
		ratcons(indk)%title=rtitle
		namelink(1)='none'
		do j=1,nlig
				namelink(j+1)=ligname(j)
		enddo
		models(jgraph)%kcon=kcon
		models(jgraph)%nsub=nsub
		models(jgraph)%kstat0=kstat0
		models(jgraph)%kstat=kstat
		ratcons(indk)%ncyc=ncyc
		if(ncyc.gt.0) then
		do i=1,ncyc	
			ratcons(indk)%nsc(i)=nsc(i)
		enddo
		do l=1,ncyc
		do m=1,nsc(l)
		
			ratcons(indk)%im(l,m)=im(l,m)
			ratcons(indk)%jm(l,m)=jm(l,m)
		enddo
		enddo
		
		endif
		if(nsetq.gt.0) then
		if(discprt)	write(7,228)
228  		format(' Fixed q(i,j), set for independent models:')
			do L=1,nsetq
			if(ieq(l).le.100.and.jeq(l).le.100.and.ifq(l).le.100.and.jfq(l).le.100) then
				ratcons(indk)%qt(ieq(l),jeq(l))=qt(ifq(l),jfq(l))
			if(discprt)	write(7,229) ieq(L),jeq(L),efacq(L),ifq(L),jfq(L)
229			format(1x,2i3,'= ',f9.3,' times ',2i3)
			endif
			enddo
		endif
		do i=1,	models(jgraph)%n
            dgammas(i)=dgamma(i)
!			write(7,321) i,statname(i),1.0d12*dgamma(i)
!321			format('  conductance of state ',i3,'(',a10,') (pS) = ',g13.6)
		enddo
		if(ncdep.eq.0) then
		if(discprt)	write(7,132)
132			format(' No concentration-dependent rates')
		else
		if(discprt)	write(7,133) nlig
133			format(/,' Number of ligands = ',i3,/,&
     		' Concentration-dependent elements:',/,&
     		'   i   j     ligand #   Ligand name')
			do L=1,ncdep
		if(discprt)		write(7,4) IX(L),JX(L),IL(L),ligname(IL(L))
4			format(1x,2i3,5x,i3,2x,a20)
			enddo
		endif
		do ik=1,100
			nb=len_trim(statname(ik))
			do jkl=nb+1,10
				statname(ik)(jkl:jkl)=' '
				enddo
				enddo
	   if(indmod) then	!don't print state names!
		if(discprt)	write(7,4011) (ligname(i)(1:10),i=1,nlig)
4011		format(/,' Number of ligands bound',/,10x,9(2x,a10))
			do i=1,models(jgraph)%n
			if(discprt)	write(7,4012) i,(nbound(i,n),n=1,nlig)
4012			format(' ',i3,': ',9(5x,i5))
			enddo
		else
		if(discprt)	write(7,401) (ligname(i)(1:10),i=1,nlig)
401			format(/,' Number of ligands bound',/,' State   ',10x,9(2x,a10))
			do i=1,models(jgraph)%n
			if(discprt)	write(7,40) i,statname(i),(nbound(i,n),n=1,nlig)
40				format(' ',i3,': ',a10,9(5x,i5))
			enddo
		endif
			
			nmod=imod
	
!		write(7,1)
1       format(/,' EC50 FOR INITIAL GUESSES:')
!	    call PRNTEC50(QT,conc,k,nlig,nset,npar,ncdep)
		if(discprt)	write(7,100)
100			format( ' Values of rate constants')
			do i=1,npar
			if(discprt)	write(7,101) i,ratcons(indk)%qij(i),titlep(i),ratcons(indk)%value(i),&
				ratcons(indk)%ligant(i)
101				format(i4,':',a10,'= ',a10,'*',g13.6,a10)
			enddo
			if(ncyc.gt.0) then
			if(discprt) write(7,103)
103			format(' Microscopic reversibility')
			do i=1,ncyc
			if(discprt)	write(7,104) i,nsc(i),(im(i,n), n=1,nsc(i))
104				format(' cycle=',i4,' ,number of states=',i4,', states:',5(i3,';'))
			enddo
			endif
		
			if(indmod.eq..false.) then
			
				if(ncyc.eq.0) then
					do j=1,nrows
					ratcons(indk)%micro(j)=' '
					enddo
				endif
		
			endif
			models(jgraph)%nlig=nlig
			do jk=1,nlig
					models(jgraph)%ligname(jk)=ligname(jk)
			enddo
		
			ncols=6
			nmr=0
			do l=1,nrows
			if(nsetq.gt.0) then
		
			do j=1,nsetq
				if(irate(l).eq.ie(j).and.jrate(l).eq.je(j)) then
				ratcons(indk)%micro(l)='constraint'
	
				endif
			enddo
			endif
			
			if(ncyc.gt.0) then
		
				do j=1,ncyc
					isetmr(j)=j
					if(irate(l).eq.im(j,1).and.jrate(l).eq.jm(j,1)) then
						ratcons(indk)%micro(l)='MR'
						obeymr(j)=.true.
					    nmr=nmr+1
					endif
				enddo
			endif
			if(fixec50)then
					if(irate(l).eq.i50.and.jrate(l).eq.j50) ratcons(indk)%micro(l)='EC50'
			endif
			enddo
			xtext='i,j '//char(124)//'Rate name'//char(124)//'Rate value'//char(124)//&
			'Conc dep.'//char(124)//'Volt dep'//char(124)//'Constraints'
			if(main.ne.-1) then
			call text_array(jgraph,Main,form1,Form1_TxtArray1,ncols,nrows,iwcell,ihcell,xtext,isw,&
			models(jgraph)%name_link,ratcons(indk)%iconc,ratcons(indk)%qij,&
			ratcons(indk)%value,ratcons(indk)%title,ratcons(indk)%titlep,NBOUND,ligname,nlig,&
			ratcons(indk)%micro,ratcons(indk)%ligant,jfix,theta0,iformText)
			endif
				ncon=models(jgraph)%ncon
			    k=models(jgraph)%n
			if(indmod.eq..false.) then
				nmrc=ncon-k+1
				
			endif			
end
