!     Last change:  D    15 Jan 104    2:18 pm
subroutine write_model(imodel,models,indrat,ratcons,irecm0,qfilem,pfilem,jgraph,irecq)
use menu_f90
REAL*8 dgamma(100)
	character*2 charmod(25,40)	!to print model
	character*2 cmodnew(25,40)	!for NEWMOD
	real*4 pstar(4)
	character*8 titles(100)
	logical nopath
	character qpath*30,qname*8,suffix*3,ndev*2,cnum*11,conex*30,xtext*100,cnum1*11
	character rfile*40,qfilem*40,qfilmsav*40,pfilem*40
	character*10 titlep(200),titlep1(200)
	integer IQ(100,100),imatrix(25,40),IC(2,200)
	integer inew(100)
	real*8 QT(100,100),temp(100)
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
	character*1 UC,ch1,ch2
	character*74 mtitle	!title for model
	character*74 mtits(100)	!to read all ditto from disc
	integer ijmod(500)
	logical discprt
	common/dp/discprt
	!Addition for getbound
	integer nbound(100,10)
	

	character*11 cnum0
	!For 'true' model used for simulation -used in getqd2
	logical autosim,first
	integer irates(200),jrates(200)
	logical statdef	!state names defined

	real*8 dgammas(100)
	character*60 :: qdir='.'
		character*100 :: qfilt='*.mec'//char(124)//'Mechanism Files(MEC)'//&
     	char(124)//'*.dat'//char(124)//'Old Files (DAT)'//&
     	char(124)//'*.*'//char(124)//'All Files'

	integer::Button7(10),RECORDS
	character*10 label7(2)

	logical obeymr(50)

	character*74 mtitles	!title for sim model
	character*2 charmods(25,40)	!to print model

	character*40 mtitle1
	common/sm/nsmax		!for getqd/charq

	
	integer KMCON(9)
	integer IV(100),JV(100)
	real HPAR(100)
	
	integer IX(100),JX(100)
	integer NSC(50),IM(50,100),JM(50,100)	!up to 50 cycles
	integer IE(200),JE(200),IF(200),JF(200)
	real EFAC(200)
	integer ieq(200),jeq(200),ifq(200),jfq(200)
	real efacq(200)
	integer IL(100),icyc(50)
	common/nmodel/cmodnew
	

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
		character*10 open_states(10,20)
		character*10 start_states(10,20)
		integer	num_states(10,20)
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
		integer icyc(50,50)
		integer im(50)
		integer jm(50)
	END TYPE rate_constant
	
	type(rate_constant) ratcons(500)

	INQUIRE(file=qfilem,exist=present)
	
	if(.not.present) then
	   iver=200
	   nrecs=0
	   do i=1,500
			jstart(i)=0
	   enddo
	   jstart(1)=20000
	   irec=20000
	   nextrec=20000
	   ireclast=irec
	   rtitles(1)=rtitle
	   OPEN(unit=14,file=qfilem,status='UNKNOWN',access='DIRECT',&
       form='BINARY',RECL=1)
	   write(14,rec=1) iver,nrecs,nextrec,ireclast,jstart
	   close(unit=14)
	   
	else
         OPEN(unit=14,file=qfilem,status='UNKNOWN',access='DIRECT',&
        form='BINARY',RECL=1)
		READ(14,REC=1,IOSTAT=I_VAR) iver,nrecs,nextrec,ireclast,jstart
        altmod=.true.
		IF(I_VAR.NE.0) THEN
			CLOSE(UNIT=14)
			Icall=gmDisplayMessageBox('Stop ','Not a proper data file',&
     			GEXCLAMATION,GOK)
			goto 99
		else
			if(iver.ne.102.and.iver.ne.200.and.iver.ne.201) then
			Icall=gmDisplayMessageBox('Stop ','Not a proper data file',&
     			GEXCLAMATION,GOK)
			CLOSE(UNIT=14)
			goto 99
			endif
		endif
	endif
	


OPEN(unit=14,file=qfilem,status='UNKNOWN',access='DIRECT', form='BINARY', RECL=1)
read(unit=14,rec=1) iver,nrecs,nextrec,ireclast,jstart

if(altmod) then
	mtitle=models(jgraph)%title_model
	do ir=1,nrecs
		irec=jstart(ir)	!previous nextrec
		read(unit=14,rec=irec) iver1,imod1,mtitle1,k1,kA1,kB1,kC1,kD1,rtitle
	    ijmod(ir)=imod1
		mtits(imod1)=mtitle1	!so mtits(i)=title of model #i
		rtitles(ir)=rtitle
	enddo
	
!   Look for lowest unused model number in qfilout
	im2=1
81	do i=1,nrecs
		if(im2.eq.ijmod(i)) then
		   im2=im2+1
		   goto 81	!repeat
		endif
	enddo
	imod=im2
	icall=gmDisplayMessageBox('','Save as a new model?',&
     		GquesTION,gyesno)
	if(icall.eq.gYESbutton) then
		call intconv(imod,cnum)
		Icall=gmDisplayMessageBox('Model ','The lowest unused number is:'//cnum(1:3),&
     		GquesTION,gyesno)
		if(icall.eq.gnobutton) goto 100  
84		do ir=1,nrecs
		irec=jstart(ir)	!previous nextrec
		read(unit=14,rec=irec) iver1,imod1,mtitle1,k1,kA1,kB1,kC1
		kF=kB+kC
		kF1=kB1+kC1
		if(imod.eq.imod1) then 		!current number occurs
		if(mtitle1.ne.mtitle.or.kA1.ne.kA.or.kF1.ne.kF) then
		Icall=gmDisplayMessageBox('Model'//cnum(1:3)//mtitle1,&
		'Already exists but denotes a different mechanism',GEXCLAMATION,gok)
		Icall=gmDisplayMessageBox('Model ','change to the lowest unused number:'//cnum(1:3),&
     		Gquestion,gyesno)
		ans='Y'
		if(icall.eq.gYESbutton) then
			   imod=im2
		else
			goto 100
		endif
		endif 
		endif
		enddo
	else
		if(imodel.lt.300) then
			imod=imodel
		else
			imod=ratcons(indk)%imod
			if(imod.le.0.and.imod.ge.300) imod=im2
		endif
	endif
else
		imod=1
endif		!end of if(.not.samefil)

100 continue 

	
	idest=0
	
	isum=0
	do i=1,ncyc
	   isum=isum + nsc(i)
	enddo
	


	indk=indrat
!imod=imodel
	k=models(jgraph)%n
	ka=models(jgraph)%ka
	kb=models(jgraph)%kb
	indmod=models(jgraph)%indmod
	if(indmod) boundef=.true.
	kstat=k
	kf=k-ka
	kc=kf-kb
	ncon=models(jgraph)%ncon
	kcon=models(jgraph)%kcon
	nsub=models(jgraph)%nsub
	kstat0=models(jgraph)%kstat0
	kstat=models(jgraph)%kstat
	npar0=2*kstat0
	npar1=2*kcon
	if(indmod) then
	npar=models(jgraph)%npar
	else
	npar=2*ncon
	endif
	nrateq=2*ncon
	nlig=models(jgraph)%nlig
	chardef=models(jgraph)%chardef
	mtitle=models(jgraph)%title_model
	rtitle=ratcons(indk)%title
	vref=models(jgraph)%vref
	nchan=models(jgraph)%nchan

	j=1
do k=1,models(jgraph)%n
	dgamma(k)=1.0d-12*(models(jgraph)%dgamma(k))

	statname(k)=models(jgraph)%statname(k)

enddo
if(indmod) then
	do j=1,kcon
		statname(j)=models(jgraph)%con_states(j)
	enddo
	do i=1,nsub
	
	do j=1,kstat0
	statname(kcon+j+kstat0*(i-1))=models(jgraph)%sub_states(i,j)
	enddo
	enddo
	if(nsub.eq.0)  then
	i=1
	do j=1,kstat-kcon
	statname(kcon+j)=models(jgraph)%sub_states(i,j)
	enddo
	endif
endif

	!	IQ(i,j)=m
	!	irate(m)=i
	!	jrate(m)=j
	      
k=1	


do m=1,ncon
	ic(1,m)=models(jgraph)%ic(1,m)
	ic(2,m)=models(jgraph)%ic(2,m)
	i=ic(1,m)
	j=ic(2,m)
	irate(k)=i
	jrate(k)=j
	irate(k+1)=j
	jrate(k+1)=i
	k=k+2
enddo

do k=1,npar
	titlep(k)=ratcons(indk)%titlep(k)
	qt(irate(k),jrate(k))=ratcons(indk)%value(k)
enddo
if(indmod.and.imodel.gt.300) then
m1=npar/2+1
l=npar+1
do m=m1,ncon
	ic(1,m)=models(jgraph)%ic(1,m)
	ic(2,m)=models(jgraph)%ic(2,m)

	irate(l)=ic(1,m)
	jrate(l)=ic(2,m)
	irate(l+1)=ic(2,m)
	jrate(l+1)=ic(1,m)
	
	l=l+2
	
enddo
do k=1,nrateq-npar
	ieq(k)=ratcons(indk)%ieq(k)
	jeq(k)=ratcons(indk)%jeq(k)
	ifq(k)=ratcons(indk)%ifq(k)
	jfq(k)=ratcons(indk)%jfq(k)
	efacq(k)=1.

	 
	qt(ieq(k),jeq(k))=qt(ifq(k),jfq(k))
enddo
else
do i=2,models(jgraph)%n
			 do j=1,i	!look at lower trangular part of QT only
				if(QT(i,j).gt.1.d-30) then
				
				    qt(i,j)=ratcons(indk)%qt(i,j)
				endif
			enddo
			enddo
do i=1,nrateq
			qt(irate(i),jrate(i))=ratcons(indk)%value(i)
enddo
endif
k=1
l=1
do i=1,nlig
	ligname(i)=models(jgraph)%ligname(i)
enddo

!!!!!!!
do m=1,ncon
	
	i=IC(1,m)
	j=IC(2,m)
	if(ratcons(indk)%iconc(k).ne.0) then
		
		ix(l)=i
		jx(l)=j
		il(l)=ratcons(indk)%iconc(k)
	!	ligname(il(l))=	ratcons(indk)%ligant(k)
		l=l+1
	endif
	if(ratcons(indk)%iconc(k+1).ne.0) then
		
		ix(l)=j
		jx(l)=i
		il(l)=ratcons(indk)%iconc(k+1)
	!	ligname(il(l))=	ratcons(indk)%ligant(k+1)
		l=l+1
	endif
	k=k+2
enddo
	ncdep=l-1
do k=1,npar
	titlep(k)=ratcons(indk)%titlep(k)
	qt(irate(k),jrate(k))=ratcons(indk)%value(k)
enddo		
do i=1,nlig
		
			do j=1,models(jgraph)%n
				nbound(j,i)=models(jgraph)%nbound(j,i)

			enddo
enddo
	if(ratcons(indk)%nsetq.gt.0) then
				nsetq=ratcons(indk)%nsetq
				do i=1,nsetq
					ieq(i)=ratcons(indk)%ieq(i)
					jeq(i)=ratcons(indk)%jeq(i)
					ifq(i)=ratcons(indk)%ifq(i)
					jfq(i)=ratcons(indk)%jfq(i)
					efacq(i)=ratcons(indk)%efacq(i)
				enddo
	endif
	kstat1=kstat
	nbytes=4+4+74+20+74+8+40+12+4*ilast*jlast+4*nrateq+4*nrateq+&
         8*nrateq+10*npar+20*nlig+4*k*nlig+4*ncdep+4*ncdep+&
         4*ncdep+8*kA+4*ncyc+4*ncyc*isum+4*ncyc*isum+&
         4*nvdep+4*nvdep+4*nvdep+16+36+5*4*nsetq+10*kstat1&
         +6*4+4*(2*ncon)+30*kstat1

	   nrecs=nrecs+1
	   irecm0=nrecs
	   irec=nextrec		!where to write the record
	   ireclast=irec
	   jstart(nrecs)=irec
	   rtitles(nrecs)=rtitle
	   nextrec=irec + nbytes + 1024 	
		
	ncyc=ratcons(indrat)%ncyc
	do i=1,ncyc	
		nsc(i)=ratcons(indrat)%nsc(i)
	enddo

	do l=1,ncyc
		i1=ratcons(indrat)%im(l)
		j1=ratcons(indrat)%jm(l)
		do j=1,nsc(l)
			icyc(j)=ratcons(indrat)%icyc(l,j)
		enddo
		do m1=1,nsc(l)-1
			m2=m1		!for skip-out
			if(icyc(m1).eq.i1.and.icyc(m1+1).eq.j1) goto 15
		enddo
		if(icyc(nsc(l)).eq.i1.and.icyc(1).eq.j1) goto 151

		call IVECREV(icyc,nsc(l),20)
		do m1=1,nsc(l)-1
			m2=m1		!for skip-out
			if(icyc(m1).eq.i1.and.icyc(m1+1).eq.j1) goto 15
		enddo
		if(icyc(nsc(l)).eq.i1.and.icyc(1).eq.j1) goto 151
	    goto 17
15	    continue
		call IVECROT(icyc,nsc(l),1-m2,20)
		goto 18
151		continue

		call IVECROT(icyc,nsc(l),1,20)

18		continue
		do j=1,nsc(l)
			ratcons(indrat)%icyc(l,j)=icyc(j)
		enddo
		do m=1,nsc(l)-1
			im(l,m)=ratcons(indrat)%icyc(l,m)
			jm(l,m)=ratcons(indrat)%icyc(l,m+1)
		enddo
		im(l,nsc(l))=ratcons(indrat)%icyc(l,nsc(l))
		jm(l,nsc(l))=ratcons(indrat)%icyc(l,1)
17		continue	
			
	
	enddo
    nchan=1
	iver1=200
	k=models(jgraph)%n
	write(14,rec=1) iver,nrecs,nextrec,ireclast,jstart
	if(iver1.eq.200) then
	    chardef=.false.
		if(imodel.gt.300) then
			chardef=.false.
		!	call generate_charmod(imodel,models,ilast,jlast,0)
		else
			ilast=models(jgraph)%ilast
			jlast=models(jgraph)%jlast
		endif
		do in=1,ilast
		do jn=1,jlast
			charmod(in,jn)=models(jgraph)%charmod(in,jn)
		enddo
		enddo
	
		write(unit=14,rec=irec) iver1,&
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
		((ic(i,j),i=1,2),j=1,ncon),(models(jgraph)%name(i),i=1,k),&
		(models(jgraph)%colour(i),i=1,k),(models(jgraph)%x(i),i=1,k),&
		(models(jgraph)%y(i),i=1,k),models(jgraph)%nwidth,models(jgraph)%nheight
	else if(iver1.eq.201) then
		if(imodel.gt.300) then
		!call generate_charmod(imodel,models,ilast,jlast,0)
		else
		ilast=models(jgraph)%ilast
		jlast=models(jgraph)%jlast
		endif
		do in=1,ilast
		do jn=1,jlast
			charmod(in,jn)=models(jgraph)%charmod(in,jn)
		enddo
		enddo
		chardef=.false.
		write(unit=14,rec=irec) iver1,&
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
		((ic(i,j),i=1,2),j=1,ncon)
	endif
irecq=irec
	close(unit=14)
	write(7,210) qfilem,nrecs
210		format(' Mechanisms file: ',a40,/,&
        ' contains ',i4,' records of rate constants +model')
		write(7,211) nrecs,irec
211		format(/,/,' Record number ',i4,' (starts at byte #',i6,')')
		write(7,214)mtitle(1:74),imod
214   	format(1x,a74,/,' Model number = ',i3,/)
		write(7,73) nchan,kA
73		format(/,' Number of channels = ',i8,/,' Number of open states = ',i3)
		if(indmod) then
			write(7,74) kcon,nsub,kstat0
74			format(/,' Number of concerted states= ',i8,/,' Number of subunits = ',i3,&
			/,' Number of states in each subunit =',i3)
		endif
		if(nsetq.gt.0) then
			write(7,228)
228  		format(' Fixed q(i,j), set for independent models:')
			do L=1,nsetq
				write(7,229) ieq(L),jeq(L),efacq(L),ifq(L),jfq(L)
229			format(1x,2i3,'= ',f9.3,' times ',2i3)
			enddo
		endif
		do i=1,kstat
		write(7,321) i,statname(i),1.0d12*dgamma(i)
321	   format('  conductance of state ',i3,'(',a10,') (pS) = ',g13.6)
	   enddo
		if(ncdep.eq.0) then
			write(7,132)
132			format(' No concentration-dependent rates')
		else
			write(7,133) nlig
133			format(/,' Number of ligands = ',i3,/,&
     		' Concentration-dependent elements:',/,&
     		'   i   j     ligand #   Ligand name')
			do L=1,ncdep
				write(7,4) IX(L),JX(L),IL(L),ligname(IL(L))
4			format(1x,2i3,5x,i3,2x,a20)
			enddo
		endif
	   if(indmod) then	!don't print state names!
			write(7,4011) (ligname(i)(1:10),i=1,nlig)
4011		format(/,' Number of ligands bound',/,10x,9(2x,a10))
			do i=1,k
				write(7,4012) i,(nbound(i,n),n=1,nlig)
4012			format(' ',i3,': ',9(5x,i5))
			enddo
		else
			write(7,401) (ligname(i)(1:10),i=1,nlig)
401			format(/,' Number of ligands bound',/,' State   ',10x,9(2x,a10))
			do i=1,k
				write(7,40) i,statname(i),(nbound(i,n),n=1,nlig)
40				format(' ',i3,': ',a10,9(5x,i5))
			enddo
		endif
	 
	
		do i=1,ilast
			write(7,671) (charmod(i,j),j=1,jlast)
671			format(4x,35a2)
		enddo
			write(7,800)
800			format( ' Values of rate constants')
			do i=1,npar
				write(7,101) i,ratcons(indk)%qij(i),titlep(i),ratcons(indk)%value(i),&
				ratcons(indk)%ligant(i)
101	   format(i3,':',a10,'= ',a10,g13.6,'*',a10)
enddo
99	end
