	subroutine write_ini(inifile,pfileb,nfix,jfix,neq,IE,JE,EFAC,
     &   IF,JF,jcon,IM2,JM2,jmic,ndisp,irecq,ans1,ylo,yhi,nrange,idiskq,
     &   nfileb,kfile,nset,conc,tcrit,burst,
     &    idatyp,qfile,imodold,setbad,tcbad,onechan,
     &    nbad1,isbad,iebad,tresolb,ans3,ans4,nlvar,
     &    fixec50,ec50,i50,j50,m50,xqlo,xqhi,kAm,kFm,
     &    chsvec,ncyc2,nsc2,qfilem,nsim,irect,logsav,imodolds,
     &    badend,excop,gaplo,gaphi,dcmod,nlen, 
     &	nfixec50,ec502,i502,j502,m502,xqlo2,xqhi2,
     &    useprim)

c readini:	ncyc2,nsc2,im2,jm2
c qmechs.dat	ncyc1,nsc1,im1,jm1
c working array: ncyc,nsc,im,jm

	integer JFIX(200),jomit(20),jfixsav(100),iparsav(10)
	integer nbad(10),nbad1(10)
	real*4 tcrit(10),tresol(10),tresolb(10)
	real*8 tresd(10),tres1
		real*4 tcbad(2,10)	!1=shut, 2=open
	integer ncbad(2,10)
	character*1 ans,UC,ans1,ans2,ans3,ans4,ans5
	character*2 ndev
	real*4 ylo(20),yhi(20)		!up to 20 gap ranges
	real*4 gaplo(10),gaphi(10)
	integer nskip(10)
	logical excopen
	integer*4 iexcop(10)
	logical slopsch,checkgrp,grouped,stable
	real*8 rcrit,gfac1,gfac2
	
	logical burst(10),chsvec(10),badend(10),onechan,autosim,useprim
	
	integer jfix1(200),jcon(200),jmic(200),jbad(200)
	logical discprt
	integer isbad(20,10),iebad(20,10)	!for bad bits (see stabplot)
	character*33 pfileb(20,10)	!path names for .SCN files
	character*40 qfilem,qfile
	integer kfile(20,10),nfileb(10)
	integer NSC2(50),IM2(50,100),JM2(50,100)	!up to 50 cycles
	integer IE(200),JE(200),IF(200),JF(200)
	logical obeymr(50),allmr
	real*4 conc(10,10)		
	real EFAC(200)
	allocatable :: jfix2,ie2,efac2,if2,jf2,je2,
     &  jcon2,IM,JM,jmic2,conca,concb
	character*40 inifile
	logical dcmodel,dcmod,fixec50,prtec50,logsav,setbad(2,10),fix502
	real*4 xs
	real*8 ec50,xqlo,xqhi,ec50out,pmax,ec502,xqlo2,xqhi2

	integer jfix2(:),ie2(:),je2(:),if2(:),jf2(:),
     & jcon2(:),IM(:,:),JM(:,:),jmic2(:),isetmr(50)
	real*4 efac2(:),conca(:),concb(:)
	logical automr(50)
c For max assoc rate
	real*8 assmax,ratemax
	integer icdep(200)
	common/amax/assmax,icdep,ratemax
	common/dp/discprt
	common/mr1/isetmr
	integer isgood1(10),iegood1(10) 
	
	common/stab/stable,nstab1,gfac1,nstab2,gfac2,istab
	common/grp1/slopsch,checkgrp,grouped,rcrit,ngpcheck
	common/detw2/tres,km,nerr

	  ALLOCATE(jfix2(20),ie2(10),je2(10),efac2(10),if2(10),jf2(10),
     &  jcon2(20),IM(8,15),JM(8,15),jmic2(20),conca(10),concb(10))
	 
	  if(nlen.le.20480) then	!old file
	    if(nlen.eq.10240) then	!old file
		OPEN(unit=19,file=inifile,status='UNKNOWN',
     &       access='DIRECT',form='UNFORMATTED',recl=10240)
		write(19,rec=1) pfileb,tres,nfix,jfix2,neq,IE2,JE2,EFAC2,
     &  	IF2,JF2,jcon2,IM,JM,jmic2,ndisp,irecq,ans1,ylo,yhi,
     &  	nrange,idiskq,
     &	nfileb,kfile,nset,concA,concB,tcrit,burst,irecq,
     & 	idatyp,qfile,imodold,setbad,tcbad,onechan,
     & 	isgood1,iegood1,tresolb
	    else if(nlen.eq.20480) then	!new .ini for old 10 state/2 ligand version
      	OPEN(unit=19,file=inifile,status='UNKNOWN',
     & 	access='DIRECT',form='UNFORMATTED',recl=20480)
		do j=1,10
		   conca(j)=conc(1,j)
		   concb(j)=conc(2,j)
		   ie2(j)=ie(j)
		   je2(j)=je(j)
		   efac2(j)=efac(j)
		   if2(j)=if(j)
		   jf2(j)=jf(j)
		enddo
		do i=1,20
		   jfix2(i)=jfix(i)
		   jcon2(i)=jcon(i)
		   jmic2(i)=jmic(i)
		enddo
		do i=1,8
		  do j=1,15
			im(i,j)=im2(i,j)
			jm(i,j)=jm2(i,j)
		   enddo
		enddo
		DEALLOCATE(jfix2,ie2,je2,efac2,if2,jf2,
     &	  jcon2,IM,JM,jmic2,conca,concb)
		write(19,rec=1) pfileb,tres,nfix,jfix2,neq,IE2,JE2,EFAC2,
     &      IF2,JF2,jcon2,IM,JM,jmic2,ndisp,irecq,ans1,ylo,yhi,
     &	nrange,idiskq,
     & 	nfileb,kfile,nset,concA,concB,tcrit,burst,irecq,
     & 	idatyp,qfile,imodold,setbad,tcbad,onechan,
     & 	nbad1,isbad,iebad,tresolb,ans3,ans4,
     & 	fixec50,ec50,i50,j50,m50,xqlo,xqhi
	    endif
	  	CLOSE(unit=19)
	
		
	  else	
         open(unit=19,file=inifile,status='UNKNOWN',
     &    access='DIRECT',form='BINARY',recl=1)
	   write(19,rec=1) pfileb,nfix,jfix,neq,IE,JE,EFAC,
     &   IF,JF,jcon,IM2,JM2,jmic,ndisp,irecq,ans1,ylo,yhi,nrange,idiskq,
     &   nfileb,kfile,nset,conc,tcrit,burst,irecq,
     &    idatyp,qfile,imodold,setbad,tcbad,onechan,
     &    nbad1,isbad,iebad,tresolb,ans3,ans4,nlvar,
     &    fixec50,ec50,i50,j50,m50,xqlo,xqhi,kAm,kFm,
     &    chsvec,ncyc2,
     &    (nsc2(i),i=1,ncyc2),
     &    ((im2(i,j),j=1,nsc2(i)),i=1,ncyc2),
     &    ((jm2(i,j),j=1,nsc2(i)),i=1,ncyc2),
     &    assmax,qfilem,nsim,irect,logsav,imodolds,
     &    badend,iexcop,gaplo,gaphi,dcmod,
     &    slopsch,checkgrp,rcrit,ngpcheck,
     &    nstab1,gfac1,nstab2,gfac2,ans5,
     &    nfixec50,ec502,i502,j502,m502,xqlo2,xqhi2,
     &    useprim,(isetmr(i),i=1,ncyc2),ratemax

	    CLOSE(unit=19)
	    
	  endif
	  
	
	end