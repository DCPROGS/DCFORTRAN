subroutine write_ini(inifile,pfileb,nfix,jfix,neq,IE,JE,EFAC,&
        IF,JF,jcon,IM2,JM2,jmic,ndisp,irecq,ans1,ylo,yhi,nrange,idiskq,&
        nfileb,kfile,nset,conc,tcrit,burst,&
         idatyp,qfile,imodold,setbad,tcbad,onechan,&
         nbad1,isbad,iebad,tresolb,ans3,ans4,nlvar,&
         fixec50,ec50,i50,j50,m50,xqlo,xqhi,kAm,kFm,&
         chsvec,ncyc2,nsc2,qfilem,nsim,irect,logsav,imodolds,&
         badend,excop,gaplo,gaphi,dcmod,nlen, &
     	nfixec50,ec502,i502,j502,m502,xqlo2,xqhi2,&
         useprim,pfiles,qmec,nmr,nlig,conc_ec1,conc_ec2,iftype)

! readini:	ncyc2,nsc2,im2,jm2
! qmechs.dat	ncyc1,nsc1,im1,jm1
! working array: ncyc,nsc,im,jm

! NB irecq1 is written but not defined,so generates a warning. It is not used
! anywhere in either Windows or DOS versions, so set=0 here to avoid warning.

	real*4 conc_ec1(10),conc_ec2(10)
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
	character*60 pfiles(20,10),qmec
	integer kfile(20,10),nfileb(10)
	integer NSC2(50),IM2(50,100),JM2(50,100)	!up to 50 cycles
	integer IE(200),JE(200),IF(200),JF(200)
	logical obeymr(50),allmr,excop(10)
	real*4 conc(10,10)		
	real EFAC(200)
	allocatable :: jfix2,ie2,efac2,if2,jf2,je2,jcon2,IM,JM,jmic2,conca,concb
	character*40 inifile
	logical dcmodel,dcmod,fixec50,prtec50,logsav,setbad(2,10),fix502
	real*4 xs
	real*8 ec50,xqlo,xqhi,ec50out,pmax,ec502,xqlo2,xqhi2

	integer jfix2(:),ie2(:),je2(:),if2(:),jf2(:),jcon2(:),IM(:,:),JM(:,:),jmic2(:),isetmr(50)
	real*4 efac2(:),conca(:),concb(:)
	logical automr(50)

	real*8 assmax,ratemax
	integer icdep(200)
	common/amax/assmax,icdep,ratemax
	common/dp/discprt
	common/mr1/isetmr
	integer isgood1(10),iegood1(10) 
	character*8 ftype
	common/stab/stable,nstab1,gfac1,nstab2,gfac2,istab
	common/grp1/slopsch,checkgrp,grouped,rcrit,ngpcheck
	common/detw2/tres,km,nerr
	character*60 pathdata,pathmec



	  irecq1=0	!not used anywhere (see comment at top) but defined to avoid warning
	  ALLOCATE(jfix2(20),ie2(10),je2(10),efac2(10),if2(10),jf2(10),&
      jcon2(20),IM(8,15),JM(8,15),jmic2(20),conca(10),concb(10))
	  nbl=len_trim(qmec)
	  ilp=1
	  do i=1,nbl
		 if(qmec(i:i).eq.'\') ilp=i
	  enddo
	  if(ilp.eq.1) then
		iplen = FULLPATHQQ(qfilem, pathdata)
		nbp=len_trim(pathdata)
		pathmec=pathdata
		qmec=pathmec		
	  endif	
	  
	  do j=1,nset
			do l=1,nfileb(j)
			nbl=len_trim(pfiles(l,j))
				    ilp=1
				    do i=1,nbl
						if(pfiles(l,j)(i:i).eq.'\') ilp=i
					enddo
					if(ilp.eq.1) then
					iplen = FULLPATHQQ(pfiles(l,j), pathdata)
					nbp=len_trim(pathdata)
					pathmec=pathdata
				
					pfiles(l,j)=pathmec
					endif
			enddo
	  enddo
	   iftype=100
       open(unit=19,file=inifile,status='UNKNOWN',&
        access='DIRECT',form='BINARY',recl=1) 
		write(19,rec=1) iftype
	    iftype=100
	    write(19,rec=1) iftype,pfiles,nfix,jfix,neq,IE,JE,EFAC,&
        IF,JF,jcon,IM2,JM2,jmic,ndisp,irecq,ans1,ylo,yhi,nrange,idiskq,&
        nfileb,kfile,nset,conc,tcrit,burst,irecq1,&
         idatyp,qfile,imodold,setbad,tcbad,onechan,&
         nbad1,isbad,iebad,tresolb,ans3,ans4,nlvar,&
         fixec50,ec50,i50,j50,m50,xqlo,xqhi,kAm,kFm,&
         chsvec,ncyc2,&
         (nsc2(i),i=1,ncyc2),&
         ((im2(i,j),j=1,nsc2(i)),i=1,ncyc2),&
         ((jm2(i,j),j=1,nsc2(i)),i=1,ncyc2),&
         assmax,qmec,nsim,irect,logsav,imodolds,&
         badend,iexcop,gaplo,gaphi,dcmod,&
         slopsch,checkgrp,rcrit,ngpcheck,&
         nstab1,gfac1,nstab2,gfac2,ans5,&
         nfixec50,ec502,i502,j502,m502,xqlo2,xqhi2,&
         useprim,(isetmr(i),i=1,ncyc2),ratemax,nmr,nlig,&
         (conc_ec1(i),i=1,nlig),&
         (conc_ec2(i),i=1,nlig)

	    CLOSE(unit=19)
	    
	 
	  
	
	end