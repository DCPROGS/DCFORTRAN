	subroutine read_ini(inifile,pfileb,nfix,jfix,neq,IE,JE,EFAC,
     &   IF,JF,jcon,IM2,JM2,jmic,ndisp,irecq,ans1,ylo,yhi,nrange,idiskq,
     &   nfileb,kfile,nset,conc,tcrit,burst,
     &    idatyp,qfile,imodold,setbad,tcbad,onechan,
     &    nbad1,isbad,iebad,tresolb,ans3,ans4,nlvar,
     &    fixec50,ec50,i50,j50,m50,xqlo,xqhi,kAm,kFm,
     &    chsvec,ncyc2,nsc2,qfilem,nsim,irect,logsav,imodolds,
     &    badend,excop,dcmod,nlen, 
     &	nfixec50,ec502,i502,j502,m502,xqlo2,xqhi2,
     &    useprim,pfiles,qmec,nmr,nlig,
     &    conc_ec1,conc_ec2,iftype,iexcop,gaplo,gaphi,nskip)

c readini:	ncyc2,nsc2,im2,jm2
c qmechs.dat	ncyc1,nsc1,im1,jm1
c working array: ncyc,nsc,im,jm
c NB irecq1 is read but not used so generates a warning. It is not used anywhere
c in the DOS version either so warning can be ignored,
	real*4 conc_ec1(10),conc_ec2(10)
	integer isgood1(10),iegood1(10) 
	integer JFIX(200),jomit(20),jfixsav(100),iparsav(10)
	integer nbad(10),nbad1(10)
	real*4 tcrit(10),tresol(10),tresolb(10)
	real*8 tresd(10),tres1
		character*60 pfiles(20,10),qmec
		real*4 tcbad(2,10)	!1=shut, 2=open
	integer ncbad(2,10)
	character*1 ans,UC,ans1,ans2,ans3,ans4,ans5
	character*2 ndev
	real*4 ylo(20),yhi(20)		!up to 20 gap ranges
	real*4 gaplo(10),gaphi(10)
	integer nskip(10)
	logical excopen
	integer*4 iexcop(10)
!	common/exop/iexcop,gaplo,gaphi,nskip
	logical slopsch,checkgrp,grouped,stable
	real*8 rcrit,gfac1,gfac2
	common/grp1/slopsch,checkgrp,grouped,rcrit,ngpcheck
	common/stab/stable,nstab1,gfac1,nstab2,gfac2,istab	
	logical burst(10),chsvec(10),badend(10),onechan,autosim,useprim
	logical excop(10)
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
		character*60 pathdata,pathmec
	character*8 ftype
	integer icdep(200)
	common/amax/assmax,icdep,ratemax
	common/dp/discprt
	common/mr1/isetmr	
			
	  ALLOCATE(jfix2(20),ie2(10),je2(10),efac2(10),if2(10),jf2(10),
     &  jcon2(20),IM(8,15),JM(8,15),jmic2(20),conca(10),concb(10))
	 
	  if(nlen.le.20480) then	!old file
	    if(nlen.eq.10240) then	!old file
		    OPEN(unit=19,file=inifile,status='UNKNOWN',
     &       access='DIRECT',form='UNFORMATTED',recl=10240)
		    read(19,rec=1) pfileb,tres,nfix,jfix2,neq,IE2,JE2,EFAC2,
     &  	IF2,JF2,jcon2,IM,JM,jmic2,ndisp,irecq,ans1,ylo,yhi,
     &  	nrange,idiskq,
     &	    nfileb,kfile,nset,concA,concB,tcrit,burst,irecq,
     & 	    idatyp,qfile,imodold,setbad,tcbad,onechan,
     & 	    isgood1,iegood1,tresolb
	    else if(nlen.eq.20480) then	!new .ini for old 10 state/2 ligand version
      	    OPEN(unit=19,file=inifile,status='UNKNOWN',
     & 	    access='DIRECT',form='UNFORMATTED',recl=20480)
		    read(19,rec=1) pfileb,tres,nfix,jfix2,neq,IE2,JE2,EFAC2,
     &      IF2,JF2,jcon2,IM,JM,jmic2,ndisp,irecq,ans1,ylo,yhi,
     &	    nrange,idiskq,
     & 	    nfileb,kfile,nset,concA,concB,tcrit,burst,irecq1,
     & 	    idatyp,qfile,imodold,setbad,tcbad,onechan,
     & 	    nbad1,isbad,iebad,tresolb,ans3,ans4,
     & 	    fixec50,ec50,i50,j50,m50,xqlo,xqhi
	    endif
	    CLOSE(unit=19)
		  call SYSTEM("copy "//inifile//", hjc0.ini")
		
		  if(discprt) write(7,10) inifile
10		  format(/," Old hjcfit.ini file, ", a30,/,"copied as "
     &	  """hjc0.ini"", and new 60 kb hjcfit.ini will be made",/)
c          Transfer old .ini data to new arrays and deallocate old
		  do j=1,10
		   conc(1,j)=concA(j)
		   conc(2,j)=concB(j)
		   ie(j)=ie2(j)
		   je(j)=je2(j)
		   efac(j)=efac2(j)
		   if(j)=if2(j)
		   jf(j)=jf2(j)
		  enddo
		  do i=1,20
		   jfix(i)=jfix2(i)
		   jcon(i)=jcon2(i)
		   jmic(i)=jmic2(i)
		 enddo
		 do i=1,8
		  do j=1,15
			im2(i,j)=im(i,j)
			jm2(i,j)=jm(i,j)
		   enddo
		 enddo
		 DEALLOCATE(jfix2,ie2,je2,efac2,if2,jf2,
     &	  jcon2,IM,JM,jmic2,conca,concb)
	  else	
	   open(unit=19,file=inifile,status='UNKNOWN',
     &    access='DIRECT',form='BINARY',recl=1)
         read(19,rec=1) iftype
		 if(iftype.eq.100) then
		
	   read(19,rec=1) iftype,pfiles,nfix,jfix,neq,IE,JE,EFAC,
     &   IF,JF,jcon,IM2,JM2,jmic,ndisp,irecq,ans1,ylo,yhi,nrange,idiskq,
     &   nfileb,kfile,nset,conc,tcrit,burst,irecq1,
     &    idatyp,qfile,imodold,setbad,tcbad,onechan,
     &    nbad1,isbad,iebad,tresolb,ans3,ans4,nlvar,
     &    fixec50,ec50,i50,j50,m50,xqlo,xqhi,kAm,kFm,
     &    chsvec,ncyc2,
     &    (nsc2(i),i=1,ncyc2),
     &    ((im2(i,j),j=1,nsc2(i)),i=1,ncyc2),
     &    ((jm2(i,j),j=1,nsc2(i)),i=1,ncyc2),
     &    assmax,qmec,nsim,irect,logsav,imodolds,
     &    badend,iexcop,gaplo,gaphi,dcmod,
     &    slopsch,checkgrp,rcrit,ngpcheck,
     &    nstab1,gfac1,nstab2,gfac2,ans5,
     &    nfixec50,ec502,i502,j502,m502,xqlo2,xqhi2,
     &    useprim,(isetmr(i),i=1,ncyc2),ratemax,nmr,nlig,
     &    (conc_ec1(i),i=1,nlig),
     &    (conc_ec2(i),i=1,nlig)
	 else
	  
		 read(19,rec=1) ftype
	   if(ftype.eq.'windows') then
	   read(19,rec=1) ftype,pfileb,nfix,jfix,neq,IE,JE,EFAC,
     &   IF,JF,jcon,IM2,JM2,jmic,ndisp,irecq,ans1,ylo,yhi,nrange,idiskq,
     &   nfileb,kfile,nset,conc,tcrit,burst,irecq1,
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
     &    useprim,(isetmr(i),i=1,ncyc2),ratemax,nmr,nlig,
     &    (conc_ec1(i),i=1,nlig),
     &    (conc_ec2(i),i=1,nlig),pathdata,pathmec
			nbl=len_trim(qfilem)
				ilp=1
				do i=1,nbl
					if(qfilem(i:i).eq.'\') ilp=i
				enddo
	            nb=len_trim(pathmec)
				if(ilp.eq.1) then
				
					qmec=pathmec(1:nb)//qfilem
				
				else
					
					qmec=pathmec(1:nb)//qfilem(ilp+1:nbl)

				endif
	            nbd=len_trim(pathdata)
				do j=1,nset
					do l=1,nfileb(i)
					nbl=len_trim(pfileb(j,l))
				    ilp=1
				    do i=1,nbl
						if(pfileb(j,l)(i:i).eq.'\') ilp=i
					enddo

					if(ilp.eq.1) then
				
					pfiles(j,l)=pathdata(1:nbd)//pfileb(j,l)
				
				else
					
					pfiles(j,l)=pathdata(1:nbd)//pfileb(j,l)(ilp+1:nbl)

				endif
				enddo
				enddo
				iftype=100		 
	   write(19,rec=1) iftype,pfiles,nfix,jfix,neq,IE,JE,EFAC,
     &   IF,JF,jcon,IM2,JM2,jmic,ndisp,irecq,ans1,ylo,yhi,nrange,idiskq,
     &   nfileb,kfile,nset,conc,tcrit,burst,irecq1,
     &    idatyp,qfile,imodold,setbad,tcbad,onechan,
     &    nbad1,isbad,iebad,tresolb,ans3,ans4,nlvar,
     &    fixec50,ec50,i50,j50,m50,xqlo,xqhi,kAm,kFm,
     &    chsvec,ncyc2,
     &    (nsc2(i),i=1,ncyc2),
     &    ((im2(i,j),j=1,nsc2(i)),i=1,ncyc2),
     &    ((jm2(i,j),j=1,nsc2(i)),i=1,ncyc2),
     &    assmax,qmec,nsim,irect,logsav,imodolds,
     &    badend,iexcop,gaplo,gaphi,dcmod,
     &    slopsch,checkgrp,rcrit,ngpcheck,
     &    nstab1,gfac1,nstab2,gfac2,ans5,
     &    nfixec50,ec502,i502,j502,m502,xqlo2,xqhi2,
     &    useprim,(isetmr(i),i=1,ncyc2),ratemax,nmr,nlig,
     &    (conc_ec1(i),i=1,nlig),
     &    (conc_ec2(i),i=1,nlig)
	   else
	   read(19,rec=1) pfileb,nfix,jfix,neq,IE,JE,EFAC,
     &   IF,JF,jcon,IM2,JM2,jmic,ndisp,irecq,ans1,ylo,yhi,nrange,idiskq,
     &   nfileb,kfile,nset,conc,tcrit,burst,irecq1,
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
     &    useprim,(isetmr(i),i=1,ncyc2),ratemax,nmr,nlig,
     &    (conc_ec1(i),i=1,nlig),
     &    (conc_ec2(i),i=1,nlig)
		
		endif
	endif
	    CLOSE(unit=19)
	    if(kAm.lt.1.or.kAm.gt.20) then
		kAM=10	!max number of open states
		kFm=90	!max number of shut states
		km=100	!total number of states
	    endif
	    allmr=.true.
	    do i=1,ncyc2		!if nsc(i) is neg, set obeymr(i)=F and restore nsc
		if(nsc2(i).lt.0) then
		   obeymr(i)=.false.
		   nsc2(i)=iabs(nsc2(i))
	       allmr=.false.
		else
		   obeymr(i)=.false.
		endif
	    
	    if(assmax.lt.1.d4.or.assmax.gt.1.d12) assmax=1.d9	!max assoc rate
	   if(im2(i,1).lt.0) then
		   automr(i)=.false.
		   im2(i,1)=iabs(im2(i,1))
		else
		   automr(i)=.true.
		endif
	    enddo
	    if(assmax.lt.1.d4.or.assmax.gt.1.d12) assmax=1.d9	!max assoc rate
	    if(ratemax.lt.1.d3.or.ratemax.gt.1.d6) ratemax=1.d6  
	  endif
	  
	continue
	do i=1,nset
c		chsvec(i)=.true.
	enddo
	end