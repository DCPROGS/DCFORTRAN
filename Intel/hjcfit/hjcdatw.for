	subroutine HJCDATw(main,hjcfitform,idatyp,nfile,kfile,pfile,nval,
     & irecs,calfacs2,stpfac,nintt,
     & avamp,rms,ffilt,trise,fc,iscan,readini,ndev,idest,
     & name,idiscn,treso,tresg,nset,conc,nlig,ligname,nmax,ntot,
     & qfile2,adcfil,nfileb,pfileb,npatch,defname,samexp,
     & cjump,nsweep,tzero,tsamp,autosim,ipatch,ptype,temp,tedit1,tedit2,
     & tedit3,tedit4,tval1,tval2,tval3,tval4,tval5,tval6,tval7,tval8,
     & tval9,tvalt,tedit5,val9)

	use menu_f90
	real*8 stpfac,confac,resfac,errfac,delmin,smin
c	real*4 stpfac,confac,resfac,errfac

	integer main,hjcfitform,val9(20,10)
	integer*4 jstrec(200)
	character mtitle*40,filnam*32,prtport*4	!for WINPRINT
	character ndev*2,name*12,ans*1,UC*1,expDATEW*11,title*70
	character tapeID*24,expnum*9
	
	real*4 conc(10,10)
	real*4 conc1(10)	!to read conc from .scn file		
	character*20 ligname(10)
	logical contig,onedisk
	integer kfile(20,10),nfile(10),nfileb(10),nintt(10)
	character*60 pfileb(20,10)	!path names for .SCN files
	character*60 pfile(20,10)
	real*4 calfacs2(20,10)
	integer nval(20,10),irecs(20,10)
	character*14 ptype(5)
	LOGICAL present,autosim
	logical sbin,shist,sres,sexp,samexp
	logical discprt,readini,cjump
c The following are now arrays
	real*4 avamp(10),rms(10),fc(10),ffilt(10),trise(10)
c Extra declarations for new scan.dat files
	real*8 dfinter,tlast
	real*8 tzerod,tsample
	logical invert,opendown,newpar,sdone
	logical disp,usepots,simulat
	logical disptran,dispderiv,dispguess
	logical shut,shutprev,shutsav,goback,backward
	character ADCFIL*30,cDATEW*11,cDATEW1*11,filtfile*20
	character*1 qfile1*35	!as in scan.dat
	character*1 qfile*40	!as in vplot and vhist
	character*1 qfile2*40	!use input -ignore value from disk!
	character adctimew*8
	character defname*6
	character cnum1*11,cnum2*11
c  For parsname etc
	character path*30,path1*30,pname*8,suffix*3	!for parsname
	logical nopath
	character*40 tedit1(20,10),tedit2(20,10),tedit3(20,10)
	character*40 tedit5(20,10),tedit4(20,10)
	real tval1(20,10),tval2(20,10),tval3(20,10),tval4(20,10)
	real tval5(20,10),tval6(20,10)
	
	real tvalt(5),tval7(20,10),tval8(20,10),tval9(20,10)
	
	common/dp/discprt
	common/sblk/sbin,shist,sres,sexp
c	common/dpp/filnam,machine,ndisc,icol	!for WINPRINT,ENDPRINT,DISCNUM
	common/dpp/filnam,prtport,ndisc,icol,mtitle !for WINPRINT,ENDPRINT,DISCNUM

	ptype(1)='outside-out'
	ptype(2)='inside-out'
	ptype(3)='cell-attached'
	ptype(4)='whole-cell'
	ptype(5)='simulated data'

	samexp=.false.
      ifile=0
      ifile1=0
	atemp=0.0		!for mean temp
	aavamp=0.0		!for mean avamp
	arms=0.0		!for mean rms
	aEmem=0.0		!for mean Emem
	n3=0
	ntot=0
	nmax=0		!largest of the nintt(j)
c	
	
	
	do j=1,nset
		nsc1=1
		do i=1,nfile(j)
		   jexp=nsc1+i-1	!expt number
		   kfile(i,j)=jexp
		enddo
	enddo

	OPEN(unit=18,file=pfile(1,1),status='UNKNOWN',
     &	access='DIRECT', form='BINARY', RECL=1)
      read(18,rec=1) iscan
	if(iscan.eq.103.or.iscan.eq.104.or.iscan.eq.-103) then
		idatyp=1
		read(unit=18,rec=1) iscan,ioffset
	else
		read(18,rec=1) name,idiscn,nfirst,nfiles,jstrec,nextrec
		if(idiscn.ne.0) then
			idatyp=2
	        ifile=kfile(1,1)
	        irec=jstrec(ifile-nfirst+1)	!where to start reading header
	        read(18,rec=irec)expnum,ifile1,kt,iscan	!get iscan
          endif 
	endif
	
	iscan1=iscan
	simulat=iscan.eq.-3001.or.iscan.eq.-103	!from SCSIM
c
c Now read all files
	do j=1,nset
	 nintt(j)=0		!# of obs for set j
	 do i=1,nfile(j)
c=	   if(pfile(i,j).ne.pfile(1,j)) then
		CLOSE(unit=18)
		OPEN(unit=18,file=pfile(i,j),status='UNKNOWN',
     &		access='DIRECT', form='BINARY', RECL=1)
c=	   endif
		read(18,rec=1) iscan
		if(iscan.eq.103.or.iscan.eq.104.or.iscan.eq.-103) then
			idatyp=1
			read(unit=18,rec=1) iscan,ioffset
		else
			read(18,rec=1) name,idiscn,nfirst,nfiles,jstrec,nextrec
			if(idiscn.ne.0) then
			idatyp=2
	        ifile=kfile(1,1)
	        irec=jstrec(ifile-nfirst+1)	!where to start reading header
	        read(18,rec=irec)expnum,ifile1,kt,iscan	!get iscan
			endif 
		endif
	   if(idatyp.eq.2) then
		read(18,rec=1) name,idiscn,nfirst,nfiles,jstrec,nextrec
		ifile=kfile(i,j)
		irec=jstrec(ifile-nfirst+1)	!where to start reading header
		if(iscan.ne.-3001) then
	  	   read(18,rec=irec)expnum,ifile1,kt,iscan,expDATEW,title,
     &  tapeID,ipatch,Emem,temp,ffilt(j),avamp(j),rms(j),calfac2,ioffset
		else	!read also imposed resolution for simulated data
		   read(18,rec=irec)expnum,ifile1,kt,iscan,expDATEW,title,
     & tapeID,ipatch,Emem,temp,ffilt(j),avamp(j),rms(j),calfac2,ioffset,
     &	    treso,tresg
		endif
c		irec1=irec+ioffset      !where to start reading data -record here
		irecs(i,j)=irec + ioffset
	   else if(idatyp.eq.1) then
c  DC fixed 20-0307 for iscanver=104, read nlig and conc()
c		if(iscan.ne.-103) then
		if(iscan.eq.103) then
		 read(unit=18,rec=1) iscan,ioffset,nint,title,expDATEW,
     &     defname,tapeID,ipatch,npatch,Emem,temper,adcfil,qfile1,cjump,
     &	 nfits,ntmax,nfmax,nbuf,novlap,srate,finter,
     &	 tsect,ioff,ndat,nsec,nrlast,avtot,navamp,avamp(j),rms(j),
     &	 nwrit,nwsav,newpar,opendown,invert,
     &	 usepots,disp,smult,scrit,vary,ntrig,navtest,dgain,iboff,
     &	 expfac,bdisp,ibflag,iautosub,xtrig,ndev,cDATEW,adctimew,
     &	 nsetup,filtfile,ffilt(j),npfilt,
     &	 sfac1,sfac2,sfac3,nscale,calfac,calfac1,calfac2,iyoff,
     &	 ioff1,disptran,dispderiv,dispguess,ampfac,tmin,
     &	 tsfac,tlfac,sdone,dfinter,tlast,
     &	 shut,shutprev,backward,prevlevel,t0sav,y0sav,vard,nshutfit,
     &	 infit,infirst,ixfprev,idiskq,ifirst,base,basevga,ibasevga,
     &	 itrig,itrigvga,itriglev,inc,incabs,
     &	 indfst,isdfst,isec,ndisp,ndisp1,
     &	 idatyp1,cDATEW1,nchan,
     &	 tcfac,facjump,shutsav,goback,
     &	 imin,imax,errfac,derivfac,confac,
     &	 nsweep,njdim,tzerod,intzero,tsample,ktjump,njfit,
     &	 njump,nnull,ktlast,
     &	 izoom,fcz,fczoom,ampz,avampsav,
     &	 itsimp,minmeth,nbasemin,iscrit,scritvar,smultmin
		else if(iscan.eq.104) then
		 read(unit=18,rec=1) iscan,ioffset,nint,title,expDATEW,
     &     defname,tapeID,ipatch,npatch,Emem,temper,adcfil,qfile1,cjump,
     &	 nfits,ntmax,nfmax,nbuf,novlap,srate,finter,
     &	 tsect,ioff,ndat,nsec,nrlast,avtot,navamp,avamp(j),rms(j),
     &	 nwrit,nwsav,newpar,opendown,invert,
     &	 usepots,disp,smult,scrit,vary,ntrig,navtest,dgain,iboff,
     &	 expfac,bdisp,ibflag,iautosub,xtrig,ndev,cDATEW,adctimew,
     &	 nsetup,filtfile,ffilt(j),npfilt,
     &	 sfac1,sfac2,sfac3,nscale,calfac,calfac1,calfac2,iyoff,
     &	 ioff1,disptran,dispderiv,dispguess,ampfac,tmin,
     &	 tsfac,tlfac,sdone,dfinter,tlast,
     &	 shut,shutprev,backward,prevlevel,t0sav,y0sav,vard,nshutfit,
     &	 infit,infirst,ixfprev,idiskq,ifirst,base,basevga,ibasevga,
     &	 itrig,itrigvga,itriglev,inc,incabs,
     &	 indfst,isdfst,isec,ndisp,ndisp1,
     &	 idatyp1,cDATEW1,nchan,
     &	 tcfac,facjump,shutsav,goback,
     &	 imin,imax,errfac,derivfac,confac,
     &	 nsweep,njdim,tzerod,intzero,tsample,ktjump,njfit,
     &	 njump,nnull,ktlast,
     &	 izoom,fcz,fczoom,ampz,avampsav,
     &	 itsimp,minmeth,nbasemin,iscrit,scritvar,smultmin,
     &	 stpfac,nlig,conc1
c read whole of conc1(10) as written in SCAN (in micromolar)
		 do i1=1,nlig
			conc(i1,j)=conc1(i1)*1.e-6		!molar
		 enddo
c
		else if(iscan.eq.-103) then		!simulated (see dskwrit2)
		 read(18,rec=1) iscanver,ioffset,nint,title,expDATEW,
     &	  tapeID,ipatch,Emem,cjump,avamp(j),rms(j),ffilt(j),calfac2,
     &	  treso,tresg
		 if(cjump) then
		    read(18,rec=1) iscanver,ioffset,nint,title,expDATEW,
     &        tapeID,ipatch,Emem,cjump,avamp(j),rms(j),ffilt(j),calfac2,
     &	    treso,tresg,nsweep,tzero,tsamp
		 endif
		endif
		irecs(i,j)=ioffset
		kt=nint
		temp=temper
		qfile(1:35)=qfile1
c	
	   endif
	   tzero=sngl(tzerod)
	   tsamp=sngl(tsample)
	   tzero=tzero/1000.    !ms
	   tsamp=tsamp/1000.    !ms
c
	   ntot=ntot+kt
	   nintt(j)=nintt(j) + kt 	!accumulate total number for set j
	   nval(i,j)=kt
	   calfacs2(i,j)=calfac2
	   avamp(j)=avamp(j)*calfac2		!convert to pA
	   if(ifile.ne.ifile1) then
		Icall= gmDisplayMessageBox('',' ERROR file',Gstop,GOK)
		stop
	   endif
c Check that filter setting and patch type same for all files
	   if(i.eq.1) then
		ffilt1=ffilt(j)
		ipatch1=ipatch
		Emem1=Emem
	   else
c=		if(ffilt(j).ne.ffilt1) then
		if(abs(ffilt(j)-ffilt1).gt.1.e-20) then
		 	Icall= gmDisplayMessageBox('','WARNING: filter setting for
     &this file not same as that for first file  ',Ginformation,GOK)
	         if(discprt) write(7,40) i,ffilt(j),ffilt1
40		   format(' WARNING: filter setting for file #',i3,
     &		' not same as that for first file (',2g13.6,')')
		endif
		if(ipatch.ne.ipatch1) then
		  	Icall= gmDisplayMessageBox('','WARNING: patch type for
     &this file not same as that for first file   ',Ginformation,GOK)
	         if(discprt) write(7,41) ipatch,i,ipatch1
41		   format(' WARNING: patch type for file #',i3,
     &		' not same as that for first file (',2i4,')')
		endif
c=		if(Emem.ne.Emem1) then
		if(abs(Emem-Emem1).gt.1.e-20) then
		 
		Icall= gmDisplayMessageBox('','WARNING: potential for
     &this file not same as that for first file   ',Ginformation,GOK)
	         if(discprt) write(7,42) i,Emem,Emem1
42		   format(' WARNING: potential for file #',i3,
     &		' not same as that for first file (',2g13.6,')')
		endif
		if(iscan.ne.iscan1) then
		  
	        Icall= gmDisplayMessageBox('','WARNING: ISCAN for
     &this file not same as that for first file   ',Ginformation,GOK) 
	         if(discprt) write(7,44) i,iscan,iscan1
44		   format(' WARNING: ISCAN for file #',i3,
     &		' not same as that for first file (',2i6,')')
		endif
	   endif
c	 calc means
	   atemp=atemp+temp
	   aavamp=aavamp+avamp(j)
	   arms=arms+rms(j)
	   aEmem=aEmem+Emem
	   n3=n3+1
		tval9(i,j)=conc(i,j)*10**6
	   if(iscan.ge.99) then		!new data disk from PC SCAN
	    n=len_trim(pfile(i,j))
	    if(idatyp.eq.2) then
			tedit1(i,j)=pfile(i,j)
			tedit2(i,j)=expDATEW
			tedit3(i,j)=title
			tedit4(i,j)=tapeid
			tval1(i,j)=kt
			tval2(i,j)=npatch
			tval3(i,j)=emem
			tval4(i,j)=temp
			tval5(i,j)=ffilt(j)
			tval6(i,j)=avamp(j)
			tval7(i,j)=rms(j)
			tval8(i,j)=calfac2
			tval9(i,j)=conc(i,j)*10**6
			tedit5(i,j)=ptype(ipatch)
	if(discprt) write(7,322)
	      if(discprt) write(7,38) j,ifile,kt,pfile(i,j)(1:n),expDATEW,
     & 	title,tapeID,ptype(ipatch),Emem,temp,
     &      ffilt(j),avamp(j),rms(j),calfac2
38		format(/,' SET # ',i3,/,
     &     ' Experiment file ',i4,': ',i6,' transitions from ',a60,
     &    '; ',a11,/,
     &    ' Title: ',a120,/,
     &    ' Tape details: ',a24,4x,
     &    ' Patch type: ',a14,4x,/,
     &    ' Membrane potential (mV) = ',f9.2,4x,
     &    ' Temperature = ',f9.1,/,
     &    ' Filter (kHz, -3 dB) = ',f12.5,4x,
     &    ' Full amplitude (pA) (approx) = ',f9.2,/,
     &    ' RMS noise (pA) = ',f10.4,4x,
     &    ' Calibration (amplitude to pA) = ',g13.6)
	    else if(idatyp.eq.1) then
			tedit1(i,j)=pfile(i,j)
			tedit2(i,j)=expDATEW
			tedit3(i,j)=title
			tedit4(i,j)=tapeid
			tval1(i,j)=kt
			tval2(i,j)=npatch
			tval3(i,j)=emem
			tval4(i,j)=temp
			tval5(i,j)=ffilt(j)
			tval6(i,j)=avamp(j)
			tval7(i,j)=rms(j)
			tval8(i,j)=calfac2
			tval9(i,j)=conc(i,j)*10**6
	tedit5(i,j)=ptype(ipatch)
      	if(discprt) write(7,322)	
      	if(discprt) write(7,28) j,title,pfile(i,j)(1:n),expDATEW,
     &	 npatch,nint,adcfil,tapeID,ptype(ipatch),Emem,
     &	 temper,ffilt(j),avamp(j),rms(j),calfac2,qfile2

28		format(/,' SET # ',i3,/,
     &       ' Title: ',a120,/,
     &	 ' Results file: ',a60,/,
     &	 ' DATEW of experiment: ',a11,' (patch number ',i3,')'/,
     &	 ' Number of transitions = ',i8,/,
     &	 ' Raw data file: ',a30,/,
     &	 ' Tape details: ',a24,/,
     &	 ' Patch type: ',a14,/,
     &	 ' Membrane potential (mV) = ',f9.2,/,
     &	 ' Temperature = ',f9.1,/,
     &       ' Filter (kHz, -3 dB) = ',f12.5,4x,/,
     &       ' Full amplitude (pA) (approx) = ',f9.2,/,
     &       ' RMS noise (pA) = ',f10.4,4x,/,
     &       ' Calibration (amplitude to pA) = ',g13.6,/,
     &	 ' Plot queue file: ',a40)
	    endif
c	   else if(iscan.eq.-3001) then	!converted simulations disk
	   else if(simulat) then	!simulation
		tedit1(i,j)='Simulated data'
			tedit2(i,j)=expDATEW
			tedit3(i,j)=title
			tedit4(i,j)=tapeid
			tval1(i,j)=kt
			tval2(i,j)=npatch
			tval3(i,j)=emem
			tval4(i,j)=temp
			tval5(i,j)=ffilt(j)
			tval6(i,j)=avamp(j)
			tval7(i,j)=rms(j)
			tval8(i,j)=calfac2
	tval9(i,j)=conc(i,j)*10**6
c	tval9(i,j)=ipatch
      tedit5(i,j)=ptype(ipatch)
		to=1000.*treso
		tg=1000.*tresg
	if(discprt) write(7,322)
322	format(/,' DEFINE FILE FOR EXPERIMENTAL DATA (.scn)')
	      if(discprt) write(7,320)ifile,title,expDATEW,kt,to,tg,
     &	ffilt(j),avamp(j),rms(j),calfac2
320		format(' Ex',i5,': ',a120,/,' Analysis DATEW ',a11,
     &	   ':  ',i6,' transitions. Simulated data; res = ',
     &	   f7.1,',',f7.1,' microsec (open, shut)',/,
     &	   ' Filter (-3dB)',f9.1,' Hz : Full amp (app) ',f8.2,'pA',
     &	   ': RMS noise ',f9.3,'pA',/,
     &       ' Calibration (amplitude to pA) = ',g13.6)
	   else if(iscan.lt.0) then	!converted old disk
	tedit1(i,j)=''
			tedit2(i,j)=expDATEW
			tedit3(i,j)=title
			tedit4(i,j)=tapeid
			tval1(i,j)=kt
			tval2(i,j)=npatch
			tval3(i,j)=emem
			tval4(i,j)=temp
			tval5(i,j)=ffilt(j)
			tval6(i,j)=avamp(j)
			tval7(i,j)=rms(j)
			tval8(i,j)=calfac2
	tval9(i,j)=conc(i,j)*10**6
      tedit5(i,j)=ptype(ipatch)
	      if(discprt) write(7,319)ifile,title,expDATEW,kt,
     &	ffilt(j),avamp(j),rms(j),calfac2
319		format(' Ex',i5,': ',a120,/,' Analysis DATEW ',a11,
     &	 ':  ',i6,' transitions (data from PDP SCAN)',/,
     &	 ' Filter (-3dB)',f9.1,' Hz : Full amp (app) ',f8.2,' pA',
     &	 ': RMS noise ',f9.3,' pA',/,
     &       ' Calibration (amplitude to pA) = ',g13.6)
	   endif
	  
c
	 enddo		!end if i=1,nfile loop
	 if(nintt(j).gt.nmax) nmax=nintt(j)
	enddo		!end if j=1,nset loop
c
	CLOSE(unit=18)
c
c    calc mean temp, avamp, rms: and check ffilt, ipatch same for all
	if(iscan.ge.99) then		!new data disk from PC SCAN
	   en=float(n3)
	   temp=atemp/en
	   aavamp=aavamp/en
	   arms=arms/en
	   Emem=aEmem/en
c
		tvalt(1)=ntot 
		tvalt(2)= temp
		tvalt(3)= aavamp
		tvalt(4)= arms
		tvalt(5)= emem
	  
	  
	  
	 
         if(discprt) write(7,39) ntot,nset,temp,aavamp,arms,Emem
39	   format(/,
     &     ' Total number of transitions = ',i8,', in ',i4,' sets.',/,
     &     ' Temperature (mean) = ',f9.1,/,
     &     ' Approx full amplitude (mean) = ',f9.2,/,
     &     ' RMS baseline noise (mean) = ',f10.4,/,
     &     ' Potential (mV) (mean) = ',f9.2,/)
	else
         en=float(n3)
	   temp=atemp/en
	   aavamp=aavamp/en
	   arms=arms/en
	   Emem=aEmem/en
c
		tvalt(1)=ntot 
		tvalt(2)= temp
		tvalt(3)= aavamp
		tvalt(4)= arms
		tvalt(5)= emem
         if(discprt) write(7,46) ntot,nset
46	   format(/,
     &     ' Total number of transitions = ',i8,', in ',i4,' sets.',/)
	endif
c	
	
	do j=1,nset
		if(ffilt(j).le.0.) then
      		if(discprt) write(7,45) j
45			format(' Filtering and rise-time not defined for set ',i3)
			idest=0
		 else
			fc(j)=ffilt(j)		!kHz (-3dB)
			trise(j)=332.1/fc(j)		!rise time (microsec)
	   
		endif
	enddo
	
	if(main.ne.-1) then 
	idest=0
	icall=3031
	call hjcfit_table(main,hjcfitform,nset,nfile,pfile,tedit1,tedit2,
     & tedit3,tedit4,tval1,tval2,tval3,tval4,tval5,tval6,tval7,tval8,
     & tval9,tvalt,tedit5,fc,ffilt,trise,autosim,icall,val9,idatyp)
	endif
	RETURN

99	continue
	idest=99
	RETURN
	end


	