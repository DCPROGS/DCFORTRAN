	subroutine READ_EDR(testfil,nint,ioff,adcmax,ad,
     &           nchan,id1,id2,calfac,srate,filt,vpa,pav,ioff0,gainfac,
     &           calfac1,srate1,filt1,vpa1,pav1,ioff1,gainfac1)
     
     
c Subroutine to read header of Dempster files (*.edr format)
c and print the (few) details.
cHeader Block
cThe header block contains the information needed to allow a program to determine the
cnumber of channels, samples etc. in the file. It is usually the first block to be read when a file
cis opened. File parameters are stored as ASCII text in the form of keywords, one word per
cline, as follows
cKEY= <value> <cr> <lf>
cwhere <value> is a number or text depending on the parameter and <cr> <lf> are the carriage
creturn and line feed characters. A typical header block (from a file with 2 channels) contains
cthe following keywords.
cVER=6.4 <cr><lf> EDR file version number
cNC=2 <cr><lf> No. of analogue input channels
cNP=102400 <cr><lf> No. of A/D samples in data block
cNBH=2048 <cr><lf> No. of bytes in file header block
cAD=5.0000 <cr> <lf> A/D converter upper limit of voltage range (V)
cADCMAX=4095 <cr><lf> Maximum A/D sample value
cDT=.1600 <cr><lf> A/D sampling interval (s)
cYN0=Im <cr> <lf> Channel 0 name (n=0 .. NC-1)
cYU0=nA <cr> <lf> Channel 0 units
cYCF0=0.0001 <cr> <lf> Channel 0 calibration factor V/units
cYAG0=10.0 <cr> <lf> Channel 0 gain factor
cYZ0=1024 <cr> <lf> Channel 0 zero level (A/D bits)
cYO0=0 <cr> <lf> Channel 0 offset into sample group in data block
cYN1=Im <cr> <lf> Channel 1 name (n=0 .. NC-1)
cYU1=nA <cr> <lf> Channel 1 units
cYCF1=0.01 <cr> <lf> Channel 1 calibration factor V/units
cYAG1=1.0 <cr> <lf> Channel 1 gain factor
cYZ1=1024 <cr> <lf> Channel 1 zero level (A/D bits)
cYO1=1 <cr> <lf> Channel 1 offset into sample group in data block
cTU=ms <cr> <lf> Time units
cID= Cell 1 <cr> <lf> Experiment identification line
cBAK=T1 <cr> <lf> BAK=T indicates a .BAK file exist
cEvent detector parameters
cDETCH=0 <cr><lf> Event detector channel
cDETRS=1024 <cr><lf> No. samples in event detector record
cDETYT=500 <cr><lf> Event detector amplitude threshold (A/D units)
cDETTT=5E-3 <cr><lf> Event detector.time threshold (s)
cDETDD=5-E2 <cr><lf> Event detector dead time (s)
cDETBA=5E-2 <cr><lf> Event detector.baseline averaging time (s)
cDETPT=10.0 <cr><lf> Event detector pretrigger percentage
cSingle-channel analysis parameters
cVARRS=1024 <cr><lf> No. samples in variance record
cVAROV=0 <cr><lf> Percentage overlap of variance records
cVARTR=0.0005 <cr><lf> MEPC rise time (MEPC freq. analysis) (s)
cVARTD=0.005 <cr><lf> MEPC decay time (MEPC freq. analysis) (s)
cUNITC=1.0 <cr><lf> Single channel current
cDWTTH=50 <cr><lf> Transition detection threshold (%)
cNote. It should not be assumed that the keywords will follow any particular order.
cData block
cThe data block contains the digitised signals, stored in the form of 16 bit binary integers. Each
cA/D sample takes up 2 bytes of space. The size of the data block is determined by the number
cof channels and number of samples per channels in the record

	character*60 testfil	!path names for .EDR files
	integer*4    ioff,ilen,DATEW,time,actualength
	character    cDATE*11,istring*11,adctime*8,title*76
	character*20 ascnum,ascnum1
	character*20 colhead(1)
	character*20 text(500)
	allocatable::string
	character*1 string(:)
	logical present,graph,dcfile,good
	character*4  filetype
	integer*2   inchan
	
	logical discprt
	common/dp/discprt
      ncols=1
      nrows=1
      dcfile=.false.
      len=2048
      i1=1
      n=0
      allocate(string(1:len))
      OPEN(unit=11,file=testFIL,status='UNKNOWN',
     &   access='DIRECT',form='BINARY',RECL=1)
	read(11,REC=1) string
	CLOSE(unit=11)
	
	lenmax=len
	do i=1,len-1
	  ival=ichar(string(i))
	  ival1=ichar(string(i+1))
	  if(ival.eq.0.and.ival1.eq.0) then
	      lenmax=i+1
	      goto 10
	  endif
	enddo
10    continue	
      isep=0
	iend=0
	istart=1
	inext=istart
	n=0		!counts converted numbers
	i=1		!row number
	j=1		!col number
	do while(inext.lt.lenmax)
	   i1=inext
	   call FINDSEP(i1,string,lenmax,isep,iend,inext)
	   n=iend-i1+1
	   ascnum='                  '
	   do k=1,n
	      if(k.le.18) ascnum(k:k)=string(k+i1-1)
	   enddo
	   if(ascnum.eq.'              ') then
	      goto 20
	      
	   else
	   text(j)=ascnum
	   
	   n=n+1
		 j=j+1		!next row
	   endif
	enddo
      	
20    continue
	ntext=j
	
	do i=1,ntext
	  do j=1,20
	    ival=ichar(text(i)(j:j))
	    
	    if(ival.eq.61) then
	      if(text(i)(1:j-1).eq.'ms') then
	          ascnum=text(i)
	      else if(text(i)(1:j-1).eq.'nA') then
	          ascnum1=text(i)    
	      else
	          call chtoreal(text(i)(j+1:20),realx)
	          if(text(i)(1:j-1).eq.'VER') then
	          ver=realx
	          else if (text(i)(1:j-1).eq.'NC') then
	          Rnc=realx
	          else if (text(i)(1:j-1).eq.'NP') then
	          Rnp=realx   
	          else if (text(i)(1:j-1).eq.'NBH') then
	          Rnbh=realx
	           else if (text(i)(1:j-1).eq.'AD') then
	          ad=realx   
	    
	          else if (text(i)(1:j-1).eq.'ADCMAX') then
	          adcmax=realx
	          else if (text(i)(1:j-1).eq.'DT') then
	          dt=realx   
	          else if (text(i)(1:j-1).eq.'YN0') then
	          yn0=realx
	          else if (text(i)(1:j-1).eq.'YU0') then
	          yu0=realx   
	          else if (text(i)(1:j-1).eq.'YCF0') then
	          ycf0=realx
	          else if (text(i)(1:j-1).eq.'YAG0') then
	          yag0=realx   
	          else if (text(i)(1:j-1).eq.'YZ0') then
	          yz0=realx
	          else if (text(i)(1:j-1).eq.'YO0') then
	          yo0=realx   
	          else if (text(i)(1:j-1).eq.'YN1') then
	          yn1=realx
	          else if (text(i)(1:j-1).eq.'YU1') then
	          yu1=realx   
	          else if (text(i)(1:j-1).eq.'YCF1') then
	          ycf1=realx
	          else if (text(i)(1:j-1).eq.'YAG1') then
	          yag1=realx   
	          else if (text(i)(1:j-1).eq.'YZ1') then
	          yz1=realx
	          else if (text(i)(1:j-1).eq.'YO1') then
	          yo1=realx   
	          else if (text(i)(1:j-1).eq.'DETCH') then
	          detch=realx
	          else if (text(i)(1:j-1).eq.'DETRS') then
	          detrs=realx   
	          else if (text(i)(1:j-1).eq.'DETYP') then
	          detyt=realx
	          else if (text(i)(1:j-1).eq.'DETTT') then
	          dettt=realx   
	          else if (text(i)(1:j-1).eq.'DETDD') then
	          detdd=realx
	          else if (text(i)(1:j-1).eq.'DETBA') then
	          detba=realx   
	          else if (text(i)(1:j-1).eq.'DETPT') then
	          detpt=realx
	          else if (text(i)(1:j-1).eq.'VARRS') then
	          varrs=realx   
	          else if (text(i)(1:j-1).eq.'VAROV') then
	          varov=realx   
	          else if (text(i)(1:j-1).eq.'VARTR') then
	          vartr=realx
	          else if (text(i)(1:j-1).eq.'VARTD') then
	          vartd=realx   
	          else if (text(i)(1:j-1).eq.'UNITC') then
	          unitc=realx
	          else if (text(i)(1:j-1).eq.'DWTTH') then
	          dwtth=realx   
	          endif
	     endif
           goto 30
          endif
	    enddo
30        continue
	  enddo
	
	
	
	nchan=Rnc
	ioff=Rnbh
	srate=dt*1e6
	gainfac=yag0
	ioff0=yo0
	
	filt=5000.
	
	nint=Rnp
	
	VpA=ycf0
	if(ascnum1.eq.'nA') VpA=ycf0*1e-3
	pAv=1/vpa
	calfac=pav/(adcmax/ad)
	
	VpA1=ycf1
	if(ascnum1.eq.'nA') VpA1=ycf1*1e-3
	pAv1=1/vpa1
	gainfac1=yag1
	
	ioff1=yo1
	filt1=5000.
	
	calfac1=pav1/(adcmax/ad)
	
	RETURN
	end


