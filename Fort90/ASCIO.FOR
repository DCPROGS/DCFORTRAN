	subroutine ASCIO(data,ndat,jcol,ndim1,ndim2,write,icolwid,colhead)
c
c Lahey V5 version 12/09/94 03:01pm (now assumes data(ndim1,ndim2) has
c been allocated before this routine called.
c To do ASCII input/output in Schroeter-Hausser format
c ASCII is in table form.  Each column has leading blank and characters
c are right justified in each col.
c NB in this version reals use 12 characters so icolwid must be
c at least 13 (to leave room for leading blank)
c character colhead*30(10) contains (ndat, up to 10) column headings;
c  length of text in each must be not more than icolwid-1
c The binary data is in DATA(ndim1,ndim2) where ndim=dimension of DATA
c in calling program.  There are NDAT values in each column, and JCOL
c columns are used, i.e. data(i,j) i=1,...,ndat; j=1,...,jcol
c Number of columns=4 (dictated by 1st line) if jcol<4 used for data
c Number of cols in table=jcol if jcol=>4 (dictated by data)
c  ICOLWID characters per column  (inc leading blank)
c  NCOL=number of columns in the table
c  NDLINE=number of data lines =NDAT
c  NTLINE=number of text lines (assumed=0 here)
c  WRITE=true if data binary data to be written to disc as ASCII
c       =false if ASCII data to be read from disc and returned as binary
c row 1= icolwid ncol ndline ntline (+blanks as nec if ncol>4)
c row 2= column headings (e.g. time voltage current . . .)
c rows 3 to ndline+2 = data
c rows to end are text lines (ntline in number)
c
c ASCII data is in ASCDATA(i) with characters stored sequentially, separated
c by at least one blank in each col within a row, and rows separated by
c char(13)=CR, char(10)=LF
c
c Modif 09/08/95 03:42pm to fix errors in allocation of ascdata(:)
	real*4 data(ndim1,ndim2)
	integer*2 videotyp
	allocatable::ascdata
	character*1 ascdata(:)
	character*1 blank,cr,lf
c	character bline*80	!to blank lines
	logical write,present,discprt,alpha
	character filnam*30,UC*1,ans*1
	character colhead*30(10)
	character*11 istring
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	logical caplock,debug
	common/dp/discprt
c
	debug()=caplock()
c
	alpha=VIDEOTYP().eq.3
	filnam='                             '
	blank=char(32)
	cr=char(13)
	lf=char(10)
c	do 14 i=1,80
c14	  bline(i:i)=blank
c	bline(1:1)='&'
c
	if(.not.write) goto 100
c
c WRITE SECTION
c
	if(icolwid.lt.13) then
	   call BELL(3)
	   if(alpha) then
		print 12,icolwid
12		format(' Called with column width = ',i3,': changed to 13')
	   else
		call WDIALOG(1,' Column width changed to 13',11)
	      icolwid=13
	   endif
	endif
c
	if(jcol.lt.4) then
	   ncol=4
	else
	   ncol=jcol
	endif
	ndline=ndat
	ntline=0		!no text lines at end in this version
c
c Get file name
13	continue
	if(alpha) then
	   call TITENT1('File name for ASCII output (+ path if nec)',
     &	filnam,30,.true.)
	else
c	      call TITENT1('File name for ASCII output (+ path if nec)',
c     &	filnam,30,.true.)
	      call Tdialog(1,'File name for ASCII output (+ path if nec)',
     &	filnam,30,.true.,11)
	endif
c Check if file is present (for WRITE, ask if it is to be overwritten)
c and then open it (NEW if not present) as TRANSPARENT file
	INQUIRE(file=filnam,exist=present,flen=len)
	if(present.and.len.gt.0) then
	   call BELL(2)
	   if(alpha) then
		print 11,filnam
11		format(
     &	' The file ',a30,' already exists: overwrite it [Y] ? ')
	   else
		ans='N'
 		call DEFOLTa(ans,defolt)
		call QDIALOG(1,
     &    'The file '//charnb(filnam)//' already exists: overwrite it'
     &        ,defolt,12,cans)
 		call GETINPa(cans,ans)
	   endif
c==============
	   read 101,ans
101	   format(a1)
	   if(UC(ans).eq.'N') goto 13
	endif
c	else
	OPEN(unit=14,file=filnam,status='UNKNOWN',
     &    access='TRANSPARENT')
c	endif
c
c Now need right-justified text and numbers to enter into table
c  ICOLWID characters per column  (inc leading blank)
c  NCOL=number of columns
c  NDLINE=number of data lines
c  NTLINE=number of text lines
c Calculate total number of characters in table
	ncrow=icolwid*ncol	!characters per row

c!!!!	nchar=(2+ndline+ntline)*ncrow+2*ncrow	!total number of char
c=========================================================================
c  Looking at loops:
c	nchar=icolwid*4+2+(ncol-4)*icolwid+jcol*icolwid+(ncol-jcol)*icolwid+
c          +ndline*(2+icolwid*jcol+icolwid*(ncol-jcol))+2
c     nchar=4+ncol*icolwid+ncol*icolwid+ndline*(2+icolwid*ncol)
c	or
c     nchar=(2+ndline)*ncrow+2*ndline+4
c     if you have ntline ?  anoher loop for ntline?
c     ntline(2+ncrow) or just ntline*ncrow ?
c	nchar=(2+ndline)*ncrow+2*ndline+4+ntline*ncrow+2*ntline
	nchar=(2+ndline+ntline)*ncrow+2*(ndline+ntline)+4
	nchar1=nchar+500
c=========================================================================

c (+2*ncrow for the CR/LF at end of each row, so 54 char per row for
c ncol=4 and icolwid=13 =110700 char for 2048 data rows
c  Blank the array initially
c	do i=1,nchar
c	  ascdata(i)=blank
c	enddo
c more????
	ALLOCATE(ascdata(nchar1))
	do i=1,nchar1
	  ascdata(i)=blank
	enddo
	k=0		!count elements in ascdata
	m=0		!counts number of fields (each of icolwid characters) filled
	call INTSTOR(icolwid,k,m,ascdata,icolwid,nchar1)
c On exit from INSTOR k=last element of ascdata defined
	call INTSTOR(ncol,k,m,ascdata,icolwid,nchar1)
	call INTSTOR(ndline,k,m,ascdata,icolwid,nchar1)
	call INTSTOR(ntline,k,m,ascdata,icolwid,nchar1)
c
c Next fill in blanks if ncol>4
	if(ncol.gt.4) then
	   do 6 j=5,ncol
	   do 6 i=1,icolwid
		k=k+1
		ascdata(k)=blank
6	   continue
	endif
c
c Now next line=column headings -use titent1 specifying length=icolwid (as
c long as this is not greater than the 30 char declaration)
	k=k+1
	ascdata(k)=cr
	k=k+1
	ascdata(k)=lf
c
	do 3 i=1,jcol
	  if(alpha) then
	  call TITENT1(' column heading =',
     &	colhead(i),icolwid,.false.)
	  else
	      call Tdialog(1,' column heading =',colhead(i),icolwid,
     &	   .false.,11)
	  endif
	  call TEXTSTOR(colhead(i),k,m,ascdata,icolwid,nchar1)
3	continue
c
c Next fill in blanks up to column #ncol (in 1st line) if jcol<ncol
	if(jcol.lt.ncol) then
	   do 7 j=jcol+1,ncol
	   do 7 i=1,icolwid
		k=k+1
		ascdata(k)=blank
7	   continue
	endif
c
c Next the numerical data
c The binary data is in DATA(20480,ndim) where ndim=dimension of DATA
c in calling program.  There are NDAT values in each column, and JCOL
c columns are used, i.e. data(i,j) i=1,...,ndat; j=1,...,jcol
c	print*,k,nchar+500,ncol,icolwid,ndline
c	pause
	do 8 i=1,ndline
	   k=k+1
	   ascdata(k)=cr	!end of previous line
	   k=k+1
	   ascdata(k)=lf
c
	   do 9 j=1,jcol
	      call REALSTOR(data(i,j),k,m,ascdata,icolwid,nchar1)
	      if(debug()) then
		   print 700,i,j,k,m,data(i,j),(ascdata(l),l=k-12,k)
700            format(' i,j,k,m,data(i,j),ascdata= ',4i6,g13.6,4x,13a1)
	      endif
9	  continue
	  if(jcol.lt.ncol) then
	     do 10 j=jcol+1,ncol
	     do 10 i1=1,icolwid
		  k=k+1
		  ascdata(k)=blank
10	     continue
	  endif
8	continue
c
c End last line
	k=k+1
	ascdata(k)=cr	!end of previous line
	k=k+1
	ascdata(k)=lf
c Next write the file (no text lines in this version)
	write(14,rec=1) (ascdata(i),i=1,k)
	goto 99
c
c
c READ SECTION
100	continue
	if(alpha) then
	call TITENT1('File name for ASCII input (+ path if nec)',
     &	filnam,30,.true.)
	else
	call TITENT1('File name for ASCII input (+ path if nec)',
     &	filnam,30,.true.)
c	call Tdialog(1,'File name for ASCII input (+ path if nec)',
c     &	filnam,30,.true.,11)
	endif
c==== to be done
c
c
99	continue
	CLOSE(unit=14)
c
	if(alpha) then
      print 90,k,filnam
	else
	call INTCONV(k,istring)
	call WDIALOG(1,istring//' characters written to '//filnam,11)
	endif
      if(discprt) write(8,90) k,filnam
90	format(1x,i9,' characters written to ',a30)
	DEALLOCATE(ascdata)
	RETURN
	end


	subroutine INTSTOR(int,k,m,ascdata,icolwid,nchar)
c To store right-justified integer in ascdata, starting at ascdata(k)
	character*1 ascdata(nchar)
	character*11 istring
c
	call INTCONV(int,istring)
	call RJUST(istring,11)
c fill first icolwid-11 bytes with blanks
	do 1 i=1,icolwid-11
	 k=k+1
	 ascdata(k)=' '
1	continue
c fill remaining bytes with the integer
	do 2 i=1,11
	k=k+1
	ascdata(k)=istring(i:i)
2     continue
c
	m=m+1		!another field filled
	RETURN
	end

	subroutine REALSTOR(real,k,m,ascdata,icolwid,nchar)
c To store right-justified integer in ascdata, starting at ascdata(k)
	character*1 ascdata(nchar)
	character*12 string
c
	x=real		!so input not changed
c Check that there are not too many sig figs.  Have 12 characters and
c need 4 for 'E-04', + one for dec point + 1 for sign if neg, so can
c have m=7 sig figs (or m=6 if neg). Use ROUND1 to round to m sig figs (see
c TRC.FOR)
	n=7
	if(x.lt.0.) n=6
c	call REALASC(ROUND1(x,n),string) !ver of REALTOCH to give exponent form
	call EFORMAT(ROUND1(x,n),string) !ver of DCFORMAT to give E12.5 format
	call RJUST(string,12)
c fill first icolwid-12 bytes with blanks
	do 1 i=1,icolwid-12
	 k=k+1
	 ascdata(k)=' '
1	continue
c fill remaining bytes with the real
	do 2 i=1,12
	 k=k+1
	 ascdata(k)=string(i:i)
2      continue
c
	m=m+1		!another field filled
	RETURN
	end


	subroutine TEXTSTOR(text,k,m,ascdata,icolwid,nchar)
c To store right-justified text in ascdata, starting at ascdata(k)
	character*1 ascdata(nchar)
	character*30 text
c
	call RJUST(text,11)
c fill first icolwid-11 bytes with blanks
	do 1 i=1,icolwid-11
	 k=k+1
	 ascdata(k)=' '
1	continue
c fill remaining bytes with the text
	do 2 i=1,11
	 k=k+1
	 ascdata(k)=text(i:i)
2     continue
c
	m=m+1		!another field filled
	RETURN
	end


	subroutine EFORMAT(x,cnum)
c Special version for ASCIO that uses E12.5 format
c EFORMAT is variant of DCFORMAT for real to ASCII conversion that
c outputs E13.6 format.
c DC subroutine for use with new Hgraph, to replace HFORMAT in the
c old Hgraph.  HFORMAT was used only for Fiw.id fixed format.
c Converts the real number X to a character array CNUM with IFIELD
c digits, of which IRIGHT are to the right of the decimal point.
c VERSION USING FORTRAN 'INTERNAL WRITE' (see 9.2.1 in Language ref)
	character cnum*(*),fmt*8
c
	fmt='(E12.5)'
	WRITE(cnum,fmt) x
	RETURN
	end


	subroutine RJUST(string,n)
	character string*(*)
c To right-justify the characters in a string (eg for use in ASCIO)
c
	call GBLANK(string,n,n1,n2)
	if(n2.eq.n) RETURN	!already right-justified
	nr=n-n2		!number of places to shift right
	nc=n2-n1+1		!number of characters to be shifted
	do 1 i=n2,n1,-1
	   j=i+nr
	   string(j:j)=string(i:i)
1     continue
c Fill left end with blanks
	do 2 i=1,n-nc
2     string(i:i)=' '
c
	RETURN
	end



