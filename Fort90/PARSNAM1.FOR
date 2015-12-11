	subroutine PARSNAM1(filnam,path,ndev,pname,suffix,fileout,
     & nopath,ndim)
	character filnam*(*),path*30,pname*8,suffix*3,ndev*2,UC*1
	character fileout*12
	logical nopath,nosuf
c Example: for filnam=c:\fortran\compress.for this routine returns
c pname=compress, suffix=for, path=c:\fortran\, ndev=C:
c NB pname is 8 characters and may be returned with trailing spaces
c version of parsname() that also returns the file name with no path
c in fileout(12) with any blanks at the end (eg no blanks within name)
c Initialise
	nopath=.false.
	nosuf=.false.
	do i=1,30
	   path(i:i)=' '        !blank initially
	enddo
	do i=1,12
	   fileout(i:i)=' '
	enddo
	ndev='  '
	pname='        '
	suffix='  '
c
c Define first and last non-blank characters in FILNAM
	n1=1		!in case filnam is all blank
	n2=1		!in case filnam is all blank
	do 10 i=1,ndim
	   ic=ichar(filnam(i:i))
	   if(ic.eq.0.or.ic.eq.32) goto 10
	   n1=i
	   goto 11		!first non-blank character found
10	continue
11	do 12 i=ndim,1,-1
	   ic=ichar(filnam(i:i))
	   if(ic.eq.0.or.ic.eq.32) goto 12
	   n2=i
	   goto 13		!last non-blank character found
12	continue
13	continue
c (1) Define path
c Look for ':'
	if(filnam(n1+1:n1+1).eq.':') then
	   ic=ichar(UC(filnam(n1:n1)))		!=first char
	   if(ic.ge.65.and.ic.le.78) then		!=A to N
		ndev=filnam(n1:n1+1)
	   endif
	endif
c Look for last '\' character
	do 1 i=n2,n1,-1
	   i1=i
	   if(filnam(i:i).eq.'\') goto 5
1	continue
c If reach here no \ found so no path given- return as blank
	nopath=.true.
c
5	continue
c Define prog name
c look for '.' character
	do 6 i=n2,n1,-1
	   i2=i
	   if(filnam(i:i).eq.'.') goto 7
6	continue
c If reach here then no '.' found so there is no suffix
	nosuf=.true.
c
7	continue
	if(nopath.and.nosuf) then
	   pname=filnam(n1:n2)
	else if(nopath) then
	   pname=filnam(n1:i2-1)
	   suffix=filnam(i2+1:n2)
	else if(nosuf) then
	   path=filnam(n1:i1)
	   pname=filnam(i1+1:n2)
	else
	   path=filnam(n1:i1)
	   pname=filnam(i1+1:i2-1)
	   suffix=filnam(i2+1:n2)
	endif
c check for trailing spaces in pname
	n1=nblank1(pname)
	fileout(1:n1)=pname(1:n1)
	if(.not.nosuf) then
	   fileout(n1+1:n1+4)='.'//suffix(1:3)
	endif
	RETURN
	end


