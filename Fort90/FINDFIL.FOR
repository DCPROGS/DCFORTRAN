	subroutine FINDFIL(filnam,ndim,ierr)
c To list file names that match the path specified in the input filnam
c and choose one of them, its complete path and name being output in
c filnam.  Uses Spindrift Util library routines
c input=filnam -e.g. 'G:\cvdat\*.dat'
c output=filnam =chosen file e.g. 'G:\cvdat\lgs.dat'
c Returns ierr=0 if no files found
c	    ierr=number of files found normally
c	    ierr=-1 if the path specified in filnam cannot be found
c
	character filnam*(*)
	character*30 path
	character*8 pname
	character*3 suffix
	character*2 ndev
	logical nopath
	allocatable::strings
	character*50 strings(:)
	character*50 string1
	integer*4 iattr(4)
c
	allocate(strings(100))
	nf=0		!number of matching files found
c
	call FINDFRST(filnam,strings(1),iattr)
	ierr=ISERROR()
	if(ierr.eq.401) then
	   print*,'Strings too short in FINDFIL'
	else if(ierr.eq.3) then       !ierr=3 if path not found
	   call BELL(1)
	   print 4,filnam
4	   format(' Path not found: ',a50)
	   ierr=-1
	   RETURN		!with ierr=-1
	else if(ierr.ne.18) then      !ierr=18 if file not found
	   nf=nf+1
	   call PARSNAME(strings(nf),path,ndev,pname,suffix,nopath,50)
	   string1=charnb(pname)//'.'//charnb(suffix)
	   print 1,nf,string1
1	   format(' (',i2,') ',a50)
5	   call FINDNEXT(strings(nf+1),iattr)
	   ierr=ISERROR()
	   if(ierr.eq.401) then
		print*,'Strings too short in FINDFIL'
	   else if(ierr.ne.18) then      !ierr=18 if file not found
		nf=nf+1
		call PARSNAME(strings(nf),path,ndev,pname,suffix,nopath,50)
		string1=charnb(pname)//'.'//charnb(suffix)
		print 1,nf,string1
		goto 5	!find more
	   endif
	endif
c
	if(nf.eq.0) then
	   call BELL(1)
	   print 2,filnam
2	   format(' No files found that match ',a50)
	   ierr=0	!for return
	else
	   print 3,nf,filnam
3	   format(1x,i4,' files found that match ',a50,/,
     &	' Use file number (0 to abort) = ')
	   call INPUTi(ifile)
	   if(ifile.le.0) then
		ierr=0
	   else
		filnam=strings(ifile)	!output file name and path
		ierr=ifile
	   endif
	endif
c
	deallocate(strings)
	RETURN
	end


