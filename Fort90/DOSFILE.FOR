	subroutine DOSFILE(file,n,filout,nerr)
	character file*(*),filout*(*)
	character filein*200
	character*1 UC,chin,chout
c To replace any illegal characters in a DOS filename with 'x'
c n= dimension of file
c nerr=0 if filout same as file()
c nerr=1 if file changed
c
c  For parsname etc
	character path*30,pname*8,suffix*3	!for parsname
	logical nopath
c
	nerr=0
	filein=file
	call PARSNAME(file,path,ndev,pname,suffix,nopath,n)
	n1=nblank3(pname,8)
	do i=1,n1
	   ic=ichar(pname(i:i))
	   if(.not.(ic.eq.31.or.ic.eq.45.or.ic.eq.95.or.
     &     (ic.ge.48.and.ic.le.57).or.
     &     (ic.ge.65.and.ic.le.90).or.
     &     (ic.ge.97.and.ic.le.122))) then
		pname(i:i)='x'
	   endif
	enddo
	n1=nblank3(path,30)
	n2=nblank3(pname,8)
	n3=nblank3(suffix,3)
c
	if(nopath) then
	   filout=pname(1:n2)//'.'//suffix(1:n3)
	else
	   filout=path(1:n1)//pname(1:n2)//'.'//suffix(1:n3)
	endif
c
	nerr=0
	do i=1,n
	   chin=UC(filein(i:i))
	   chout=UC(filout(i:i))
	   if(ichar(chin).eq.0) chin=char(32)	!space
	   if(ichar(chout).eq.0) chout=char(32)	!space
	   if(chin.ne.chout) nerr=1
	   filein(i:i)=chin
	   filout(i:i)=chout
	enddo
	return
	end

