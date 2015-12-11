	character filnam*40
	character path*30,pname*8,suffix*3,ndev*2
	logical nopath
c
2	call TITENT0('Path and name:',filnam,40,.false.)
	ndim=40
	call PARSNAME(filnam,path,ndev,pname,suffix,nopath,ndim)
	print 1,filnam,path,ndev,pname,suffix,nopath
1	format(
     & ' filnam = ',a40,/,
     & ' path = ',a30,/,
     & ' ndev = ',a2,/,
     & ' pname = ',a8,/,
     & ' suffix = ',a3,/,
     & ' nopath = ',l4)
	pause
	goto 2
	end

