c To test getscfil
	integer kfile(10)
	character consfil*40			!path for consam.dat file
	character*40 pfile(10)	!path names for SCANDAT files (char*40 here)
	character*40 scantfil(10)  !path names for SCANTEMP files
	logical readini

	readini=.false.
	call GETSCFIL(consfil,scantfil,nfile,kfile,pfile,
     & nsamp,srate,readini,idest)
	readini=.true.
	call GETSCFIL(consfil,scantfil,nfile,kfile,pfile,
     & nsamp,srate,readini,idest)
	end

