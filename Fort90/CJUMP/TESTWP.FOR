	program testwp

	character	command*50,filnam*15

	filnam='c:\config.sys'
	command='type '//filnam//'|more'
	call mode(18)
	call clears(0,480,15)
	call system('type c:\config.sys|more')
	pause
	call clears(0,480,1)
	call system(command)
	pause
	call clears(0,480,0)
99	end
