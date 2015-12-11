	program permain
c========================================================================
	character ans,getch
	character*13 getint
	call iniplt(0,.false.,1)
	call viewport(0,10000,0,7500)
	call plot_graph()
	ans=getch()
	call endplt
	call erascr()
	call iniplt(4,.false.,1)
	call viewport(0,10000,0,7500)
	call plot_graph()
	ans=getch()
	call endplt
	end
