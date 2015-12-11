	function AFUNC(tval)
	common/aafunc/ trise
c
      afunc=0.0
	zo=tval/trise
	AFUNC=erfs(0.88604*zo)
	return
	end

