	function AFUNC(tval)
	common/aafunc/ trise
c
	zo=tval/trise
	AFUNC=erfs(0.88604*zo)
	return
	end

