	print 1
1	format(' u = ')
	read *,u
	x=u/sqrt(2.)
	z=erfs(x)
	P=0.5*(erfs(x)+1.)
	P1=erfs(abs(x))
	print 2,z,P,P1,1-p1
2	format(
     & ' x=u/sqrt(2)',/,
     & ' erf(x) = ',g13.6,/,
     & ' P = 0.5(erf(x)+1.) = ',g13.6,/,
     & ' P1 = erf(abs(x)) = ',g13.6,/,
     & ' 1 - P1 = ',g13.6,/)
c     & '',/,
c     & '',/,
	end

