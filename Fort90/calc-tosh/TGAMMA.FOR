	program tgamma
c Test gamma pdf and cumulative form
5	print 1
1	format(' rate = ')
	call INPUTr(rate)
	print 2
2	format(' n = ')
	call INPUTi(n)
	print 3
3	format(' t = ')
	call INPUTr(t)
	pdf=gammapdf(t,rate,n)
	q=1.0 - gammainc(t,rate,n)
	print 4,t,pdf,q
4	format(
     & ' t = ',g13.6,/,
     & ' pdf = ',g13.6,/,
     & ' P(T<t) = ',g13.6)
	goto 5
	end

