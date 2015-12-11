	subroutine VECREV(X,Xrev,n,nd1,nd2)
c To reverse order of elements 1 to n in a array X. Result in Xrev (which can
c be same as X in call)
	real*4 X(nd1),Xrev(nd2)
c Swap elements 1,n, then 2,n-1 etc. Needs n/2 steps altogether.
c (the temp value, xt, is needed only if x,xrev are same in call)
	do 1 i=1,n/2
	  k=n-i+1		!=n,n-1,...
	  xt=x(i)
	  xrev(i)=x(k)
	  xrev(k)=xt
1	continue
	RETURN
	end


