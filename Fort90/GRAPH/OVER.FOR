	logical function real4(r)
      r=abs(r)
      if(r.le.10e-37.or.r.ge.10e+37) then
            real4=.true.
      endif
	end

      logical function real8(r)
      real*8 r
      r=dabs(r)
      if(r.le.10d-307.or.r.ge.10d+307) then
            real8=.true.
      endif
	end

      logical function integer2(i)
      integer*2 i
      if(i.le.-32766.or.i.ge.32766) then
            integer2=.true.
      endif
      end

      logical function integer4(i)
      integer*4 i
      if(i.le.-2147483646.or.i.ge.2147483646) then
            integer4=.true.
      endif
	end

