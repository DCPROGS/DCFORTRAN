	program TLT
c To test line thickness control in new Hgraph
	idev=0	!screen
      print 1
1	format(' idev,thickness = ')
	read*,idev,ith
	print 2,idev,char(ith)
2	format(' idev = ',i3,'   thick = ',a1)
	pause
	call INIPLT(idev,.false.,1.0)
	call SCALE(0.,100.,0.,100.)
	call LOADVTB('2'//'W'//char(ith))
	call MOVETO(1.,49.,0,0)
	call MOVETO(99.,49.,1,0)
	pause
	call ENDPLT
	end


