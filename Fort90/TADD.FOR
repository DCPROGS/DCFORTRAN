c To test adding integer*4 in assembler
	integer*4 i,j,addint

3	print 1
1	format(' i,j = ')
	read 2,i,j
2	format(2i8)
c      i = 3
c      j = 4
** Function to add two integers.
      k = addint(i,j)
      print*,  "sum = ",k
	goto 3
	end

