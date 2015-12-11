	character*1 default,answer
1	default='n'
	call DCASK('question',default,answer)
	print*, 'answer = ',answer
	goto 1
	end
