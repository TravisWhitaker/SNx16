;Fibonacci implementation. Put the nth fibonacci number in R2 where n is in R1.
	mov r1, 16 ; Computing the nth fibonacci.
fib:	mov r4, r1
	mov r3, r1
	mul r3, 1
	mov r10, 18
	je [r10] ; jump to return
	mov r3, r1
	add r3, -1
	mul r3, 1
	mov r10, 13
	je [r10] ; jump to return
	add r1, -1 ; nth fib requires n-1 iterations
	mov r3, 0  ; second to last result
	mov r4, 1  ; last result
	mov r10, 6 ; return offset
fibl:	mov r5, r1
	mul r5, 1
	je [r10] ; jump to return
	mov r6, r3 ; save second to last in temp
	mov r3, r4 ; last result is now second to last
	add r4, r6 ; new result is last result plus second to last result
	add r1, -1
	j fibl
fibr:	mov r2, r4
