	.387
data1 segment

    size_h 		dw ?
    size_v 		dw ?

	cmdArg 		db 20 dup('$')
	cmdNum1 	db 10 dup('$')
	num1Len 	db ?
	num2Len 	db ?
	cmdNum2 	db 10 dup('$')
	newline db 10, 13, '$'
	
	num1val 	dw ?
	num2val 	dw ?
	
	
	
data1 ends

code1 segment
program_start:
    ;stack initialization
    mov ax, seg stack1
    mov ss, ax
    mov sp, offset stack_top


	
	;set data segment to es to save command line argument and not overwrite initial ds
	mov 	ax, seg data1
	mov 	es, ax
	mov 	si, 082h					;set si to beginning of command line arguments string
	mov 	di, offset cmdArg
	xor 	cx,cx      					;cx=0
	mov 	cl, byte ptr ds:[080h]  	;cx = num of characters
	get_cmd:
		mov 	al, byte ptr ds:[si]	;copy arguments to variable in data segment
		mov 	byte ptr es:[di], al
		
		inc 	si
		inc 	di
		
		loop 	get_cmd
		
		
	;data segment initialization
    mov 	ax, seg data1
    mov 	ds, ax
		
	
	call 	parse_args
	call 	to_int
	
	

    ;set display mode
    mov ah, 0
    ;set 320x200 resolution
    mov al, 13h
    int 10h

    ; set es to start of video card memory
    mov ax, 0a000h
    mov es, ax
    
	
	
    main_loop:
        call draw1

        call get_input

        call clear_screen

        jmp main_loop
	
;...............................................................
parse_args:
	push 	si
	push 	di
	push 	ax
	
	mov 	si, offset cmdArg
	mov 	di, offset cmdNum1
	xor 	cl,cl
	
	getNum1:
		mov 	al, byte ptr ds:[si]
		cmp 	al, ' '
		je 		getNum2
		
		mov 	byte ptr ds:[di], al
		
		inc 	si
		inc 	di
		inc 	cl
		jmp 	getNum1
		
	getNum2:
		mov 	di, offset num1Len
		mov 	byte ptr ds:[di], cl
		xor 	cl, cl
		
		mov 	di, offset cmdNum2
		inc 	si
		
		num2Loop:
			mov 	al, byte ptr ds:[si]
			cmp 	al, '$'
			je 		endF
			
			mov 	byte ptr ds:[di], al
			
			inc 	si
			inc 	di
			inc 	cl
			jmp 	num2Loop
	
	endF:
		mov 	di, offset num2Len
		mov 	byte ptr ds:[di], cl
		xor 	cl, cl
		
		pop 	si
		pop 	di
		pop 	ax
		ret
	
;...............................................................	
to_int:
	push 	cx
	xor 	cx,cx
	mov 	si, offset num1Len
	mov 	cl, byte ptr ds:[si]    ;get length of num1
	
	parse_num1:
		mov 	si, offset cmdNum1
		mov 	ax,1
		mov 	bx,10
		dec 	cx
		set_bx:				;set bx to power of ten
			mul 	bx
			loop 	set_bx
		mov 	bx, ax  	;save result of multiplication
		;mov 	ax, bx
		xor 	cx,cx		;cx=0
		mov 	si, offset num1Len
		mov 	cl, byte ptr ds:[si]    ;get length of num1
		mov 	di, offset num1val
		
		mov 	si, offset cmdNum1
		num1_parse_loop:
			xor 	ax, ax			;ax=0
			mov 	al, byte ptr ds:[si]	;load next number digit
			sub 	ax, '0'					;get value of digit character
			mul 	bx						;ax = ax * bx (set ax = value of Nth digit, digit*10^power)
			
			add 	word ptr ds:[di], ax	;add value to result storage
			
			mov 	ax, bx					;set division parameters
			mov 	bx, 10
			div 	bx						;ax (bx) = ax/10
			mov 	bx, ax					;save bx value
			
			inc 	si
			loop 	num1_parse_loop
	parse_num2:
		xor 	cx,cx
		mov 	si, offset num2Len
		mov 	cl, byte ptr ds:[si]    ;get length of num2
		
		mov 	si, offset cmdNum2
		mov 	ax,1
		mov 	bx,10
		dec 	cx
		dec 	cx
		set_bx1:
			mul 	bx
			loop 	set_bx1
		mov 	bx, ax
		mov 	ax, bx
		xor 	cx,cx
		mov 	si, offset num2Len
		mov 	cl, byte ptr ds:[si]    ;get length of num1
		mov 	di, offset num2val
		
		mov 	si, offset cmdNum2
		num2_parse_loop:
			xor 	ax, ax
			mov 	al, byte ptr ds:[si]
			sub 	ax, '0'
			mul 	bx
			
			add 	word ptr ds:[di], ax
			
			mov 	ax, bx
			mov 	bx, 10
			div 	bx
			mov 	bx, ax
			
			inc 	si
			loop 	num2_parse_loop
			
	mov 	si, offset num1val
	mov 	di, offset size_h
	
	mov 	ax, word ptr ds:[si]
	mov 	bx, 2
	div 	bx
	mov 	word ptr ds:[di], ax
	
	
	mov 	si, offset num2val
	mov 	di, offset size_v
	
	mov 	ax, word ptr ds:[si]
	mov 	bx, 2
	div 	bx
	mov 	word ptr ds:[di], ax
			
	
	pop 	cx
	ret
;...............................................................
RIGHT_ARROW equ 4Dh
LEFT_ARROW  equ 4Bh
UP_ARROW    equ 48h
DOWN_ARROW  equ 50h
ESCAPE_KEY  equ 01h

get_input:
    xor ah, ah
    int 16h

    right_arr:
    cmp ah, RIGHT_ARROW
    jne left_arr

        ;check if range isnt higher than size
        cmp word ptr ds:[size_h], 160
        je read_key_end

        inc word ptr ds:[size_h]
    
    jmp read_key_end


    left_arr:
    cmp ah, LEFT_ARROW
    jne up_arr

        ;check for range
        cmp word ptr ds:[size_h], 0
        je read_key_end

        dec word ptr ds:[size_h]

    jmp read_key_end


    up_arr:
    cmp ah, UP_ARROW
    jne down_arr

        ; range check
        cmp word ptr ds:[size_v], 100
        je read_key_end

        inc word ptr ds:[size_v]

    jmp read_key_end


    down_arr:
    cmp ah, DOWN_ARROW
    jne escape

        ;check for range
        cmp word ptr ds:[size_v], 0
        je read_key_end
  
        dec word ptr ds:[size_v]

    jmp read_key_end


    escape:
    cmp ah, ESCAPE_KEY
    je end_program

    read_key_end:
    ret
;...............................................................
tval 		dw 0
sinres 		dw ?
cosres 		dw ?
draw1:
	push 	cx
	finit
	mov 	word ptr cs:[tval], 0
	mov 	cx, 2048
	draw1_loop:
		dec 	cx
		push 	cx
		fild 	word ptr cs:[tval]		;load t
		fcos							;cos(t)
		fild 	word ptr ds:[size_h]	;load first parameter
		fmul							;a*cos(t)

		
		fistp 	word ptr cs:[X]  ;X = center_x + a*cos(t)
		add 	word ptr cs:[X], 160
		
		fild 	word ptr cs:[tval] 		;load t
		fsin							;sin(t)
		fild 	word ptr ds:[size_v]	;load b value
		fmul							;b*sin(t)

		
		fistp 	word ptr cs:[Y]			
		add 	word ptr cs:[Y], 100	; Y = center_y + b*sin(t)
		mov 	byte ptr cs:[colour], 12
		
		call 	draw_point
		
		inc 	word ptr cs:[tval]
		pop 	cx
		
		cmp 	cx, 0
		jne 	draw1_loop
		
	pop 	cx
	ret
	
	
	
;...............................................................
;draw_point args:
X 	dw ?
Y 	dw ?
colour 	db ?
draw_point:
	mov 	ax,0a000h
	mov 	es, ax
	mov 	ax,word ptr cs:[Y]
	mov 	bx, 320
	mul 	bx 	;ax = 320*y
	
	mov 	bx, word ptr cs:[X]
	add 	bx, ax 	;bx = 320*y + x
	mov 	al, byte ptr cs:[colour]
	
	mov 	byte ptr es:[bx], al
	ret
;...............................................................................


clear_screen:
	mov 	ax,0a000h
	mov 	es,ax
	xor 	ax,ax
	mov 	di,ax
	cld 
	
	mov 	cx, 320*200
	rep stosb
    ret

;...............................................................
end_program:
    ; change display mode
    mov ah, 0
    ; use text mode
    mov al, 3h
    int 10h 

    mov ax, 4c00h ; end program
    int 21h

;...............................................................
print:	;dx needs to be initialized before function call
	mov 	ax, seg data1
	mov 	ds, ax
	mov 	ah, 9 ;print text stored in ds:dx
	int 	21h
	
	ret

code1 ends


stack1 segment stack
            dw 2048 dup(?)
stack_top   dw ?
stack1 ends

end program_start