IDEAL
MODEL small
STACK 100h
DATASEG
;------------------------
;Author: adi cohen
;assembly final project:
;the game - snake
;------------------------
; my vars
	;bmp files:
	StartPhoto  db 'Main.bmp',0 
	Instractions db 'Rules.bmp',0
	GameOver db 'lose.bmp',0
	QuitGame db 'QuitGame.bmp',0
	Pausee	db 'pause.bmp',0
	FileRec db 'Rec.txt',0
	FileHandle  dw  ? 
	Header db 54 dup (0) 
	Palette  db 256*4 dup (0) 
	ScrLine db 320 dup (0) 
	
	;status:
	MsgName db 'Enter player name:$'
	PlayerName db 'Name:$'
	Score db 'Score:$'
	Length_ db 'Length:$'
	Rekord	db 'Record:$'
	MyName db 1 dup (11)
	db 12 dup (?)
	MyScore dw  0
	MyLength dw 3
	CurrentRecord dw 0
	RememberTakeRec dw ?
	
	;data for random functions
	RndSeed dw ?
	Random dw 1 dup (0,0)
	
	;wall's Game
	YWall dw 0
	XWALL dw 0
	
	;the snake itself
	SnakeToSave dw 1 dup (100,128,100,124,100,120)
	YAndXOfSnake dw 1 dup (100,128,100,124,100,120)
	dw 5952 dup (0)

;my contacts:
	NEXTFILE equ [bp+4]
	
	;Initialize needed colors-
	INDEXTOCHANGECOLOR equ [bp+10]
	RCOLOR equ [bp+8]
	GCOLOR	equ [bp+6]
	BCOLOR equ [bp+4]

	;CLOCK and random location
	CLOCK  equ es:6Ch 
	OFFSETRANDOM equ [bp+4]
	SOUND equ [bp+4]

	;for put in the stack the next step (after check if it's button of the Game)
	STEP equ [bp+4]

	;status of the snake
	OFFSETOFMYNAME equ [bp+4]
	OFFSETMYSCOREORLENGTH equ [bp+6]
	WHATTOADD equ [bp+4]
	
	;limits of values for location of the food:
	LIMITX equ 248
	LIMITY equ 192
	
	;buttons:
	BPRESSED equ 030h
	SPRESSED equ 01Fh
	IPRESSED equ 017h
	ESCPRESSED equ 1h
	MPRESSED equ 32h
	
	UPPRESSED equ 48h
	DOWNPRESSED equ 50h
	LEFTPRESSED equ 04Bh 
	RIGHTPRESSED equ 04Dh

	;colors:
	RED equ 3
	BLACK equ 0
	GREAN equ 2
	BLUE equ 1
	WHITE equ 15

	;snake_defenitions:
	OFFSETOFTHESNAKE equ [bp+4]
	DATRAFROMKEYBOARD equ [bp-10]
	COLOR equ [byte ptr bp+4]
	SNAKESLENGTH equ [bp+6]

	;walls
	XWALLUPDOWN equ 320
	YWallUPWIDTH equ 3
	YWALLDOWNWIDTH equ 4
	XWALLLEFTRIGHT equ 3
	YWALLLEFTRIGHT equ 200

	;pixels of the walls
	YTOPAINT equ [bp+12]
	XTOPAINT equ [bp+10]
	YPLACE equ [bp+8]
	XPLACE equ [bp+6]

CODESEG
	proc OpenGraphicMode
		;----------------------------
		;this proc open graphics mode
		;Argument:none
		;Return value:none
		;----------------------------

			push ax
			mov ax,13h
			int 10h
			pop ax 
			ret
	endp OpenGraphicMode

	proc CloseGraphicMode
		;-----------------------------
		;this proc close graphics mode
		;Argument:none
		;Return value:none
		;-----------------------------
			push ax
			xor ah,ah
			mov al,2
			int 10h
			pop ax
			ret
	endp CloseGraphicMode

	proc OpenVoice
		;------------------------
		;the proc open voice card
		;Argument:none
		;Return value:none
		;------------------------
			push ax
			in 	al, 61h
			or 	al, 00000011b
			out 61h, al
			pop ax
			ret
	endp OpenVoice

	proc CloseVoice
		;------------------------------
		;the proc close voice card
		;Argument:none
		;Return value:none
		;------------------------------
		push ax
		in al,61h
		and al,11111100b
		out 61h,al
		pop ax
		ret
	endp CloseVoice
	
		proc PrintMyStrings
		;---------------------------------------------------------------
		;PrintMyStrings proc
		;PrintMyStrings print the string in the Game (length and score)
		;Argument:(in stack)
			;*offset of the string	word
		;Return value:none
		;---------------------------------------------------------------
			
		push bp
		mov bp,sp
		push dx ax
		mov dx,[bp+4]
		mov ah,9h
		int 21h
		pop ax dx bp
		ret 2
	endp PrintMyStrings

	proc PrintMyChars
		;---------------------------
		;PrintMyChars proc
		;PrintMyChars print char
		;Argument:(in stack)
			;*the char to print	byte
		;Return value:none
		;----------------------------
		
		push bp
		mov bp,sp
		push dx ax
		mov dl,[byte bp+4]	;the char
		mov ah,2h
		int 21h
		pop ax dx bp
		ret 2
	endp PrintMyChars

	proc InitRandom
	; ------------------------------------------
	; InitRandom Proc
	; Initialize the Random seed with the CLOCK.
	; Return:
	;	None
	; ------------------------------------------
		push ax es

		mov ax, 40h
		mov es, ax
		mov ax, [CLOCK]
		mov [RndSeed],ax
		pop es ax
		ret
	endp InitRandom
	
	proc ReadFile
		push bp
		mov bp,sp
		push ax bx cx dx
		mov ah,3Fh 
		mov bx, [filehandle] 
		mov cx,2
		mov dx,[bp+4]
		int 21h
		pop dx cx bx ax bp
		ret 2
	endp ReadFile

	proc GenerateRandNum
	; --------------------------------------------------------------------
	; GenerateRandNum Proc
	; Generates a pseudo-random 15-bit number.
	;
	; NOTE:
	;   the algorithm describe http://stackoverflow.com/a/43978947/5380472
	;
	; Return Value -> AX:
	;   AX contains the random number.
	; --------------------------------------------------------------------
		push bx cx dx si di

		; 32-bit multiplication in 16-bit mode (DX:AX * CX:BX == SI:DI)
		mov  ax, [RndSeed]
		xor  dx, dx
		mov  cx, 041C6h
		mov  bx, 04E6Dh
		xor  di, di
		push ax
		mul  bx
		mov  si, dx
		xchg di, ax
		mul  bx
		add  si, ax
		pop  ax
		mul  cx
		add  si, ax

		; Do addition
		add  di, 3039h
		adc  si, 0

		; Save seed
		mov  [RndSeed], di

		; Get result and mask bits
		mov  ax, si
		and  ah, 07Fh

		pop di si dx cx bx
		ret
	endp GenerateRandNum

	proc RandomWithRange
	;------------------------------------------------------------------
	;RandomWithRange Proc
	;Returns a random number between the ranges it's get (parammeters)
	;Argument (in stack):
		;*LOW_LIMIT		word
		;*HIGH_LIMIT	word
		
	;Return value -> ax:
		;a Random number
	;------------------------------------------------------------------
		
		
		LOW_LIMIT	equ	[bp + 6]
		HIGH_LIMIT	equ	[bp + 4]

		push bp
		mov bp, sp
		push dx

		mov dx, HIGH_LIMIT
		sub dx, LOW_LIMIT

		call GenerateRandNum
		and ax, dx

		add ax, LOW_LIMIT

		pop dx bp
		ret 4
	endp RandomWithRange

	proc IimerData
	;---------------------------------------------------------------------------------
	;TimerData proc
	;IimerData checks the keyboard for new "move" of the snake while 1/18 second
	;updates the existing information (if it is legal-checked in the proc CHECKBUTOON)
	;Argument (in stack in registers)
		;*PUTSTEP	word	
		;*cx - valuse (1,2,or 3) *0.055= time to wait before move
	;Return value-> PUTSTEP
		;A scan code that's belong to one of the button of the snake
	;---------------------------------------------------------------------------------
			
		push bp
		mov bp,sp
		push cx ax bx
		xor bh,bh
		mov  ax, 40h
		mov  es, ax
		mov  ax, [CLOCK]	
FirstTick:
		cmp  ax, [CLOCK]
		je  FirstTick
DelayLoop:
		mov  ax, [CLOCK]
Tick:
		cmp  ax, [CLOCK]
		je  Tick
		cmp bh,1
		je not_data
		mov ah,1h
		int 16h
		jz not_data
		mov ah,0
		int 16h
		call CheckButton
		cmp bh,1
		je not_data
		mov STEP,ax
		inc bh
not_data:
		loop  DelayLoop
exit_Timer:
		pop bx ax cx bp
		ret
	endp IimerData

	proc Music
	;-----------------------------------------
	;Music proc
	;make a SOUND according to the parammeter while 1/4 sec
	;Argument: (in stack)
		;*SOUND		word
		;*[bp+6] - time to wait before stop sound
	;Return value->none
	;------------------------------------------
		push bp
		mov bp,sp
		push ax
		call OpenVoice
		mov al, 0B6h
		out 43h, al
		mov ax,SOUND ;for change the Hz
		out 42h, al
		mov al, ah
		out 42h, al
		mov cx,[bp+6] ;time to wait before stop SOUND
		call Timer
		call CloseVoice
		pop ax bp
		ret 4
	endp Music

	proc Timer
	;---------------------------------------------------
	;Timer proc
	;stops the runnig for 1/4 sec (for the proc Music)
	;Argument:(in ragister)
	;	*cx - num*0.055 = time to make a SOUND	word
	;Return value: none
	;---------------------------------------------------
		push ax
		mov  ax, 40h 
		mov  es, ax
		mov  ax, [CLOCK] 
@@FirstTick:
		cmp  ax, [CLOCK]
		je  @@FirstTick 
@@DelayLoop: 
		mov  ax, [CLOCK] 
@@Tick: 
		cmp  ax, [CLOCK] 
		je  @@Tick
		loop  @@DelayLoop
		pop ax
		ret
	endp Timer


	proc RandomFoodLocation
	;---------------------------------------------------
	;RandomFoodLocation proc
	;RandomFoodLocation make new x and y to the
	;"food" of the snake and put them in var "RANDOM"
	;Argument: (in Registers)
		;*di-the addres of the y of RANDOM	word
		;*si-the addres of the x of RANDOM	word
	;Return value:none
	;---------------------------------------------------
		push ax di si
		
		; y aixs
		push 4 LIMITY
		call RandomWithRange
		mov [di], ax
		
		; x aixs
		push 4 LIMITX
		call RandomWithRange
		mov [si], ax
		
		pop si di ax
		ret
	endp RandomFoodLocation

	proc Food
	;------------------------------------------------------------------------------
	;Food proc
	;Food Initializing si and di to the addres of y and x of Random
	;if the location of the snake's food same to the location of any snake's parts
	;the proc make a new location
	;Argument:	(in stack)
		;*OFFSETRandom	WORD
	;Return value:none
	;------------------------------------------------------------------------------
		push bp
		mov bp,sp
		push dx di si bx
		mov di,OFFSETRANDOM
		mov si,OFFSETRANDOM
		add si,2
find_another_location:
		call RandomFoodLocation
		push [MyLength] offset YAndXOfSnake
		call CheckFoodLleagal
		cmp bx,0001h
		je find_another_location
		push BLUE
		call Paint
		pop bx si di dx bp
		ret 2
	endp Food


	proc CheckFoodLleagal
	;---------------------------------------------------------------------------------------
	;CheckFoodLleagal proc
	;CheckFoodLleagal compare any location of snake's part with the location of the food
	;Argument:(in stack and registers)
		;*OFFSETOFTHESNAKE	word
		;*SNAKESLENGTH	word
		;*di-the addres of y of the food	word
		;si-the addres of x of the food		word
	;Return value:
		;bl -> 1
			;if has same location
		;bl -> 0
			;'if hasn't same location
	;--------------------------------------------------------------------------------------
		push bp
		mov bp,sp
		push di si cx dx
		mov bx,OFFSETOFTHESNAKE
		mov cx,SNAKESLENGTH
		check_location_of_food_and_snake:
		mov dh,[byte ptr di] ;the y location of the food
		mov dl,[byte ptr si] ;the x location of the food
		cmp [byte ptr bx],dh
		jne not_same_location
		cmp [byte ptr bx+2],dl
		jne not_same_location
		mov bx,0001h
		jmp food_illeagel
not_same_location:
		add bx,4
		loop check_location_of_food_and_snake
food_illeagel:
		pop dx cx si di bp
		ret 4
		
	endp CheckFoodLleagal

	proc CheckButton
	;----------------------------------------------------
	;CheckButton proc 
	;CheckButton comapre the data from the keyboard with
	;every lleagal button of the snake - (a move or escape)
	;Argument:(in Registers)
	;*ah-the scan code of the accept button
	;Return value:
	;BH -> 1 if it's illleagal asking key
	;BH -> 0 if it's lleagal asking key 
	;----------------------------------------------------
		xor bh,bh
		cmp ah,UPPRESSED
		je Game_button

		cmp ah,DOWNPRESSED
		je Game_button

		cmp ah,LEFTPRESSED
		je Game_button

		cmp ah,RIGHTPRESSED
		je Game_button

		cmp ah,ESCPRESSED
		je Game_button

		inc bh
Game_button:
		ret
	endp CheckButton


	proc PaintScreen
	;------------------------------------------------
	;PaintScreen proc
	;PaintScreen paint the walls of the Game
	;Argument: (in stack)
		;*YTOPAINT	word
		;*XTOPAINT	word
		;*YPLACE	word
		;*XPLACE	word	
	;Return value:none
	;------------------------------------------------
		push bp
		mov bp,sp
		push ax bx cx dx di si
		mov ah,0Ch
		mov al,COLOR
		xor bh,bh
		mov cx,YTOPAINT ;for y
		mov di,YPLACE
		push [di]
		mov si,XPLACE
		push [si]
@@loopb:
		push cx
		mov cx,XTOPAINT ;for x
		push [si]
@@loopa:
		push cx
		mov cx,[si]
		mov dx,[di]
		int 10h
		inc [word ptr si]
		pop cx
		loop @@loopa ;stop print the x
		pop [si]
		inc [word ptr di]
		pop cx
		loop @@loopb ;stop print the y
		pop [si] [di]
		pop si di dx cx bx ax bp
		ret 10
		
	endp PaintScreen
	
	proc paintInsideSnake
	;--------------------------------------------
	;Paint proc
	;the proc has been use to 3 painting:
	;1.paint all the part's of the snake
	;2."delete" the tail of the snake every move
	;3.paint the "food" of the snake
	;Argument-(in stack and registers)
		;*di- the addres of y of the paint	word
		;*si- the addres of x of the paint	word
		;*COLOR		byte 
	;Return value:none
	;--------------------------------------------
		push bp
		mov bp,sp
		push ax bx cx dx
		mov ah,0Ch
		mov al,COLOR
		xor bh,bh
		mov cx,2 ;for y
		push [di] [si]
		inc [word ptr si]
		inc [word ptr di]

@@loopc1:

		push cx
		mov cx,2 ;for x
		push [si]

@@loopd1:

		push cx
		mov cx,[si]
		mov dx,[di]
		int 10h
		inc [word ptr si]
		pop cx
		loop @@loopd1 ;stop print the x
		pop [si]
		inc [word ptr di]
		pop cx
		loop @@loopc1 ;stop print the y
		pop [si] [di] dx cx bx ax bp
		ret 2
	endp paintInsideSnake

	proc Paint
	;--------------------------------------------
	;Paint proc
	;the proc has been use to 3 painting:
	;1.paint all the part's of the snake
	;2."delete" the tail of the snake every move
	;3.paint the "food" of the snake
	;Argument-(in stack and registers)
		;*di- the addres of y of the paint	word
		;*si- the addres of x of the paint	word
		;*COLOR		byte 
	;Return value:none
	;--------------------------------------------
		push bp
		mov bp,sp
		push ax bx cx dx
		mov ah,0Ch
		mov al,COLOR
		xor bh,bh
		mov cx,4 ;for y
		push [di]
		push [si]

@@loopc:

		push cx
		mov cx,4 ;for x
		push [si]

@@loopd:

		push cx
		mov cx,[si]
		mov dx,[di]
		int 10h
		inc [word ptr si]
		pop cx
		loop @@loopd ;stop print the x
		pop [si]
		inc [word ptr di]
		pop cx
		loop @@loopc ;stop print the y
		pop [si] [di]
		cmp al,BLACK
		je stop_paint
		cmp al,BLUE
		je stop_paint
		push WHITE
		call paintInsideSnake
		stop_paint:
		pop dx cx bx ax bp
		ret 2
	endp Paint
	
	proc CheckRecord
	push bp
	mov bp,sp
	push bx ax di
	mov di,[bp+4] ;offset of CURRENTRECORD
	mov bx,OFFSETMYSCOREORLENGTH
	mov ax,[bx]
	cmp ax,[di]
	jbe not_update_record
	mov [di],ax
	push offset CURRENTRECORD
	mov dx,1076h
	call PrintMyStatus
	not_update_record:
	pop di ax bx bp
	ret 4
	endp CheckRecord
	
	proc FoodORNot
	;--------------------------------------------
	;FoodORNot proc
	;FoodORNot checks the next move of the snake
	;if it's going into wall, the food, or itself
	
	;Argument (in stack)
		;*OFFSETOFTHESNAKE	word
	;Return value:
		;bh -> 1 if the snake go into wall or himself
		;bh -> 0 if this clear (BLACK)
		;bh -> 2 if this a food
	;---------------------------------------------
		push bp
		mov bp,sp
		push di si dx cx ax
		xor bh,bh
		mov ah,0Dh
		mov di,OFFSETOFTHESNAKE
		mov si,di
		add si,2
		mov cx,[si]
		mov dx,[di]
		int 10h
		cmp al,GREAN
		jne maybe_Blue
		jmp wall_or_body_disqualification
		
maybe_Blue:

		cmp al,BLUE
		jne maybe_Red
		jmp thats_food
maybe_Red:

		cmp al,RED
		jne None_of_colors
		jmp wall_or_body_disqualification
		
None_of_colors:
		jmp exit_check_color
thats_food:
		mov bh,2
		
		push offset MyScore 10
		call ReloadStatus
		push offset MyScore
		mov dx,0876h
		call PrintMyStatus
		
		push offset MyLength 1
		call ReloadStatus
		push offset MyLength
		mov dx,0C76h
		call PrintMyStatus
		
		push offset MyScore offset CurrentRecord
		call CheckRecord
				
		jmp exit_check_color
wall_or_body_disqualification:
		mov bh,1
		exit_check_color:
		pop ax cx dx si di bp
		ret 2
		
	endp FoodORNot


	proc ShiftRightData
	;-----------------------------------------------------------------------
	;ShiftRightData proc
	;ShiftRightData moves 4 bytes left all of the snake's part
	;use to make "new" tail-(the first part before the last in of the snake)
	;Argument:(in registers)
		;*cx - number of part that need to move right
		;*di - the y of one before the last snake's part
		;Return value:none
	;-----------------------------------------------------------------------
		push dx
mov_right_locations:
		push di
		mov si,di
		add si,2
		mov dx,[di]
		add di,4
		mov [di],dx
		mov dx,[si]
		add si,4
		mov [si],dx
		pop di
		sub di,4
		loop mov_right_locations
		pop dx
		ret
		
	endp ShiftRightData

	proc tail_addres
	;--------------------------------------------------------------------------------
	;TailAddres proc
	;the proc found the address of the y and x location of the snake's tail
	;Argument:(in registers)
		;*di-OFFSETOFTHESNAKE	word
	;Return value:
		;*cx-number of snake's part that need to move right (for ShiftRightData proc)
		;*di-the addres of the y of the snake's tail
	;--------------------------------------------------------------------------------
		push bp
		mov bp,sp
		mov di,OFFSETOFTHESNAKE
		xor cx,cx
still_searching:
		cmp [byte ptr di+4],0
		je find_tail
		inc cx
		add di,4
		jmp still_searching
		;di have the y of the tail
		find_tail:
		pop bp
		ret 2
	endp tail_addres

	proc destroy_tail
	;-----------------------------------------------------------------
	;DestroyTail proc
	;the proc paint the snake's tail in BLACK (actually "destroy him)
	;Argument:(in registers)
		;*di-the Adrres of the y of the snake's tail	word
	;Return value:none
	;-----------------------------------------------------------------
		push si di
		mov si,di
		add si,2 ;si have the addres of the x of the tail
		push BLACK
		call Paint
		pop di si
		ret
	endp destroy_tail

	proc check_neg_moves
	;--------------------------------------------------------------------
	;CheckNegMoves proc
	;the proc check if the next step (move of the snake) in the stack
	;in possible (not the negative of the current step
	;Argument:(in stack and registers)
		;*ax the current step of the snake	word
		;*STEP	word
	;Return value:
		;dl -> 1 if the next move is the negative of the current move
		;dl -> 0 if the next move is lleagal
	;--------------------------------------------------------------------
		push bp
		mov bp,sp
		push bx
		mov bx,STEP ;the next move
		xor dl,dl


		cmp ah,UPPRESSED
		jne check_neg_down
		cmp bh,DOWNPRESSED
		je dont_take_this_data

check_neg_down:
		cmp ah,DOWNPRESSED
		jne check_neg_right
		cmp bh,UPPRESSED
		je dont_take_this_data

check_neg_right:
		cmp ah,RIGHTPRESSED
		jne check_neg_left
		cmp bh,LEFTPRESSED
		je dont_take_this_data

check_neg_left:
		cmp ah,LEFTPRESSED
		jne button_fine
		cmp bh,RIGHTPRESSED
		je dont_take_this_data

		jmp button_fine

dont_take_this_data:
		inc dl

button_fine:
		pop bx bp
		ret
	endp check_neg_moves
	
	proc PassFirstStep
	push bp
	mov bp,sp
	push bx
	mov bx,offset YAndXOfSnake
	push [bx] [bx+2]
	mov ax,[bx+4]
	sub [word ptr bx],4
	cmp [bx],ax
	je pass_down
	add [word ptr bx],8
	cmp [bx],ax
	je pass_up
	add bx,2
	mov ax,[bx+4]
	sub [word ptr bx],4
	cmp ax,[bx]
	je pass_right
	jmp pass_left
	pass_up:
	mov ah,UPPRESSED
	jmp exit_PassFirstStep
	pass_down:
	mov ah,DOWNPRESSED
	jmp exit_PassFirstStep
	pass_right:
	mov ah,RIGHTPRESSED
	jmp exit_PassFirstStep
	pass_left:
	mov ah,LEFTPRESSED
	exit_PassFirstStep:
	mov bx,offset YAndXOfSnake
	pop [bx+2] [bx] bx bp
	ret 2
	endp PassFirstStep
	
	proc StartSound
	push 6 152Fh
	call Music
	push 6 0A97h
	call Music
	push 6 054Bh
	call Music
	ret
	endp StartSound

	proc TheSnake
	;---------------------------------------------------------
	;TheSnake proc 
	;this proc is the director function of the snake
	;the proc print the snake from the initial form (length=3)
	;the proc also initializes the snake's direction up
	;when the player disqualified the proc ends
	;Argument:(in stack)
		;*OFFSETOFTHESNAKE	word
	;Return value: none
	;---------------------------------------------------------
		push bp
		mov bp,sp
		push ax cx di si
		mov cx,[MyLength] ;the snake start with 3 parts
		;(now paint the snake in first time)
		mov di,OFFSETOFTHESNAKE ;the y of the snake
All_snake:
		mov si,di
		add si,2 ;push the index of x
		push RED
		call Paint
		add di,4
		loop All_snake
		push offset Random
		call food
		cmp bx,1
		jne old_play
		call StartSound
		old_play:
		push offset YAndXOfSnake
		call PassFirstStep
		push ax
		jmp cant_move_back
data_again:
		push offset MyLength
		call HowMuchToWait ;if not so put in cx - 1;
		mov cx,1
		call IimerData
		call check_neg_moves
		cmp dl,1
		je cant_move_back
		mov ax,DATRAFROMKEYBOARD
cant_move_back:
		cmp ah,ESCPRESSED
		jne others
		mov bh,2
		add sp,2
		pop si di cx ax bp
		ret 2
others:
		mov di,OFFSETOFTHESNAKE
		mov si,OFFSETOFTHESNAKE
		add si,2
		cmp ah,UPPRESSED
		jne maybe_down
		push OFFSETOFTHESNAKE
		call Growing
		sub [word ptr di],4
		call FoodORNot
		cmp bh,1
		jne continue_up
		push 4 0768h
		call Music
		add sp,2
		pop si di cx ax bp
		ret 2
continue_up:
		jmp paint_new_head
maybe_down:
		cmp ah,DOWNPRESSED
		jne maybe_left
		push OFFSETOFTHESNAKE
		call Growing
		add [word ptr di],4
		call FoodORNot
		cmp bh,1
		jne continue_down
		push 4 0768h
		call Music
		add sp,2
		pop si di cx ax bp
		ret 2
continue_down:
		jmp paint_new_head
maybe_left:
		cmp ah,LEFTPRESSED
		jne maybe_right
		push OFFSETOFTHESNAKE
		call Growing
		sub [word ptr si],4
		call FoodORNot
		cmp bh,1
		jne continue_left
		push 4 0768h
		call Music
		add sp,2
		pop si di cx ax bp
		ret 2
		continue_left:
		jmp paint_new_head
maybe_right:
		cmp ah,RIGHTPRESSED
		jne look_for_data
		push OFFSETOFTHESNAKE
		call Growing
		add [word ptr si],4
		call FoodORNot
		cmp bh,1
		jne paint_new_head
		push 4 0768h
		call Music
		add sp,2
		pop si di cx ax bp
		ret 2
paint_new_head:
		push RED
		call Paint
		look_for_data:
		jmp data_again
	endp TheSnake
	
	proc HowMuchToWait
	;----------------------------------------------------------------
	;HowMuchToWait proc
	;checks the length of the snake and raise his speed suitability
	;Argument (in stack)
		;*offset of my_length	word
	;Return value:
		;*if length under/equal 30 - cx -> 3
		;*if length under/equal 50 - cx -> 2
		;*if length above 50 - cx -> 1
	;-----------------------------------------------------------------
		push bp
		mov bp,sp
		push bx
		mov bx,[bp+4] ;offset of my_length
		cmp [byte ptr bx],3
		jbe lvl1
		cmp [byte ptr bx],5
		jbe lvl2
		mov cx,1
		jmp end_HowMuchToWait
lvl1:
		mov cx,3
		jmp end_HowMuchToWait
lvl2:
		mov cx,2
end_HowMuchToWait:
		pop bx bp
		ret 2
	endp HowMuchToWait

	proc WhereToCheckFood
	;-----------------------------------------------------------------------------------
	;WhereToCheckFood proc
	;the proc change the value of the snake's head location According to the direction
	;(uses to check if there is there a food/wall/clean area)
	;Argument:(in registers)
		;ah-the direction	byte
		;di-the addres of y location of snake's head	word
		;si-the addres of x location of snake's head	word		
	;Return value:none
	;-----------------------------------------------------------------------------------
		cmp ah,UPPRESSED
		je check_up

		cmp ah,DOWNPRESSED
		je check_down

		cmp ah,LEFTPRESSED
		je check_left

		cmp ah,RIGHTPRESSED
		je check_right
check_up:
		sub [word ptr di],4
		jmp know_where_to_check

check_down:
		add [word ptr di],4
		jmp know_where_to_check

check_left:
		sub [word ptr si],4
		jmp know_where_to_check

check_right:
		add [word ptr si],4
		jmp know_where_to_check
know_where_to_check:
		ret
	endp WhereToCheckFood


	proc Growing
	;----------------------------------------------------------------
	;Growing proc
	;the proc checks if has need to "destroy" the snake's tail or not
	;(if the snakes ate the food)
	;Argument:(in stack and Registers)
		;*OFFSETOFTHESNAKE	word
	;Return value:none
	;-----------------------------------------------------------------
		push bp
		mov bp,sp
		push cx si di
		mov di,OFFSETOFTHESNAKE
		mov si,OFFSETOFTHESNAKE
		add si,2
		push [di] [si]
		call WhereToCheckFood
		push OFFSETOFTHESNAKE
		call FoodORNot
		cmp bh,1
		jne thats_not_wall
		pop [si] [di] di si cx bp
		ret
		thats_not_wall:
		cmp bh,2
		jne not_Growing
		pop [si] [di]
		push di si
		mov di,offset Random
		mov si,offset Random
		add si,2
		push BLACK
		call Paint
		push offset Random
		call food
		pop si di
		push OFFSETOFTHESNAKE
		call tail_addres
		inc cx
		call ShiftRightData
		push 1 0568h
		call Music
		jmp exit_Growing
not_Growing:
		pop [si] [di]
		push OFFSETOFTHESNAKE
		call tail_addres
		call destroy_tail
		sub di,4
		call ShiftRightData
		exit_Growing:
		pop di si cx bp
		ret
	endp Growing


	proc InitializeMarker
	;-----------------------------------------------------------------------
	;InitializeMarker proc
	;the proc initializes the marker of the screen to the desiRED location
	;Argument:(in registers)
		;*dx-line and column	word (dh-line dl-column)
	;Return value:none
	;-----------------------------------------------------------------------
		push ax bx
		mov ah,2
		xor bh,bh
		int 10h
		pop bx ax
		ret
	endp InitializeMarker

	proc ReloadStatus
	;---------------------------------------------------
	;ReloadStatus proc
	;update the data of the status (length and score)
	;Argument:(in stack)
		;*OffsetMyScoreOrlength		word
		;*WhatToAdd		word
	;Return value:none
	;----------------------------------------------------
		push bp
		mov bp,sp
		push di ax
		mov di,OFFSETMYSCOREORLENGTH
		mov ax,WHATTOADD
		add [word ptr di],ax
		pop ax di bp
		ret 4
	endp ReloadStatus


	proc PrintInitialStatus
	;-----------------------------------------------
	;PrintInitialStatus proc
	;the proc initializes the playes data and print
	;it on the middle-left side of the screen
	;Argument:none
	;Return value:none
	;-----------------------------------------------
		push dx
		mov dx,0270h
		call InitializeMarker
		push offset PlayerName
		call PrintMyStrings
		
		mov dx,0470h
		call InitializeMarker
		mov dx,offset MyName
		add dx,2
		push dx
		call PrintMyStrings
		
		mov dx,0670h
		call InitializeMarker
		push offset Score
		call PrintMyStrings
		
		mov dx,0876h
		call InitializeMarker
		push offset MyScore
		call PrintMyStatus
		
		mov dx,0A70h
		call InitializeMarker
		push offset Length_
		call PrintMyStrings
		
		mov dx,0C76h
		call InitializeMarker
		push offset MyLength
		call PrintMyStatus
		
		mov dx,0E70h
		call InitializeMarker
		push offset Rekord
		call PrintMyStrings
		
		mov dx,1076h
		push offset CurrentRecord
		call PrintMyStatus
		
		
		pop dx
		ret
		endp PrintInitialStatus

	proc PrintMyStatus
		;---------------------------------------------------------
		;PrintMyStatus proc
		;the proc print any value it gets in the desiRED location
		;Argument:(in stack and Registers)
			;*[bp+4]= the addres of the desiRED value to print it	word
		;Return value:none			
		;---------------------------------------------------------
		push bp
		mov bp,sp
		push dx di
		call InitializeMarker
		mov di,[bp+4] ;the player - score/length/ record (of all players in the current Game)
		mov ax,[di]
		call DiveMe
continue_PrintMyStatus:
		cmp ax,0000h
		je stop_PrintMyStatus
		dec dl
		call InitializeMarker
		call DiveMe
		jmp continue_PrintMyStatus
stop_PrintMyStatus:
		pop di dx bp
		ret 2
	endp PrintMyStatus

	proc DiveMe
	;------------------------------------------------------------
	;DiveMe proc
	;the proc "breaks" (divide in 10) every digit
	;in the number in pass him to print
	;Argument:(in registers)
		;*ax - the number that need to divide	word
	;Return value:
		;ax - the parameter/10 (this digit has been print already)
	;-------------------------------------------------------------
		push bx dx ax
		xor dx,dx
		mov bx,10
		div bx
		add dl,30h
		push dx
		call PrintMyChars
		pop ax
		mov bx,10
		xor dx,dx
		div bx
		pop dx bx
		ret
	endp DiveMe
	
	proc ChangeColorTable
	;------------------------------------------------------------
	;ChangeColorTable proc
	;changes color in the pallate according to the parammeters
	;note:every parammeter from the colors is 0-255 but need to divide by 4
	;cause video palette maximal value is 63.
	;Argument (in stack)
		;*INDEXTOCHANGECOLOR	word
		;*RCOLOR	word
		;*BCOLOR	word
		;*GCOLOR	word
	;Return value:none
	;------------------------------------------------------------
		push bp
		mov bp,sp   
		push ax dx   
		
		mov   dx,3C8h       
		mov al,INDEXTOCHANGECOLOR
		out dx,al	;move the index of the changing color to port 3c8h
		inc dx
		
		;RED
		mov al, RCOLOR
		shr al, 2
		out dx, al
		
		;GREAN
		mov al, GCOLOR
		shr al, 2
		out dx, al
		
		;BLUE
		mov al, BCOLOR
		shr al, 2
		out dx, al
		pop dx ax bp
		ret 8
	 endp ChangeColorTable
	
	proc ChangeMyColors
	;-------------------------------------------------------
	;ChangeMyColors proc
	;the proc initializes the values of 5 color for the Game
	;Argument:none
	;Return value:none
	;-------------------------------------------------------
		push 0 0 0 0 ;BLACK color
		call ChangeColorTable
		
		push 1 0 0 255	;BLUE color
		call ChangeColorTable

		push 2 0 255 0 ;GREAN color
		call ChangeColorTable
		
		push 3 255 0 0	;RED color
		call ChangeColorTable
		
		push 15 255 255 255	;WHITE color
		call ChangeColorTable
		ret
	endp ChangeMyColors

	proc Game
	;---------------------------------------------------
	;Game proc
	;paint the walls for the snake
	;Argument:(in stack)
		;*offset of YWALL	word
		;*offset of XWALL	word
	;Return value:
		;If the player is disqualified - bh -> 1
		;If the player choose to quit - bh -> 2
	;----------------------------------------------------
		push bp
		mov bp,sp
		push si di
		mov si,[bp+4] ;addres of x wall
		mov di,[bp+6] ;addres of y wall
		mov [word ptr si],0
		mov [word ptr di],0
		;for "clear" the screan from the last photo
		push YWALLLEFTRIGHT XWALLUPDOWN offset YWall offset XWall BLACK
		call PaintScreen
		call ChangeMyColors

		push YWallUPWIDTH XWALLUPDOWN offset YWall offset XWall GREAN
		call PaintScreen ;for paint upper wall


		push YWALLLEFTRIGHT XWALLLEFTRIGHT offset YWall offset XWall GREAN 
		call PaintScreen ;for paint left wall

		mov [word ptr di],196
		push YWALLDOWNWIDTH XWALLUPDOWN offset YWall offset XWall GREAN
		call PaintScreen ;for paint bottom wall

		mov [word ptr di] ,0
		mov [word ptr si],252
		push YWALLLEFTRIGHT XWALLLEFTRIGHT offset YWall offset XWall GREAN 
		call PaintScreen ;for paint right wall
		mov [word ptr si],317
		push YWALLLEFTRIGHT XWALLLEFTRIGHT offset YWall offset XWall GREAN
		call PaintScreen ;paint the most right side of the screen (for the status)

		cmp bx,0
		je same_player
		push offset YWall offset XWall offset MYNAME
		call EnterPlayerName
		same_player:
		call PrintInitialStatus

		push offset YAndXOfSnake
		call TheSnake
		call CmpRecords
		pop di si bp
		ret 4
	endp Game
	
	proc CmpRecords
	push ax
	mov ax,[CurrentRecord]
	cmp ax,[RememberTakeRec]
	jbe dontChangeRec
	push offset FileRec
	call OpenFile
	;mov [MyScore],0000h
	push offset CurrentRecord
	call WriteToFile
	call CloseFile
	dontChangeRec:
	pop ax
	ret
	endp CmpRecords
	
	proc WriteToFile 
		push bp
		mov bp,sp
		push ax bx cx dx 
		mov ah,40h 
		mov bx, [filehandle]
		mov  cx,10
		mov  dx,[bp+4]
		int  21h
		pop dx cx bx ax bp
		ret 2
	endp WriteToFile
	
	proc InitializePlayerData
	;--------------------------------------------------------------
	;InitializePlayerData proc
	;the proc reebot the player's status to the initial values
	;length-3 || score-0 || name-(clean the MyName buffer)
	;Argument (in stack)
		;*[bp+12] - offset of MyName	word
		;*[bp+10] - offset of MyScore	word
		;*[bp+8] - offset of MyLength	word
		;*[bp+6] - offset of SnakeToSave	word
		;*OFFSETOFTHESNAKE	word
	;Return value:none
	;--------------------------------------------------------------
		push bp
		mov bp,sp
		push ax bx cx
		mov bx,[bp+12] ;offset of MYNAME
		inc bx ;dont touch the buffer of how much chars
		mov cx,6
		zero_string_of_MYNAME:
		mov [word ptr bx],0000h
		add bx,2
		loop zero_string_of_MYNAME

		mov bx,[bp+10] ;offset of MyScore
		mov [word ptr bx],0000h
		
		mov bx,[bp+8] ;offset of MyLength
		mov [word ptr bx],0003h
		
		;Initialize 3 first snake's parts
		mov cx,6
		mov bx,[bp+6]	;offset of SnakeToSave 
		InitializeSnake:
		mov ax,[bx]
		mov [bx+12],ax ;the next part of snake from 3 parts (in YAndXOfSnake array)
		add bx,2
		loop InitializeSnake

		mov bx,OFFSETOFTHESNAKE
		add bx,12
		mov cx,5952 ;beyond 3 part is need to clean data of snake
		zero_snake_part_2:
		mov [word ptr bx],0000h
		add bx,2
		loop zero_snake_part_2
		pop cx bx ax bp
		ret 10
	endp InitializePlayerData

	proc EnterPlayerName
	;--------------------------------------------------------------
	;EnterPlayerName proc
	;the proc gets the player's name
	;(also take care and clean the area that used to gets the name)
	;Argument:(in stack)
		;*OFFSETOFMYNAME	word
		;*offset of YWALL	word
		;*offset of XWALL	word
	;Return value:none
	;---------------------------------------------------------------
		push bp
		mov bp,sp
		push ax bx dx si di 
		mov dx,0706h
		call InitializeMarker
		mov dx,offset MsgName
		mov ah,9
		int 21h
		mov dx,090Ch
		call InitializeMarker
		mov dx,OFFSETOFMYNAME
		mov ah,0Ah
		int 21h
		mov si,dx
		xor bh,bh
		mov bl,[byte ptr si+1]
		add bx,2
		mov [byte ptr bx+si],'$'
		push offset MYNAME
		call CapsLockFirstLetter
		mov si,[bp+6] ;offset XWall
		mov di,[bp+8] ;offset YWall
		mov [word ptr si],48
		mov [word ptr di],56

		push 30 140 offset YWall offset XWall BLACK
		call PaintScreen ;paint again in BLACK the area that used to get name
		pop di si dx bx ax bp
		ret 6
	endp EnterPlayerName

	proc CapsLockFirstLetter
	;------------------------------------------------------------------
	;CapsLockFirstLetter proc
	;the proc checks if the first letter the player name is CapsLock
	;if it doesn't change it to CapsLock
	;Argument:(in stack)
		;*OFFSETOFMYNAME	word
	;Return value:none
	;------------------------------------------------------------------
		push bp
		mov bp,sp
		push ax bx
		mov bx,OFFSETOFMYNAME
		mov ah,[byte ptr bx+2] ;the first 2 buffer for get Player name from the user
		cmp ah,'a'
		jb that_Caps_lock
		sub [byte ptr bx+2],20h
that_Caps_lock:
		pop bx ax bp
		ret 2
	endp CapsLockFirstLetter


	proc OpenFile
	;-----------------------------------------------------------
	; OpenFile proc
	; opens the  currently consumed bmp file
	;Argument (in stack)
		;*NEXTFILE	word
	;Return value:
		;in the var FileHandle -> the file handle of that file
	;-----------------------------------------------------------
	push bp
	mov bp,sp
	push dx ax
	 mov  ah, 3Dh 
	 mov al,2
	 mov  dx, [bp+4] 
	 int  21h 
	 mov  [filehandle], ax 
	 pop ax dx bp
	 ret 2
	endp OpenFile

	proc CloseFile
	;-----------------------
	;CloseFile proc
	;close the current file
	;Argument:none
	;Return value:none
	;-----------------------
		push ax bx
		mov ah,3Eh 
		mov bx, [filehandle] 
		int  21h   
		pop bx ax
		ret 
	endp CloseFile

	proc ReadHeader 
	;----------------------------------
	;ReadHeader proc
	;reads the header of the file
	;Argument (in stack)
		;*offset of Header	word
	;Return value:none
	;----------------------------------
		push bp
		mov bp,sp
		push ax cx
		mov ah,3fh 
		mov bx, [filehandle] 
		mov  cx,54 
		mov  dx,[bp+4] ;offset of Header 
		int  21h
		pop cx ax bp
		ret 2
	endp ReadHeader  
 
	proc ReadPalette
	;-----------------------------------------------------------
	;ReadPalette proc
	; Read BMP file color palette, 256 colors * 4 bytes (400h)
	;Argument (in stack)
		;*offset of Palette		word
	;Return value:the pallate of the bmp file
	;-----------------------------------------------------------
		push bp
		mov bp,sp
		push ax cx
		mov ah,3fh 
		mov  cx,400h                           
		mov  dx,[bp+4] ;offset Palette 
		int  21h  
		pop cx ax bp
		ret
	endp ReadPalette 
 
	proc CopyPal
	;------------------------------------------------------------------------------------
	;CopyPal proc
	;Copy the colors palette to the video memory  
	;Argument (in stack)
		;*offset of pallate
	;Return value - the colors that needed for the currently bmp file in the video memory
	;------------------------------------------------------------------------------------
		push bp
		mov bp,sp
		push si cx dx ax
		mov     si,[bp+4] ;offset of Palette      
		mov     cx,256
	; The number of the first color should be sent to port 3C8h 
	; The palette is sent to port 3C9h		
		mov     dx,3C8h 
		mov     al,0              
		; Copy starting color to port 3C8h 
		out     dx,al
		; Copy palette itself to port 3C9h  
		inc       dx                       
PalLoop: 
		; Note: Colors in a BMP file are saved as BGR values rather than RGB. 
		mov al,[si+2]             ; Get RED value. 
		shr al,2                     ; Max. is 255, but video palette maximal 
		; value is 63.  Therefore dividing by 4. 
		out      dx,al   ; Send it. 
		mov     al,[si+1]   ; Get green value. 
		shr      al,2 
		out      dx,al                    ; Send it. 
		mov     al,[si]                  ; Get BLUE value. 
		shr      al,2 
		out      dx,al    	              ; Send it. 
		add      si,4                     ; Point to next color. 
		; (There is a null chr. after every color.) 
		loop    PalLoop
		pop ax dx cx si bp
		ret 2
	endp CopyPal  
 
	proc CopyBitmap 
	;----------------------------------------------------------
	;CopyBitmap proc
	; BMP graphics are saved upside-down. 
	; Read the graphic line by line (200 lines in VGA format), 
	; displaying the lines from bottom to top.
	;----------------------------------------------------------
		push es ax cx di dx si
		mov ax, 09FECh
		mov es, ax
		mov     cx,200   
PrintBMPLoop: 
		push    cx 
		; di = cx*320, point to the correct screen line 
		mov     di,cx                    
		shl      cx,6                     
		shl      di,8                     
		add      di,cx 
		; Read one line 
		mov     ah,3fh 
		mov     cx,320
		mov     dx,offset ScrLine 
		int      21h                      
		; Copy one line into video memory 
		cld                               ; Clear direction flag, for movsb 
		mov     cx,320 
		mov     si,offset ScrLine 
		rep      movsb                    ; Copy line to the screen       
		pop cx 
		loop    PrintBMPLoop
		pop si dx di cx ax es
		ret 
	endp CopyBitmap

	proc ShowPhotos
	;--------------------------------------------
	;StartPhoto proc
	;Shows up the needed bmp picture
	;Argument (in stack)
		;*[bp+4] - offset of the bmp file to show
	;Return value:none
	;--------------------------------------------
		push bp
		mov bp,sp
		push [bp+4];the name of the photo to show
		call OpenFile
		push offset Header
		call ReadHeader 
		push offset Palette
		call  ReadPalette 
		call CopyPal
		call CopyBitmap
		call CloseFile
		pop bp
		ret 2
	endp ShowPhotos
	
	proc TakeCurrentRecord
	push ax
	push offset FileRec
	call OpenFile
	push offset CurrentRecord
	call ReadFile
	mov ax,[CurrentRecord]
	mov [RememberTakeRec],ax
	call CloseFile
	pop ax
	ret
	endp TakeCurrentRecord
	
start:
	mov ax, @data
	mov ds, ax
	call TakeCurrentRecord
	;mov [CurrentRecord],0000h
	;mov [RememberTakeRec],0000h
	;use 2 lines to restart to record :)
	call InitRandom	
	call OpenGraphicMode
	
go_main:
;menu screen
	push offset StartPhoto
	call ShowPhotos
	
data_from_main:

	mov ah,0
	int 16h
	mov bx,1 ;need to ask a new player name
	cmp ah,SPRESSED
	je go_Game
	cmp ah,IPRESSED
	je show_rules
	cmp ah,ESCPRESSED
	jne data_from_main
	jmp Quit_Game
	
show_rules:

	;rules screen
	push offset Instractions
	call ShowPhotos

data_from_rules_screen:

	mov ah,0
	int 16h
	cmp ah,BPRESSED
	je go_main
	jmp data_from_rules_screen
	push YWALLLEFTRIGHT XWALLUPDOWN offset YWall offset XWall BLACK
	call PaintScreen
 
go_Game:
	
	push offset YWall offset XWall
	call Game ;Game screen
	cmp bh,1
	jne pause_screen ;the player choose to stop playing
	;lose screen
	push offset GameOver
	call ShowPhotos
	push offset MYNAME offset MyScore offset MyLength offset SnakeToSave offset YAndXOfSnake
	call InitializePlayerData
	
data_from_Gameover_screen:

	mov ah,0
	int 16h
	cmp ah,ESCPRESSED
	je Quit_Game
	cmp ah,SPRESSED
	jne data_from_Gameover_screen
	mov bx,1	;need to ask a new name
	jmp go_Game

;pause screen
pause_screen:

	push offset pausee
	call ShowPhotos
	data_for_pause:
	mov ah,0
	int 16h
	cmp ah,ESCPRESSED
	je Quit_Game
	cmp ah,BPRESSED
	jne data_for_pause
	mov bx,0	;doesn't need a new player name
	jmp go_Game

;quit screen
Quit_Game:
	
	push offset MYNAME offset MyScore offset MyLength offset SnakeToSave offset YAndXOfSnake
	call InitializePlayerData
	push offset QuitGame
	call ShowPhotos
	
data_for_QuitGame:

	mov ah,0
	int 16h
	cmp ah,ESCPRESSED
	je exit
	cmp ah,MPRESSED
	jne data_for_QuitGame
	jmp go_main

exit:
	call CloseGraphicMode
	mov ax, 4c00h
	int 21h
	END start