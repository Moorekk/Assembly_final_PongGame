TITLE final_ballgame of ASM             

INCLUDE Irvine32.inc
	startPosy = 1
	startPosx = 0
	fps = 10				;update screen every fps ms
	ballrate = 80			;ballspeed, move ball every ballrate ms
	ttWidth = 60
	ttHight = 14
	paddleWidth = 2
	paddleHeight = 4		;paddleheight
	endscore = 10	
	numberLength = 7
	initial PROTO
	restartinitial PROTO
	;stages
	startStage PROTO
	gameStage PROTO
	countdownStage PROTO
	endgameStage PROTO
	;use for gameStage
	updatescreen PROTO
	keyboardInput PROTO
	moveball PROTO
	;draw the console
	drawstart PROTO
	drawframe PROTO
	drawpaddle PROTO, paddlePos:COORD
	drawball PROTO
main          EQU start@0

.data
	;time data
	tickCountStart DWORD ?
	tickCountNow DWORD ?
	tickCountIntervalStart DWORD ?
	tickBallMoveIntervalStart DWORD ?
	count BYTE 0
	;console data
	outputHandle DWORD 0
	cellsWritten DWORD ?
	pos COORD <>
	;start stage data
	ball1 BYTE " ___    _    _     _       ___    _    __  __  ___ "
 	ball2 BYTE "| _ )  /_\  | |   | |     / __|  /_\  |  \/  || __|"
 	ball3 BYTE "| _ \ / _ \ | |__ | |__  | (_ | / _ \ | |\/| || _| "
 	ball4 BYTE "|___//_/ \_\|____||____|  \___|/_/ \_\|_|  |_||___|"
	screenballPos COORD <startPosx + 11, startPosy>
	screenballLength DWORD ?
	PressToStart1 BYTE ".-. .-. .-. .-. .-.   .-. .-. .-. .-. .-.   .-. .-.   .-. .-. .-. .-. .-. "
	PressToStart2 BYTE "|-' |(  |-- `-. `-.   `-. |-' |-| |   |--    |  | |   `-.  |  |-| |(   |  "
	PressToStart3 BYTE "'   ' ' `-' `-' `-'   `-' '   ` ' `-' `-'    '  `-'   `-'  '  ' ' ' '  '  "
	presstostartPos COORD <startPosx, startPosy + 8>
	presstostartLength DWORD ?
	;count down stage data
	number10 BYTE "   _   "
	number11 BYTE "  / |  "
	number12 BYTE "  | |  "
	number13 BYTE " _| |_ "
	number14 BYTE "|_____|"
	
	number20 BYTE " ____  "
	number21 BYTE "|___ \ "
	number22 BYTE "  __) |"
	number23 BYTE " / __/ "
	number24 BYTE "|_____|"

	number30 BYTE " _____ "
	number31 BYTE "|___ / "
	number32 BYTE " |_  \ "
	number33 BYTE "  _)  |"
	number34 BYTE "|____/ "
	number3Pos COORD <startPosx, startPosy + ttHight + 3>
	number2Pos COORD <startPosx + numberLength + 2, startPosy + ttHight + 3>
	number1Pos COORD <startPosx + numberLength * 2 + 4, startPosy + ttHight + 3>
	;end game stage data
	winstring BYTE "you wins!"
	losestring BYTE "you loses..."
	AstringPos COORD <11,7>
	BstringPos COORD <37,7>
	;frame data
	frame1 BYTE	0DAh, (ttWidth - 2) dup(0C4h), 0BFh
	frame2 BYTE 0B3h, (ttWidth - 2) DUP(' '), 0B3h
	frame3 BYTE 0C0h, (ttWidth - 2) DUP(0C4h), 0D9h
	attribute WORD (ttWidth * 3) DUP(0Ah)
	framePos COORD <startPosx,startPosy>
	scorestring BYTE "score     :    ",'0'
	scoreA BYTE ?
	scoreB BYTE ?
	scorestringPos COORD <startPosx, startPosy + ttHight + 1>
	scoreAPos COORD <startPosx + 8, startPosy + ttHight + 1>
	scoreBPos COORD <startPosx + 12, startPosy + ttHight + 1>
	;paddle data
	paddleAinitPos COORD <startPosx + 1, 4>
	paddleBinitPos COORD <startPosx + ttWidth - 3, 4>
	paddleAPos COORD <>
	paddleBPos COORD <>
	paddle1 BYTE 0DAh, 0BFh
	paddle2 BYTE 2 DUP(0B3h)
	paddle3 BYTE 0C0h, 0D9h
	;ball data
	ballinitPos COORD <30,7>
	ballPos COORD <>
	ballunitDir COORD <2,1>
	ballsignDir COORD <1,1>
	ball BYTE 0FEh
.code
main PROC
	tostartstage::
	INVOKE initial
	INVOKE startStage
	exit
main ENDP
initial PROC
	INVOKE GetStdHandle, STD_OUTPUT_HANDLE
	mov outputHandle, eax
	call Clrscr

	mov scoreA, 0
	mov scoreB, 0
	mov screenballLength, LENGTHOF ball1
	mov presstostartLength, LENGTHOF PressToStart1

	ret
initial ENDP
restartinitial PROC
	mov ax, paddleAinitPos.x
	mov paddleAPos.x, ax
	mov ax, paddleAinitPos.y
	mov paddleAPos.y, ax
	
	mov ax, paddleBinitPos.x
	mov paddleBPos.x, ax
	mov ax, paddleBinitPos.y
	mov paddleBPos.y, ax
	
	mov ax, ballinitPos.x
	mov ballPos.x, ax
	mov ax, ballinitPos.y
	mov ballPos.y, ax
	
	call Randomize
	mov eax, 4
	call RandomRange
	inc eax
	.IF eax == 2
		mov ballsignDir.y, -1
	.ELSEIF eax == 3
		mov ballsignDir.x, -1
	.ELSEIF eax == 4
		mov ballsignDir.x, -1
		mov ballsignDir.y, -1
	.ENDIF
	ret
restartinitial ENDP
startStage PROC
	INVOKE drawstart
	call ReadChar
	startwhile:
	.IF al == 20h 		;space
		INVOKE gameStage
	.ELSEIF al == 1Bh 	;esc
		call Clrscr
		exit
	.ENDIF
	jmp startwhile
	ret
startStage ENDP
gameStage PROC
	restart::
	;if game end(somebody wins)
	.IF scoreA >= endscore || scoreB >=endscore
		INVOKE endgameStage
	.ENDIF

	INVOKE restartinitial
	INVOKE countdownStage
	
	INVOKE GetTickCount
	mov tickCountStart, eax
	mov tickCountIntervalStart, eax
	mov tickBallMoveIntervalStart, eax
	mainwhile:
		;input keyboard
		INVOKE keyboardInput
		mov eax, 10
		call Delay
		;get time
		INVOKE GetTickCount
		mov tickCountNow, eax
		;if to move the ball
		sub eax, tickBallMoveIntervalStart
		cmp eax, ballrate
			jl ballnotmove
			INVOKE moveball
			mov eax, tickCountNow
			mov tickBallMoveIntervalStart, eax
		ballnotmove:
		mov eax, tickCountNow
		sub eax, tickCountIntervalStart
		;if to update screen
		cmp eax, fps			;frame interval over 100 ms
			jl continue 		;if every 100 ms
			INVOKE updatescreen
			mov eax, tickCountNow
			mov tickCountIntervalStart, eax
		continue:
		jmp mainwhile
	ret
gameStage ENDP
updatescreen PROC
	;update screen
	call ClrScr
	INVOKE drawframe
	INVOKE drawpaddle, paddleAPos
	INVOKE drawpaddle, paddleBPos
	INVOKE drawball
	ret
updatescreen ENDP
countdownStage PROC
	call Clrscr
	INVOKE drawframe
	INVOKE drawpaddle, paddleAPos
	INVOKE drawpaddle, paddleBPos
	INVOKE drawball
	mov eax, 1000
	call Delay
	;draw number3
	mov ax, number3Pos.x
	mov pos.x, ax
	mov ax, number3Pos.y
	mov pos.y, ax
	INVOKE WriteConsoleOutputCharacter, outputHandle, addr number30, numberLength, pos, addr cellsWritten
	inc pos.y
	INVOKE WriteConsoleOutputCharacter, outputHandle, addr number31, numberLength, pos, addr cellsWritten
	inc pos.y
	INVOKE WriteConsoleOutputCharacter, outputHandle, addr number32, numberLength, pos, addr cellsWritten
	inc pos.y
	INVOKE WriteConsoleOutputCharacter, outputHandle, addr number33, numberLength, pos, addr cellsWritten
	inc pos.y
	INVOKE WriteConsoleOutputCharacter, outputHandle, addr number34, numberLength, pos, addr cellsWritten
	inc pos.y
	mov eax, 1000
	call Delay
	;draw number2
	mov ax, number2Pos.x
	mov pos.x, ax
	mov ax, number2Pos.y
	mov pos.y, ax
	INVOKE WriteConsoleOutputCharacter, outputHandle, addr number20, numberLength, pos, addr cellsWritten
	inc pos.y
	INVOKE WriteConsoleOutputCharacter, outputHandle, addr number21, numberLength, pos, addr cellsWritten
	inc pos.y
	INVOKE WriteConsoleOutputCharacter, outputHandle, addr number22, numberLength, pos, addr cellsWritten
	inc pos.y
	INVOKE WriteConsoleOutputCharacter, outputHandle, addr number23, numberLength, pos, addr cellsWritten
	inc pos.y
	INVOKE WriteConsoleOutputCharacter, outputHandle, addr number24, numberLength, pos, addr cellsWritten
	inc pos.y
	mov eax, 1000
	call Delay
	;draw number1
	mov ax, number1Pos.x
	mov pos.x, ax
	mov ax, number1Pos.y
	mov pos.y, ax
	INVOKE WriteConsoleOutputCharacter, outputHandle, addr number10, numberLength, pos, addr cellsWritten
	inc pos.y
	INVOKE WriteConsoleOutputCharacter, outputHandle, addr number11, numberLength, pos, addr cellsWritten
	inc pos.y
	INVOKE WriteConsoleOutputCharacter, outputHandle, addr number12, numberLength, pos, addr cellsWritten
	inc pos.y
	INVOKE WriteConsoleOutputCharacter, outputHandle, addr number13, numberLength, pos, addr cellsWritten
	inc pos.y
	INVOKE WriteConsoleOutputCharacter, outputHandle, addr number14, numberLength, pos, addr cellsWritten
	inc pos.y
	ret
countdownStage ENDP
endgameStage PROC
	call Clrscr
	INVOKE drawframe
	INVOKE drawpaddle, paddleAPos
	INVOKE drawpaddle, paddleBPos
	.IF scoreA >= endscore
	;draw winstring
		INVOKE WriteConsoleOutputCharacter, outputHandle, addr winstring, LENGTHOF winstring, AstringPos, addr cellsWritten
		INVOKE WriteConsoleOutputCharacter, outputHandle, addr losestring, LENGTHOF losestring, BstringPos, addr cellsWritten
	.ELSEIF scoreB >= endscore
		INVOKE WriteConsoleOutputCharacter, outputHandle, addr winstring, LENGTHOF winstring, BstringPos, addr cellsWritten
		INVOKE WriteConsoleOutputCharacter, outputHandle, addr losestring, LENGTHOF losestring, AstringPos, addr cellsWritten
	.ENDIF
	checkinput:
	call ReadChar
	.IF al == 1Bh ;esc
		jmp tostartstage
	.ENDIF
	mov ecx, 1000
	call Delay
	jmp checkinput
	ret
endgameStage ENDP
moveball PROC
	;move the ballPos 
	;ballmove = ballunitDir * ballsignDir
	;ballPos = ballPos + ballmove
	;for x
	xor eax, eax
	xor ebx, ebx
	mov ax, ballsignDir.x
	and ax, 10000000b
	mov bx, ballunitDir.x
	.IF ax == 00h		;+
		add ballPos.x, bx
	.ELSEIF ax == 80h	;-
		sub ballPos.x, bx
	.ENDIF
	;for y
	xor eax, eax
	xor ebx, ebx
	mov ax, ballsignDir.y
	and ax, 10000000b
	mov bx, ballunitDir.y
	.IF ax == 00h		;+
		add ballPos.y, bx
	.ELSEIF ax == 80h	;-
		sub ballPos.y, bx
	.ENDIF

	;if out of range
	;change y dir(upperbound or lowerbound)
	.IF ballPos.y <= (startPosy + 1)
		mov ax, ballsignDir.y
		xor ax, 10000000b
		mov ballsignDir.y, ax
	.ELSEIF ballPos.y >= (startPosy + ttHight - 2)
		mov ax, ballsignDir.y
		xor ax, 10000000b
		mov ballsignDir.y, ax
	.ENDIF
	;change x dir
	;if touch paddle
	.IF ballPos.x <= (startPosx + 4) 
		mov bx, paddleAPos.y
		mov cx, paddleAPos.y
		add cx, paddleHeight
		.IF (ballPos.y >= bx) && (ballPos.y <= cx)
			mov ax,ballsignDir.x
			xor ax, 10000000b
			mov ballsignDir.x, ax
		.ENDIF

	.ELSEIF ballPos.x >= (startPosx + ttWidth - 4) 
		mov bx, paddleBPos.y
		mov cx, paddleBPos.y
		add cx, paddleHeight
		.IF (ballPos.y >= bx) && (ballPos.y <= cx)
			mov ax, ballsignDir.x
			xor ax, 10000000b
			mov ballsignDir.x, ax
		.ENDIF
	.ENDIF

	;if score
	.IF ballPos.x <= (startPosx + 1)
		inc scoreB
		jmp restart
	.ELSEIF ballPos.x >= (startPosx + ttWidth - 1)
		inc scoreA
		jmp restart
	.ENDIF
	
	ret
moveball ENDP
keyboardInput PROC
	call ReadKey
	jz nokeypress
	;w,s for playerA
	;o,l for playerB
	.IF al == 57h || al == 77h		;W, w
		dec paddleAPos.y
	.ELSEIF al == 53h || al == 73h	;S, s
		inc paddleAPos.y
	.ELSEIF al == 4Fh || al == 6Fh	;O, o
		dec paddleBPos.y
	.ELSEIF al == 4Ch || al == 6Ch	;L, l
		inc paddleBPos.y
	.ELSEIF al == 1Bh				;esc
		jmp tostartstage
	.ENDIF

	;check if the paddle out of range
	paddlelowerBound = ttHight - paddleHeight
	paddleupperBound = 2
	;check paddleA
	mov ax, paddlelowerBound
	cmp ax, paddleAPos.y			;paddleA out of lowerBound
	jge notout1
	mov paddleAPos.y, paddlelowerBound
	notout1:
	mov ax, paddleupperBound
	cmp ax, paddleAPos.y			;paddleA out of upperBound
	jle notout2
	mov paddleAPos.y, paddleupperBound
	notout2:
	;check paddleB
	mov ax, paddlelowerBound
	cmp ax, paddleBPos.y			;paddleB out of lowerBound
	jge notout3
	mov paddleBPos.y, paddlelowerBound
	notout3:
	mov ax, paddleupperBound
	mov ax, 2
	cmp ax, paddleBPos.y			;paddleB out of upperBound
	jle nokeypress
	mov paddleBPos.y, paddleupperBound
	nokeypress:
	INVOKE FlushConsoleInputBuffer, outputHandle
	ret
keyboardInput ENDP
drawstart PROC
	call Clrscr
	writescreen:
	mov ax, screenballPos.x
	mov pos.x, ax
	mov ax, screenballPos.y
	mov pos.y, ax
	INVOKE WriteConsoleOutputCharacter, outputHandle, addr ball1, screenballLength, pos, addr cellsWritten
	inc pos.y
	INVOKE WriteConsoleOutputCharacter, outputHandle, addr ball2, screenballLength, pos, addr cellsWritten
	inc pos.y
	INVOKE WriteConsoleOutputCharacter, outputHandle, addr ball3, screenballLength, pos, addr cellsWritten
	inc pos.y
	INVOKE WriteConsoleOutputCharacter, outputHandle, addr ball4, screenballLength, pos, addr cellsWritten
	mov ax, presstostartPos.x
	mov pos.x, ax
	mov ax, presstostartPos.y
	mov pos.y, ax
	INVOKE WriteConsoleOutputCharacter, outputHandle, addr PressToStart1, presstostartLength, pos, addr cellsWritten
	inc pos.y
	INVOKE WriteConsoleOutputCharacter, outputHandle, addr PressToStart2, presstostartLength, pos, addr cellsWritten
	inc pos.y
	INVOKE WriteConsoleOutputCharacter, outputHandle, addr PressToStart3, presstostartLength, pos, addr cellsWritten
	ret

drawstart ENDP
drawframe PROC
	mov ax, framePos.x
	mov pos.x, ax
	mov ax, framePos.y
	mov pos.y, ax
	INVOKE WriteConsoleOutputAttribute, outputHandle, addr attribute, ttWidth, pos, addr cellsWritten
    INVOKE WriteConsoleOutputCharacter, outputHandle, addr frame1, ttWidth, pos, addr cellsWritten
	inc pos.y
	
	INVOKE WriteConsoleOutputAttribute, outputHandle, addr attribute, ttWidth, pos, addr cellsWritten
	mov ecx, (ttHight-2)
	framemiddle:
	push ecx
	INVOKE WriteConsoleOutputCharacter, outputHandle, addr frame2, ttWidth, pos, addr cellsWritten
	inc pos.y
	pop ecx
	LOOP framemiddle
	INVOKE WriteConsoleOutputAttribute, outputHandle, addr attribute, ttWidth, pos, addr cellsWritten
 
    INVOKE WriteConsoleOutputCharacter, outputHandle, addr frame3, ttWidth, pos, addr cellsWritten

	;draw score
	INVOKE WriteConsoleOutputCharacter, outputHandle, addr scorestring, 11, scorestringPos, addr cellsWritten

	INVOKE SetConsoleCursorPosition, outputHandle, scoreAPos
	xor eax, eax
	mov al, scoreA
	call WriteDec
	INVOKE SetConsoleCursorPosition, outputHandle, scoreBPos
	xor eax, eax
	mov al, scoreB
	call WriteDec
	
	ret
drawframe ENDP
drawpaddle PROC, paddlePos: COORD
	mov ax, paddlePos.x
	mov pos.x, ax
	mov ax, paddlePos.y
	mov pos.y, ax
	INVOKE WriteConsoleOutputCharacter, outputHandle, addr paddle1, paddleWidth, pos, addr cellsWritten
	inc pos.y
	mov ecx, (paddleHeight-2)
	paddlemiddle:
	push ecx
	INVOKE WriteConsoleOutputCharacter, outputHandle, addr paddle2, paddleWidth, pos, addr cellsWritten
	inc pos.y
	pop ecx
	LOOP paddlemiddle
    INVOKE WriteConsoleOutputCharacter, outputHandle, addr paddle3, paddleWidth, pos, addr cellsWritten
	ret
drawpaddle ENDP
drawball PROC
	mov ax, ballPos.x
	mov pos.x, ax
	mov ax, ballPos.y
	mov pos.y, ax
	INVOKE WriteConsoleOutputCharacter, outputHandle, addr ball, 1, pos, addr cellsWritten
	ret
drawball ENDP
END main