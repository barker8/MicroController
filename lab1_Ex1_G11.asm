;SHAI COHEN    061985453
;this program write on the LCD our names
;the program desplay "BARAK AVREKI" and move it to the right.
;in the end of the line the last char move to the first cell in the second line
LIST 	P=PIC16F877
include 	<P16f877.inc>
org		0x00



reset:
	nop

	goto start

	org		0x10

start:	


	bcf			STATUS, RP0
	bcf			STATUS, RP1	;bank 0
	clrf		PORTD		;clear old data from PORTD
	clrf		PORTE		;clear old data from PORTE

	bsf			STATUS, RP0	;bank 1
	movlw		0x06		;set data as digital
	movwf		ADCON1
	bcf			INTCON,GIE			;No interrupt
	movlw		0x0F
	movwf		TRISB
	bcf			OPTION_REG,0x7		;Enable PortB Pull-Up
	clrf 		TRISE
	clrf		TRISD
	bcf			STATUS,RP0			;Bank0	
 	call		init	;initialized the LCD


	movlw		0x08
	movwf		0x5f		;count 7 times for print result
	clrf		0x44		; capture the numbera
	movlw		0x05		; for the counter num A
	movwf		0x40
	movlw		0x01		; for the counter num B
	movwf		0x50
	movlw		0x01		; for the counter print B
	movwf		0x42	
	movlw		0x01		; for the counter print B
	movwf		0x43	
	movlw		0x80		 ;PLACE for the data on the LCD
	movwf		0x61
	
			
	movlw		0x05
	movwf		0x88		;print B
	goto		disp_a		;print A
wkb:
	

		bcf			PORTB,0x4			;scan Row 1
		bsf			PORTB,0x5
		bsf			PORTB,0x6
		bsf			PORTB,0x7
		btfss		PORTB,0x0
		goto		kb01
		btfss		PORTB,0x1
		goto		kb02
		btfss		PORTB,0x2
		goto		kb03
		btfss		PORTB,0x3
		goto		kb0a

		bsf			PORTB,0x4
		bcf			PORTB,0x5			;scan Row 2
		btfss		PORTB,0x0
		goto		kb04
		btfss		PORTB,0x1
		goto		kb05
		btfss		PORTB,0x2
		goto		kb06
		btfss		PORTB,0x3
		goto		kb0b

		bsf			PORTB,0x5
		bcf			PORTB,0x6			;scan Row 3
		btfss		PORTB,0x0
		goto		kb07
		btfss		PORTB,0x1
		goto		kb08
		btfss		PORTB,0x2
		goto		kb09
		btfss		PORTB,0x3
		goto		kb0c

		bsf			PORTB,0x6
		bcf			PORTB,0x7			;scan Row 4
		btfss		PORTB,0x0
		goto		kb0e
		btfss		PORTB,0x1
		goto		kb00
		btfss		PORTB,0x2
		goto		kb0f
		btfss		PORTB,0x3
		goto		kb0d

		goto		wkb

kb00:	movlw		0x00
		goto		disp_0
kb01:	movlw		0x01
		goto		disp_1
kb02:	movlw		0x02
		goto		disp_0
kb03:	movlw		0x03
		goto		disp_error
kb04:	movlw		0x04
		goto		disp_error
kb05:	movlw		0x05
		goto		disp_error
kb06:	movlw		0x06
		goto		disp_error
kb07:	movlw		0x07
		goto		disp_error
kb08:	movlw		0x08
		goto		disp_error
kb09:	movlw		0x09
		goto		disp_error
kb0a:	movlw		0x0A
		goto		disp_error
kb0b:	movlw		0x0B
		goto		disp_error
kb0c:	movlw		0x0C
		goto		disp_error
kb0d:	movlw		0x0D
		goto		disp_error
kb0e:	movlw		0x0E
		goto		disp_error
kb0f:	movlw		0x0F
		goto		disp_error

;**********************************************************
disp_0:
	
		movlw	0x30			; CHAR (the data )
		movwf	0x20
		call 	lcdd
		call	mdel
		BCF		0x44,0
		RLF		0x44, 1
		goto 	counter
;**********************************************************
disp_1:
	
		movlw	0x31			; CHAR (the data )
		movwf	0x20
		call 	lcdd
		call	mdel
		BSF		0x44,0
		RLF		0x44, 1	
		goto 	counter

		 	
;**********************************************************
disp_a:
		movlw	0x41			; CHAR (the data )
		movwf	0x20
		call 	lcdd
		call	mdel
		goto 	counter
;**********************************************************
disp_b:
		decfsz	0x42,1
		goto 	disp_c
		movlw	0x42			; CHAR (the data )
		movwf	0x20
		call 	lcdd
		call	mdel
		movlw	0x05		; for the counter 4 num 
		movwf	0x40
		call 	num_A
		goto 	counter
		
		
;**********************************************************
disp_c:
		
		decfsz	0x43,1
		goto 	result
		movlw	0xbf		
		movwf	0x61
		call	rotate_down
		movlw	0x43			; CHAR (the data )
		movwf	0x20
		call 	lcdd
		call	mdel
		movlw	0x05		; for the counter 4 num
		movwf	0x40
		call	num_B
		goto	counter
		

counter:

rotate_right:
		incf	0x61,1
		movf	0x61,w
		movwf	0x20
		call 	lcdc
		call	mdel

		decfsz	0x40,1
		goto	wkb
		goto	disp_b

rotate_down:
		incf	0x61,1
		movf	0x61,w
		movwf	0x20
		call 	lcdc
		call	mdel
		return
	

disp_error:
	;******************************E***************************
	movlw	0xc5			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	movlw	0x45			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
;******************************R***************************	
	movlw	0xc6			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	movlw	0x52			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
;******************************R***************************	
	movlw	0xc7			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	movlw	0x52			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
;******************************O***************************
	movlw	0xc8			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	movlw	0x4F			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
;******************************R***************************	
	movlw	0xc9			 ;PLACE for the data on the LCD
	movwf	0x20
	call 	lcdc
	call	mdel

	movlw	0x52			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel


	call	delay_2
	goto	start	

num_A:
	clrf	0x45	;num a
	RRF		0x44, 1
	movf	0x44,w
	movwf	0x45	;num a	
	clrf	0x44
	return

num_B:
	clrf	0x46	;num b
	RRF		0x44,1
	BCF		0x44,7
	movf	0x44,w
	movwf	0x46	;num b	
	clrf	0x44
	return

num_C:
	clrf	0x47	;num b
	RRF		0x44,1
	BCF		0x44,7
	movf	0x44,w
	movwf	0x47	;num b	
	clrf	0x44
	return

B_D_F:
	BTFSS	0x47,2
	call	a_power_b	;1000	
	BTFSS 	0x47,3
	call	a_double_b	;0100
	call	zeros_in_b	;1100
	return

A_C_E_G:
	BTFSC	0x47,2	
	call	a_parts_b
	BTFSS	0x47,3
	call 	a_minus_b
	BTFSS	0x47,0
	goto	ones_in_a
	goto	ones_pairs_in_b
	return
result:
 	call	init	;initialized the LCD
	call	num_C
	BTFSS	0x47,1	
	call	B_D_F	
	call	A_C_E_G		
	goto 	end_code


a_parts_b:
	goto end_code
	return

a_power_b
	movlw	0x85	;place on lcd
	movwf	0x61
	call 	rotate_down
;******************************A***************************
	movlw	0x41			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
;******************************^*"***************************
	movlw	0x5e			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
;******************************B***************************
	movlw	0x42			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
;******************************=***************************
	movlw	0x3d			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
	
	movf	0x45,w
	movwf	0x48
	movwf	0x60
	movf	0x46,w
	movwf	0x49
	decf	0x49,1
	movlw	0x01
	movwf	0x4a
	clrf	0x4b
Conversions:
	decfsz	0x4a,0
	incf	0x4b,1
	movf	0x48,w
	movwf	0x46
	movf	0x60,w
	movwf	0x45
	movlw	0x07
	movwf	0x21
	clrf	0x60
	movlw	0x02
	movwf	0x4a
	clrw
	call	multiplication
	decfsz	0x49,1
	goto	Conversions
	movlw	0xc3	;place on lcd
	movwf	0x61
	btfss	0x4b,0
	goto	special
	goto	print

special:
	incf	0x49,1
	btfss	0x49,0
	goto	Zero_situation
 	goto	One_situation
Zero_situation:
	movlw	0x01
	movwf	0x60
	goto	print
One_situation:	
	movf	0x48,w
	movwf	0x60
	goto	print



a_double_b:

	movlw	0x85	;place on lcd
	movwf	0x61
	call 	rotate_down
;******************************A***************************
	movlw	0x41			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
;****************************** "*" ***************************
	movlw	0x2a			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
;******************************B***************************
	movlw	0x42			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
;******************************=***************************
	movlw	0x3d			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down

	movlw	0x07
	movwf	0x21
	clrf	0x60
	clrw
	call	multiplication
	movlw	0xc3	;place on lcd
	movwf	0x61
	goto	print

multiplication:
	btfsc	0x46,0
	addwf	0x45,0x60
	movwf	0x60
	rlf		0x45,1
	rrf		0x46,1
	decfsz	0x21,1
	goto	multiplication
	return
	
a_minus_b:

	movlw	0x85	;place on lcd
	movwf	0x61
	call 	rotate_down
;******************************A***************************
	movlw	0x41			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
;******************************-***************************
	movlw	0x2d			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
;******************************B***************************
	movlw	0x42			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
;******************************=***************************
	movlw	0x3d			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down


	movlw 	0X00
   	movwf 	0X60
    movf	0x46,w	
	subwf 	0X45,0
	movwf 	0X60
	movlw	0xc3	;place on lcd
	movwf	0x61
	btfss	0x60,07
	goto	print
	movf	0x45,w	
	subwf 	0X46,0
	movwf 	0X60
	bsf		0x60,07
	goto	print
	
ones_pairs_in_b:

	movlw	0x82		
	movwf	0x61
	call 	rotate_down
;******************************P***************************
	movlw	0x50			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
;******************************A***************************
	movlw	0x41			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
;******************************I***************************
	movlw	0x49			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
;******************************R***************************
	movlw	0xc9			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
;******************************S***************************
	movlw	0x53			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
;******************************space***************************
	call 	rotate_down
;******************************I***************************
	movlw	0x49			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
;******************************N***************************
	movlw	0x4e			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
;******************************space***************************
	call 	rotate_down
;******************************A***************************
	movlw	0x42			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
;******************************:***************************
	movlw	0x3a			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel	
	call 	rotate_down

	movlw	0x05
	movwf 	0x21
	movlw 	0x01
	movwf 	0x22
	movlw	0x00
	movwf 	0x60
	movlw	0xc3	;place on lcd
	movwf	0x61

x:
	decfsz	0x21,1
	goto	y	
	goto	print
y:
	decfsz	0x22,1
	rrf		0x46,1	
	btfss	0x46,0
	goto	x
	rrf		0x46,1
	decf	0x21,1
	btfss	0x46,0	
	goto	x
	incf	0x60,1
	goto	x



ones_in_a:
	movlw	0x82		
	movwf	0x61
	call 	rotate_down
;******************************O***************************
	movlw	0x4f			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
;******************************N***************************
	movlw	0x4e			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
;******************************E***************************
	movlw	0x45			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
;******************************S***************************
	movlw	0x53			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
;******************************space***************************
	call 	rotate_down
;******************************I***************************
	movlw	0x49			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
;******************************N***************************
	movlw	0x4e			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
;******************************space***************************
	call 	rotate_down
;******************************A***************************
	movlw	0x41			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
;******************************:***************************
	movlw	0x3a			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel	
	call 	rotate_down

	movlw 0x04
	movwf 0x21
	movlw 0x00
	movwf 0x60

e:
	btfsc 	0x45,0
	incf	0x60,1
	rrf		0x45,1	
	decfsz	0x21,1
	goto	e
	movlw	0xc3	;place on lcd
	movwf	0x61
;	movf	0x60,w	
;	movwf	0x62
	goto print	

zeros_in_b:	
movlw	0x82		
	movwf	0x61
	call 	rotate_down
;******************************Z***************************
	movlw	0x5a			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
;******************************E***************************
	movlw	0x45			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
;******************************R***************************
	movlw	0x52			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
;******************************O***************************
	movlw	0x4F			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
;******************************S***************************
	movlw	0x53			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
;******************************space***************************
	call 	rotate_down
;******************************I***************************
	movlw	0x49			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
;******************************N***************************
	movlw	0x4e			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
;******************************space***************************
	call 	rotate_down
;******************************B***************************
	movlw	0x42			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel
	call 	rotate_down
;******************************:***************************
	movlw	0x3a			; CHAR (the data )
	movwf	0x20
	call 	lcdd
	call	mdel	
	call 	rotate_down


	movlw 0x04
	movwf 0x21
	movlw 0x00
	movwf 0x60

q:
	btfss 	0x46,0
	incf	0x60,1
	rrf		0x46,1	
	decfsz	0x21,1
	goto	q
	movlw	0xc3	;place on lcd
	movwf	0x61
	movf	0x60,w	
	movwf	0x62
	goto	 print	
	
	

print:
	call 	rotate_down
	btfsc	0x60,7
	call	disp1	
	call	disp0
z:
	rlf		0x60,1
	decfsz	0x5f,1
	goto	print
;	goto	start
	goto	end_code	
;**********************************************************
disp0:
	
		movlw	0x30			; CHAR (the data )
		movwf	0x20
		call 	lcdd
		call	mdel
		BCF		0x44,0
		RLF		0x44, 1
		return
;**********************************************************
disp1:
	
		movlw	0x31			; CHAR (the data )
		movwf	0x20
		call 	lcdd
		call	mdel
		BSF		0x44,0
		RLF		0x44, 1	
		goto 	z
		return

		 	
;**********************************************************
	
;*****************************
;subroutine to initialize LCD*
;*****************************
init:	bcf	STATUS,RP0

	movlw	0x30
	movwf	0x20
	call 	lcdc
	call	del_41

	movlw	0x30
	movwf	0x20
	call 	lcdc
	call	del_01

	movlw	0x30
	movwf	0x20
	call 	lcdc
	call	mdel

	movlw	0x01		; display clear
	movwf	0x20
	call 	lcdc
	call	mdel

	movlw	0x06		; I/D=1,S=0 :increment,no  shift 000001 I/D S
	movwf	0x20
	call 	lcdc
	call	mdel

	movlw	0x0e		; D=1,C=B=0 :set display ,no cursor, no blinking
	movwf	0x20
	call 	lcdc
	call	mdel

	movlw	0x38		; dl=1 ( 8 bits interface,n=1:2 lines,f=0:5x7 dots)
	movwf	0x20
	call 	lcdc
	call	mdel
	return

;***********************************
;subroutine to write command to LCD*
;***********************************

lcdc:	bcf		STATUS, RP0        
		movlw		0x00		; E=0,RS=0 
		movwf		PORTE
		movf		0x20,w
		addlw		0x71
		btfsc		STATUS,0x01
		goto		one
		goto		two
one:	
		movlw		0xc0
		goto		three
two:	
		movf		0x20,w
		movwf		PORTD
three:	
		movlw		0x01		; E=1,RS=0
		movwf		PORTE

        call		sdel

		movlw		0x00		; E=0,RS=0
		movwf		PORTE
return

;***********************************
;subroutine to write data to LCD*
;***********************************

lcdd:	bcf		STATUS, RP0        
		movlw		0x02		; E=0, RS=1
		movwf		PORTE
		movf		0x20,w
		movwf		PORTD
        movlw		0x03		; E=1, rs=1  
		movwf		PORTE
		call		sdel
		movlw		0x02		; E=0, rs=1  
		movwf		PORTE
return

del_41:	movlw		0xcd
		movwf		0x23
lulaa6:	movlw		0x20
		movwf		0x22
lulaa7:	decfsz		0x22,1
		goto		lulaa7
		decfsz		0x23,1
		goto 		lulaa6 
		return
	
del_01:	movlw		0x20
		movwf		0x22
lulaa8:	decfsz		0x22,1
		goto		lulaa8
		return
;************************************************
sdel:	movlw		0xff		; movlw = 1 cycle
		movwf		0x23		; movwf	= 1 cycle
lulaa2:	movlw		0xfa
		movwf		0x22
lulaa1:	decfsz		0x22,1		; decfsz= 1/2 cycle
		goto		lulaa1		; goto	= 2 cycles
		decfsz		0x23,1
		goto 		lulaa2 
		return
;************************************************
mdel:	movlw		0x0a
		movwf		0x24
lulaa5:	movlw		0x19
		movwf		0x23
lulaa4:	movlw		0xfa
		movwf		0x22
lulaa3:	decfsz		0x22,1
		goto		lulaa3
		decfsz		0x23,1
		goto 		lulaa4 
		decfsz		0x24,1
		goto		lulaa5
		return

;*******************************************
delay:						
	movlw		0xFF    
	movwf		0x30	;N1
count3:
	movlw		0xFF	;N2 
	movwf		0x31
count2:
	movlw		0xFF	;N3
	movwf		0x32
count1:
	decfsz		0x32
	GOTO 		count1
	decfsz		0x31
	GOTO 		count2
	decfsz		0x30
	GOTO 		count3	
	return
;*******************************************
delay_2:						
	movlw		0x96    
	movwf		0x30	;N1
count33:
	movlw		0x96	;N2 
	movwf		0x31
count22:
	movlw		0x96	;N3
	movwf		0x32
count11:
	decfsz		0x32
	GOTO 		count11
	decfsz		0x31
	GOTO 		count22
	decfsz		0x30
	GOTO 		count33	
	return



end_code:

end