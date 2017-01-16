;
;  MDZ80 V0.9.0 Z80 Disassembly of fdd3000.bin
;  2017/01/14 00:30
;

; jedit
; tab = 4 spc


; ***************************************************************
;	BIOS
;	Versao para densidade dupla
;
;	VERSAO FINAL EM : 85/10/29
;	ACTUALIZACAD : 3/6/86
;
; BY : Antonio Nobrega
;
; Esta versao substitui a versao de 20/1/83. O novo
; BIOS tem processamento de erro para recuperacao
; de problemas de hardware.
; Emendado a nao detecao de erro de read a segunda tentativa
;
; ***************************************************************
;
;
;	CP/M REFERENCE CONSTANTS
;
;
;IMAG	equ	$100		; offset para a REL -1 ou 0
;MSIZE	equ	20			; Capacidade da memoria em K bytes
;BIAS	equ	(MSIZE - 20)*1024+IMAG	; deslocamento em relacao aos
;CCP	equ	BIAS		; 20 K bytes
;BDOS	equ	CCP + $806
;CBIOS	equ	CCP + $1600
;
;	real values :
BIAS	equ	$e000
CCP		equ	BIAS		;; $e000
BDOS	equ	CCP + $806	;; $e806
CBIOS	equ	CCP + $1600	;; $f600
;
IOBYTE	equ	$03			; Estrutura do IOBYTE nao implementado
CDISK	equ	$04			; Drive corrente
NDRIVE	equ	$04			; Numero de drives previsto
NTENT	equ	$0a			; Numero de tentativas em caso de erro no acesso a disco
SECLOG	equ	$80			; Sector logico
SECFIS	equ	$0100		; Sector fisico
SECI	equ	$00			; Sector fisico representado em 1 byte
;           
;	Caracteres ASCII
;           
LF		equ	$0a			; Line feed
CR		equ	$0d			; Carriage return
FFEED	equ	$0c			; Form feed
EOF		equ	$1a			; End of file
ESCAPE	equ	$1b
;
;	Protocolo comunicacoes CONIN CONOUT 
;
ASTAT	equ	$00
BSTAT	equ	$40
CSTAT	equ	$10
DSTAT	equ	$50
STMSK	equ	DSTAT
COM		equ	$2f			; Endereco do porto de comunicacoes
SERVICO	equ	$0f
MASK	equ	$5f
BYTEON	equ	$0a
STANDBY	equ	$05
WSTA	equ	$06			; Flag indica conin esta espera caractere
;
; ********************************************************************
; *							Descricao do hardware					 *
; ********************************************************************
;
;
;	Comunicacao serie
;	=================
;
;	Chip WD 2123
;	============
;
; 	Enderecos:
;
CANALA	equ	$80			; Endereco do porto A
SIOASTS	equ	CANALA + 1	; Endereco do registo de status
SIOACMD	equ	CANALA + 1	; Endereco do registo de comando
SIOADAT	equ	CANALA		; Endereco do registo de data
SIOABR	equ	$10			; Endereco do registo de baud/rate
;           
CANALB	equ	$40			; Endereco do porto B
SIOBSTS	equ	CANALB + 1	; Endereco do registo de status
SIOBCMD	equ	CANALB + 1	; Endereco do registo de comando
SIOBDAT	equ	CANALB		; Endereco do registo de data
SIOBBR	equ	$11			; Endereco do registo de baud/rate
;
;	Comandos de programacao
;
ACOM	equ	%00000111	; input e output enable
;
;	Flags do registo de status
;
RXRDY	equ	%00000001	; Receiver READY
ERR		equ	%01111000	; Erro na recepcao ou na emissao
SIOERR	equ	$37			; Palavra de comando com reset flag
SIOWORD	equ	$27			; Palavra de comando com RQT="0"
RTS		equ	$20
SIOMASK	equ	$81
;
;
;	Controlador de disco
;	====================
;
;	Chip   FD1770
;	=============
;
;	Enderecos:
;
FDC		equ	$c0
FDCSTS	equ	FDC			; Reg de stat
FDCMD	equ	FDC			; Reg de comando
FDCTRK	equ	FDC + 1		; Reg de pista 
FDCSEC	equ	FDC + 2		; Reg de sector
FDCDAT	equ	FDC + 3		; Reg de dados
;
;	Comandos de progamacao
;
RSTCMD	equ	$05			; Restore
SEKCMD	equ	$15			; Seek c/ verify
STICMD	equ	$55			; Step in
STOCMD	equ	$75			; Step out
RDSCMD	equ	$80			; Read sector
RDMCMD	equ	$98			; Read multiple sectors
WRSCMD	equ	$a0			; Write sector c/ precompensacao
RDACMD	equ	$cc			; Read address
FRICMD	equ	$d0			; Force interrupt
;
;	Flags do status
;
BUSY	equ	$00			; Busy	
DLOST	equ	$02			; Data lost
NFOUND	equ	$04			; Record not found
WPROTE	equ	$06			; Write protect
;
;	Outros registos
;
DRQ		equ	$2f			; Registo de comunicacoes
							;		bit 0-6 usado comunicacao em nibbles
							;		bit 7 DRQ
HRD		equ	$e0			; Regosto de controle
							;		bit 0 - drive sel 0
							;		bit 1 - drive sel 1
							;		bit 2 - drive sel 2
							;		bit 3 - drive sel 3
							;		bit 4 - side select
							;		bit 5 - /DDEN (densidade)
							;		bit 6 - /BOOT
							;		bit 7 - IN USE
;
;	Programacao
;
DENSID	equ	%01011111	; Programacao do porto de controle
INTPRG	equ	%11011111
MSKDRV	equ	%00001111	; Mascara para os drives
LUZ		equ	$07			; Ascende luz no drive seleccionado
;
;
; *************************************************************
; *						Tabela de JUMPs do BIOS				  *
; *************************************************************
;     
		org	$f600
			
		jp	BOOT			;BOOT
		jp	WBOOT			;WBOOT 
		jp	CONST			;CONST
		jp	CONIN			;CONIN
		jp	CONOUT			;CONOUT
		jp	IMPR			;IMPR 
		jp	PUNCH			;PUNCH
		jp	READER			;READER                 
		jp	HOME			;HOME
		jp	SELDSK			;SELDSK
		jp	SELTRK			;SELTRK
		jp	SETSEC			;SETSEC
		jp	SETDMA			;SETDMA
		jp	READ			;READ 
		jp	WRITE			;WRITE
		jp	IMPRST			;IMPRST
		jp	SECTRAN			;SECTRAN
;
;	Logo initial da versao do bios
;
SIGNON:	db	ESCAPE
		db 'H'
		db ESCAPE
		db 'J'
		db	"CP/M Version 2.2"
		db	CR, LF
		db	"Copyright by DIGITAL RESEARCH, Inc."
		db	CR, LF, LF
		db	"Cbios Version A1.1 Copyright by TMX PORTUGAL"
		db	CR, LF, LF
		db	"3, June 1986"
		db	CR, LF, LF
		db '$'
;
;
; *********************************************************
; *					Subrotinas do BIOS					  *
; *********************************************************
;
;
;	*********************************************
;	*					BOOT					*
;	*********************************************
;
		; Procedimento a ser executado depois do LOADER
		; em que inicializa o IOBYTE o disco corrente a
		; zero, e afixa a mensagem inicial. Por fim ini-
		; cializa a page 0 do CP/M com os valores conve-
		; nientes passando em seguida o comando ao CPP.					;
BOOT:	xor		a
		ld		(IOBYTE),a	; Reset IOBYTE
		ld		(CDISK),a	; Inicializa o drive selecionado
		ld		(UNIT),a    
		call	INIT        
		call	INITRS		; Inicializa canais rs_232
		ld		hl,SIGNON   
		call	PMSG		; Imprimir a mensagem inicial 
		jp		GOCPM
;
;
;	*********************************************
;	*					WBOOT					*
;	*********************************************
;
		; Inicializa o hardware, carrega o CCP e o BDOS
		; e inicializa a pagina 0 do CP/M passando em se-
		; guida o comando ao CCP.
		;

WBOOT:	ld		sp,STACK
		di
		call	INIT
		xor		a
		call	LIGHT
		ld		hl,CCP		; Endereco de base do CP/M
		ld		c,FDCDAT
		ld		b,$00
		ld		d,$01
WBLOOP:	ld		a,d
		call	RDLOOP
		jr		nz,WBOOT
		ld		a,$10
		inc		d
		cp		d
		jr		nz,WBLOOP
		call	STEPIN		; Pista 1
		xor		a
		call	RDLOOP		; Sector 0 Pista 1
		call	LIGHTOFF
GOCPM:	ld		a,$C3
		ld		($0000),a
		ld		hl,CBIOS+3
		ld		($0001),hl
		ld		($0005),a
		ld		hl,BDOS
		ld		($0006),hl
		xor		a
		ld		(BLREAD),a	; Nada no BUFFER de blocagem
		ld		(BLALT),a
		ld		bc,$0080
		call	SETDMA
		ld		a,(CDISK)
		ld		c,a
		jp		CCP
;
;
;
; *********************************************************
; *			Subrotinas de i/o de caracteres				  *
; *********************************************************
;
;
;	*********************************************
;	*					CONST					*
;	*********************************************
;   
		; Indica o status da consola
		;	parametros de saida
		;		reg A - FF se existe caracter
		;			  - 00 se nao existe caractere
		;	altera o resgisto A
		;
CONST:	in		a,(COM)
		and		MASK
		cp		$04
		jr		z,CONSON
		xor		a
		ret
CONSON:	ld		a,$FF
		ret
;
;	*********************************************
;	*					CONIN					*
;	*********************************************
;
CONIN:	ld		(SVSTK),sp
		ld		sp,STACK
		call	RXBYTE		; Recebe byte do terminal
		ld		c,a
		ld		sp,(SVSTK)
		ret
;
;	*********************************************
;	*					CONOUT					*
;	*********************************************
;
CONOUT:	ld	(	SVSTK),sp
		ld		sp,STACK
		ld		a,c
		call	TXBYTE		; Transmite byte
		ld		sp,(SVSTK)
		ret
;
;	*********************************************
;	*					IMPR					*
;	*********************************************
;
IMPR:	ld		(SVSTK),sp
		ld		sp,STACK
		call	PRINTER
		ld		sp,(SVSTK)
		ret
;
;	*********************************************
;	*					IMPRST					*
;	*********************************************
;
IMPRST:	ld		(SVSTK),sp
		ld		sp,STACK
		call	PRINSTS
		ld		sp,(SVSTK)
		ret
;
;	*********************************************
;	*					READER					*
;	*********************************************
;
READER:	ld		(SVSTK),sp
		ld		sp,STACK
		call	RDR
		ld		sp,(SVSTK)
		ret
;
;	*********************************************
;	*					PUNCH					*
;	*********************************************
;
PUNCH:	ld		(SVSTK),sp
		ld		sp,STACK
		call	PUN
		ld		sp,(SVSTK)
		ret
;
;
;
; *********************************************************
; *				Subrotinas de i/o de disco				  *
; *********************************************************
;
;
;	*********************************************
;	*					SECTRAN					*
;	*********************************************
;
SECTRAN:ld		a,c			; Determina se sao os 128 bytes me
		and		$01			; significativos ou os mais do sector
		ld		(HIGH1),a	; a ler
		ld		a,c
		rra
		ld		c,a			; determina a sector logico de 256 bytes a ser lido 
		ex		de,hl
		add		hl,bc
		ld		l,(hl)
		ld		h,$00
		ret
;
;	*********************************************
;	*					SELTRK					*
;	*********************************************
;
SELTRK:	ld		a,c
		ld		(TRACK),a
		ret
;
;	*********************************************
;	*					SETSEC					*
;	*********************************************
;
SETSEC:	ld		a,c
		dec		a
		ld		(SECTOR),a
		ret
;
;	*********************************************
;	*					SETDMA					*
;	*********************************************
;
SETDMA:	ld		(POINTR),bc
		ret
;
;	*********************************************
;	*					SELDSK					*
;	*********************************************
;
SELDSK:	ld		hl,$0000
		ld		a,c
		cp		NDRIVE
		ret		nc
		ld		(UNITDR),a
		ld		l,a
		add		hl,hl
		add		hl,hl
		add		hl,hl
		add		hl,hl
		ld		de,DPHTAB
		add		hl,de
		ret
;
;	*********************************************
;	*					HOME					*
;	*********************************************
;
HOME:	ld		a,(UNITDR)
		ld		c,a
		call	LIGHT
		ld		a,RSTCMD
		out		(FDCMD),a
		call	TEMP
HOMLP:	in		a,(FDCSTS)
		bit		0,a
		jr		nz,HOMLP
		call	LIGHTOFF
		xor		a
		ret
;
;	*********************************************
;	*					READ					*
;	*********************************************
;
READ:	ld		(SVSTK),sp
		ld		sp,STACK
		call	SAVJMP		; guarda salto de int e coloca novo jmp
		ld		a,(BLREAD)	; Testar se o BUFFER ja foi
		and		a			; lido alguma vez
		jr		z,RDCONT    
		call	IGUAL		; Testa se os sectores no BUFFER e para ser lido sao iguais se SIM
							;		entao A = 0
							;		senao A = FF 
		and		a           
		jr		z,REXISTE   
		ld		a,(BLALT)	; Testa se o sector no BUFFER foi alterado se SIM
							;		entao "blalt" = FF
							;		senao "blalt" = 0
		and		a
		jr		z,RDCONT
		call	WR256
		and		a
		jr		nz,RDFIM
RDCONT:	ld		hl,BLDRIV
		ld		a,(UNITDR)	; Bldriv <- UNITdr
		ld		(hl),a
		inc		hl
		ld		a,(TRACK)	; Blpist <- track
		ld		(hl),a
		inc		hl
		ld		a,(SECTOR)	; Blsect <- sector
		ld		(hl),a      
		inc		hl          
		xor		a           
		ld		(hl),a		; Blalt <- 0 (* nao alterado *)
		dec		a           
		ld		(BLREAD),a  
		call	RD256       
		and		a           
		jr		nz,RDFIM	; Erro
		ld		(BLALT),a   
REXISTE:call	RTRANSF     
		xor		a           
RDFIM:	ld		(ERRORD),a	; Flag de erro de leitura
		call	PUTJMP		; Repoi salto
		ld		sp,(SVSTK)
		ret
;
;	*********************************************
;	*					WRITE					*
;	*********************************************
;
WRITE:	ld		(SVSTK),sp
		ld		sp,STACK
		call	SAVJMP		; guarda $38
		push	bc			; Salvar se e acesso a directoria se e o primeiro
							; ou se e um acesso normal
		ld		a,(BLREAD)	; Testa se o BUFFER ja foi
		and		a			; lido alguma vez
		jr		z,WRCONT    
		call	IGUAL		; Testa se os sectores no BUFFER e para ser lido sao iguais se SIM
							;	entao A = 0
							;	senao A = FF
		and		a           
		jr		z,WEXISTE   
		ld		a,(BLALT)	; Testa se o secton no BUFFER foi alterado se SIM
							;	entao "blalt" A = FF
							;	senao "blalt" A = 0
		and		a           
		jr		z,WRCONT    
		call	WR256       
		and		a           
		jr		nz,WRPFIM   
WRCONT:	ld		hl,BLDRIV   
		ld		a,(UNITDR)  
		ld		(hl),a		; Bldriv <- UNITdr
		inc		hl          
		ld		a,(TRACK)   
		ld		(hl),a		; Blpist <- track
		inc		hl          
		ld		a,(SECTOR)  
		ld		(hl),a		; Blsect <- sector
		inc		hl          
		xor		a           
		ld		(hl),a		; Blalt <- 0 (* nao alterado *)
		dec		a           
		ld		(BLREAD),a  
		call	RD256       
		and		a           
		jr		nz,WRPFIM	; Erro
WEXISTE:call	WTRANSF
		pop		bc
		ld		a,c
		cp		$01
		jr		nz,WRNDIR
		call	WR256
		and		a
		jr		nz,WRFIM
		ld		(BLREAD),a
WRNDIR:	xor		a
		jr		WRFIM
WRPFIM:	pop		bc
WRFIM:	call	PUTJMP
		ld		sp,(SVSTK)
		ret
;
;	*********************************************
;	*					OUTRAS					*
;	*********************************************
;
		;		*** INIT ***
		;
		; Inicializa todo o hardware
		; controlador de disco
;
;
INIT:	ld		a,INTPRG
		out		(HRD),a
		ld		b,NDRIVE
		ld		a,$FE
INTLP:	out		(HRD),a
		rlc		a
		djnz	INTLP
		ld		a,DENSID
		out		(HRD),a
		and		$FE
		out		(HRD),a
		ld		(IMAGSEL),a
		ld		a,RSTCMD
		out		(FDCMD),a
		call	TEMP
INITLP:	in		a,(FDCSTS)
		bit		0,a
		jr		nz,INITLP
		xor		a
		ld		(BLREAD),a
		ld		(BLALT),a
		ld		(UNIT),a
		dec		a
		ld		b,NDRIVE
		ld		hl,DRVTBL
INLP:	ld		(hl),a
		inc		hl
		djnz	INLP
		call	LIGHTOFF
		ret
;
;
		;		*** RD256 ***
		;
		; Le 256 bytes para o BUFFER
;
;
RD256:	ld		a,(BLDRIV)
		ld		c,a
		call	LIGHT
		ld		a,NTENT
		ld		(RETRY),a
		call	PREPARA		; tenta dez vezes o seek
		and	a               
		jr		nz,RDERR    
RTRY:	ld		hl,BLBUFF	; enderecco buffer
		call	RDDSK		; tenta leitura
		and		a           
		jr		z,RFIM		; se o ok sai
		ld		a,(RETRY)
		dec		a
		ld		(RETRY),a
		jr		nz,RTRY
RDERR:	ld		b,$FF
RDOUT:	call	LIGHTOFF
		ld		a,b
		ret
RFIM:	ld		b,$00
		jr		RDOUT
RDDSK:	im		1
		ei
		ld		a,RDSCMD
		out		(FDCMD),a
RLP:	in		a,(DRQ)
		rla
		jr		nc,RLP
		ini
		jr		RLP
;
;
		;		*** WR256 ***
;
;
WR256:	ld		a,(BLDRIV)	; Acesso ao driver do buffer
		ld		c,a         
		call	LIGHT       
		ld		a,NTENT		
		ld		(RETRY),a	; 10 tentatives
		call	PREPARA		; tenta escrita
		and		a			; erro?
		jr		nz,WRERR	; sim jump sai
WRTRY:	ld		hl,BLBUFF   
		call	WRDSK		; tenta escrita
		and		a			; erro?
		jr		z,WFIM      
		bit		6,a			; disco protegio?
		call	nz,DSKPRT	; sim logo para consola
		cp		'R'			; Retry?
		jr		z,WRTRY		; sim tenta nova escrita
		ld		a,(RETRY)	; tenta dez vezes
		dec		a           
		ld		(RETRY),a   
		jr		nz,WRTRY    
WRERR:	ld		b,$FF		; flag erro
WROUT:	call	LIGHTOFF	; desliga LED
		ld		a,b         
		ret                 
WFIM:	ld		b,$00		; flag write ok
		jr		WROUT       
WRDSK:	ei                  
		ld		c,FDCDAT	; endereco porto data
		ld		a,WRSCMD
		out		(FDCMD),a
WRLP:	in		a,(DRQ)
		rla
		jr		nc,WRLP
		outi
		jr		WRLP
;
;
		;		*** PRINTER ***
		;
		; Transmite byte recebido registo C
		; para canal serie seleccionado
;
;
PRINTER:ld		a,SIOWORD
		out		(SIOACMD),a
		call	ERROA
IMPHR:	in		a,(SIOASTS)
		bit		7,a			; cts low?
		jr		z,IMPHR		; nao espera
		bit		0,a			; registo de tx esta ready?
		jr		z,IMPHR		; nao espera
IMPTX:	ld		a,c
		out		(SIOADAT),a
		ret
;
;
		;		*** PRINSTS ***
		;
		; Da o estado do periferico (rs232)
		; associoado com o canal seleccionado
;
;
PRINSTS:call	ERROA
		in		a,(SIOASTS)
		bit		7,a
		jr		nz,LPTON
		xor		a
		ret
LPTON:	ld	a,$FF
		ret
;
;
		;		*** PUN ***
		;
		; Out do caractere  pelo canal B
;
;
PUN:	ld		a,SIOWORD
		out		(SIOBCMD),a
PUNCTS:	in		a,(SIOBSTS)
		and		$81			; espera cts low e thr empty
		cp		$81
		jr		nz,PUNCTS
		ld		a,c
		out		(SIOBDAT),a
		ret
;
;
		;		*** RDR ***
		;
		; Le caractere do canal B
;
;
RDR:	ld		a,SIOWORD
		out		(SIOBCMD),a	; forca CTS low
RDRLP:	in		a,(SIOBSTS)	; get status
		and		$7F         
		cp		$08         
		jr		nc,RDRERR   
		bit		RXRDY,a		; byte ready?
		jr		z,RDRLP		; espera loop
		in		a,(SIOBDAT)	; le byte
		ld		c,a         
		ld		a,ACOM		; forca CTS high
		out		(SIOBCMD),a
		ld		a,c
		ret
RDRERR:	ld		a,$37
		out		(SIOBCMD),a
		jr		RDRLP
;
;
		;		*** WTRANSF ***
		;
		; Transfer 128 bytes do endereco POINTR
		; para o BUFFER - high ou low
;
;
WTRANSF:ld		hl,BLBUFF
		ld		bc,SECLOG
		ld		a,(HIGH1)
		and		a
		jr		z,WLOW
		add		hl,bc
WLOW:	ex		de,hl
		ld		hl,(POINTR)
		ldir
		xor		a
		dec		a
		ld		(BLALT),a	; PROVISORIO
		ret
;
;
		;		*** RTRANSF ***
		;
		; Transfer 128 bytes do BUFFER - high ou low
		; para o POINTR
;
;
RTRANSF:ld		hl,BLBUFF
		ld		bc,SECLOG
		ld		a,(HIGH1)
		and		a
		jr		z,RLOW
		add		hl,bc
RLOW:	ld		de,(POINTR)
		ldir
		ret
;
;
		;		*** IGUAL ***
		;
		; Testa se o sector que esta no BUFFER e
		; igual ao que se quer ler ou escrever
;
;
IGUAL:	push	ix
		ld		ix,BLDRIV
		ld		a,(UNITDR)
		cp		(ix+$00)
		jr		nz,DIFER
		ld		a,(TRACK)
		cp		(ix+$01)
		jr		nz,DIFER
		ld		a,(SECTOR)
		cp		(ix+$02)
		jr		nz,DIFER
		ld		a,(ERRORD)	; ve se houve erro no read anterior
		and		a
		jr		nz,DIFER
		pop		ix
		ret
DIFER:	ld		a,$FF
		pop		ix
		ret
;
; Entry: B=sector count, C=starting sector, HL=start address 
; Reads every other sector (1 sector interleave)
		;
		;		*** RDLOOP ***
;
RDLOOP:	out		(FDCSEC),a
		ld		a,RDSCMD
		out		(FDCMD),a
RLLP:	in		a,(DRQ)
		rla
		jr		nc,RLLP
		ini
		jr		nz,RLLP
RELLP:	in		a,(FDCSTS)
		bit		BUSY,a
		jr		nz,RELLP
		and		$1C
		ret
;
;
		;		*** POINT ***
		;
		; Devolve no reg HL o apontador do
		; descritor do drive especificado
		; no reg A
;
;
POINT:	ld		hl,DRVTBL
		and		a
PNT1:	ret		z
		inc		hl
		dec		a
		jr		PNT1
;
;
		;		*** STEPIN ***
		;
		; Faz avancar a cabeca do drive de uma
		; posicao
;
;
STEPIN:	ld		a,STICMD
		out		(FDCMD),a
		call	TEMP
SETPLP:	in		a,(FDCSTS)
		bit		BUSY,a
		jr		nz,SETPLP
		ret
;
;
		;		*** LIGHT ***
		;
		; Seleciona o drive o drive e acende a luz
		; do mesmo
;
;
LIGHT:	ld		c,a
		ld		(TUNIT),a
		ld		a,(IMAGSEL)
		res		LUZ,a
		or		MSKDRV
		ld		(IMAGSEL),a
		out		(HRD),a
		ld		a,(UNIT)
		call	POINT
		in		a,(FDCTRK)
		ld		(hl),a
		ld		b,$FE
LIGCNT:	dec		c
		jp		m,LIGEND
		rlc		b
		jr		LIGCNT
LIGEND:	ld		a,(IMAGSEL)
		and		b
		out		(HRD),a
		ld		(IMAGSEL),a
		ld		a,(TUNIT)
		ld		(UNIT),a
		call	POINT
		ld		a,(hl)
		cp		$FF
		jr		nz,LIGNRST
		ld		a,RSTCMD
		out		(FDCMD),a
		call	TEMP
LIGLP:	in		a,(FDCSTS)
		bit		BUSY,a
		jr		nz,LIGLP
		xor		a
LIGNRST:out		(FDCTRK),a
		ret
;
;
		;		*** INITRS ***
		;
		; Inicializa os canais com os parametros
		; dados pela tabela SIOTAB
;
;
INITRS:	ld		hl,SIOTAB	; Iniclaiza HL com ADD tabela
		call	PUTMOD		; Constroi a palavra de modo
		out		(SIOACMD),a 
		ld		a,ACOM      
		out		(SIOACMD),a	; Palavra a comando
		ld		a,(hl)      
		out		(SIOABR),a	; Baud rate
		;                   
		; Canal A programada
		;                   
		inc		hl			; Inicializa HL com parametros canal B
		call	PUTMOD
		out		(SIOBCMD),a
		ld		a,ACOM
		out		(SIOBCMD),a
		ld		a,(hl)
		out		(SIOBBR),a
		ret
;
;
		;		*** PUTMOD ***
		;
		; Forma a palavra de modo
;
;
PUTMOD:	ld		a,(hl)		; Baud rate factor
		inc		hl          
		add		a,(hl)		; Bits/char
		inc		hl          
		add		a,(hl)		; Parity enable
		inc		hl          
		add		a,(hl)		; Parity select
		inc		hl          
		add		a,(hl)		; Stop bits
		inc		hl			; Endereca baud rate
		ret
;
;
		;		*** LIGHTOFF ***
		;
		; A apaga a luz do drive selecionado
;
;
LIGHTOFF:ld		a,(IMAGSEL)
		set		LUZ,a
		ld		(IMAGSEL),a
		or		MSKDRV
		out		(HRD),a
		ld		a,(IMAGSEL)
		out		(HRD),a
		res		LUZ,a
		ld		(IMAGSEL),a
		ret
;
;
		;		*** TXBYTE ***
		;
		; Transmite um byte para a consola
;
;
TXBYTE:	push	de
		push	bc
		ld		(CHATMP),a	; Guarda caractere temporario  
		ld		b,a			; Caractere em A
TX0:	call	TXBY        
		jr		z,TXOUT     
TX1:	in		a,(COM)     
		and		$5F         
		cp		$07         
		jr		nz,TX1      
		ld		a,(CHATMP)  
		ld		b,a         
		jr		TX0         
TXOUT:	xor		a           
		pop		de          
		pop		bc          
		ret                 
;                           
;                           
TXBY:	ld		a,b         
		and		$0F         
		ld		e,a			; E fica com 1.o nibble
		ld		a,b         
		and		$F0         
		rrca                
		rrca                
		rrca                
		rrca                
		ld		d,a			; D fica com 2.o nibble
READY:	ld		a,BYTEON    
		out		(COM),a     
		in		a,(COM)     
		and		MASK		; Deixa passar bits 6, 4, 3, 2, 1, 0
		cp		SERVICO     
		jr		nz,READY    
		ld		a,e         
		or		CSTAT       
		out		(COM),a		; Tx. 1.o nibble e  bit 4 a "1" 
		ld		b,a         
		ld		c,$00       
TXTW1:	in		a,(COM)		; Espera eco
		dec		c           
		jr		z,TXERR     
		and		MASK		; Limpa bits 7 e 5
		cp		b           
		jr		nz,TXTW1	; Eco diferente espera
		ld		a,d         
		or		BSTAT		; Set flag
		out		(COM),a		; TX 2.o nibble e bit 6 a "1"
		ld		b,a         
		ld		c,$00       
TXTW2:	in		a,(COM)		; Espera eco
		dec		c           
		jr		z,TXERR     
		and		MASK        
		cp		b           
		jr		nz,TXTW2    
		ld		a,STMSK		; Envia fim de caractere
		out		(COM),a     
		xor		a           
		ret                 
TXERR:	ld		a,$07       
		out		(COM),a     
		and		a			; Set flag erro
		ret
;
;
		;		*** RXBYTE ***
		;
		; Recebe um byte
;
;
RXBYTE:	push	bc
		push	de
RX0:	call	RXBY
		jr		z,RXOUT		; Byte ok sai
RX1:	in		a,(COM)     
		and		$5F         
		cp		$07         
		jr		z,RX1       
		jr		RX0         
RXOUT:	ld		a,b			; Coloca caractere em A
		pop		de          
		pop		bc          
		ret                 
;                           
;                           
RXBY:	ld		a,WSTA		; Flag estou espera byte
		out		(COM),a     
		in		a,(COM)     
		and		MASK        
		cp		STANDBY     
		jr		nz,RXBY     
		ld		a,$0F       
		out		(COM),a		; Transmite flag ready
		ld		c,$00       
RXTW1:	in		a,(COM)     
		dec		c           
		jr		z,RXERR     
		ld		e,a         
		and		STMSK       
		cp		CSTAT       
		jr		nz,RXTW1    
		ld		a,e         
		out		(COM),a		; Transmite eco
		and		$0F         
		ld		e,a			; Guarda 1.o nibble
		ld		c,$00       
RXTW2:	in		a,(COM)     
		dec		c           
		jr		z,RXERR     
		bit		6,a			; Testa se e 2.o nibble
		jr		z,RXTW2     
		out		(COM),a		; Transmite eco
		and		$0F         
		rlca                
		rlca                
		rlca                
		rlca                
		or		e           
		and		$7F         
		ld		b,a			; Guarda caractere
RXTW3:	in		a,(COM)     
		and		$5F         
		cp		$07         
		jr		z,RXERR     
		cp		STMSK       
		jr		nz,RXTW3    
		ld		a,STMSK		; Fora de servico
		out		(COM),a
		xor		a
		ret
RXERR:	ld		a,$07
		out		(COM),a
		and		a
		ret
;
;
		;		*** TEMP ***
		;
		; Temporizador de 50 uS
;
;
TEMP:	push	bc
		ld		b,$12
Xfb78:	djnz	$
		pop		bc
		ret
;
;
		;		*** SEEK ***
		;
		; Posiciona a cabeca do drive selecionado na
		; pista especificada na posicao de memoria
		; "track"
;
;
SEEK:	push	bc
		ld		a,NTENT
		ld		(TENTAT),a	; n.o de tentativas de seek
		in		a,(FDCTRK)  
		ld		b,a         
		ld		a,(BLPIST)  
		cp		b           
		jr		z,SEKOK		; ja la estava !!
SEEKLP:	ld		a,(BLPIST)  
		out		(FDCDAT),a  
		call	SEKINT		; Tenta seek
		and		%10111111	; Mascara bit write protected
		and		a			; Erro?
		jr		z,SEKOK		; nao jump
		call	HOME		; forca restor
		ld		a,(TENTAT)	; n.o de tentativas
		dec		a           
		ld		(TENTAT),a  
		jr		nz,SEEKLP	; ate dez tentativas
		inc		a			; set erro seek
		jr		SEKERR      
SEKOK:	xor		a           
SEKERR:	pop		bc          
		ret                 
SEKINT:	ld		a,SEKCMD    
		im		1           
		ei                  
		out		(FDCMD),a	; Comando de ir para pista selecionada
Xfbb1:	jr		$			; espera saida interrupt
;
;
		;		*** PREPARA ***
		;
		; Prepara a leitura ou a escrita de
		; um sector. Para isso actualiza o reg
		; e sector posiciona a cabeca na pista
		; e prepara os registos
;
;
PREPARA:ld		a,(BLSECT)
		out		(FDCSEC),a
		ld		c,FDCDAT	; Enderedo do reg de dados
		jp		SEEK		; Posiciona e cabeca
;
ERRDSK:	ld		hl,ABORT
		call	PMSG
ERRAGN:	call	RXBYTE
		res		5,a
		push	af
		call	TXBYTE
		pop		af
		cp		'A'
		jp		z,WBOOT
		cp		'I'
		ret		z
		cp		'R'
		jr		nz,ERRAGN
		ret
;
; DSKPRT : coloca msg erro distette protegida
;
DSKPRT:	ld		hl,MSGPRO
		call	PMSG
		jr		ERRDSK
;
; TIMOUT : timeout do pun, e printer 
;
; SAVJMP : guarda salto do int e coloca novo salto
;
;
SAVJMP:	push	af
		push	bc
		ld		hl,$0038
		ld		de,BUFINT
		ld		bc,$0003
		ldir
		dec		hl
		ld		de,INTRPT
		ld		a,d
		ld		(hl),a
		ld		a,e
		dec		hl
		ld		(hl),a
		dec		hl
		ld		(hl),$C3	; jmp
		pop		bc
		pop		af
		ret
;
; PUTJMP : repoi salto $38
;
PUTJMP:	push	af			; salva reg
		ld		de,$0038
		ld		hl,BUFINT
		ld		bc,$0003
		ldir
		pop		af
		ret
;
; INTRPT : devolve subrotina status do disco
;
INTRPT:	pop		af			; destroi ret
		in		a,(FDCSTS)	; le status
		and		$5C			; mascara bits erro
		ret
;
;
		;		*** PMSG ***
		;
		; Imprime uma string que apontada pelo
		; par HL e termina em $
;
;
PMSG:	ld		a,(hl)
		ld		c,a
		cp		'$'
		inc		hl
		ret		z
		call	TXBYTE		; transmite log cpm
		jr		PMSG
;
ERROA:	in	a,(SIOASTS)
		and	ERR
		ret	z
		ld	a,$37
		out	(SIOACMD),a
		ret
;
ERROB:	in	a,(SIOBSTS)
		and	ERR
		ret	z
		ld	a,$37
		out	(SIOBCMD),a
		ret

;
;
; ***********************************************************
; *			Estruturas de dados, tabelas e mensagens 		*
; ***********************************************************
;	*********************************************
;	*					MENSAGENS				*
;	*********************************************
;
MSGPRO:	db	ESCAPE, 'Y', 55, 32
		db	LF, LF
		db	"Diskette hardware protected$"
;
ABORT:	db	CR, LF
		db	"Abort, Ignore, Retry?$"
;	
;	*********************************************
;	*					TABELAS					*
;	*********************************************
;
SECTB0:	db	$01,$06,$0B,$10,$05,$0A,$0F,$04
		db	$09,$0E,$03,$08,$0D,$02,$07,$0C
;
;	DISK PARAMETER BLOCK FOR STANDARD 3" FLOPPY
;			Densidade dupla
;
DPBLKO:	dw	  32
		db	   3
		db	   7
		db	   0
		dw	 151
		dw	  63
		db	 %11110000
		db	 %00000000
		dw	  16
		dw	   2
;
;	DISK PARAMETER HEADERS FOR A 4 DISK SYSTEM
;
DPHTAB:	dw		SECTB0, $0000		; DPH FOR UNIT 0
		dw		$00000, $0000
		dw		DIRBUF, DPBLKO
		dw		CHK0,   ALL0
;
		dw		SECTB0, $0000		; DPH FOR UNIT 1
		dw		$00000, $0000
		dw		DIRBUF, DPBLKO
		dw		CHK1,   ALL1
;
		dw		SECTB0, $0000		; DPH FOR UNIT 2
		dw		$00000, $0000
		dw		DIRBUF, DPBLKO
		dw		CHK2,   ALL2
;
		dw		SECTB0, $0000		; DPH FOR UNIT 3
		dw		$00000, $0000
		dw		DIRBUF, DPBLKO
		dw		CHK3,   ALL3
;
;	*********************************************
;	*			Estruturas de dados				*
;	*********************************************
;
SIOTAB:	db		$02, $0c, $00, $00, $80, $08	; Parametros canais RS-232
		db		$02, $0c, $00, $00, $80, $08
BUFINT:	ds	3			; buffer que guarda 3 bytes $38
RETRY:	ds	1			; Numero de tentativas de leitura / escrita
TENTAT:	ds	1			; N.o de tentativas de seek
IMAGSEL:ds	1			; Imagem do registo de controle 
HIGH1:	ds	1			; Determ. sector HIGH ou LOW
DRVTBL:	ds	4			; Tabela com a imagem dos drives
UNIT:	ds	1           
TUNIT:	ds	1           
UNITDR:	ds	1			; Drive selecionado
TRACK:	ds	1			; Pista selecionada
SECTOR:	ds	1			; Sector selecionado
POINTR:	ds	2			; Endereco selecionado
BLDRIV:	ds	1			; Estrutura de dados para a blocagem / 
BLPIST:	ds	1			; Desblocagem
BLSECT:	ds	1           
BLREAD:	ds	1           
BLALT:	ds	1           
BLBUFF:	ds	SECFIS      
		ds	32			; Stack
STACK:	ds	1           
SVSTK:	ds	2			; Variavel para armazenar o SP
CHATMP:	ds	1           

;
;	*****************************************************
;	*	Estruturas de dados	do BDOS definidas no BIOS	*
;	*****************************************************
;
DIRBUF:	ds	128			; SCRATCH DIRECTORY BUFFER
ALL0:	ds	32			; UNIT 0 ALLOCATION BUFFER
CHK0:	ds	16			; UNIT 0 CHECK VECTOR
ALL1:	ds	32			; UNIT 1 ALLOCATION BUFFER
CHK1:	ds	16			; UNIT 1 CHECK VECTOR
ALL2:	ds	32			; UNIT 2 ALLOCATION BUFFER
CHK2:	ds	16			; UNIT 2 CHECK VECTOR
ALL3:	ds	32			; UNIT 3 ALLOCATION BUFFER
CHK3:	ds	16			; UNIT 3 CHECK VECTOR
ERRORD:	ds	1

