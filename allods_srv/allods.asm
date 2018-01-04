;NO "SLI" USED. "SLI" SUXX'N'MUSDIE.
;Красный бордер - время, теряемое впустую (ожидание)

;все координаты - это коорд-ты ног спрайта
SCRWD   EQU 24
BRIM    EQU 32-SCRWD
CHELS   EQU 200 ;число перснажей
LPF     EQU 220 ;строк спрайтов за фрейм
ONEBYTE EQU #5C01
SHADSCR EQU #A000
SCR     EQU #4000
SPRS    EQU #7000
IMER    EQU #5D5D
IMTAB   EQU #5B00
ADRBUF  EQU #B800 ;7 BYTES=1 SPRITE ON SCR
MAXSPR  EQU 255
ARWBUF  EQU #5D39
        ORG #C000
MUZ     INCBIN "GUARDIAN
        ORG #7000
SC1     INCBIN "allods",#1800
        ORG #E800
SC2     INCBIN "allodmag",#1800
        ORG #6000
        LD A,201
        LD (PRSP),A
        CALL INIMOUS
        CALL MUZ
        LD HL,CHELBUF
        LD B,CHELS
SETCH0  CALL RND
        AND #C0
        LD (HL),A
        INC HL
        CALL RND
        AND #39
        CP 48
        JR C,$+4
        SUB 48
        ADD A,'SPRS
        LD (HL),A
        INC HL
        CALL RND
        LD (HL),A
        INC HL
        CALL RND
        LD (HL),A
        INC HL
        DJNZ SETCH0
        LD (HL),-1
        LD DE,SPRS
        LD HL,SC1
        CALL GETSPRS
        LD HL,SC2
        CALL GETSPRS
        CALL 7766

        LD HL,ADRBUF
        LD DE,ADRBUF+1
        LD BC,MAXSPR*7-1
        LD (HL),B ;DISABLE "SUB B:JP C"
        LDIR 
        CALL SETIM
BEG     CALL NEWPICT
        LD IX,CHELBUF
ST      LD L,(IX)
        INC L
        JR Z,STQ
        DEC L
        LD H,(IX+1)
        LD E,(IX+2)
        LD D,(IX+3)
        PUSH BC
        LD A,H
        ADD A,2
        XOR H
        AND 6
        XOR H
        LD H,A
        PUSH HL
        LD A,H
        SUB 'SPRS
        LD H,A
        DUP 3
        ADD HL,HL
        EDUP 
        LD A,H
        AND 14
        LD L,A
        LD H,0
        LD BC,DIRS
        ADD HL,BC
        LD A,(HL)
       ADD A,A
        ADD A,E
        LD E,A
        INC HL
        LD A,(HL)
       ADD A,A
        ADD A,D
        LD D,A
        POP HL
        CALL RND
        CP 8
        JR NC,NON
        LD L,0
        SRL A
        RR L
        RRA 
        RR L
        XOR H
        AND 1
        XOR H
        LD H,A
NON     LD (IX),L
        LD (IX+1),H
        LD (IX+2),E
        LD (IX+3),D
        LD A,H
        AND 6
        CP 6
        JR NZ,$+4
        RES 2,H
        CALL PRIY
        LD BC,4
        ADD IX,BC
        POP BC
        JR ST
STQ     CALL ENDPICT
        CALL 8020
        JP C,BEG
QUIT    LD IY,23610
        LD HL,10072
        EXX 
        IM 1
        CALL MUZ
        EI 
        RET 

DIRS    DB 0,1
        DB -1,1
        DB -1,0
        DB -1,-1
        DB 0,-1
        DB 1,-1
        DB 1,0
        DB 1,1

SETIM   DI 
        LD HL,ON_INT
        LD DE,IMER
        LD A,D
        LD BC,ONINTL
        LDIR 
        LD HL,IMTAB
        LD (HL),A
        INC B
        LD D,H
        LD E,B
        LD A,H
        LDIR 
        LD I,A
        IM 2
        EI 
        RET 

ON_INT  DISP IMER
        PUSH AF
       LD A,4
       OUT (-2),A
        PUSH BC
        PUSH DE
        PUSH HL
        EXX 
        EX AF,AF'
        PUSH AF
        PUSH BC
        PUSH DE
        PUSH HL
        PUSH IX
        PUSH IY
        LD (IY+5),PRSPQ
        LD (IY+6),'PRSPQ
        CALL MUZ+5
        CALL RE
PRSHADF EQU $+1
        LD A,0
        DEC A
        PUSH AF
        CALL Z,REMAP
        CALL PRAR
        POP AF
        CALL Z,ARMAP
        CALL MANAGE
       XOR A
       OUT (-2),A
        CALL PRSP
        POP IY
        POP IX
        POP HL
        POP DE
        POP BC
        POP AF
        EXX 
        EX AF,AF'
        POP HL
        POP DE
        POP BC
        POP AF
        EI 
        RET 
        ENT 
ONINTL  EQU $-ON_INT

NEWPICT LD (CLSHSP+1),SP
        LD SP,SHADSCR+6144
        LD HL,-1;#2222
        LD DE,-1;#8888
        LD B,88
CLSHAD0 DUP 16
        PUSH HL
        EDUP 
        DUP 16
        PUSH DE
        EDUP 
        DJNZ CLSHAD0
CLSHSP  LD SP,0
        LD HL,SHADSCR+512
        LD DE,SHADSCR
        LD BC,512
        LDIR 
        LD IY,ADRBUF
        LD (RLOKSP+1),IY;BEGIN NEW PICTURE
        LD A,237
        LD (PRSP),A
        RET 

ENDPICT LD DE,PRSPQQ
        LD A,E
        CALL LDIY
NOEPIC LD A,2
       OUT (-2),A
        HALT 
        LD A,(PRSP)
        CP 201
        JR NZ,NOEPIC
       XOR A
       OUT (-2),A
ARXYOLD LD BC,0
        CALL ARMAP+4
        LD HL,PRSHADF
        INC (HL)
        PUSH HL
        LD HL,2+SHADSCR
        LD DE,SCR
        LD C,24
PRSHAD0 LD B,4
PRSHAD1 PUSH BC
        DUP SCRWD-1
        LDI 
        EDUP 
        LD A,(HL)
        LD (DE),A
        INC D
        SET 5,L
        DUP SCRWD-1
        LDD 
        EDUP 
        LD A,(HL)
        LD (DE),A
        INC D
        LD BC,32
        ADD HL,BC
        POP BC
        DJNZ PRSHAD1
        LD A,E
        ADD A,32
        LD E,A
        JR C,$+6
        LD A,D
        SUB 8
        LD D,A
        DEC C
        JR NZ,PRSHAD0
        POP HL
        LD (HL),C
        RET 

PRIY    LD B,16
        LD A,D
        CP B
        JR NC,$+4
        LD B,D
        INC B
        SUB 192
        JR C,PR16C
        CP B
        RET NC
        LD C,A
        CPL 
        ADD A,B
        RET Z
        LD B,0
        INC C
        SLA C
        ADD HL,BC
        ADD HL,BC
        LD B,A
        LD D,191
PR16C   LD A,E
        CP SCRWD*8+16
        RET NC
        LD (IY+3),L
        LD (IY+4),H
        DUP 3
        SRL D
        RR E
        EDUP 
        LD HL,SHADSCR
        ADD HL,DE
        LD (IY),B
        LD (IY+1),L
        LD (IY+2),H
        AND 7
        ADD A,A
        LD HL,TABRLS
        ADD A,L
        LD L,A
        JR NC,$+3
        INC H
        LD A,(HL)
        INC HL
        LD D,(HL)
LDIY    LD (IY+5),A
        LD (IY+6),D
        LD BC,7
        ADD IY,BC
        CP (IY-2)
        RET Z
;случилось прерывание
        LD (IY-2),A
        LD (IY-1),D
        RET 

TABRLS  DW RL0,RL7,RL6,RL5,RL4,RL3,RL2,RL1

PRSP    LD (PRSPQ+1),SP;CALL EVERY FRAME
        LD DE,-34
        LD C,LPF
        JP RLOKSP
PRSPQQ  LD A,201        ;LAST SPRITE
        LD (PRSP),A ;"PICT FINISHED" SIGN
PRSPQ   LD SP,0 ;every LPF lines
        RET 

RL1     LD (RLOKSP+1),SP
        LD SP,HL
        EXX 
RL10    LD A,(HL)
        EXX 
        POP BC
        POP HL
        LD DE,-#80
        ADD HL,HL
        RL E
        RL C
        RL B
        RL D
        AND D
        XOR E
        EXX 
        LD (HL),A
        INC L
        LD A,(HL)
        EXX 
        AND B
        XOR H
        EXX 
        LD (HL),A
        INC L
        LD A,(HL)
        EXX 
        AND C
        XOR L
        EXX 
        LD (HL),A
        ADD HL,DE
        DJNZ RL10
        JP RLOKSP

RL4     LD (RLOKSP+1),SP
        LD SP,HL
        LD HL,ONEBYTE
        EXX 
        LD (RL4C+1),A
RL40    LD A,D
        EXX 
        POP BC
        POP DE
        LD (HL),C
        RLD 
        LD C,(HL)
        LD (HL),B
        RLD 
        LD B,(HL)
        EXX 
        AND (HL)
       LD C,A
        EXX 
        XOR A
        LD (HL),E
        RLD 
        LD E,(HL)
        LD (HL),D
        RLD 
        EXX 
       XOR C
        LD (HL),A
        INC L
        LD A,(HL)
        EXX 
        AND B
        XOR (HL)
        EXX 
        LD (HL),A
        INC L
        LD A,(HL)
        EXX 
        AND C
        XOR E
        EXX 
        LD (HL),A
        ADD HL,DE
        DJNZ RL40
RL4C    LD C,0
        JP RLOKSP

RL7     LD (RLOKSP+1),SP
        LD SP,HL
        EXX 
RL70    LD A,(HL)
        EXX 
        POP BC
        POP HL
        LD DE,#FF01
        SRL H
        RR L
        RR E
        RR B
        RR C
        RR D
        AND B
        XOR H
        EXX 
        LD (HL),A
        INC L
        LD A,(HL)
        EXX 
        AND C
        XOR L
        EXX 
        LD (HL),A
        INC L
        LD A,(HL)
        EXX 
        AND D
        XOR E
        EXX 
        LD (HL),A
        ADD HL,DE
        DJNZ RL70
        JP RLOKSP

RL6     LD (RLOKSP+1),SP
        LD SP,HL
        EXX 
        SCF 
        LD E,-30
        INC L
        INC L
RL60    LD A,(HL)
        EXX 
        LD E,A
        POP BC
        POP HL
        SBC A,A
        DUP 2
        RR B
        RR C
        RRA 
        EDUP 
        AND E
        LD E,A
        XOR A
        DUP 2
        RR H
        RR L
        RRA 
        EDUP 
        XOR E
        EXX 
        LD (HL),A
        DEC L
        LD A,(HL)
        EXX 
        AND C
        XOR L
        EXX 
        LD (HL),A
        DEC L
        LD A,(HL)
        EXX 
        AND B
        XOR H
        EXX 
        LD (HL),A
        ADD HL,DE
        DJNZ RL60
        JP RLOKE

RL3     LD (RLOKSP+1),SP
        LD SP,HL
        EXX 
        SCF 
RL30    LD A,(HL)
        EXX 
        LD C,A
        POP HL
        SBC A,A
        DUP 3
        ADD HL,HL
        INC L
        RLA 
        EDUP 
        EX DE,HL
        AND C
        LD C,A
        POP HL
        XOR A
        DUP 3
        ADD HL,HL
        RLA 
        EDUP 
        XOR C
        EXX 
        LD (HL),A
        INC L
        LD A,(HL)
        EXX 
        AND D
        XOR H
        EXX 
        LD (HL),A
        INC L
        LD A,(HL)
        EXX 
        AND E
        XOR L
        EXX 
        LD (HL),A
        ADD HL,DE
        DJNZ RL30
        JP RLOKSP

RL2     LD (RLOKSP+1),SP
        LD SP,HL
        EXX 
        SCF 
RL20    LD A,(HL)
        EXX 
        LD C,A
        POP HL
        SBC A,A
        DUP 2
        ADD HL,HL
        INC L
        RLA 
        EDUP 
        EX DE,HL
        AND C
        LD C,A
        POP HL
        XOR A
        DUP 2
        ADD HL,HL
        RLA 
        EDUP 
        XOR C
        EXX 
        LD (HL),A
        INC L
        LD A,(HL)
        EXX 
        AND D
        XOR H
        EXX 
        LD (HL),A
        INC L
        LD A,(HL)
        EXX 
        AND E
        XOR L
        EXX 
        LD (HL),A
        ADD HL,DE
        DJNZ RL20
        JP RLOKSP

RL5     LD (RLOKSP+1),SP
        LD SP,HL
        EXX 
        SCF 
        LD E,-30
        INC L
        INC L
RL50    LD A,(HL)
        EXX 
        LD E,A
        POP BC
        POP HL
        SBC A,A
        DUP 3
        RR B
        RR C
        RRA 
        EDUP 
        AND E
        LD E,A
        XOR A
        DUP 3
        RR H
        RR L
        RRA 
        EDUP 
        XOR E
        EXX 
        LD (HL),A
        DEC L
        LD A,(HL)
        EXX 
        AND C
        XOR L
        EXX 
        LD (HL),A
        DEC L
        LD A,(HL)
        EXX 
        AND B
        XOR H
        EXX 
        LD (HL),A
        ADD HL,DE
        DJNZ RL50
RLOKE   LD E,-34
RLOKSP  LD SP,0
        DEC SP
        LD A,C
        POP BC ;HEIGHT
        SUB B
        JP C,PRSPQ
        LD C,A
        POP HL ;SCREEN ADDR
        EXX 
        POP HL ;SPRITE ADDR
        RET    ;PROG ADDR (or quit)

RL0     LD (RLOKSP+1),SP
        LD SP,HL
        EXX 
        INC E
RL00    LD A,(HL)
        EXX 
        POP BC
        POP HL
        AND B
        XOR H
        EXX 
        LD (HL),A
        INC L
        LD A,(HL)
        EXX 
        AND C
        XOR L
        EXX 
        LD (HL),A
        ADD HL,DE
        DJNZ RL00
        DEC E
        JP RLOKSP

GETSPRS PUSH DE
        LD DE,#4000
        PUSH DE
        LD BC,#1800
        LDIR 
        POP HL
        POP DE
        LD B,96
GSPRS1  PUSH BC
        PUSH HL
        CALL GETSPR
        POP HL
        LD A,L
        ADD A,4
        LD L,A
        AND 31
        JR NZ,GSPRSE
        LD A,L
        ADD A,32
        LD L,A
        JR NZ,GSPRSE
        LD A,H
        ADD A,8
        LD H,A
GSPRSE  POP BC
        DJNZ GSPRS1
        RET 

GETSPR  LD BC,#720
        ADD HL,BC
        LD BC,#10FF
GETSPR0 PUSH HL
        LD A,(HL)
        INC L
        EX AF,AF'
        LD A,(HL)
        INC L
        INC L
        XOR (HL)
        DEC L
        LD (DE),A
        INC DE
        EX AF,AF'
        XOR (HL)
        LD (DE),A
        INC DE
        LD A,(HL)
        INC L
        LDI 
        LD (DE),A
        INC DE
        POP HL
        CALL UHL
        DJNZ GETSPR0
        RET 

DHL     INC H
        LD A,H
        AND 7
        RET NZ
        LD A,L
        ADD A,32
        LD L,A
        RET C
        LD A,H
        SUB 8
        LD H,A
        RET 

UHL     LD A,H
        DEC H
        AND 7
        RET NZ
        LD A,L
        SUB 32
        LD L,A
        RET C
        LD A,H
        SUB -8
        LD H,A
        RET 

RND     PUSH HL
        LD HL,(23670)
        LD A,H
        AND 31
        ADD A,7
        LD H,A
        INC L
        LD (23670),HL
        LD A,R
        XOR (HL)
        POP HL
        RET 

INKEY   LD A,239
        IN A,(-2)
        RRCA 
        RLA 
        RLA 
        OR #C2
        LD C,A
        LD A,#DF
        IN A,(-2)
        RRA 
        JR C,$+4
        RES 4,C
        RRA 
        JR C,$+4
        RES 5,C
        LD A,-5
        IN A,(-2)
        RRA 
        JR C,$+4
        RES 2,C
        LD A,-3
        IN A,(-2)
        RRA 
        JR C,$+4
        RES 3,C
        LD A,-2
        IN A,(-2)
        RRA 
        JR C,$+4
        RES 0,C
        LD A,#7F
        IN A,(-2)
        CPL 
        AND 31
        JR Z,$+4
        RES 1,C
        LD A,-6
MOUSEF  IN A,(#DF)
        RRA 
        JR C,$+4
        RES 1,C
        RRA 
        JR C,$+4
        RES 0,C
INKEYF  RET 
        RR C
        EX AF,AF'
        RR C
        EX AF,AF'
        RL C
        EX AF,AF'
        RL C
        RET 

MANAGE  CALL INKEY
        LD HL,(PRAR+1)
       LD (ARXYOLD+1),HL
ARVEL   LD DE,0
        LD A,C
KEY     EQU $+1
        CP 0
        LD (KEY),A
        RRA 
        RRA 
        CPL 
        JR NZ,MANTORM
        AND 15
        JR NZ,MANNOT
MANTORM SRA D
        INC D
        JR Z,$+3
        DEC D
        SRA E
        INC E
        JR Z,$+3
        DEC E
MANNOT  RRA 
        JR NC,$+3
        DEC D
        RRA 
        JR NC,$+3
        INC D
        RRA 
        JR NC,$+3
        INC E
        RRA 
        JR NC,$+3
        DEC E
        LD A,D
        CP 1
        JR Z,MANYOK
        CP -1
        JR Z,MANYOK
        INC A
        JP P,$+4
        INC A
        SRA A
;       SRA A
        JR Z,VEROK+1
MANYOK  ADD A,H
        CP 192
        JR C,VEROK
        XOR A
        BIT 7,D
        LD D,A
        JR NZ,VEROK
        LD A,191
VEROK   LD H,A
        LD A,E
        CP 1
        JR Z,MANXOK
        CP -1
        JR Z,MANXOK
        INC A
        JP P,$+4
        INC A
        SRA A
;       SRA A
        JR Z,HOROK+1
MANXOK  ADD A,L
        BIT 7,E
        JR Z,HORNOL
        JR C,HOROK
        XOR A
        LD E,A
HORNOL  JR NC,HOROK
        XOR A
        LD E,A
        DEC A
HOROK   LD L,A
        LD (ARVEL+1),DE
        LD (PRAR+1),HL
MANAGEF LD A,-5
        IN A,(#DF)
OLDX    LD E,0
        LD (OLDX+1),A
        SUB E
        JP P,MPX
        ADD A,L
        JR C,MXQ
        XOR A
        LD L,A
MPX     ADD A,L
        JR NC,$+3
        SBC A,A
MXQ     LD L,A
        LD A,-1
        IN A,(#DF)
OLDY    LD D,0
        LD (OLDY+1),A
        SUB D
        NEG 
        JP P,MPY
        ADD A,H
        JR C,MYQ
        XOR A
        LD H,A
MPY     ADD A,H
        LD H,191
        CP H
        JR NC,$+3
MYQ     LD H,A
        LD (PRAR+1),HL
        RET 

PRAR    LD BC,0
        LD A,B
        CALL 8881
        INC A
        LD B,A
        LD A,-1
        ADD A,A
        DJNZ $-1
        LD (ARROWN+1),A
        LD (RE+1),HL
        LD DE,SPRAR
        EX DE,HL
        LD IX,ARWBUF
        LD B,12
ARROW0  LD C,(HL);MASK
        INC HL
        PUSH HL
        LD L,(HL);SPRITE
ARROWN  LD H,0
        LD A,(DE)
        LD (IX),A
        SCF 
        SBC A,A
ARROW1  RL C
        RLA 
        ADD HL,HL
        JR C,ARROW1
        AND (IX)
        OR H
        LD (DE),A
        INC E
        LD A,E
        AND 31
        LD A,(DE)
        LD (IX+15),A
        JR Z,ARROW3
        AND C
        OR L
        LD (DE),A
ARROW3  POP HL
        INC HL
        INC IX
        DEC E
        CALL DDE
        LD A,D
        CP 88
        RET Z
        DJNZ ARROW0
        RET 

ARMAP   LD BC,(PRAR+1)
        LD A,C
        AND 7
        DUP 3
        SRL B
        RR C
        EDUP 
        LD HL,2+SHADSCR
        ADD HL,BC
        INC A
        LD B,A
        LD A,-1
        ADD A,A
        DJNZ $-1
        LD (ARMAPN+1),A
        LD (REMAP+1),HL
        LD A,C
        AND 31
        CP SCRWD
        RET NC
        LD DE,SPRAR
        EX DE,HL
        LD IX,ARWBUF
        LD B,12
ARMAP0  LD C,(HL);MASK
        INC HL
        PUSH HL
        LD L,(HL);SPRITE
ARMAPN  LD H,0
        LD A,(DE)
        LD (IX),A
ARMAPN1 SCF 
        SBC A,A
ARMAP1  RL C
        RLA 
        ADD HL,HL
        JR C,ARMAP1
        AND (IX)
        OR H
        LD (DE),A
        LD A,E
        INC DE
        DEC A
        AND 31
        CP SCRWD
        JR NC,ARMAP3
        LD A,(DE)
        LD (IX+15),A
        AND C
        OR L
        LD (DE),A
ARMAP3  LD HL,31
        ADD HL,DE
        EX DE,HL
        POP HL
        INC HL
        INC IX
        LD A,D
        CP 24+'SHADSCR
        RET Z
        DJNZ ARMAP0
        RET 

RE      LD HL,0
        LD IX,ARWBUF
        LD BC,#C1F
RE0     LD A,(IX)
        LD (HL),A
        INC L
        LD A,L
        AND C
        JR Z,RE2
        LD A,(IX+15)
        LD (HL),A
RE2     INC IX
        DEC L
        CALL DHL
        LD A,H
        CP 88
        RET Z
        DJNZ RE0
        RET 

REMAP   LD HL,0
        LD DE,31
        LD A,L
        SUB 2
        AND E
        CP SCRWD
        RET NC
        LD IX,ARWBUF
        LD B,12
REMAP0  LD A,(IX)
        LD (HL),A
        INC HL
        LD A,(IX+15)
        LD (HL),A
        INC IX
        ADD HL,DE
        LD A,H
        CP 24+'SHADSCR
        RET Z
        DJNZ REMAP0
        RET 

INIMOUS EI 
        HALT 
        IN A,(-1)
        CP -1
        JR NZ,INIMOFS
        IN A,(#DF)
        LD (OLDY+1),A
        LD B,A
        LD A,-5
        IN A,(#DF)
        LD (OLDX+1),A
        CP B
        RET NZ
        LD A,-6
        IN A,(#DF)
        CP B
        RET NZ
INIMOFS LD A,62
        LD (MOUSEF),A
        LD A,201
        LD (MANAGEF),A
        RET 

DDE     INC D
        LD A,D
        AND 7
        RET NZ
        LD A,E
        ADD A,32
        LD E,A
        RET C
        LD A,D
        ADD A,-8
        LD D,A
        RET 

SPRAR DW #3F,#401F,#600F,#5007,#4803,#4401
DW #4E00,#7801,#5803,#CA1,#8E3,#F7

END
CHELBUF DS CHELS*4
