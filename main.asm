assume cs:codesg,ss:stacksg,ds:datasg
datasg segment			
 min_low        db 4 dup(0)
 min_high       db 4 dup(0)

 max_low        db 4 dup(0)
 max_high       db 4 dup(0)

 sum_low        db 4 dup(0) ; sum in GetFactorSum
 sum_high       db 4 dup(0) ; sum in GetFactorSum
 
 i_high         db 4 dup(0) ; i in main

 j_low          db 4 dup(0) ; j in GetFactorSum
 j_high         db 4 dup(0) ; j in GetFactorSum

 tmp_low        db 4 dup(0)
 tmp_high       db 4 dup(0)

 range_low        db 4 dup(0)
 range_high       db 4 dup(0)

 arg_low        db 4 dup(0)
 arg_high       db 4 dup(0)

 out_low        db 4 dup(0)
 out_high       db 4 dup(0)
 str_buf        db 8 dup(0)
datasg ends

stacksg segment
	db 128 dup(0)
stacksg ends

codesg segment
large_div proc near
                push    bx
                ;H/N
                mov     bx, ax ;L
                mov     ax, dx
                mov     dx, 0
                div     cx  
                push    ax 
                
                mov     ax, bx
                div     cx
                mov     cx, dx ;yu

                pop     dx
                pop     bx
                ret

large_div endp

REVERSE PROC
    push ax
    push dx
    push cx
    MOV SI, OFFSET str_buf
    MOV CX, 0H
 
    LOOP1:
    MOV AX, [SI]
    CMP AL, '$'
    JE LABEL1
 
    PUSH [SI]
 
    INC SI
    INC CX
 
    JMP LOOP1
 
    LABEL1:
    MOV SI, OFFSET str_buf
        LOOP2:
        ;if count not equal to zero
        CMP CX,0
        JE  break
        ; pop the top of stack
        POP DX
        ; make dh, 0
        XOR DH, DH
        MOV [SI], DX
        INC SI
        DEC CX
 
        JMP LOOP2
 
    break:
    MOV [SI],'$ '
    pop cx
    pop DX
    pop ax
    RET
         
REVERSE ENDP

output          proc near  
                push cx
                xor di, di
                mov ax, word ptr out_low
                mov dx, word ptr out_high
next:
                mov cx, 10
                call large_div
                add cx, '0'
                mov byte ptr str_buf[di], cl
                inc di

                cmp ax, 0
                jne next
                cmp dx, 0
                jne next
fi:             
                mov byte ptr str_buf[di], '$'
                call REVERSE
                mov dx, offset str_buf
                mov ah,09h
                int 21h
                pop cx
                ret
output          endp
start:
                mov     ax,stacksg
                mov     ss,ax
                mov     sp,40h
                mov     bp,sp
                mov     ax,datasg
                mov     ds,ax
                
                mov     word ptr min_low, 00dah
                mov     word ptr min_high, 000h
                mov     word ptr max_low, 0000h
                mov     word ptr max_high, 0022h
                mov     word ptr arg_low, 00h
                mov     word ptr arg_high, 00h
; big for start
                mov     ax, word ptr min_high
                mov     word ptr i_high, ax
                jmp     lab_10367                  ; first test i max
; if (i_high==min_high && min_high==max_high)
if1_test1:
                mov     ax, word ptr i_high         ; i_high == min_high ?
                cmp     ax, word ptr min_high
                jz      lab_1012f            
                jmp     lab_101ba

lab_1012f:
                mov     ax, word ptr min_high       ; min_high == max_high ? 
                cmp     ax, word ptr max_high    
                jz      lab_1013b
                jmp     lab_101ba

; part 1 loop start
lab_1013b:
                mov     ax, word ptr min_low        ; i low = min low
                mov     cx, ax
                jmp     lab_101ae

lab_10143:
                mov     ax, word ptr i_high         ; arg high = i high
                mov     word ptr arg_high, ax       
                mov     ax, cx          ; arg low  = i low
                mov     word ptr arg_low, ax 
                call    GetFactorSum                     ; get sum
                mov     ax, word ptr sum_high
                mov     word ptr tmp_high, ax       ; tmp high = sum high           
                mov     ax, word ptr sum_low
                mov     word ptr tmp_low, ax        ; tmp low = sum low
                mov     ax, word ptr tmp_high       
                mov     word ptr arg_high, ax       ; arg high = tmp high
                mov     ax, word ptr tmp_low
                mov     word ptr arg_low, ax        ; arg low = tmp low
                call    GetFactorSum
                mov     ax, word ptr i_high         ; i high == sum high ?
                cmp     ax, word ptr sum_high
                jnz     lab_101aa                   
                mov     ax, cx
                cmp     ax, word ptr sum_low        ; i low == sum low ? 
                jnz     lab_101aa                   
                mov     ax, word ptr i_high         ; i high> tmp high ?
                cmp     ax, word ptr tmp_high       
                jbe     lab_1018a
                jmp     lab_101aa

lab_1018a:
                mov     ax, word ptr i_high         ; i high == tmp high ?
                cmp     ax, word ptr tmp_high
                jnz     lab_1019e
                mov     ax, cx          ; i low >= tmp_low ?
                cmp     ax, word ptr tmp_low
                jb      lab_1019e
                jmp     lab_101aa

lab_1019e:
                mov     ax, word ptr i_high    ; out put !!!
                mov     word ptr out_high, ax
                mov     ax, cx
                mov     word ptr out_low, ax
                call    output
                mov     ax, word ptr tmp_high    ; out put !!!
                mov     word ptr out_high, ax
                mov     ax, word ptr tmp_low
                mov     word ptr out_low, ax
                call    output
                mov	    ah, 2
                mov     dl, 13
                int     21h
                mov     dl, 10
                int     21h

lab_101aa:                          ; i_low ++
                inc     cx
lab_101ae:                          ; i low  <= max low
                mov     ax, cx
                cmp     ax, word ptr max_low
                jbe     fuck1
                jmp     exit                   ; break
; part 1 end
fuck1:
                jmp     lab_10143
; if (i_high==min_high && min_high!=max_high)
lab_101ba:
                mov     ax, word ptr i_high         ; i_high==min_high ?
                cmp     ax, word ptr min_high
                jz      lab_101c6
tt1:
                jmp     lab_10245

lab_101c6:
                mov     ax, word ptr min_high       ; min_high!=max_high ?
                cmp     ax, word ptr max_high
                jz      tt1
; part 2 start
                mov     ax, word ptr min_low        ; i low = min low
                mov     cx, ax

lab_101D5:                              
                mov     ax, word ptr i_high         ; arg_high=i_high;
                mov     word ptr arg_high, ax
                mov     ax, cx          ; arg_low=i_low;
                mov     word ptr arg_low, ax
                call    GetFactorSum                      ; get sum 
                mov     ax, word ptr sum_high       
                mov     word ptr tmp_high, ax       ; tmp_high=sum_high;
                mov     ax, word ptr sum_low
                mov     word ptr tmp_low, ax        ; tmp_low=sum_low;
                mov     ax, word ptr tmp_high
                mov     word ptr arg_high, ax       ; arg_high=tmp_high;
                mov     ax, word ptr tmp_low
                mov     word ptr arg_low, ax        ; arg_low=tmp_low;
                call    GetFactorSum     ; GetFactorSum(void)   ; get sum

; if (i_high==sum_high && i_low==sum_low )
                mov     ax, word ptr i_high         
                cmp     ax, word ptr sum_high       ; i_high==sum_high ?
                jnz     short lab_1023C
                mov     ax, cx
                cmp     ax, word ptr sum_low        ; i_low==sum_low ?
                jnz     short lab_1023C

                mov     ax, word ptr i_high         ; i_high>tmp_high
                cmp     ax, word ptr tmp_high       
                jbe     short lab_1021C
                jmp     short lab_1023C

; if (i_high==tmp_high && i_low>=tmp_low)
 lab_1021C:                             
                mov     ax, word ptr i_high        ; i_high==tmp_high ?
                cmp     ax, word ptr tmp_high
                jnz     short lab_10230
                mov     ax, cx         ; i_low>=tmp_low ?
                cmp     ax, word ptr tmp_low
                jb      short lab_10230
                jmp     short lab_1023C

 lab_10230:                                 
                mov     ax, word ptr i_high    ; out put !!!
                mov     word ptr out_high, ax
                mov     ax, cx
                mov     word ptr out_low, ax
                call    output
                mov     ax, word ptr tmp_high    ; out put !!!
                mov     word ptr out_high, ax
                mov     ax, word ptr tmp_low
                mov     word ptr out_low, ax
                call    output
                mov	    ah, 2
                mov     dl, 13
                int     21h
                mov     dl, 10
                int     21h

 lab_1023C:                              
                inc     cx
                cmp     cx, 0
                jz      tmp_jmp1
                jmp     lab_101D5
;  _main           endp
tmp_jmp1:       jmp     lab_10363
; part 2 end maybe
lab_10245:                             
; if (i_high!=min_high && min_high!=max_high && i_high!=max_high)                                         
                mov     ax, word ptr i_high     ; i_high!=min_high
                cmp     ax, word ptr min_high
                jnz     short lab_10251
                jmp     lab_102DC

 lab_10251:                                     ; min_high!=max_high
                mov     ax, word ptr min_high
                cmp     ax, word ptr max_high
                jnz     short lab_1025D

tt2:
                jmp     lab_102DC

 lab_1025D:                                     ; i_high!=max_high
                mov     ax, word ptr i_high
                cmp     ax, word ptr max_high
                jz      tt2
; part 3 for start
                mov     cx, 0       ; i_low=0

 lab_1026C:                                     
                mov     ax, word ptr i_high     ; arg_high=i_high;
                mov     word ptr arg_high, ax
                mov     ax, cx      ; arg_low=i_low;
                mov     word ptr arg_low, ax
                call    GetFactorSum                  ; GetFactorSum(void)
                mov     ax, word ptr sum_high   ; tmp_high=sum_high;
                mov     word ptr tmp_high, ax   
                mov     ax, word ptr sum_low    ; tmp_low=sum_low;
                mov     word ptr tmp_low, ax
                mov     ax, word ptr tmp_high   ; arg_high=tmp_high;
                mov     word ptr arg_high, ax
                mov     ax, word ptr tmp_low    ; arg_low=tmp_low;
                mov     word ptr arg_low, ax
                call    GetFactorSum                 ; GetFactorSum(void)
                mov     ax, word ptr i_high
                cmp     ax, word ptr sum_high  ; i_high==sum_high ?
                jnz     short lab_102D3
                mov     ax, cx
                cmp     ax, word ptr sum_low   ; i_low==sum_low
                jnz     short lab_102D3
                mov     ax, word ptr i_high    
                cmp     ax, word ptr tmp_high  ; i_high>tmp_high
                jbe     short lab_102B3
                jmp     short lab_102D3

 lab_102B3:                              
                mov     ax, word ptr i_high    ; i_high==tmp_high
                cmp     ax, word ptr tmp_high
                jnz     short lab_102C7
                mov     ax, cx
                cmp     ax, word ptr tmp_low   ; i_low>=tmp_low
                jb      short lab_102C7
                jmp     short lab_102D3

 lab_102C7:                                                          
                mov     ax, word ptr i_high    ; out put !!!
                mov     word ptr out_high, ax
                mov     ax, cx
                mov     word ptr out_low, ax
                call    output
                mov     ax, word ptr tmp_high    ; out put !!!
                mov     word ptr out_high, ax
                mov     ax, word ptr tmp_low
                mov     word ptr out_low, ax
                call    output
                mov	    ah, 2
                mov     dl, 13
                int     21h
                mov     dl, 10
                int     21h

 lab_102D3:                                                         
                inc     cx
                cmp     cx, 0
                jz      tmp_jmp2
                jmp     lab_1026C
; part 3 end
tmp_jmp2:       jmp     lab_10363
; if (i_high==max_high)
 lab_102DC:                              
                mov     ax, word ptr i_high
                cmp     ax, word ptr max_high        ; i_high==max_high
                jnz     tmp_jmp2
                mov     cx, 0       ; i_low = 0
                jmp     lab_10358

 lab_102ED:                              
                mov     ax, word ptr i_high     ; arg_high=i_high;
                mov     word ptr arg_high, ax   
                mov     ax, cx      ; arg_low=i_low;
                mov     word ptr arg_low, ax
                call    GetFactorSum                  ; GetFactorSum(void)
                mov     ax, word ptr sum_high
                mov     word ptr tmp_high, ax   ; tmp_high=sum_high;
                mov     ax, word ptr sum_low
                mov     word ptr tmp_low, ax    ; tmp_low=sum_low;
                mov     ax, word ptr tmp_high
                mov     word ptr arg_high, ax   ; arg_high=tmp_high;
                mov     ax, word ptr tmp_low
                mov     word ptr arg_low, ax    ; arg_low=tmp_low;
                call    GetFactorSum                  ; GetFactorSum(void)
; if (i_high==sum_high && i_low==sum_low )
                mov     ax, word ptr i_high     ; i_high==sum_high
                cmp     ax, word ptr sum_high
                jnz     short lab_10354
                mov     ax, cx      ; i_low==sum_low
                cmp     ax, word ptr sum_low
                jnz     short lab_10354
                mov     ax, word ptr i_high     ; i_high>tmp_high
                cmp     ax, word ptr tmp_high
                jbe     short lab_10334
                jmp     short lab_10354

 lab_10334:                              
                mov     ax, word ptr i_high     ; i_high==tmp_high
                cmp     ax, word ptr tmp_high
                jnz     short lab_10348
                mov     ax, cx      ; i_low>=tmp_low
                cmp     ax, word ptr tmp_low
                jb      short lab_10348
                jmp     short lab_10354

 lab_10348:                              
                mov     ax, word ptr i_high    ; out put !!!
                mov     word ptr out_high, ax
                mov     ax, cx
                mov     word ptr out_low, ax
                call    output
                mov     ax, word ptr tmp_high    ; out put !!!
                mov     word ptr out_high, ax
                mov     ax, word ptr tmp_low
                mov     word ptr out_low, ax
                call    output

                mov	    ah, 2
                mov     dl, 13
                int     21h
                mov     dl, 10
                int     21h
 lab_10354:                              
                                        
                inc     cx          ; i low ++

 lab_10358:                              
                mov     ax, cx
                cmp     ax, word ptr max_low    ; i_low<=max_low
                jbe     fuck2
                jmp     lab_10363 
fuck2:
                jmp     lab_102ED
 lab_10363:                              
                                        
                inc     word ptr i_high         ; i high ++

 lab_10367:                                 
                mov     ax, word ptr i_high 
                cmp     ax, word ptr max_high   ; i_high<=max_high
                ja      exit
                jmp     if1_test1                         
exit:  	
	            mov     ax, 4c00h
  	            int     21h

GetFactorSum    proc near              
                push    cx
                mov     ax, word ptr arg_low
                mov     dx, word ptr arg_high
                mov     cx, 02h
                call    large_div
                mov     word ptr range_low, ax
                mov     word ptr range_high, dx

                push    si
                mov     word ptr sum_high, 0
                mov     word ptr sum_low, 0
; for start
                mov     word ptr j_high, 0
                jmp     lab_100E2

lab_10029:                             
                mov     ax, word ptr j_high
                cmp     ax, word ptr arg_high       ; j_high == arg_high ?
                jnz     short lab_1008E
; f1 start
                mov     word ptr j_low, 0           ; j_low=0
                jmp     short lab_10083             

; if (j_high==arg_high && j_low==arg_low)
lab_1003A:                              
                mov     ax, word ptr j_high         ; j_high==arg_high ? 
                cmp     ax, word ptr arg_high       
                jnz     short lab_1004F
                mov     ax, word ptr j_low
                cmp     ax, word ptr arg_low         ; j_low==arg_low ?
                jnz     short lab_1004F
                jmp     lab_100F0

lab_1004F:                            

                mov     ax, word ptr arg_low
                mov     dx, word ptr arg_high
                mov     cx, word ptr j_low
                cmp     cx, 0
                jz      lab_1007F
                call    large_div                   ; arg % j == 0 ?
                cmp     cx, 00h
                jnz     lab_1007F

                mov     ax, word ptr sum_low
                mov     bx, word ptr sum_high
                mov     cx, word ptr j_low
                mov     dx, word ptr j_high

                add     ax, cx
                adc     bx, dx
            
                mov     word ptr sum_low, ax
                mov     word ptr sum_high, bx
lab_1007F: 
                inc     word ptr j_low          ; j_low ++

lab_10083:                              
                mov     ax, word ptr j_low
                cmp     ax, word ptr range_low    ; j_low<=arg_low ? 
                jbe     short lab_1003A
                jmp     short lab_100DE
; else 
lab_1008E:                              
                mov     word ptr j_low, 0       ; j_low = 0

; if (j_high==arg_high && j_low==arg_low)
lab_10094:                              
                mov     ax, word ptr j_high     
                cmp     ax, word ptr arg_high   ; j_high==arg_high
                jnz     short lab_100A8
                mov     ax, word ptr j_low
                cmp     ax, word ptr arg_low    ; j_low==arg_low
                jnz     short lab_100A8
                jmp     short lab_100F0
lab_100A8:                              
                mov     ax, word ptr arg_low
                mov     dx, word ptr arg_high
                mov     cx, word ptr j_low
                cmp     cx, 0
                jz      lab_100D8
                call    large_div                   ; arg % j == 0 ?
                cmp     cx, 00h
                jnz     lab_100D8

                mov     ax, word ptr sum_low
                mov     bx, word ptr sum_high
                mov     cx, word ptr j_low
                mov     dx, word ptr j_high

                add     ax, cx
                adc     bx, dx
            
                mov     word ptr sum_low, ax
                mov     word ptr sum_high, bx

lab_100D8:                              
                inc     word ptr j_low      ; j low ++
                cmp     word ptr j_low, 0
                je      lab_100DE
                jmp     short lab_10094
lab_100DE:                              
                inc     word ptr j_high     ; j high ++

lab_100E2:                             
                mov     ax, word ptr j_high
                cmp     ax, word ptr range_high   ; j_high<=arg_high ?
                ja      lab_100EE
                jmp     lab_10029

lab_100EE:                              
                jmp     lab_100F0
lab_100F0:                                                   
                pop     si
                pop     cx
                ret
GetFactorSum          endp
codesg ends
end start