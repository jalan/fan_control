    #include    <p18F2423.inc>  ; Define processor-specific variables
    list        p=18F2423       ; Include a .lst output

;==============================================================================;
; Configuration bits: see p18F2423.inc for full details                        ;
;==============================================================================;

    CONFIG  OSC     =   INTIO67 ; Use internal oscillator; RA6 and RA7 are used
                                ; for port I/O
    CONFIG  FCMEN   =   OFF     ; Fail-safe clock monitor disabled
    CONFIG  IESO    =   OFF     ; Oscillator switchover mode disabled
    CONFIG  PWRT    =   OFF     ; Power-up timer is disabled
    CONFIG  BOREN   =   OFF     ; Brown-out reset disabled in hardware and
                                ; software
    CONFIG  BORV    =   2       ; Brown-out voltage is 2V
    CONFIG  WDT     =   OFF     ; Watchdog timer disabled (control is placed on
                                ; the SWDTEN bit)
    CONFIG  WDTPS   =   32768   ; Watchdog timer postscaler is 1:32768
    CONFIG  MCLRE   =   ON      ; MCLR pin enabled; RE3 input pin disabled
    CONFIG  LPT1OSC =   OFF     ; Timer1 configured for higher power operation
    CONFIG  PBADEN  =   OFF     ; PORTB<4:0> pins are configured as digital I/O
                                ; on reset
    CONFIG  CCP2MX  =   PORTC   ; CCP2 input/output is multiplexed with RC1
    CONFIG  STVREN  =   ON      ; Stack full/underflow will cause reset
    CONFIG  LVP     =   ON      ; Low-voltage programming is enabled
    CONFIG  XINST   =   OFF     ; Extended instruction set is disabled
    CONFIG  DEBUG   =   ON      ; Background debugger enabled, using RB6 and RB7
    CONFIG  CP0     =   OFF     ; Block 0 (000800-001FFFh) not code-protected
    CONFIG  CP1     =   OFF     ; Block 1 (002000-003FFFh) not code-protected
    CONFIG  CPB     =   OFF     ; Boot block (000000-0007FFh) not code-protected
    CONFIG  CPD     =   OFF     ; Data EEPROM not code-protected
    CONFIG  WRT0    =   OFF     ; Block 0 (000800-001FFFh) not write-protected
    CONFIG  WRT1    =   OFF     ; Block 1 (002000-003FFFh) not write-protected
    CONFIG  WRTB    =   OFF     ; Boot block (000000-0007FFh) not
                                ; write-protected
    CONFIG  WRTC    =   OFF     ; Configuration registers (300000-3000FFh) not
                                ; write-protected
    CONFIG  WRTD    =   OFF     ; Data EEPROM not write-protected
    CONFIG  EBTR0   =   OFF     ; Block 0 (000800-001FFFh) not protected from
                                ; table reads
    CONFIG  EBTR1   =   OFF     ; Block 1 (002000-003FFFh) not protected from
                                ; table reads
    CONFIG  EBTRB   =   OFF     ; Boot block (000000-0007FFh) not protected from
                                ; table reads

;==============================================================================;
; Symbol table                                                                 ;
;==============================================================================;

    udata_acs       0x00

duty_cycle          res 1   ; (Desired duty cycle)/10%, e.g. 50% -> 5

temp_internal       res 1   ; Temperature inside highboy (F)
temp_external       res 1   ; Temperature outside highboy (F)
temp_diff           res 1   ; temp_internal - temp_external (F)

temp_int_tens       res 1   ; |
temp_int_ones       res 1   ; |
temp_ext_tens       res 1   ; |-- We have only two digits on the display for
temp_ext_ones       res 1   ; |   temperatures

fan_1_period_hi     res 1   ; 8 high bits of the 16-bit period for one pulse
fan_1_period_lo     res 1   ; 8 low bits of the 16-bit period for one pulse
fan_2_period_hi     res 1   ; 8 high bits of the 16-bit period for one pulse
fan_2_period_lo     res 1   ; 8 low bits of the 16-bit period for one pulse

fan_1_thousands     res 1   ; |
fan_1_hundreds      res 1   ; |
fan_2_thousands     res 1   ; |-- RPM values to send to the display
fan_2_hundreds      res 1   ; |

dividend_hi         res 1   ; |-- Dividend for DIVIDE_24BY16 routine, also where
dividend_mid        res 1   ; |   the quotient ends up, and will serve as input
dividend_lo         res 1   ; |   to the BIN_TO_BCD routine

divisor_hi          res 1   ; |
divisor_lo          res 1   ; |-- Divisor for DIVIDE_24BY16 routine

loop_count_24by16   res 1   ; Loop counter used by DIVIDE_24BY16 routine

remainder_hi        res 1   ; |
remainder_lo        res 1   ; |-- Remainder from DIVIDE_24BY16 routine

bcd_ten_thousands   res 1   ; |
bcd_thousands       res 1   ; |
bcd_hundreds        res 1   ; |
bcd_tens            res 1   ; |-- Output from the BIN_TO_BCD routine
bcd_ones            res 1   ; |

routine_temp        res 1   ; Used by several routines, just be careful

fan_1_error         res 1   ; If this is set, fan 1 is stuck
fan_2_error         res 1   ; If this is set, fan 2 is stuck

delay_1             res 1   ; |
delay_2             res 1   ; |-- Used in the WAIT_ routines

;==============================================================================;
; Reset and interrupt vectors                                                  ;
;==============================================================================;

        ORG     0x0000
RESET_VECTOR
        BRA     MAIN

; These should never happen at this point, but just in case...
        ORG     0x0008
HP_INTERRUPT
        RETFIE          ; High-priority interrupt vector: just return and
                        ; re-enable interrupts

        ORG     0x0018
LP_INTERRUPT
        RETFIE          ; Low-priority interrupt vector: just return and
                        ; re-enable interrupts

;==============================================================================;
; Main routine                                                                 ;
;==============================================================================;

        ORG     0x0028
MAIN
        CALL    INITIALIZE
MAIN_LOOP_0
        CALL    CHECK_TV
        CALL    GET_TEMPS
        CALL    GET_TEMP_DIGITS
        CALL    TEMP_TO_DUTY
MAIN_LOOP_1
        CALL    DUTY_TO_PWM
    ; Give the fans a moment to get going
        CALL    WAIT_100_MS
        CALL    WAIT_100_MS
        CALL    WAIT_100_MS
        CALL    WAIT_100_MS
        CALL    WAIT_100_MS
        CALL    GET_FAN_PERIODS

    ; Handle possible stuck fan error
        MOVF    fan_1_error, W, A
        BNZ     FAN_ERROR_HANDLE
        MOVF    fan_2_error, W, A
        BNZ     FAN_ERROR_HANDLE
        BRA     NO_ERROR
FAN_ERROR_HANDLE
            BSF     LATB, RB3, A        ; Turn on "error" light
            CLRF    fan_1_thousands, A  ; |
            CLRF    fan_1_hundreds, A   ; |
            CLRF    fan_2_thousands, A  ; |-- We will display '00' on the
            CLRF    fan_2_hundreds, A   ; |   RPM display

            BTFSC   fan_1_error, 1, A
                CALL    DISP_1      ; Fan 1 is stuck
            BTFSC   fan_2_error, 1, A
                CALL    DISP_2      ; Fan 2 is stuck
            
            MOVF    duty_cycle, W, A    
            ADDLW   -D'10'
            BZ      DUTY_IS_MAXED       ; duty_cycle is 10 already
                CALL    INC_DUTY_CYCLE      ; Increase duty_cycle
DUTY_IS_MAXED
            BRA     MAIN_LOOP_1

NO_ERROR
        BCF     LATB, RB3, A            ; Turn off "error" light
        CALL    GET_RPM_DIGITS
    ; Display the speed of fan 1 for a second
        CALL    DISP_1
        CALL    WAIT_100_MS
        CALL    WAIT_100_MS
        CALL    WAIT_100_MS
        CALL    WAIT_100_MS
        CALL    WAIT_100_MS
        CALL    WAIT_100_MS
        CALL    WAIT_100_MS
        CALL    WAIT_100_MS
        CALL    WAIT_100_MS
        CALL    WAIT_100_MS
    ; Display the speed of fan 2 for a second
        CALL    DISP_2
        CALL    WAIT_100_MS
        CALL    WAIT_100_MS
        CALL    WAIT_100_MS
        CALL    WAIT_100_MS
        CALL    WAIT_100_MS
        CALL    WAIT_100_MS
        CALL    WAIT_100_MS
        CALL    WAIT_100_MS
        CALL    WAIT_100_MS
        CALL    WAIT_100_MS
    ; Start over
        BRA     MAIN_LOOP_0

;==============================================================================;
; Other routines                                                               ;
;==============================================================================;

INITIALIZE  ; Configure pins and peripherals

; The debugger seems to keep enabling interrupts, so this is necessary for now
        CLRF    INTCON, A

; Clear error indicators
        CLRF    fan_1_error, A
        CLRF    fan_2_error, A

;----------------------;
; Set up and start PWM ;
;----------------------;

; Set the period to ~100 Hz by putting 155 in PR2 and setting the TMR2 prescaler
; to 16. The period is given by the following:
;     (PWM period) = [(PR2)+1]*4*TOSC*(TMR2 prescaler)
; (TOSC is by default 10^-6 on this chip)
        MOVLW   D'155'
        MOVWF   PR2, A
        BSF     T2CON, T2CKPS1, A

; Record the duty cycle level
        MOVLW   D'0'
        MOVWF   duty_cycle, A

; Set the PWM 1 duty cycle to 0% by default
;     (PWM duty period) = (CCPR1L:CCP1CON<5:4>)*TOSC*(TMR2 prescaler)

        BCF     CCP1CON, DC1B1, A   ; Just leave these two cleared throughout,
        BCF     CCP1CON, DC1B0, A   ; since we don't need the fine resolution

; Set the PWM 2 duty cycle to 0% by default (requires inversion off-chip)
;     (PWM duty period) = (CCPR2L:CCP2CON<5:4>)*TOSC*(TMR2 prescaler)

        BCF     CCP2CON, DC2B1, A   ; Just leave these two cleared throughout,
        BCF     CCP2CON, DC2B0, A   ; since we don't need the fine resolution

        CALL    DUTY_TO_PWM

; Set the appropriate pins to output
        BCF     TRISC, CCP1, A
        BCF     TRISC, RC1, A       ; RC1 = CCP2

; Start TIMER2
        BSF     T2CON, TMR2ON, A

; Start PWM 1
        BSF     CCP1CON, CCP1M3, A
        BSF     CCP1CON, CCP1M2, A

; Start PWM 2
        BSF     CCP2CON, CCP2M3, A
        BSF     CCP2CON, CCP2M2, A

;------------------------------------------------;
; Configure A/D converter for temperature inputs ;
;------------------------------------------------;

    ; By default, the power supply voltages (0 and 5V) are used as references

    ; Make RA0 and RA1 analog inputs
        BSF     ADCON1, PCFG3, A
        BSF     ADCON1, PCFG2, A
        BCF     ADCON1, PCFG1, A
        BSF     ADCON1, PCFG0, A

    ; Use left-justified results
        BCF     ADCON2, ADFM, A

    ; We'll do slow conversions
        BSF     ADCON2, ACQT2, A
        BSF     ADCON2, ACQT1, A
        BSF     ADCON2, ACQT0, A

    ; Turn on the converter
        BSF     ADCON0, ADON, A

;----------------------------------------;
; Prepare MSSP for seven-segment display ;
;----------------------------------------;

        ; SSPSTAT looks good by default

        BSF     SSPCON1, SSPM1, A   ; Master mode with clock = FOSC/64
        BSF     SSPCON1, SSPEN, A   ; MSSP enabled

        BCF     TRISC, SDO, A   ; Serial data output enabled
        BCF     TRISC, SCK, A   ; Clock output enabled
        BCF     TRISC, RC7, A   ; |
        BCF     PORTC, RC7, A   ; |-- This is for latch control

;---------------------------------------------------------------;
; Prepare outputs for lights, and turn on the "operating" light ;
;---------------------------------------------------------------;

    ; This "initializes" PORTB? Data sheet is unclear
        CLRF    PORTB, A
        CLRF    LATB, A

    ; Pin 25, for the "operating" light
        BCF     TRISB, RB4, A   ; Set to output
        BSF     LATB, RB4, A    ; It's on by default

    ; Pin 24, for the "error" light
        BCF     TRISB, RB3, A   ; Set to output
        BCF     LATB, RB3, A    ; It's off by default

;------------------------------------------------------;
; Enable "weak" internal pull-ups on tachometer inputs ;
;------------------------------------------------------;

        MOVLW   B'00000110'         ; |-- Use pull-ups on RB1 and RB2
        MOVWF   LATB, A             ; |
        BCF     INTCON2, RBPU, A    ; Enable pull-ups

    RETURN        

;-------------------------------------------------------------------------------

CHECK_TV    ; If the TV is off, disable PWM and wait for it to come back on

        BTFSC   PORTB, RB0, A   ; Is the TV on?
            RETURN                  ; If yes, don't change anything

TV_IS_OFF
    ; Disable the fans
        CLRF    duty_cycle, A
        CALL    DUTY_TO_PWM
    ; Clear the display
        CALL    DISP_OFF
    ; Turn off the lights
        BCF     LATB, RB3, A    ; Turn off "error" light
        BCF     LATB, RB4, A    ; Turn off "operating" light
    ; Is the TV still off?
STILL_OFF
        BTFSS   PORTB, RB0, A   ; Is the TV on?
            BRA     STILL_OFF       ; If no, don't do anything
    ; When the TV comes back on, just reset
        RESET

;-------------------------------------------------------------------------------

GET_TEMPS           ; Fill in temp_internal, temp_external, and temp_diff

                    ; "The user is responsible for ensuring the required
                    ;  acquisition time has passed between selecting the desired
                    ;  input channel and setting the GO/!DONE bit"

    ; I'm only using 8 bits of the converter, so the temperature in F is
    ; [ADRESH]*500/256. This means the temperature in F is about [ADRESH]*2.
    ; This is off by about 1.5 at 70 F and about 2.5 at 100 F

    ; Select channel for temp_internal
        BCF     ADCON0, CHS3, A
        BCF     ADCON0, CHS2, A
        BCF     ADCON0, CHS1, A
        BCF     ADCON0, CHS0, A
    ; Wait a bit
        CALL    WAIT_10_MS
    ; Start the conversion
        BSF     ADCON0, GO, A
    ; Wait for it to finish
WAIT_FOR_TEMP_1
        BTFSC   ADCON0, GO, A
            BRA     WAIT_FOR_TEMP_1        ; Not done yet
    ; Save the temperature in F
        RLNCF   ADRESH, W, A
        MOVWF   temp_internal, A

    ; Select channel for temp_external
        BCF     ADCON0, CHS3, A
        BCF     ADCON0, CHS2, A
        BCF     ADCON0, CHS1, A
        BSF     ADCON0, CHS0, A
    ; Wait a bit
        CALL    WAIT_10_MS
    ; Start the conversion
        BSF     ADCON0, GO, A
    ; Wait for it to finish
WAIT_FOR_TEMP_2
        BTFSC   ADCON0, GO, A
            BRA     WAIT_FOR_TEMP_2        ; Not done yet
    ; Save the temperature in F
        RLNCF   ADRESH, W, A
        MOVWF   temp_external, A

    ; Put -temp_external in W
        COMF    temp_external, W, A
        ADDLW   D'1'
    ; Add temp_internal to W
        ADDWF   temp_internal, W, A
    ; Store the result
        MOVWF   temp_diff, A

    RETURN

;-------------------------------------------------------------------------------

GET_TEMP_DIGITS     ; Convert the temperatures to BCD for displaying

    ; Set temp_internal as the input
        CLRF    dividend_hi, A
        CLRF    dividend_mid, A
        MOVF    temp_internal, W, A
        MOVWF   dividend_lo, A
    ; Convert to BCD
        CALL    BIN_TO_BCD
    ; Save the tens digit
        MOVF    bcd_tens, W, A
        MOVWF   temp_int_tens, A
    ; Save the ones digit
        MOVF    bcd_ones, W, A
        MOVWF   temp_int_ones, A

    ; Set temp_external as the input
        CLRF    dividend_hi, A
        CLRF    dividend_mid, A
        MOVF    temp_external, W, A
        MOVWF   dividend_lo, A
    ; Convert to BCD
        CALL    BIN_TO_BCD
    ; Save the tens digit
        MOVF    bcd_tens, W, A
        MOVWF   temp_ext_tens, A
    ; Save the ones digit
        MOVF    bcd_ones, W, A
        MOVWF   temp_ext_ones, A

    RETURN

;-------------------------------------------------------------------------------

TEMP_TO_DUTY    ; Based on temp_internal, temp_external, and temp_diff, set
                ; duty_cycle

        MOVF    temp_internal, W, A
        ADDLW   -D'100'
        BN      TTD_0   ; If it's above 100 F inside, go 100%
            MOVLW   D'10'
            MOVWF   duty_cycle, A
            RETURN

TTD_0
        MOVF    temp_internal, W, A
        ADDLW   -D'75'
        BNN     TTD_1   ; If it's below 75 F inside, go 0%
            MOVLW   D'0'
            MOVWF   duty_cycle, A
            RETURN

TTD_1
        MOVF    temp_diff, W, A
        BNN     TTD_2       ; If it's above 75 F inside, but it is hotter
            MOVLW   D'5'    ; outside, play it safe with 50%
            MOVWF   duty_cycle, A
            RETURN

TTD_2
    ; Handle what's left
        MOVLW   D'5'                ; Start with 50%
        MOVWF   duty_cycle, A

    ; For each 10 F of difference, add 10%
        MOVF    temp_diff, W, A
TTD_LOOP
        ADDLW   -D'10'
        BN      TTD_CHECK      ; If negative, we're done
            INCF    duty_cycle, F, A
            BRA     TTD_LOOP

TTD_CHECK
    ; Make sure duty_cycle doesn't go above ten
        MOVF    duty_cycle, W, A
        ADDLW   -D'11'
        BN      TTD_END
            MOVLW   D'10'
            MOVWF   duty_cycle, A

TTD_END
    RETURN

;-------------------------------------------------------------------------------

DUTY_TO_PWM         ; Provide the PWM modules with values for duty_cycle

    ; Update the PWM 1 duty cycle
        MOVLW   high(PWM_1_DUTY_TABLE)
        MOVWF   PCLATH, A
        RLNCF   duty_cycle, W, A
        CALL    PWM_1_DUTY_TABLE
        MOVWF   CCPR1L, A

    ; Update the PWM 2 duty cycle
        MOVLW   high(PWM_2_DUTY_TABLE)
        MOVWF   PCLATH, A
        RLNCF   duty_cycle, W, A
        CALL    PWM_2_DUTY_TABLE
        MOVWF   CCPR2L, A

    RETURN

;-------------------------------------------------------------------------------

GET_FAN_PERIODS     ; Use TIMER1 to fill in fan_1_period_hi, fan_1_period_lo,
                    ; fan_2_period_hi, and fan_2_period_lo

    ; If duty_cycle is 0, just return
        MOVF  duty_cycle, W, A
        BNZ     CONTINUE_GET_FAN_PERIODS
            CLRF    fan_1_error, A
            RETURN

CONTINUE_GET_FAN_PERIODS
; Fan 1
    ; Clear TIMER1
        CLRF    TMR1L, A
        CLRF    TMR1H, A
    ; Hold PWM 1 high
        MOVLW   D'157'
        MOVWF   CCPR1L, A
    ; Wait for the duty cycle change to take effect
        CALL    WAIT_10_MS
    ; Start timing
        BSF     T1CON, TMR1ON, A
    ; Wait for a low
WAIT_FOR_LO_1_1
        BTFSC   PIR1, TMR1IF, A     ; Did TIMER1 overflow?
            BRA     DEAD_FAN_1          ; Yes, a fan isn't spinning
        BTFSC   PORTB, RB1, A       ; Is it low?
            BRA     WAIT_FOR_LO_1_1     ; No, go back and wait
    ; Clear timer
        CLRF    TMR1L, A
        CLRF    TMR1H, A
    ; Now wait for high
WAIT_FOR_HI_1_1
        BTFSC   PIR1, TMR1IF, A     ; Did TIMER1 overflow?
            BRA     DEAD_FAN_1          ; Yes, a fan isn't spinning
        BTFSS   PORTB, RB1, A       ; Is it high?
            BRA     WAIT_FOR_HI_1_1     ; No, go back and wait
    ; Low-to-high transition occurred, so stop, clear, start timer
        BCF     T1CON, TMR1ON, A
        CLRF    TMR1L, A
        CLRF    TMR1H, A
        BSF     T1CON, TMR1ON, A
    ; Now wait for low again
WAIT_FOR_LO_2_1
        BTFSC   PIR1, TMR1IF, A     ; Did TIMER1 overflow?
            BRA     DEAD_FAN_1          ; Yes, a fan isn't spinning
        BTFSC   PORTB, RB1, A       ; Is it low?
            BRA     WAIT_FOR_LO_2_1     ; No, go back and wait
    ; It's low, so stop timing
        BCF     T1CON, TMR1ON, A
    ; Save the timing results
        MOVF    TMR1H, W, A
        MOVWF   fan_1_period_hi, A
        MOVF    TMR1L, W, A
        MOVWF   fan_1_period_lo, A

; Fan 2
    ; Clear TIMER1
        CLRF    TMR1L, A
        CLRF    TMR1H, A
    ; Hold PWM 2 high
        MOVLW   D'0'
        MOVWF   CCPR2L, A
    ; Wait for the duty cycle change to take effect
        CALL    WAIT_10_MS
    ; Start timing
        BSF     T1CON, TMR1ON, A
    ; Wait for a low
WAIT_FOR_LO_1_2
        BTFSC   PIR1, TMR1IF, A     ; Did TIMER1 overflow?
            BRA     DEAD_FAN_2          ; Yes, a fan isn't spinning
        BTFSC   PORTB, RB2, A       ; Is it low?
            BRA     WAIT_FOR_LO_1_2     ; No, go back and wait
    ; Clear timer
        CLRF    TMR1L, A
        CLRF    TMR1H, A
    ; Now wait for high
WAIT_FOR_HI_1_2
        BTFSC   PIR1, TMR1IF, A     ; Did TIMER1 overflow?
            BRA     DEAD_FAN_2          ; Yes, a fan isn't spinning
        BTFSS   PORTB, RB2, A       ; Is it high?
            BRA     WAIT_FOR_HI_1_2     ; No, go back and wait
    ; Low-to-high transition occurred, so stop, clear, start timer
        BCF     T1CON, TMR1ON, A
        CLRF    TMR1L, A
        CLRF    TMR1H, A
        BSF     T1CON, TMR1ON, A
    ; Now wait for low again
WAIT_FOR_LO_2_2
        BTFSC   PIR1, TMR1IF, A     ; Did TIMER1 overflow?
            BRA     DEAD_FAN_2          ; Yes, a fan isn't spinning
        BTFSC   PORTB, RB2, A       ; Is it low?
            BRA     WAIT_FOR_LO_2_2     ; No, go back and wait
    ; It's low, so stop timing
        BCF     T1CON, TMR1ON, A
    ; Save the timing results
        MOVF    TMR1H, W, A
        MOVWF   fan_2_period_hi, A
        MOVF    TMR1L, W, A
        MOVWF   fan_2_period_lo, A

; If we got here, there was no error
    ; Re-enable PWM
        CALL    DUTY_TO_PWM

        CLRF    fan_1_error
        CLRF    fan_2_error
        BCF     PIR1, TMR1IF, A     ; Clear TIMER1 interrupt flag
        RETURN

DEAD_FAN_1
        SETF    fan_1_error   ; This indicates there is a stuck fan
        BCF     T1CON, TMR1ON, A    ; Turn off TIMER1
        BCF     PIR1, TMR1IF, A     ; Clear TIMER1 interrupt flag
        RETURN

DEAD_FAN_2
        SETF    fan_2_error   ; This indicates there is a stuck fan
        BCF     T1CON, TMR1ON, A    ; Turn off TIMER1
        BCF     PIR1, TMR1IF, A     ; Clear TIMER1 interrupt flag
        RETURN

;-------------------------------------------------------------------------------

INC_DUTY_CYCLE      ; Increase duty_cycle by 1

        MOVLW   D'1'
        ADDWF   duty_cycle, F, A
        RETURN

;-------------------------------------------------------------------------------

GET_RPM_DIGITS      ; Use fan_1_period_hi, fan_1_period_lo, fan_2_period_hi, and
                    ; fan_2_period_lo to set fan_1_thousands, fan_1_hundreds,
                    ; fan_2_thousands, and fan_2_hundreds

    ; If duty_cycle is 0, just return a bunch of zeroes
        MOVF    duty_cycle, W, A
        BNZ     CONTINUE_GET_RPM_DIGITS
            CLRF    fan_1_thousands, A
            CLRF    fan_1_hundreds, A
            CLRF    fan_2_thousands, A
            CLRF    fan_2_hundreds, A
            RETURN            

CONTINUE_GET_RPM_DIGITS
; Fan 1
    ; Put D'3750000'=H'393870' as the dividend
        MOVLW   H'39'
        MOVWF   dividend_hi, A
        MOVLW   H'38'
        MOVWF   dividend_mid, A
        MOVLW   H'70'
        MOVWF   dividend_lo, A
    ; Supply the divisor
        MOVF    fan_1_period_hi, W, A
        MOVWF   divisor_hi, A
        MOVF    fan_1_period_lo, W, A
        MOVWF   divisor_lo, A
    ; Divide it
        CALL    DIVIDE_24BY16
    ; Convert the quotient to BCD
        CALL    BIN_TO_BCD
    ; Save the thousands digit
        MOVF    bcd_thousands, W, A
        MOVWF   fan_1_thousands, A
    ; Save the hundreds digit
        MOVF    bcd_hundreds, W, A
        MOVWF   fan_1_hundreds, A

; Fan 2
    ; Put D'3750000'=H'393870' as the dividend
        MOVLW   H'39'
        MOVWF   dividend_hi, A
        MOVLW   H'38'
        MOVWF   dividend_mid, A
        MOVLW   H'70'
        MOVWF   dividend_lo, A
    ; Supply the divisor
        MOVF    fan_2_period_hi, W, A
        MOVWF   divisor_hi, A
        MOVF    fan_2_period_lo, W, A
        MOVWF   divisor_lo, A
    ; Divide it
        CALL    DIVIDE_24BY16
    ; Convert the quotient to BCD
        CALL    BIN_TO_BCD
    ; Save the thousands digit
        MOVF    bcd_thousands, W, A
        MOVWF   fan_2_thousands, A
    ; Save the hundreds digit
        MOVF    bcd_hundreds, W, A
        MOVWF   fan_2_hundreds, A

    RETURN

;-------------------------------------------------------------------------------

DISP_1      ; Show the speed of fan 1 on the display
            ; Show the internal temperature on the display

    ; Display the hundreds digit of fan 1 RPM
        MOVF    fan_1_hundreds, W, A
        CALL    DISP_W
    ; Display the thousands digit of fan 1 RPM
        MOVF    fan_1_thousands, W, A
        CALL    DISP_W
    ; Display a "1" to indicate that this is fan 1
        MOVLW   D'1'
        CALL    DISP_W

    ; Display the ones digit of the internal temperature
        MOVF    temp_int_ones, W, A
        CALL    DISP_W
    ; Display the tens digit of the internal temperature
        MOVF    temp_int_tens, W, A
        CALL    DISP_W
    ; Display an "I" to indicate that this is the internal temperature
        MOVLW   D'1'
        CALL    DISP_W

    RETURN

;-------------------------------------------------------------------------------

DISP_2      ; Show the speed of fan 2 on the display
            ; Show the external temperature on the display

    ; Display the hundreds digit of fan 2 RPM
        MOVF    fan_2_hundreds, W, A
        CALL    DISP_W
    ; Display the thousands digit of fan 2 RPM
        MOVF    fan_2_thousands, W, A
        CALL    DISP_W
    ; Display a "2" to indicate that this is fan 2
        MOVLW   D'2'
        CALL    DISP_W

    ; Display the ones digit of the external temperature
        MOVF    temp_ext_ones, W, A
        CALL    DISP_W
    ; Display the tens digit of the external temperature
        MOVF    temp_ext_tens, W, A
        CALL    DISP_W
    ; Display an "O" to indicate that this is the external (outside) temperature
        MOVLW   D'0'
        CALL    DISP_W

    RETURN

;-------------------------------------------------------------------------------

DISP_OFF    ; Don't display anything on the display

    ; Set W to all high
        MOVLW   H'FF'
    ; Send that to the display six times
        CALL    DISP_DIRECT
        CALL    DISP_DIRECT
        CALL    DISP_DIRECT
        CALL    DISP_DIRECT
        CALL    DISP_DIRECT
        CALL    DISP_DIRECT

    RETURN

;-------------------------------------------------------------------------------

DISP_W  ; Send the digit (0-9) in W to the display

    ; Store the digit away
        MOVWF   routine_temp, A
    ; Prepare for table call
        MOVLW   high(DIGIT_TO_CODE)
        MOVWF   PCLATH, A
    ; Get digit and convert it via table lookup
        RLNCF   routine_temp, W, A
        CALL    DIGIT_TO_CODE
    ; Send it to the display
        MOVWF   SSPBUF, A
    ; Wait for transmission to end
LOOP_DISP_W
        BTFSS   SSPSTAT, BF, A      ; Transmit complete?
            BRA     LOOP_DISP_W          ; No, keep waiting
    ; Hit the display latch
        BSF     PORTC, RC7, A
        BCF     PORTC, RC7, A

        RETURN

;-------------------------------------------------------------------------------

DISP_DIRECT     ; Send what is in W straight to the display
                ; Preserves content of W

    ; Send it to the display
        MOVWF   SSPBUF, A
    ; Wait for transmission to end
LOOP_DISP_DIRECT
        BTFSS   SSPSTAT, BF, A      ; Transmit complete?
            BRA     LOOP_DISP_DIRECT    ; No, keep waiting
    ; Hit the display latch
        BSF     PORTC, RC7, A
        BCF     PORTC, RC7, A

        RETURN

;-------------------------------------------------------------------------------

DIVIDE_24BY16   ; Divide a 24-bit number by a 16-bit number

                ; Dividend: dividend_hi:dividend_mid:dividend_lo
                ; Divisor: divisor_hi:divisor_lo
                ; A counter is needed: loop_count_24by16
                ; Temporary storage for remainder: remainder_hi:remainder_lo
                ; Resulting quotient: dividend_hi:dividend_mid:dividend_lo

                ; (I found this routine online, license unknown, but obviously
                ;  intended for public use. The author is Nikolai Golovchenko.

    CLRF    remainder_hi, A
    CLRF    remainder_lo, A
    MOVLW   D'24'
    MOVWF   loop_count_24by16, A
LOOP_24BY16
    RLCF    dividend_lo, W, A
    RLCF    dividend_mid, F, A
    RLCF    dividend_hi, F, A
    RLCF    remainder_lo, F, A
    RLCF    remainder_hi, F, A
    RLCF    dividend_lo, F, A
    MOVF    divisor_lo, W, A
    SUBWF   remainder_lo, F, A
    MOVF    divisor_hi, W, A
    BTFSS   STATUS, C, A
    INCFSZ  divisor_hi, W, A
    SUBWF   remainder_hi, F, A
    BTFSC   STATUS, C, A
    BSF     dividend_lo, 0, A
    BTFSC   dividend_lo, 0, A
    GOTO    SKIP_24BY16
    ADDWF   remainder_hi, F, A
    MOVF    divisor_lo, W, A
    ADDWF   remainder_lo, F, A
SKIP_24BY16
    DECFSZ  loop_count_24by16, F, A
    GOTO    LOOP_24BY16
    RETURN

;-------------------------------------------------------------------------------

BIN_TO_BCD  ; Convert 16-bit binary to 5-digit BCD

            ; Binary input: dividend_mid:dividend_lo
            ; BCD output: bcd_ten_thousands:bcd_thousands:bcd_hundreds:bcd_tens:
            ;             bcd_ones

            ; (I found this routine under the same circumstances as above. The
            ;  authors are Juan Mayoral, Scott Dattalo, and John Payson.)
 
    SWAPF   dividend_lo, W, A
    ADDWF   dividend_lo, W, A
    ANDLW   H'0F'
    BTFSC   STATUS, DC, A
    ADDLW   H'16'
    BTFSC   STATUS, DC, A
    ADDLW   H'06'
    ADDLW   H'06'
    BTFSS   STATUS, DC, A
    ADDLW   -H'06'
    BTFSC   dividend_lo, 4, A
    ADDLW   H'1B'
    BTFSS   STATUS, DC, A
    ADDLW   -H'06'
    MOVWF   bcd_ones, A
    SWAPF   dividend_mid, W, A
    ADDWF   dividend_mid, W, A
    ANDLW   H'0F'
    BTFSC   STATUS, DC, A
    ADDLW   H'16'
    BTFSC   STATUS, DC, A
    ADDLW   H'06'
    ADDLW   H'06'
    BTFSS   STATUS, DC, A
    ADDLW   -H'06'
    BTFSC   dividend_mid, 0, A
    ADDLW   H'0B'
    BTFSS   STATUS, DC, A
    ADDLW   -H'06'
    BTFSC   dividend_mid, 4, A
    ADDLW   H'1B'
    BTFSS   STATUS, DC, A
    ADDLW   -H'06'
    ADDLW   H'06'
    ADDWF   bcd_ones, W, A
    BTFSS   STATUS, DC, A
    ADDLW   -H'06'
    MOVWF   bcd_ones, A
    MOVWF   bcd_tens, A
    SWAPF   bcd_tens, F, A
    MOVLW   H'0F'
    ANDWF   bcd_ones, F, A
    ANDWF   bcd_tens, F, A
    RRCF    dividend_mid, W, A
    ANDLW   H'0F'
    ADDLW   H'06'
    BTFSS   STATUS, DC, A
    ADDLW   -H'06'
    ADDLW   H'06'
    ADDWF   bcd_tens, W, A
    BTFSS   STATUS, DC, A
    ADDLW   -H'06'
    BTFSC   dividend_lo, 5, A
    ADDLW   H'09'
    BTFSS   STATUS, DC, A
    ADDLW   -H'06'
    BTFSC   dividend_lo, 6, A
    ADDLW   H'0C'
    BTFSS   STATUS, DC, A
    ADDLW   -H'06'
    BTFSC   dividend_lo, 7, A
    ADDLW   H'18'
    BTFSS   STATUS, DC, A
    ADDLW   -H'06'
    BTFSC   dividend_mid, 0, A
    ADDLW   H'2B'
    BTFSS   STATUS, DC, A
    ADDLW   -H'06'
    BTFSC   dividend_mid, 5, A
    ADDLW   H'0F'
    BTFSS   STATUS, DC, A
    ADDLW   -H'06'
    BTFSC   dividend_mid, 6, A
    ADDLW   H'0E'
    BTFSS   STATUS, DC, A
    ADDLW   -H'06'
    BTFSC   dividend_mid, 7, A
    ADDLW   H'0C'
    BTFSS   STATUS, DC, A
    ADDLW   -H'06'
    MOVWF   bcd_tens, A
    SWAPF   bcd_tens, W, A
    ANDLW   H'0F'
    BTFSC   dividend_mid, 1, A
    ADDLW   H'0B'
    BTFSS   STATUS, DC, A
    ADDLW   -H'06'
    BTFSC   dividend_mid, 5, A
    ADDLW   H'07'
    BTFSS   STATUS, DC, A
    ADDLW   -H'06'
    BTFSC   dividend_mid, 6, A
    ADDLW   H'09'
    BTFSS   STATUS, DC, A
    ADDLW   -H'06'
    BTFSC   dividend_mid, 7, A
    ADDLW   H'0D'
    BTFSS   STATUS, DC, A
    ADDLW   -H'06'
    MOVWF   bcd_hundreds, A
    SWAPF   bcd_hundreds, W, A
    MOVWF   bcd_thousands, A
    MOVLW   H'0F'
    ANDWF   bcd_tens, F, A
    ANDWF   bcd_hundreds, F, A
    ANDWF   bcd_thousands, F, A
    RRCF    dividend_mid, W, A
    MOVWF   bcd_ten_thousands, A
    RRCF    bcd_ten_thousands, W, A
    ANDLW   H'0F'
    ADDLW   H'06'
    BTFSS   STATUS, DC, A
    ADDLW   -H'06'
    ADDLW   H'06'
    ADDWF   bcd_thousands, W, A
    BTFSS   STATUS, DC, A
    ADDLW   -H'06'
    BTFSC   dividend_mid, 6, A
    ADDLW   H'1C'
    BTFSS   STATUS, DC, A
    ADDLW   -H'06'
    BTFSC   dividend_mid, 7, A
    ADDLW   H'38'
    BTFSS   STATUS, DC, A
    ADDLW   -H'06'
    MOVWF   bcd_thousands, A
    MOVWF   bcd_ten_thousands, A
    SWAPF   bcd_ten_thousands, F, A
    MOVLW   H'0F'
    ANDWF   bcd_thousands, F, A
    ANDWF   bcd_ten_thousands, F, A
    RETURN

;-------------------------------------------------------------------------------

WAIT_10_MS  ; Wait 10 ms without using one of the timers

    MOVLW   H'F2'
    MOVWF   delay_1, A
    MOVLW   H'02'
    MOVWF   delay_2, A
WAIT_10_MS_0
    DECFSZ  delay_1, F, A
    GOTO    $ + 6
    DECFSZ  delay_2, F, A
    GOTO    WAIT_10_MS_0
    NOP
    NOP
    NOP
    RETURN

;-------------------------------------------------------------------------------

WAIT_100_MS     ; Wait 100 ms without using one of the timers

    MOVLW   H'86'
    MOVWF   delay_1, A
    MOVLW   H'14'
    MOVWF   delay_2, A
WAIT_100_MS_0
    DECFSZ  delay_1, F, A
    GOTO    $ + 6
    DECFSZ  delay_2, F, A
    GOTO    WAIT_100_MS_0
    NOP
    NOP
    NOP
    RETURN

;==============================================================================;
; Tables in program memory: be sure none of these cross page boundaries        ;
;==============================================================================;

; How to make a table call, assuming the table starts at 0x0nn00:
;       MOVLW   high(TABLE)
;       MOVWF   PCLATH, A
;       ; Now get your index into W. Make sure it's even
;       CALL    TABLE

;-------------------------------------------------------------------------------

; These are just the eight high bits that go in CCPRxL. The low bits are left at
; 0, since we don't need the fine resolution.

        ORG     0x2000
PWM_1_DUTY_TABLE
        ADDWF   PCL, F, A
        RETLW   D'0'        ;   0%, off
        RETLW   D'16'       ;  10%
        RETLW   D'32'       ;  20%
        RETLW   D'47'       ;  30%
        RETLW   D'63'       ;  40%
        RETLW   D'79'       ;  50%
        RETLW   D'94'       ;  60%
        RETLW   D'110'      ;  70%
        RETLW   D'125'      ;  80%
        RETLW   D'141'      ;  90%
        RETLW   D'157'      ; 100%, on

        ORG     0x2100
PWM_2_DUTY_TABLE
        ADDWF   PCL, F, A
        RETLW   D'157'      ;   0%, off
        RETLW   D'141'      ;  10%
        RETLW   D'125'      ;  20%
        RETLW   D'110'      ;  30%
        RETLW   D'94'       ;  40%
        RETLW   D'79'       ;  50%
        RETLW   D'63'       ;  60%
        RETLW   D'47'       ;  70%
        RETLW   D'32'       ;  80%
        RETLW   D'16'       ;  90%
        RETLW   D'0'        ; 100%, on

;-------------------------------------------------------------------------------

        ORG     0x2200
DIGIT_TO_CODE
        ADDWF   PCL, F, A
        RETLW   ~H'7E'       ; 0
        RETLW   ~H'0C'       ; 1
        RETLW   ~H'B6'       ; 2
        RETLW   ~H'9E'       ; 3
        RETLW   ~H'CC'       ; 4
        RETLW   ~H'DA'       ; 5
        RETLW   ~H'FA'       ; 6
        RETLW   ~H'0E'       ; 7
        RETLW   ~H'FE'       ; 8
        RETLW   ~H'CE'       ; 9

        END
