.data
msg_3:
   .word 5
   .ascii "%.*s\NUL"
msg_2:
   .word 1
   .ascii "\NUL"
msg_1:
   .word 5
   .ascii "Wrong"
msg_0:
   .word 7
   .ascii "Correct"
.text
.global main
main:
    PUSH {lr}
    SUB sp, sp, #4
    MOV r1, sp
    LDR r0, =1
    AND r0, r0, #0
    MOV r2, r0
    LDR r0, =1
    AND r0, r0, #0
    ORR r2, r2, r0
    EOR r2, r2, #1
    STR r2, [r1]
    LDR r0, [sp]
    CMP r0, #1
    BEQ label_0
label_1:
    LDR r1, =msg_1
    MOV r0, r1
    BL p_print_string
    BL p_print_ln
label_2:
    ADD sp, sp, #4
    MOV r0, #0
    POP {pc}
done:
label_0:
    LDR r1, =msg_0
    MOV r0, r1
    BL p_print_string
    BL p_print_ln
    B label_2
p_print_ln:
    PUSH {lr}
    LDR r0, =msg_2
    ADD r0, r0, #4
    BL puts
    MOV r0, #0
    BL fflush
    POP {pc}
p_print_string:
    PUSH {lr}
    LDR r1, [r0]
    ADD r2, r0, #4
    LDR r0, =msg_3
    ADD r0, r0, #4
    BL printf
    MOV r0, #0
    BL fflush
    POP {pc}
