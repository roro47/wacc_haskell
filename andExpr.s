.data
msg_2:
   .word 6
   .ascii "false\NUL"
msg_1:
   .word 5
   .ascii "true\NUL"
msg_0:
   .word 1
   .ascii "\NUL"
.text
.global main
main:
    PUSH {lr}
    SUB sp, sp, #4
    MOV r1, sp
    LDR r0, =1
    STR r0, [r1]
    SUB sp, sp, #4
    MOV r1, sp
    LDR r0, =0
    STR r0, [r1]
    LDR r1, [sp, #4]
    LDR r0, [sp]
    AND r1, r1, r0
    MOV r0, r1
    BL p_print_bool
    BL p_print_ln
    LDR r1, [sp, #4]
    AND r1, r1, #1
    MOV r0, r1
    BL p_print_bool
    BL p_print_ln
    LDR r1, [sp]
    AND r1, r1, #0
    MOV r0, r1
    BL p_print_bool
    BL p_print_ln
    ADD sp, sp, #8
    MOV r0, #0
    POP {pc}
done:
p_print_ln:
    PUSH {lr}
    LDR r0, =msg_0
    ADD r0, r0, #4
    BL puts
    MOV r0, #0
    BL fflush
    POP {pc}
p_print_bool:
    PUSH {lr}
    CMP r0, #0
    LDRNE r0, =msg_1
    LDREQ r0, =msg_2
    ADD r0, r0, #4
    BL printf
    MOV r0, #0
    BL fflush
    POP {pc}
