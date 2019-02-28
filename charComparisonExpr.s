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
    MOV r0, #'a'
    STRB r0, [r1]
    SUB sp, sp, #4
    MOV r1, sp
    MOV r0, #'z'
    STRB r0, [r1]
    MOV r2, #1
    LDR r1, [sp, #4]
    LDR r0, [sp]
    CMP r1, r0
    BEQ label_0
label_1:
    MOV r2, #0
label_0:
    MOV r0, r2
    BL p_print_bool
    BL p_print_ln
    MOV r2, #1
    LDR r1, [sp, #4]
    LDR r0, [sp]
    CMP r1, r0
    BNE label_2
label_3:
    MOV r2, #0
label_2:
    MOV r0, r2
    BL p_print_bool
    BL p_print_ln
    MOV r2, #1
    LDR r1, [sp, #4]
    LDR r0, [sp]
    CMP r1, r0
    BLT label_4
label_5:
    MOV r2, #0
label_4:
    MOV r0, r2
    BL p_print_bool
    BL p_print_ln
    MOV r2, #1
    LDR r1, [sp, #4]
    LDR r0, [sp]
    CMP r1, r0
    BLE label_6
label_7:
    MOV r2, #0
label_6:
    MOV r0, r2
    BL p_print_bool
    BL p_print_ln
    MOV r2, #1
    LDR r1, [sp, #4]
    LDR r0, [sp]
    CMP r1, r0
    BGT label_8
label_9:
    MOV r2, #0
label_8:
    MOV r0, r2
    BL p_print_bool
    BL p_print_ln
    MOV r2, #1
    LDR r1, [sp, #4]
    LDR r0, [sp]
    CMP r1, r0
    BGE label_10
label_11:
    MOV r2, #0
label_10:
    MOV r0, r2
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
