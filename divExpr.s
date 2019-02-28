.data
msg_3:
   .word 45
   .ascii "DivideByZeroError: divide or modulo by zero\n\NUL"
msg_2:
   .word 5
   .ascii "%.*s\NUL"
msg_1:
   .word 3
   .ascii "%d\NUL"
msg_0:
   .word 1
   .ascii "\NUL"
.text
.global main
main:
    PUSH {lr}
    SUB sp, sp, #4
    MOV r1, sp
    LDR r0, =5
    STR r0, [r1]
    SUB sp, sp, #4
    MOV r1, sp
    LDR r0, =3
    STR r0, [r1]
    LDR r1, [sp, #4]
    LDR r2, [sp]
    MOV r0, r1
    MOV r1, r2
    BL p_check_divide_by_zero
    BL __aeabi_idiv
    BL p_print_int
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
p_print_int:
    PUSH {lr}
    MOV rtemp 16, rtemp 0
    LDR rtemp 16, =msg_1
    ADD r0, r0, #4
    BL printf
    MOV r0, #0
    BL fflush
    POP {pc}
p_print_string:
    PUSH {lr}
    LDR r1, [r0]
    ADD r2, r0, #4
    LDR r0, =msg_2
    ADD r0, r0, #4
    BL printf
    MOV r0, #0
    BL fflush
    POP {pc}
p_throw_runtime_error:
    BL p_print_string
    MOV r0, #-1
    BL exit
p_check_divide_by_zero:
    PUSH {lr}
    CMP r1, #0
    LDREQ r0, =msg_3
    BLEQ p_throw_runtime_error
    POP {pc}
