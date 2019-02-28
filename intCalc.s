.data
msg_3:
   .word 82
   .ascii "OverflowError: the result is too small/large to store in a 4-byte signed-integer.\n"
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
    LDR r0, =42
    STR r0, [r1]
    SUB sp, sp, #4
    MOV r1, sp
    LDR r0, =30
    STR r0, [r1]
    SUB sp, sp, #4
    MOV r2, sp
    LDR r0, [sp, #8]
    LDR r1, [sp, #4]
    ADDS r0, r0, r1
    BLVS p_throw_overflow_error
    STR r0, [r2]
    LDR r1, [sp]
    MOV r0, r1
    BL p_print_int
    BL p_print_ln
    ADD sp, sp, #12
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
p_throw_overflow_error:
    LDR r0, =msg_3
    BL BL p_throw_runtime_error
