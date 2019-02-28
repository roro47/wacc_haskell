.data
.text
.global main
main:
    PUSH {lr}
    SUB sp, sp, #4
    MOV r1, sp
    LDR r0, =5
    STR r0, [r1]
    LDR r0, [sp]
    BL exit
    ADD sp, sp, #4
    MOV r0, #0
    POP {pc}
done:
