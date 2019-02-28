.data
.text
.global main
main:
    PUSH {lr}
    SUB sp, sp, #4
    MOV r1, sp
    MOV r0, #'!'
    STRB r0, [r1]
    ADD sp, sp, #4
    MOV r0, #0
    POP {pc}
done:
