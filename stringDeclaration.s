.data
msg_0:
   .word 12
   .ascii "Hello World!"
.text
.global main
main:
    PUSH {lr}
    SUB sp, sp, #4
    MOV r1, sp
    LDR r0, =msg_0
    STR r0, [r1]
    ADD sp, sp, #4
    MOV r0, #0
    POP {pc}
done:
