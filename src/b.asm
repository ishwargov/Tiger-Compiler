.globl main
main :
li $t0, 5
li $t1, 0
li $t2, 3
L0 :
bgt $t1, $t2, L1
move $a0, $t1
li $v0, 1
syscall
addi $t1, $t1, 1
j L0
L1 :
li $t4, 6
li $a0, 10
syscall

