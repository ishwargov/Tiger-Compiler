.globl main

main :

li $t0, 5

move $t2, $t0

li $t3, 4

add $t1, $t2, $t3

move $t7, $t0

move $t8, $t1

add $t5, $t7, $t8

li $t6, 32

add $t4, $t5, $t6

move $a0, $t4

li $v0, 4

syscall

li $a0, 10

syscall


