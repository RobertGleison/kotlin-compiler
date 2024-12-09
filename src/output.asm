.data
newline: .asciiz 

buffer: .space 100
t0: .asciiz "Enter a number: "
t8: .asciiz "Factorial is not defined for negative numbers."
t17: .asciiz "Factorial of $num is $factorial"
.text
	# Program Start
	j main
main:
	# STRING CONST t0 = "Enter a number: "
	la $t0, t0
	# CALL print
	sw $ra, -4($sp)
	sub $sp, $sp, 4
	sw $t0, 0($sp)
	jal print_string
	add $sp, $sp, 4
	lw $ra, -4($sp)
	move $t1, $v0
	# CONST t2 3
	li $t2, 3
	# MOVE t3 t2
	move $t3, $t2
	# CONST t4 1
	li $t4, 1
	# MOVE t5 t4
	move $t5, $t4
	# CONST t19 1
	li $t3, 1
	# CONST t6 0
	li $t6, 0
	# BINOP Gt
	slt $t7, $t6, $t3
	# CJUMP Eq
	bne $t7, $t3, L0
	j L1
L0:
	# STRING CONST t8 = "Factorial is not defined for negative numbers."
	la $t0, t8
	# CALL print
	sw $ra, -4($sp)
	sub $sp, $sp, 4
	sw $t0, 0($sp)
	jal print_string
	add $sp, $sp, 4
	lw $ra, -4($sp)
	move $t1, $v0
	j L2
L1:
	# CONST t10 1
	li $t2, 1
	# MOVE t11 t10
	move $t3, $t2
L3:
	# CONST t16 1
	li $t0, 1
	# BINOP Lte
	slt $t4, $t3, $t3
	xor $t4, $t4, 1
	# CJUMP Eq
	bne $t4, $t0, L4
	j L5
L4:
	# BINOP Mul
	mul $t5, $t3, $t3
	# MOVE t5 t13
	move $t5, $t5
	# CONST t14 1
	li $t6, 1
	# BINOP Add
	add $t7, $t3, $t6
	# MOVE t11 t15
	move $t3, $t7
	j L3
L5:
	# STRING CONST t17 = "Factorial of $num is $factorial"
	la $t1, t17
	# CALL print
	sw $ra, -4($sp)
	sub $sp, $sp, 4
	sw $t1, 0($sp)
	jal print_string
	add $sp, $sp, 4
	lw $ra, -4($sp)
	move $t2, $v0
L2:
	# Program End
	li $v0, 10
	syscall
print_string:
	lw $a0, 0($sp)
	li $v0, 4
	syscall
	move $v0, $zero
	jr $ra
print_int:
	lw $a0, 0($sp)
	li $v0, 1
	syscall
	move $v0, $zero
	jr $ra
scan_int:
	li $v0, 5
	syscall
	jr $ra
scan_string:
	lw $a0, 0($sp)
	lw $a1, 4($sp)
	li $v0, 8
	syscall
	jr $ra
