.data
newline: .asciiz 

buffer: .space 100
t5: .asciiz "Both a and b are true"
t8: .asciiz "At least one of a or b is true"
t10: .asciiz "Both a and b are false"
.text
	# Program Start
	j main
main:
	# CONST t0 1
	li $t0, 1
	# MOVE t1 t0
	move $t1, $t0
	# CONST t2 0
	li $t2, 0
	# MOVE t3 t2
	move $t3, $t2
	# CONST t13 1
	li $t5, 1
	# BINOP And
	slt $t8, $zero, $t1
	slt $t9, $zero, $t3
	and $t4, $t8, $t9
	# CJUMP Eq
	bne $t4, $t5, L0
	j L1
L0:
	# STRING CONST t5 = "Both a and b are true"
	la $t5, t5
	# CALL print
	sw $ra, -4($sp)
	sub $sp, $sp, 4
	sw $t5, 0($sp)
	jal print_string
	add $sp, $sp, 4
	lw $ra, -4($sp)
	move $t6, $v0
	j L2
L1:
	# CONST t12 1
	li $t4, 1
	# BINOP Or
	slt $t8, $zero, $t1
	slt $t9, $zero, $t3
	or $t7, $t8, $t9
	# CJUMP Eq
	bne $t7, $t4, L3
	j L4
L3:
	# STRING CONST t8 = "At least one of a or b is true"
	la $t0, t8
	# CALL print
	sw $ra, -4($sp)
	sub $sp, $sp, 4
	sw $t0, 0($sp)
	jal print_string
	add $sp, $sp, 4
	lw $ra, -4($sp)
	move $t1, $v0
	j L5
L4:
	# STRING CONST t10 = "Both a and b are false"
	la $t2, t10
	# CALL print
	sw $ra, -4($sp)
	sub $sp, $sp, 4
	sw $t2, 0($sp)
	jal print_string
	add $sp, $sp, 4
	lw $ra, -4($sp)
	move $t3, $v0
L5:
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
