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
	and $t4, $t1, $t3
	# CJUMP Eq
	bne $t4, $t5, L0
	j L1
L0:
	# CALL print
	sw $t5, 0($sp)
	jal print
	move $t6, $v0
	j L2
L1:
	# CONST t12 1
	li $t4, 1
	# BINOP Or
	or $t7, $t1, $t3
	# CJUMP Eq
	bne $t7, $t4, L3
	j L4
L3:
	# CALL print
	sw $t0, 0($sp)
	jal print
	move $t1, $v0
	j L5
L4:
	# CALL print
	sw $t2, 0($sp)
	jal print
	move $t3, $v0
L5:
L2:
	# Program End
print:
	li $v0, 1
	lw $a0, 0($sp)
	# syscall to print
	jr $ra
scan:
	li $v0, 5
	# syscall to read
	jr $ra
