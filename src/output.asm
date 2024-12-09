.data
newline: .asciiz 

buffer: .space 100
.text
	# Program Start
	j main
main:
	# CONST t0 12
	li $t0, 12
	# MOVE t1 t0
	move $t1, $t0
	# CONST t2 10
	li $t2, 10
	# BINOP Add
	add $t3, $t1, $t2
	# MOVE t4 t3
	move $t4, $t3
	# CALL print
	sub $sp, $sp, 4
	sw $ra, -4($sp)
	sw $t4, 0($sp)
	jal print_int
	add $sp, $sp, 4
	lw $ra, -4($sp)
	move $t5, $v0
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
