.data
newline: .asciiz "\n"
t1: .asciiz "Teste\n"
t3: .asciiz "printando aqui\n"
.text
	# Program Start
	j main
main:
	# CALL scan
	sw $ra, -4($sp)
	jal scan_int
	lw $ra, -4($sp)
	move $t0, $v0
	# STRING CONST t1
	la $t1, t1
	# CALL print
	sub $sp, $sp, 4
	sw $ra, -4($sp)
	sw $t1, 0($sp)
	jal print_string
	add $sp, $sp, 4
	lw $ra, -4($sp)
	move $t2, $v0
	# STRING CONST t3
	la $t3, t3
	# CALL print
	sub $sp, $sp, 4
	sw $ra, -4($sp)
	sw $t3, 0($sp)
	jal print_string
	add $sp, $sp, 4
	lw $ra, -4($sp)
	move $t4, $v0
	# CALL print
	sub $sp, $sp, 4
	sw $ra, -4($sp)
	sw $t0, 0($sp)
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
