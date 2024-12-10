.data
newline: .asciiz "\n"
buffer: .space 100
t2: .asciiz "Enter your name: "
t4: .asciiz "Hello, \$name!"
.text
	# Program Start
	j main
main:
	# CALL scan
	sw $ra, -4($sp)
	sub $sp, $sp, 8
	la $t8, buffer
	li $t9, 100
	sw $t8, 0($sp)
	sw $t9, 4($sp)
	jal scan_string
	add $sp, $sp, 8
	lw $ra, -4($sp)
	move $t0, $v0
	# MOVE t1 t0
	move $t1, $t0
	# STRING CONST t2
	la $t2, t2
	# CALL print
	sub $sp, $sp, 4
	sw $ra, -4($sp)
	sw $t2, 0($sp)
	jal print_string
	add $sp, $sp, 4
	lw $ra, -4($sp)
	move $t3, $v0
	# STRING CONST t4
	la $t4, t4
	# CALL print
	sub $sp, $sp, 4
	sw $ra, -4($sp)
	sw $t4, 0($sp)
	jal print_string
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
scan_string:
	lw $a0, 0($sp)
	lw $a1, 4($sp)
	li $v0, 8
	syscall
	move $v0, $a0
	jr $ra
scan_int:
	li $v0, 5
	syscall
	jr $ra
