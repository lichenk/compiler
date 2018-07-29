	.file	"src/simple.ll"
	.text
	.globl	_c0_main
	.align	16, 0x90
	.type	_c0_main,@function
_c0_main:                               # @_c0_main
.Leh_func_begin0:
# BB#0:
	xorl	%eax, %eax
	ret
.Ltmp0:
	.size	_c0_main, .Ltmp0-_c0_main
.Leh_func_end0:

	.section	.eh_frame,"a",@progbits
.LEH_frame0:
.Lsection_eh_frame0:
.Leh_frame_common0:
.Lset0 = .Leh_frame_common_end0-.Leh_frame_common_begin0 # Length of Common Information Entry
	.long	.Lset0
.Leh_frame_common_begin0:
	.long	0                       # CIE Identifier Tag
	.byte	1                       # DW_CIE_VERSION
	.asciz	 "zR"                   # CIE Augmentation
	.byte	1                       # CIE Code Alignment Factor
	.byte	120                     # CIE Data Alignment Factor
	.byte	16                      # CIE Return Address Column
	.byte	1                       # Augmentation Size
	.byte	3                       # FDE Encoding = udata4
	.byte	12                      # DW_CFA_def_cfa
	.byte	7                       # Register
	.byte	8                       # Offset
	.byte	144                     # DW_CFA_offset + Reg (16)
	.byte	1                       # Offset
	.align	8
.Leh_frame_common_end0:
.L_c0_main.eh = 0


	.section	".note.GNU-stack","",@progbits
