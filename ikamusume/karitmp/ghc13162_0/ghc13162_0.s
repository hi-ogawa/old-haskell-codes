.data
	.align 4
.align 1
.globl __stginit_Main
.type __stginit_Main, @object
__stginit_Main:
.globl __stginit_ZCMain
.type __stginit_ZCMain, @object
__stginit_ZCMain:
.data
	.align 4
.align 1
sfD_closure:
	.long	ghczmprim_GHCziTypes_Czh_static_info
	.long	115
.section .data
	.align 4
.align 1
Main_main_srt:
	.long	base_SystemziIO_putChar_closure
	.long	sfD_closure
.data
	.align 4
.align 1
.globl Main_main_closure
.type Main_main_closure, @object
Main_main_closure:
	.long	Main_main_info
	.long	0
	.long	0
	.long	0
.text
	.align 4,0x90
	.long	Main_main_srt-(Main_main_info)+0
	.long	0
	.long	196630
.globl Main_main_info
.type Main_main_info, @object
Main_main_info:
.LcfT:
	leal -12(%ebp),%eax
	cmpl 84(%ebx),%eax
	jb .LcfV
	addl $8,%edi
	cmpl 92(%ebx),%edi
	ja .LcfX
	movl $stg_CAF_BLACKHOLE_info,-4(%edi)
	movl 100(%ebx),%eax
	movl %eax,0(%edi)
	leal -4(%edi),%eax
	pushl %eax
	pushl %esi
	pushl %ebx
	call newCAF
	addl $12,%esp
	testl %eax,%eax
	je .LcfY
.LcfZ:
	movl $stg_bh_upd_frame_info,-8(%ebp)
	leal -4(%edi),%eax
	movl %eax,-4(%ebp)
	movl $base_SystemziIO_putChar_closure,%esi
	movl $sfD_closure+1,-12(%ebp)
	addl $-12,%ebp
	jmp stg_ap_p_fast
.LcfX:
	movl $8,116(%ebx)
.LcfV:
	jmp *-8(%ebx)
.LcfY:
	jmp *(%esi)
	.size Main_main_info, .-Main_main_info
.section .data
	.align 4
.align 1
ZCMain_main_srt:
	.long	base_GHCziTopHandler_runMainIO_closure
	.long	Main_main_closure
.data
	.align 4
.align 1
.globl ZCMain_main_closure
.type ZCMain_main_closure, @object
ZCMain_main_closure:
	.long	ZCMain_main_info
	.long	0
	.long	0
	.long	0
.text
	.align 4,0x90
	.long	ZCMain_main_srt-(ZCMain_main_info)+0
	.long	0
	.long	196630
.globl ZCMain_main_info
.type ZCMain_main_info, @object
ZCMain_main_info:
.Lcgh:
	leal -12(%ebp),%eax
	cmpl 84(%ebx),%eax
	jb .Lcgj
	addl $8,%edi
	cmpl 92(%ebx),%edi
	ja .Lcgl
	movl $stg_CAF_BLACKHOLE_info,-4(%edi)
	movl 100(%ebx),%eax
	movl %eax,0(%edi)
	leal -4(%edi),%eax
	pushl %eax
	pushl %esi
	pushl %ebx
	call newCAF
	addl $12,%esp
	testl %eax,%eax
	je .Lcgm
.Lcgn:
	movl $stg_bh_upd_frame_info,-8(%ebp)
	leal -4(%edi),%eax
	movl %eax,-4(%ebp)
	movl $base_GHCziTopHandler_runMainIO_closure,%esi
	movl $Main_main_closure,-12(%ebp)
	addl $-12,%ebp
	jmp stg_ap_p_fast
.Lcgl:
	movl $8,116(%ebx)
.Lcgj:
	jmp *-8(%ebx)
.Lcgm:
	jmp *(%esi)
	.size ZCMain_main_info, .-ZCMain_main_info
.section .note.GNU-stack,"",@progbits
.ident "GHC 7.4.1"
