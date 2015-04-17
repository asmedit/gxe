#ifdef quick_pc_to_pointers
#undef quick_pc_to_pointers
#endif

#ifdef MODE32
#define	quick_pc_to_pointers(cpu) {					\
	uint32_t pc_tmp32 = cpu->pc;					\
	struct DYNTRANS_TC_PHYSPAGE *ppp_tmp;				\
	ppp_tmp = cpu->cd.DYNTRANS_ARCH.phys_page[pc_tmp32 >> 12];	\
	if (ppp_tmp != NULL) {						\
		cpu->cd.DYNTRANS_ARCH.cur_ic_page = &ppp_tmp->ics[0];	\
		cpu->cd.DYNTRANS_ARCH.next_ic =				\
		    cpu->cd.DYNTRANS_ARCH.cur_ic_page +			\
		    DYNTRANS_PC_TO_IC_ENTRY(pc_tmp32);			\
	} else								\
		DYNTRANS_PC_TO_POINTERS(cpu);				\
}

#ifndef quick_pc_to_pointers_arm
#define	quick_pc_to_pointers_arm(cpu) {					\
	if (cpu->cd.arm.cpsr & ARM_FLAG_T) {				\
		cpu->cd.arm.next_ic = &nothing_call;			\
	} else								\
		quick_pc_to_pointers(cpu);				\
}
#endif

#else
#define quick_pc_to_pointers(cpu)	DYNTRANS_PC_TO_POINTERS(cpu)
#endif


