# # Select architecture: RV32 or RV64.
# ARCH ?= RV64

# ifeq ($(ARCH),32)
#   override ARCH := RV32
# else ifeq ($(ARCH),64)
#   override ARCH := RV64
# endif

# ifeq ($(ARCH),RV32)
#   SAIL_XLEN := riscv_xlen32.sail
# else ifeq ($(ARCH),RV64)
#   SAIL_XLEN := riscv_xlen64.sail
# else
#   $(error '$(ARCH)' is not a valid architecture, must be one of: RV32, RV64)
# endif

# SAIL_FLEN := riscv_flen_D.sail
# SAIL_VLEN := riscv_vlen.sail

# # Instruction sources, depending on target
# SAIL_CHECK_SRCS = riscv_addr_checks_common.sail riscv_addr_checks.sail riscv_misa_ext.sail
# SAIL_DEFAULT_INST = riscv_insts_base.sail riscv_insts_aext.sail riscv_insts_cext.sail riscv_insts_mext.sail riscv_insts_zicsr.sail riscv_insts_next.sail riscv_insts_hints.sail
# SAIL_DEFAULT_INST += riscv_insts_fext.sail riscv_insts_cfext.sail
# SAIL_DEFAULT_INST += riscv_insts_dext.sail riscv_insts_cdext.sail

# SAIL_DEFAULT_INST += riscv_insts_zba.sail
# SAIL_DEFAULT_INST += riscv_insts_zbb.sail
# SAIL_DEFAULT_INST += riscv_insts_zbc.sail
# SAIL_DEFAULT_INST += riscv_insts_zbs.sail

# SAIL_DEFAULT_INST += riscv_insts_zfh.sail
# # Zfa needs to be added after fext, dext and Zfh (as it needs
# # definitions from those)
# SAIL_DEFAULT_INST += riscv_insts_zfa.sail

# SAIL_DEFAULT_INST += riscv_insts_zkn.sail
# SAIL_DEFAULT_INST += riscv_insts_zks.sail

# SAIL_DEFAULT_INST += riscv_insts_zbkb.sail
# SAIL_DEFAULT_INST += riscv_insts_zbkx.sail

# SAIL_DEFAULT_INST += riscv_insts_zicond.sail

# SAIL_DEFAULT_INST += riscv_insts_vext_utils.sail
# SAIL_DEFAULT_INST += riscv_insts_vext_vset.sail
# SAIL_DEFAULT_INST += riscv_insts_vext_arith.sail
# SAIL_DEFAULT_INST += riscv_insts_vext_fp.sail
# SAIL_DEFAULT_INST += riscv_insts_vext_mem.sail
# SAIL_DEFAULT_INST += riscv_insts_vext_mask.sail
# SAIL_DEFAULT_INST += riscv_insts_vext_vm.sail
# SAIL_DEFAULT_INST += riscv_insts_vext_red.sail

# SAIL_SEQ_INST  = $(SAIL_DEFAULT_INST) riscv_jalr_seq.sail
# SAIL_RMEM_INST = $(SAIL_DEFAULT_INST) riscv_jalr_rmem.sail riscv_insts_rmem.sail

# SAIL_SEQ_INST_SRCS  = riscv_insts_begin.sail $(SAIL_SEQ_INST) riscv_insts_end.sail
# SAIL_RMEM_INST_SRCS = riscv_insts_begin.sail $(SAIL_RMEM_INST) riscv_insts_end.sail

# # System and platform sources
# SAIL_SYS_SRCS =  riscv_csr_map.sail
# SAIL_SYS_SRCS += riscv_vext_control.sail    # helpers for the 'V' extension
# SAIL_SYS_SRCS += riscv_next_regs.sail
# SAIL_SYS_SRCS += riscv_sys_exceptions.sail  # default basic helpers for exception handling
# SAIL_SYS_SRCS += riscv_sync_exception.sail  # define the exception structure used in the model
# SAIL_SYS_SRCS += riscv_next_control.sail    # helpers for the 'N' extension
# SAIL_SYS_SRCS += riscv_softfloat_interface.sail riscv_fdext_regs.sail riscv_fdext_control.sail
# SAIL_SYS_SRCS += riscv_csr_ext.sail         # access to CSR extensions
# SAIL_SYS_SRCS += riscv_sys_control.sail     # general exception handling

# SAIL_RV32_VM_SRCS = riscv_vmem_sv32.sail riscv_vmem_rv32.sail
# SAIL_RV64_VM_SRCS = riscv_vmem_sv39.sail riscv_vmem_sv48.sail riscv_vmem_rv64.sail

# SAIL_VM_SRCS = riscv_pte.sail riscv_ptw.sail riscv_vmem_common.sail riscv_vmem_tlb.sail
# ifeq ($(ARCH),RV32)
# SAIL_VM_SRCS += $(SAIL_RV32_VM_SRCS)
# else
# SAIL_VM_SRCS += $(SAIL_RV64_VM_SRCS)
# endif

# # Non-instruction sources
# PRELUDE = prelude.sail prelude_mapping.sail $(SAIL_XLEN) $(SAIL_FLEN) $(SAIL_VLEN) prelude_mem_metadata.sail prelude_mem.sail

# SAIL_REGS_SRCS = riscv_reg_type.sail riscv_freg_type.sail riscv_regs.sail riscv_pc_access.sail riscv_sys_regs.sail
# SAIL_REGS_SRCS += riscv_pmp_regs.sail riscv_pmp_control.sail
# SAIL_REGS_SRCS += riscv_ext_regs.sail $(SAIL_CHECK_SRCS)
# SAIL_REGS_SRCS += riscv_vreg_type.sail riscv_vext_regs.sail

# SAIL_ARCH_SRCS = $(PRELUDE)
# SAIL_ARCH_SRCS += riscv_types_common.sail riscv_types_ext.sail riscv_types.sail
# SAIL_ARCH_SRCS += riscv_vmem_types.sail $(SAIL_REGS_SRCS) $(SAIL_SYS_SRCS) riscv_platform.sail
# SAIL_ARCH_SRCS += riscv_mem.sail $(SAIL_VM_SRCS)
# SAIL_ARCH_RVFI_SRCS = $(PRELUDE) rvfi_dii.sail riscv_types_common.sail riscv_types_ext.sail riscv_types.sail riscv_vmem_types.sail $(SAIL_REGS_SRCS) $(SAIL_SYS_SRCS) riscv_platform.sail riscv_mem.sail $(SAIL_VM_SRCS) riscv_types_kext.sail
# SAIL_ARCH_SRCS += riscv_types_kext.sail    # Shared/common code for the cryptography extension.

# SAIL_STEP_SRCS = riscv_step_common.sail riscv_step_ext.sail riscv_decode_ext.sail riscv_fetch.sail riscv_step.sail
# RVFI_STEP_SRCS = riscv_step_common.sail riscv_step_rvfi.sail riscv_decode_ext.sail riscv_fetch_rvfi.sail riscv_step.sail

# # Control inclusion of 64-bit only riscv_analysis
# ifeq ($(ARCH),RV32)
# SAIL_OTHER_SRCS     = $(SAIL_STEP_SRCS)
# SAIL_OTHER_COQ_SRCS = riscv_termination_common.sail riscv_termination_rv32.sail
# else
# SAIL_OTHER_SRCS     = $(SAIL_STEP_SRCS) riscv_analysis.sail
# SAIL_OTHER_COQ_SRCS = riscv_termination_common.sail riscv_termination_rv64.sail riscv_analysis.sail
# endif

# SAIL_SRCS      = $(addprefix ../sail-riscv/model/,$(SAIL_ARCH_SRCS) $(SAIL_SEQ_INST_SRCS)  $(SAIL_OTHER_SRCS))


# IMPORTANT: Don't forget to checkout to miralis branch

include ../miralis-sail-riscv/Makefile
RISCV_SAIL_SRCS      = $(addprefix ../miralis-sail-riscv/,$(SAIL_SRCS))


# git checkout 17113857 --> commit used for risc-v

.PHONY: test test2 clean

install: 
	sudo apt-get install opam
	sudo apt-get install z3

	opam init
	eval $(opam env)

	opam install dune
	opam install libsail.0.17.1
	opam install sail.0.17.1
	
all: build

build:
	dune build --release
	cp _build/default/sail_plugin_virt.cmxs plugin.cmxs
	chmod +rwx plugin.cmxs

basic: build
	./virt-sail sail_arch/basic.sail -o out.rs

basic-alt: build
	./virt-sail sail_arch/basic_alt.sail -o out.rs

csr: build
	./virt-sail sail_arch/csr.sail -o out.rs

mret: build
	./virt-sail sail_arch/mret.sail -o out.rs

basic_types: build
	./virt-sail sail_arch/basic_types.sail -o out.rs

trap: build
	./virt-sail sail_arch/arch_trap.sail -o out.rs

wfi: build
	./virt-sail sail_arch/wfi.sail -o out.rs

riscv: build
	./virt-sail $(RISCV_SAIL_SRCS) ../miralis-sail-riscv/model/main.sail -o out.rs

generate: build
	./virt-sail sail_arch/mret.sail -o ./rust_arch/mret/src/sail.rs

generate-riscv: build
	./virt-sail $(RISCV_SAIL_SRCS) ../miralis-sail-riscv/model/main.sail -o ./src/lib.rs
	python3 manual_fixes.py

riscv-c:  
	sail -c $(RISCV_SAIL_SRCS) 1> riscv_c.c 

verify: build
	cd rust_arch/mret && cargo kani 

decoder: build
	./virt-sail sail_arch/decoder.sail -o ./src/lib.rs
	python3 manual_fixes.py

clean:
	-dune clean
