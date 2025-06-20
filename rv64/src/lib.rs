//! Softcore RISC-V 64
//!
//! This library is a wrapper around a Rust translation of the official [RISC-V executable
//! specification][1]. The software core can be used to test the behavior of the hardware, for
//! instance to check if a memory access is allowed, or the register state after taking a trap.
//! This is especially helpful to test or verify low-level software, such as kernels, hypervisors,
//! or firmware.
//!
//! The raw translation is exposed in the [raw] module. A more polished (and slightly more stable)
//! interface is exposed through the [Core] methods.
//!
//! [1]: https://github.com/riscv/sail-riscv

pub mod config;
pub mod registers;

/// The raw translation of the official RISC-V executable specification.
///
/// The [RISC-V executable specification][1] is written in [Sail][2], a domain specific language to
/// specify Instruction Set Architectures (ISA). The translation is automated using a custom
/// Sail-to-Rust back-end for the Sail compiler.
///
/// [1]: https://github.com/riscv/sail-riscv
/// [2]: https://github.com/rems-project/sail
#[rustfmt::skip]
pub mod raw;

pub use raw::{Core, ExceptionType, Privilege, ast};
use registers::GeneralRegister;
use softcore_prelude::BitVector;

// ———————————————————————— Initialization Constants ———————————————————————— //

const DEFAULT_PMP_CFG: raw::Pmpcfg_ent = raw::Pmpcfg_ent {
    bits: BitVector::new(0),
};
const DEFAULT_HPM_EVENT: raw::HpmEvent = raw::HpmEvent {
    bits: BitVector::new(0),
};
const DEFAULT_TLB_ENTRY: Option<raw::TLB_Entry> = None;
const ZEROES: BitVector<64> = BitVector::new(0);

impl Core {
    /// Get the value of a general purpose register.
    pub fn get(&mut self, reg: GeneralRegister) -> u64 {
        let reg = match reg {
            raw::regidx::Regidx(reg) => reg.bits() as i128,
        };
        raw::rX(self, raw::regno::Regno(reg)).bits()
    }

    /// Set the value of a general purpose register.
    pub fn set(&mut self, reg: GeneralRegister, value: u64) {
        let reg = match reg {
            raw::regidx::Regidx(reg) => reg.bits() as i128,
        };
        raw::wX(self, raw::regno::Regno(reg), BitVector::new(value));
    }

    /// Get the value of a CSR identified by its CSR index.
    ///
    /// This function panics if the CSR is not implemented given the core configuration.
    pub fn get_csr(&mut self, csr: u64) -> u64 {
        raw::read_CSR(self, BitVector::new(csr)).bits()
    }

    /// Return the current privilege mode.
    pub fn mode(&self) -> Privilege {
        self.cur_privilege
    }

    /// Set the privilege mode
    pub fn set_mode(&mut self, mode: Privilege) {
        self.cur_privilege = mode
    }

    /// Decode an instruction
    pub fn decode_instr(&mut self, instr: u32) -> ast {
        raw::encdec_backwards(self, BitVector::new(instr as u64))
    }

    /// Return true if the CSR is defined (and enabled) on the core
    pub fn is_csr_defined(&mut self, csr_id: usize) -> bool {
        raw::is_CSR_defined(self, BitVector::new(csr_id as u64))
    }

    /// Inject an exception, triggerring the appropriate trap handler
    ///
    /// The target privilege mode depends on the current execution mode and the *deleg CSR
    /// registers.
    /// The `tval` is the trap value, which depends on the exception type. Memory access fault will
    /// usually provide the faulting address.
    pub fn inject_exception(&mut self, exception: ExceptionType, tval: u64) {
        let current_level = self.cur_privilege;
        let target_level = raw::exception_delegatee(self, exception, current_level);
        raw::trap_handler(
            self,
            target_level,
            false,
            raw::exceptionType_to_bits(exception),
            self.PC,
            Some(BitVector::new(tval)),
            None,
        );
    }

    /// Return the `pmpaddr<index>` register.
    pub fn get_pmpaddr(&self, index: usize) -> u64 {
        self.pmpaddr_n[index].bits()
    }

    /// Set the `pmpaddr<index>` register to the given value.
    pub fn set_pmpaddr(&mut self, index: usize, val: u64) {
        raw::pmpWriteAddrReg(self, index as i128, BitVector::new(val));
    }

    /// Set the `pmpcfg<index>` register to the given value.
    pub fn set_pmpcfg(&mut self, index: usize, val: u64) {
        raw::pmpWriteCfgReg(self, index as i128, BitVector::new(val));
    }

    /// Check if an 8 byte access is allowed with the current mode and PMP configuration.
    ///
    /// Return None is the check succeed, or an error otherwise.
    pub fn pmp_check(
        &mut self,
        addr: u64,
        access_kind: raw::AccessType<()>,
    ) -> Option<raw::ExceptionType> {
        let addr = raw::physaddr::Physaddr(BitVector::new(addr));
        let width = 8;
        raw::pmpCheck::<8>(self, addr, width, access_kind, self.cur_privilege)
    }
}

/// Returns a fresh core instance with the provided configuration.
pub const fn new_core(config: raw::Config) -> Core {
    Core {
        PC: BitVector::new(0),
        nextPC: BitVector::new(0),
        x1: BitVector::new(0),
        x2: BitVector::new(0),
        x3: BitVector::new(0),
        x4: BitVector::new(0),
        x5: BitVector::new(0),
        x6: BitVector::new(0),
        x7: BitVector::new(0),
        x8: BitVector::new(0),
        x9: BitVector::new(0),
        x10: BitVector::new(0),
        x11: BitVector::new(0),
        x12: BitVector::new(0),
        x13: BitVector::new(0),
        x14: BitVector::new(0),
        x15: BitVector::new(0),
        x16: BitVector::new(0),
        x17: BitVector::new(0),
        x18: BitVector::new(0),
        x19: BitVector::new(0),
        x20: BitVector::new(0),
        x21: BitVector::new(0),
        x22: BitVector::new(0),
        x23: BitVector::new(0),
        x24: BitVector::new(0),
        x25: BitVector::new(0),
        x26: BitVector::new(0),
        x27: BitVector::new(0),
        x28: BitVector::new(0),
        x29: BitVector::new(0),
        x30: BitVector::new(0),
        x31: BitVector::new(0),
        cur_privilege: raw::Privilege::Machine,
        cur_inst: BitVector::new(0),
        misa: raw::Misa {
            bits: BitVector::new(0),
        },
        mstatus: raw::Mstatus {
            bits: BitVector::new(0),
        },
        menvcfg: raw::MEnvcfg {
            bits: BitVector::new(0),
        },
        senvcfg: raw::SEnvcfg {
            bits: BitVector::new(0),
        },
        mie: raw::Minterrupts {
            bits: BitVector::new(0),
        },
        mip: raw::Minterrupts {
            bits: BitVector::new(0),
        },
        medeleg: raw::Medeleg {
            bits: BitVector::new(0),
        },
        mideleg: raw::Minterrupts {
            bits: BitVector::new(0),
        },
        mtvec: raw::Mtvec {
            bits: BitVector::new(0),
        },
        mcause: raw::Mcause {
            bits: BitVector::new(0),
        },
        mepc: BitVector::new(0),
        mtval: BitVector::new(0),
        mscratch: BitVector::new(0),
        scounteren: raw::Counteren {
            bits: BitVector::new(0),
        },
        mcounteren: raw::Counteren {
            bits: BitVector::new(0),
        },
        mcountinhibit: raw::Counterin {
            bits: BitVector::new(0),
        },
        mcycle: BitVector::new(0),
        mtime: BitVector::new(0),
        minstret: BitVector::new(0),
        minstret_increment: false,
        mvendorid: BitVector::new(0),
        mimpid: BitVector::new(0),
        marchid: BitVector::new(0),
        mhartid: BitVector::new(0),
        mconfigptr: BitVector::new(0),
        stvec: raw::Mtvec {
            bits: BitVector::new(0),
        },
        sscratch: BitVector::new(0),
        sepc: BitVector::new(0),
        scause: raw::Mcause {
            bits: BitVector::new(0),
        },
        stval: BitVector::new(0),
        tselect: BitVector::new(0),
        vstart: BitVector::new(0),
        vl: BitVector::new(0),
        vtype: raw::Vtype {
            bits: BitVector::new(0),
        },
        pmpcfg_n: [DEFAULT_PMP_CFG; 64],
        pmpaddr_n: [ZEROES; 64],
        vr0: BitVector::new(0),
        vr1: BitVector::new(0),
        vr2: BitVector::new(0),
        vr3: BitVector::new(0),
        vr4: BitVector::new(0),
        vr5: BitVector::new(0),
        vr6: BitVector::new(0),
        vr7: BitVector::new(0),
        vr8: BitVector::new(0),
        vr9: BitVector::new(0),
        vr10: BitVector::new(0),
        vr11: BitVector::new(0),
        vr12: BitVector::new(0),
        vr13: BitVector::new(0),
        vr14: BitVector::new(0),
        vr15: BitVector::new(0),
        vr16: BitVector::new(0),
        vr17: BitVector::new(0),
        vr18: BitVector::new(0),
        vr19: BitVector::new(0),
        vr20: BitVector::new(0),
        vr21: BitVector::new(0),
        vr22: BitVector::new(0),
        vr23: BitVector::new(0),
        vr24: BitVector::new(0),
        vr25: BitVector::new(0),
        vr26: BitVector::new(0),
        vr27: BitVector::new(0),
        vr28: BitVector::new(0),
        vr29: BitVector::new(0),
        vr30: BitVector::new(0),
        vr31: BitVector::new(0),
        vcsr: raw::Vcsr {
            bits: BitVector::new(0),
        },
        mhpmevent: [DEFAULT_HPM_EVENT; 32],
        mhpmcounter: [ZEROES; 32],
        float_result: BitVector::new(0),
        float_fflags: BitVector::new(0),
        f0: BitVector::new(0),
        f1: BitVector::new(0),
        f2: BitVector::new(0),
        f3: BitVector::new(0),
        f4: BitVector::new(0),
        f5: BitVector::new(0),
        f6: BitVector::new(0),
        f7: BitVector::new(0),
        f8: BitVector::new(0),
        f9: BitVector::new(0),
        f10: BitVector::new(0),
        f11: BitVector::new(0),
        f12: BitVector::new(0),
        f13: BitVector::new(0),
        f14: BitVector::new(0),
        f15: BitVector::new(0),
        f16: BitVector::new(0),
        f17: BitVector::new(0),
        f18: BitVector::new(0),
        f19: BitVector::new(0),
        f20: BitVector::new(0),
        f21: BitVector::new(0),
        f22: BitVector::new(0),
        f23: BitVector::new(0),
        f24: BitVector::new(0),
        f25: BitVector::new(0),
        f26: BitVector::new(0),
        f27: BitVector::new(0),
        f28: BitVector::new(0),
        f29: BitVector::new(0),
        f30: BitVector::new(0),
        f31: BitVector::new(0),
        fcsr: raw::Fcsr {
            bits: BitVector::new(0),
        },
        mcyclecfg: raw::CountSmcntrpmf {
            bits: BitVector::new(0),
        },
        minstretcfg: raw::CountSmcntrpmf {
            bits: BitVector::new(0),
        },
        mtimecmp: BitVector::new(0),
        stimecmp: BitVector::new(0),
        htif_tohost: BitVector::new(0),
        htif_done: false,
        htif_exit_code: BitVector::new(0),
        htif_cmd_write: false,
        htif_payload_writes: BitVector::new(0),
        tlb: [DEFAULT_TLB_ENTRY; raw::num_tlb_entries as usize],
        satp: BitVector::new(0),
        hart_state: raw::HartState::HART_ACTIVE(()),
        config,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::raw::*;
    use crate::registers::*;

    #[test]
    fn pmp_check() {
        let mut core = new_core(config::U74);
        let addr = 0x8000_0000;
        let access = AccessType::Read(());

        // Check the default access rights
        assert!(
            core.pmp_check(addr, access).is_none(),
            "M-mode can access all memory by default"
        );

        core.set_mode(Privilege::User);
        assert_eq!(
            core.pmp_check(addr, access),
            Some(ExceptionType::E_Load_Access_Fault(())),
            "U-mode has no access by default"
        );

        // Now let's add a PMP entry to allow reads from U-mode
        let pmp_addr = addr >> 2; // There is a shift of 2 in the pmpaddr registers
        core.set_pmpaddr(0, pmp_addr);
        core.set_pmpaddr(1, 2 * pmp_addr);
        core.set_pmpcfg(0, 0b0000_1001 << 8); // Entry 1, Read-only access, ToR matching mode
        assert!(
            core.pmp_check(addr, access).is_none(),
            "PMP allow read access"
        );
    }

    #[test]
    fn decoder() {
        let mut ctx = new_core(config::U74);
        let uimm0 = BitVector::new(0);

        // Load/Store

        assert_eq!(
            ctx.decode_instr(0xff87b703),
            ast::LOAD((
                BitVector::new(0xFFF - 7), // immediate is -8
                X15,
                X14,
                false,
                word_width::DOUBLE,
                false,
                false
            ))
        );

        // CSR instructions

        // csrrw x0, mstatus, x0
        assert_eq!(
            ctx.decode_instr(0x30001073),
            ast::CSRReg((BitVector::new(0x300), X0, X0, csrop::CSRRW))
        );
        // csrrs x0, mstatus, x0
        assert_eq!(
            ctx.decode_instr(0x30002073),
            ast::CSRReg((BitVector::new(0x300), X0, X0, csrop::CSRRS))
        );
        // csrrc x0, mstatus, x0
        assert_eq!(
            ctx.decode_instr(0x30003073),
            ast::CSRReg((BitVector::new(0x300), X0, X0, csrop::CSRRC))
        );
        // csrrwi x0, mstatus, 0
        assert_eq!(
            ctx.decode_instr(0x30005073),
            ast::CSRImm((BitVector::new(0x300), uimm0, X0, csrop::CSRRW))
        );
        // csrrsi x0, mstatus, 0
        assert_eq!(
            ctx.decode_instr(0x30006073),
            ast::CSRImm((BitVector::new(0x300), uimm0, X0, csrop::CSRRS))
        );
        // csrrci x0, mstatus, 0
        assert_eq!(
            ctx.decode_instr(0x30007073),
            ast::CSRImm((BitVector::new(0x300), uimm0, X0, csrop::CSRRC))
        );

        // Illegal
        assert_eq!(
            ctx.decode_instr(0x30001072),
            ast::ILLEGAL(BitVector::new(0x30001072))
        );
    }

    #[test]
    fn general_purpose_registers() {
        let mut ctx = new_core(config::U74);

        // Test X0 (ZERO) - should always be hardwired to 0
        assert_eq!(ctx.get(X0), 0, "X0 should be hardwired to 0");
        assert_eq!(ctx.get(ZERO), 0, "ZERO should be hardwired to 0");

        // Try to write to X0 - should remain 0
        ctx.set(X0, 0xDEADBEEF);
        assert_eq!(ctx.get(X0), 0, "X0 should remain 0 after write attempt");

        // Test some other registers using ABI names
        ctx.set(RA, 0x12345678);
        assert_eq!(ctx.get(RA), 0x12345678, "RA register should store value");
        assert_eq!(ctx.get(X1), 0x12345678, "X1 and RA should be the same");

        ctx.set(SP, 0x87654321);
        assert_eq!(ctx.get(SP), 0x87654321, "SP register should store value");
        assert_eq!(ctx.get(X2), 0x87654321, "X2 and SP should be the same");

        // Test function argument registers
        ctx.set(A0, 0xAAAAAAAA);
        ctx.set(A1, 0xBBBBBBBB);
        assert_eq!(ctx.get(A0), 0xAAAAAAAA, "A0 register should store value");
        assert_eq!(ctx.get(A1), 0xBBBBBBBB, "A1 register should store value");
        assert_eq!(ctx.get(X10), 0xAAAAAAAA, "X10 and A0 should be the same");
        assert_eq!(ctx.get(X11), 0xBBBBBBBB, "X11 and A1 should be the same");

        // Test saved registers
        ctx.set(S0, 0xCCCCCCCC);
        ctx.set(S1, 0xDDDDDDDD);
        assert_eq!(ctx.get(S0), 0xCCCCCCCC, "S0 register should store value");
        assert_eq!(ctx.get(S1), 0xDDDDDDDD, "S1 register should store value");
        assert_eq!(ctx.get(FP), 0xCCCCCCCC, "FP and S0 should be the same");
        assert_eq!(ctx.get(X8), 0xCCCCCCCC, "X8 and S0 should be the same");
        assert_eq!(ctx.get(X9), 0xDDDDDDDD, "X9 and S1 should be the same");

        // Test temporary registers
        ctx.set(T0, 0xEEEEEEEE);
        ctx.set(T6, 0xFFFFFFFF);
        assert_eq!(ctx.get(T0), 0xEEEEEEEE, "T0 register should store value");
        assert_eq!(ctx.get(T6), 0xFFFFFFFF, "T6 register should store value");
        assert_eq!(ctx.get(X5), 0xEEEEEEEE, "X5 and T0 should be the same");
        assert_eq!(ctx.get(X31), 0xFFFFFFFF, "X31 and T6 should be the same");

        // Verify X0 is still 0 after all the other operations
        assert_eq!(
            ctx.get(X0),
            0,
            "X0 should still be 0 after other register operations"
        );
    }

    #[test]
    fn csr_defined() {
        let mut core = new_core(config::U74);

        // Test standard machine-level CSRs that should exist
        assert!(core.is_csr_defined(0x300), "mstatus should be defined");
        assert!(core.is_csr_defined(0x301), "misa should be defined");
        assert!(core.is_csr_defined(0x304), "mie should be defined");
        assert!(core.is_csr_defined(0x305), "mtvec should be defined");
        assert!(core.is_csr_defined(0x341), "mepc should be defined");
        assert!(core.is_csr_defined(0x342), "mcause should be defined");
        assert!(core.is_csr_defined(0x343), "mtval should be defined");
        assert!(core.is_csr_defined(0x344), "mip should be defined");

        // Test PMP configuration registers
        // U74 core has 16 PMP entries, so pmpcfg0, pmpcfg2, pmpcfg4, pmpcfg6, etc. (even ones) should exist
        assert!(core.is_csr_defined(0x3A0), "pmpcfg0 should be defined");
        assert!(core.is_csr_defined(0x3A2), "pmpcfg2 should be defined");
        assert!(core.is_csr_defined(0x3A4), "pmpcfg4 should be defined");
        assert!(core.is_csr_defined(0x3A6), "pmpcfg6 should be defined");

        // Test that odd pmpcfg registers don't exist (RV64 uses even pmpcfg registers only)
        assert!(
            !core.is_csr_defined(0x3A1),
            "pmpcfg1 should not be defined on RV64"
        );
        assert!(
            !core.is_csr_defined(0x3A3),
            "pmpcfg3 should not be defined on RV64"
        );
        assert!(
            !core.is_csr_defined(0x3A5),
            "pmpcfg5 should not be defined on RV64"
        );

        // Test PMP address registers
        // U74 core has 16 PMP entries, so pmpaddr0-pmpaddr15 should exist
        assert!(core.is_csr_defined(0x3B0), "pmpaddr0 should be defined");
        assert!(core.is_csr_defined(0x3B5), "pmpaddr5 should be defined");
        assert!(core.is_csr_defined(0x3BF), "pmpaddr15 should be defined");

        // Test that PMP address registers beyond 16 don't exist on U74
        assert!(
            !core.is_csr_defined(0x3C0),
            "pmpaddr16 should not be defined on U74"
        );
        assert!(
            !core.is_csr_defined(0x3C8),
            "pmpaddr24 should not be defined on U74"
        );
        assert!(
            !core.is_csr_defined(0x3CF),
            "pmpaddr31 should not be defined on U74"
        );

        // Test some CSRs that definitely shouldn't exist
        assert!(
            !core.is_csr_defined(0x000),
            "CSR 0x000 should not be defined"
        );
        assert!(
            !core.is_csr_defined(0xFFF),
            "CSR 0xFFF should not be defined"
        );
        assert!(
            !core.is_csr_defined(0x200),
            "CSR 0x200 should not be defined"
        );
    }

    #[test]
    fn inject_exception() {
        let mut core = new_core(config::U74);

        // Set initial state
        core.set_mode(Privilege::User);
        core.PC = BitVector::new(0x1000);
        let initial_pc = core.PC.bits();

        assert_eq!(core.mode(), Privilege::User, "Initial mode should be User");

        // Inject a load access fault exception
        let fault_addr = 0x8000_0000;
        core.inject_exception(ExceptionType::E_Load_Access_Fault(()), fault_addr);

        // After exception, should be in Machine mode
        assert_eq!(
            core.mode(),
            Privilege::Machine,
            "Mode should be Machine after exception"
        );

        // Check that mepc was set to the PC at the time of the exception
        assert_eq!(
            core.mepc.bits(),
            initial_pc,
            "mepc should contain the PC when exception occurred"
        );

        // Check that mtval contains the fault address
        assert_eq!(
            core.mtval.bits(),
            fault_addr,
            "mtval should contain the fault address"
        );
    }
}
