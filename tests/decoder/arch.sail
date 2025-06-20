default Order dec
$include <prelude.sail>

// ———————————————————————————————— Predule ————————————————————————————————— //

val EXTZ : forall 'n 'm, 'm >= 'n. (implicit('m), bits('n)) -> bits('m)
function EXTZ(m, v) = sail_zero_extend(v, m)

val EXTS : forall 'n 'm, 'm >= 'n. (implicit('m), bits('n)) -> bits('m)
function EXTS(m, v) = sail_sign_extend(v, m)

val not : forall ('p : Bool). bool('p) -> bool(not('p))
function not(p) = not_bool(p)

val eq_anything : forall ('a : Type). ('a, 'a) -> bool
overload operator == = {eq_anything}

overload ~ = {not_bool, not_vec}

infix 4 >=_u

val operator >=_u : forall 'n. (bits('n), bits('n)) -> bool
function operator >=_u (x, y) = unsigned(x) >= unsigned(y)

// —————————————————————————————— Architecture —————————————————————————————— //

// Arch size
type xlen : Int = 64
type xlen_bytes : Int = 8
type xlenbits = bits(xlen)

// Privilege levels
type priv_level = bits(2)
enum Privilege  = {User, Supervisor, Machine}

val privLevel_to_bits : Privilege -> priv_level
function privLevel_to_bits (p) =
  match (p) {
    User       => 0b00,
    Supervisor => 0b01,
    Machine    => 0b11
  }

// ——————————————————————————————— Registers ———————————————————————————————— //

// register identifiers
type regidx  = bits(5)
type cregidx = bits(3)    /* identifiers in RVC instructions */
type csreg   = bits(12)   /* CSR addressing */

mapping bool_bits : bool <-> bits(1) = {
  true   <-> 0b1,
  false  <-> 0b0
}

// —————————————————————————————— Instructions —————————————————————————————— //

enum iop = {RISCV_ADDI, RISCV_SLTI, RISCV_SLTIU, RISCV_XORI, RISCV_ORI, RISCV_ANDI}
enum csrop = {CSRRW, CSRRS, CSRRC}
enum Retired = {RETIRE_SUCCESS, RETIRE_FAIL}

scattered union ast
val encdec : ast <-> bits(32)
scattered mapping encdec

val execute : ast -> Retired

// —————————————————————————————————— CSR registers ——————————————————————————————————— //

union clause ast = CSR  : (bits(12), regidx, regidx, bool, csrop)

mapping encdec_csrop : csrop <-> bits(2) = {
  CSRRW <-> 0b01,
  CSRRS <-> 0b10,
  CSRRC <-> 0b11
}

type csrRW = bits(2)
function csrAccess(csr : csreg) -> csrRW = csr[11..10]
function csrPriv(csr : csreg) -> priv_level = csr[9..8]

mapping clause encdec = CSR(csr, rs1, rd, is_imm, op)
  <-> csr @ rs1 @ bool_bits(is_imm) @ encdec_csrop(op) @ rd @ 0b1110011

// —————————————————————————————————— MRET —————————————————————————————————— //

union clause ast = MRET : unit

mapping clause encdec = MRET()
  <-> 0b0011000 @ 0b00010 @ 0b00000 @ 0b000 @ 0b00000 @ 0b1110011

// —————————————————————————————————— SRET —————————————————————————————————— //

union clause ast = SRET : unit

mapping clause encdec = SRET() 
  <-> 0b0001000 @ 0b00010 @ 0b00000 @ 0b000 @ 0b00000 @ 0b1110011

// —————————————————————————————————— WFI —————————————————————————————————— //

union clause ast = WFI : unit

mapping clause encdec = WFI()
  <-> 0b000100000101 @ 0b00000 @ 0b000 @ 0b00000 @ 0b1110011

// —————————————————————————————————— Sfence vma —————————————————————————————————— //

union clause ast = SFENCE_VMA : (bits(5), bits(5))

mapping clause encdec = SFENCE_VMA(rs1, rs2)
  <-> 0b0001001 @ rs2 @ rs1 @ 0b000 @ 0b00000 @ 0b1110011

// —————————————————————————————————— Hfence vvma —————————————————————————————————— //

union clause ast = HFENCE_VVMA : (bits(5), bits(5))

mapping clause encdec = HFENCE_VVMA(rs1, rs2)
  <-> 0b0010001 @ rs2 @ rs1 @ 0b000 @ 0b00000 @ 0b1110011

// —————————————————————————————————— Hfence gvma —————————————————————————————————— //

union clause ast = HFENCE_GVMA : (bits(5), bits(5))

mapping clause encdec = HFENCE_GVMA(rs1, rs2)
  <-> 0b0110001 @ rs2 @ rs1 @ 0b000 @ 0b00000 @ 0b1110011
