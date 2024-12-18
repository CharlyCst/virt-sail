path = 'src/lib.rs'

file_content = ""

# Open the file in read mode
with open(path, 'r') as file:
    # Read the entire content of the file into a single string
    file_content = file.read()


def replace(old, new):
    global file_content
    file_content = file_content.replace(old, new)


# Fixes for CSR operations
replace("#[derive(Eq, PartialEq, Clone, Copy, Debug)]\npub enum exception {",
                                     "#[derive(Eq, PartialEq, Clone, Debug)]\npub enum exception {")

replace("rvfi_wX<const N: usize>", "rvfi_wX")
replace("rX<const N: usize>", "rX")
replace("wX<const N: usize>", "wX")
replace("(idx && (v__22.subrange::<4, 12, 8>()","((v__22.subrange::<4, 12, 8>()")
replace("(idx && (v__12.subrange::<4, 12, 8>()", "((v__12.subrange::<4, 12, 8>()")
replace("bitvector_concat(_get_Mtvec_Base(sail_ctx, m)", "bitvector_concat::<62,2>(_get_Mtvec_Base(sail_ctx, m)")
replace(" ones(", " ones::<64>(")
replace("subrange_bits(v, ((8 * i) + 7)", "subrange_bits_8(v, ((8 * i) + 7)")
replace("zero_extend_16(subrange_bits(value, (vstart_length - 1), 0));", "zero_extend_16(subrange_bits_8(value, (vstart_length - 1), 0));")


replace("bitvector_concat(sail_ctx.pmpcfg_n[((n * 4) + 7)].bits, bitvector_concat(sail_ctx.pmpcfg_n[((n * 4) + 6)].bits, bitvector_concat(sail_ctx.pmpcfg_n[((n * 4) + 5)].bits, bitvector_concat(sail_ctx.pmpcfg_n[((n * 4) + 4)].bits, bitvector_concat(sail_ctx.pmpcfg_n[((n * 4) + 3)].bits, bitvector_concat(sail_ctx.pmpcfg_n[((n * 4) + 2)].bits, bitvector_concat(sail_ctx.pmpcfg_n[((n * 4) + 1)].bits, sail_ctx.pmpcfg_n[((n * 4) + 0)].bits)))))))",
        "        bitvector_concat::<8, 56>(\
            sail_ctx.pmpcfg_n[((n * 4) + 7)].bits,\
            bitvector_concat::<8, 48>(\
                sail_ctx.pmpcfg_n[((n * 4) + 6)].bits,\
                bitvector_concat::<8, 40>(\
                    sail_ctx.pmpcfg_n[((n * 4) + 5)].bits,\
                    bitvector_concat::<8, 32>(\
                        sail_ctx.pmpcfg_n[((n * 4) + 4)].bits,\
                        bitvector_concat::<8, 24>(\
                            sail_ctx.pmpcfg_n[((n * 4) + 3)].bits,\
                            bitvector_concat::<8, 16>(\
                                sail_ctx.pmpcfg_n[((n * 4) + 2)].bits,\
                                bitvector_concat::<8, 8>(\
                                    sail_ctx.pmpcfg_n[((n * 4) + 1)].bits,\
                                    sail_ctx.pmpcfg_n[((n * 4) + 0)].bits,\
                                ),\
                            ),\
                        ),\
                    ),\
                ),\
            ),\
        )")

# Fixes for pmpCheck
replace("AccessType<()>", "AccessType")
replace("pmpCheckRWX<const N: usize>", "pmpCheckRWX")
replace("pmpCheckPerms<const N: usize>", "pmpCheckPerms")
replace("pmpMatchEntry<const N: usize>", "pmpMatchEntry")
replace("accessToFault<const N: usize>", "accessToFault")
replace("to_bits(sail_ctx, 64, width);", "BitVector::new(64);")

with open(path, 'w') as file:
    file.write(file_content)


print("String saved to 'output_file.txt'.")