/* RISC-V simulator.

   Copyright (C) 2005-2014 Free Software Foundation, Inc.
   Contributed by Mike Frysinger.

   This file is part of simulators.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* This file contains the main simulator decoding logic.  i.e. everything that
   is architecture specific.  */

#include "config.h"

#include <strings.h>
#include <limits.h>
#include <inttypes.h>
#include <time.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/time.h>

#include "sim-main.h"
#include "sim-fpu.h"
#include "sim-syscall.h"
#include "sim-bits.h"

#include "elf/riscv.h"
#include "elfxx-riscv.h"

#include "opcode/riscv.h"

#include "gdb/sim-riscv.h"

#include "targ-vals.h"


#define TRACE_REG(cpu, reg) TRACE_REGISTER (cpu, "wrote %s = %#" PRIxTW, riscv_gpr_names_abi[reg], cpu->regs[reg])

static const struct riscv_opcode *riscv_hash[OP_MASK_OP + 1];
#define OP_HASH_IDX(i) ((i) & (riscv_insn_length (i) == 2 ? 0x3 : 0x7f))

#define RISCV_ASSERT_RV32(cpu, fmt, args...) \
  do { \
    if (RISCV_XLEN (cpu) != 32) \
      { \
	SIM_DESC sd = CPU_STATE (cpu); \
	TRACE_INSN (cpu, "RV32I-only " fmt, ## args); \
	sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL); \
      } \
  } while (0)

#define RISCV_ASSERT_RV64(cpu, fmt, args...) \
  do { \
    if (RISCV_XLEN (cpu) != 64) \
      { \
	SIM_DESC sd = CPU_STATE (cpu); \
	TRACE_INSN (cpu, "RV64I-only " fmt, ## args); \
	sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL); \
      } \
  } while (0)

#ifndef DEFAULT_RISCV_ATTR
#define DEFAULT_RISCV_ATTR 0
#endif

static unsigned gxlen = 0; /* width of an x-register */

struct riscv_set_options
{
  int pic; /* Generate position-independent code.  */
  int rvc; /* Generate RVC code.  */
  int rve; /* Generate RVE code.  */
  int relax; /* Emit relocs the linker is allowed to relax.  */
  int arch_attr; /* Emit arch attribute.  */
};

static struct riscv_set_options riscv_opts =
{
  0,	/* pic */
  0,	/* rvc */
  0,	/* rve */
  1,	/* relax */
  DEFAULT_RISCV_ATTR, /* arch_attr */
};

static riscv_subset_list_t riscv_subsets;

static bfd_boolean
riscv_subset_supports (const char *feature)
{
  if (riscv_opts.rvc && (strcasecmp (feature, "c") == 0))
    return TRUE;

  return riscv_lookup_subset (&riscv_subsets, feature) != NULL;
}

/* Table of known PULP extension groups.y */

static struct pulp_ext_group_info pulp_ext_groups[] =
{
#define PULP_EXT_GROUP(NAME, MAJOR, MINOR, FLAGS0, FLAGS1)	\
  {NAME, MAJOR, MINOR, FLAGS0, FLAGS1},

  /* from opcodes/riscv.h */
  PULP_ALL_EXT_GROUPS
  {NULL, 0, 0, 0, 0}

#undef PULP_EXT_GROUP
};

/* Determine whether an insn_class is supported by currently enabled PULP
   extension groups. */

static bfd_boolean
riscv_pulp_ext_group_supports (enum riscv_insn_class insn_class)
{
  struct pulp_ext_group_info *group;

  for (group = pulp_ext_groups; group->name; group++)
    {
      if (riscv_lookup_subset_version (&riscv_subsets, group->name,
				       group->major, group->minor) != NULL)
	{
	  if (insn_class < 64)
	    return group->ext0_flags >> insn_class & 1;
	  else if (insn_class < 128)
	    return group->ext1_flags >> insn_class & 1;
	  else
	    {
	      fprintf (stderr, "internal error: too many custom pulp extensions");
	      abort ();
	    }
	}
    }

  return FALSE;
}

static bfd_boolean
riscv_multi_subset_supports (enum riscv_insn_class insn_class)
{
  switch (insn_class)
    {
    case INSN_CLASS_I: return riscv_subset_supports ("i");
    case INSN_CLASS_C: return riscv_subset_supports ("c");
    case INSN_CLASS_A: return riscv_subset_supports ("a");
    case INSN_CLASS_M: return riscv_subset_supports ("m");
    case INSN_CLASS_F: return riscv_subset_supports ("f");
    case INSN_CLASS_D: return riscv_subset_supports ("d");
    case INSN_CLASS_ZFINX: return riscv_subset_supports ("zfinx");
    case INSN_CLASS_ZDINX: return riscv_subset_supports ("zdinx");
    case INSN_CLASS_D_AND_C:
      return riscv_subset_supports ("d") && riscv_subset_supports ("c");

    case INSN_CLASS_F_AND_C:
      return riscv_subset_supports ("f") && riscv_subset_supports ("c");

    case INSN_CLASS_ZDINX_AND_C:
      return riscv_subset_supports ("zdinx") && riscv_subset_supports ("c");

    case INSN_CLASS_ZFINX_AND_C:
      return riscv_subset_supports ("zfinx") && riscv_subset_supports ("c");

    case INSN_CLASS_Q: return riscv_subset_supports ("q");

    /* pulpv0 and pulpv1 compatibility */
#define INSN_CLASS(NAME, ARCH)						\
    case INSN_CLASS_XPULP_##NAME:					\
      return riscv_lookup_subset_version (&riscv_subsets, ARCH, 0, 0) != NULL \
	|| riscv_pulp_ext_group_supports (insn_class);

      /* from opcodes/riscv.h */
      PULP_EXTENSION_COMPAT_MAP

#undef INSN_CLASS

    /* pulpv2 onwards */
#define INSN_CLASS(NAME, ARCH)						\
    case INSN_CLASS_XPULP_##NAME:					\
      return riscv_lookup_subset_version (&riscv_subsets, ARCH, 2, 0) != NULL \
	|| riscv_pulp_ext_group_supports (insn_class);

    /* from opcode/riscv.h */
    PULP_EXTENSION_MAP

#undef INSN_CLASS

    default:
      fprintf(stderr, "Unreachable");
      abort ();
      return FALSE;
    }
}

/* Print error and exit */
static void
riscv_sim_error_handler (const char *fmt, ...)
{
  va_list ap;

  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);
  va_end (ap);
  exit(1);
}

/* Set which ISA and extensions are available.  */

static void
riscv_set_arch (const char *s)
{
  riscv_parse_subset_t rps;
  rps.subset_list = &riscv_subsets;
  rps.error_handler = riscv_sim_error_handler;
  rps.xlen = &gxlen;

  riscv_release_subset_list (&riscv_subsets);
  riscv_parse_subset (&rps, s);
}

static INLINE void
store_rd (SIM_CPU *cpu, int rd, unsigned_word val)
{
  if (rd)
    {
      cpu->regs[rd] = val;
      TRACE_REG (cpu, rd);
    }
}

static INLINE void
store_rd_dinx (SIM_CPU *cpu, int rd, unsigned64 val)
{
  if (rd && RISCV_XLEN (cpu) == 64)
    {
      cpu->regs[rd] = val;
      TRACE_REG (cpu, rd);
    }
  else if (rd && RISCV_XLEN (cpu) == 32)
    {
#if 0 /* TODO: zdinx probably will only allow even register pairs (odd
	 reserved), but gcc currently doesn't have this restriction during code
	 generation */
      if ((rd % 2) != 0)
	{
	  SIM_DESC sd = CPU_STATE (cpu);
	  TRACE_INSN (cpu, "STORE INTO ODD REGISTER PAIR (RV32)");
	  sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL);
	}
#endif
      cpu->regs[rd] = val & 0xffffffff;
      cpu->regs[rd + 1] = (val >> 32) & 0xffffffff;
      TRACE_REG (cpu, rd);
      TRACE_REG (cpu, rd + 1);
    }
}

static INLINE void
store_frd (SIM_CPU *cpu, int rd, unsigned_word val)
{
  cpu->fpregs[rd].w[0] = val;
}

static inline void
store_frd64 (SIM_CPU *cpu, int rd, uint64_t val)
{
  cpu->fpregs[rd].v[0] = val;
}

static INLINE unsigned_word
fetch_csr (SIM_CPU *cpu, const char *name, int csr, unsigned_word *reg)
{
  /* Handle pseudo registers.  */
  switch (csr)
    {
    /* Allow certain registers only in respective modes.  */
    case CSR_CYCLEH:
    case CSR_INSTRETH:
    case CSR_TIMEH:
      RISCV_ASSERT_RV32 (cpu, "CSR: %s", name);
      break;
    }

  return *reg;
}

static INLINE void
store_csr (SIM_CPU *cpu, const char *name, int csr, unsigned_word *reg,
	   unsigned_word val)
{
  switch (csr)
    {
    /* These are pseudo registers that modify sub-fields of fcsr.  */
    case CSR_FRM:
      val &= 0x7;
      *reg = val;
      cpu->csr.fcsr = (cpu->csr.fcsr & ~0xe0) | (val << 5);
      break;
    case CSR_FFLAGS:
      val &= 0x1f;
      *reg = val;
      cpu->csr.fcsr = (cpu->csr.fcsr & ~0x1f) | val;
      break;
    /* Keep the sub-fields in sync.  */
    case CSR_FCSR:
      *reg = val;
      cpu->csr.frm = (val >> 5) & 0x7;
      cpu->csr.fflags = val & 0x1f;
      break;

    /* Allow certain registers only in respective modes.  */
    case CSR_CYCLEH:
    case CSR_INSTRETH:
    case CSR_TIMEH:
      RISCV_ASSERT_RV32 (cpu, "CSR: %s", name);

      /* corev */
    case CSR_COREV_LPSTART0:
      cpu->csr.lpstart0 = val;
      break;
    case CSR_COREV_LPEND0:
      cpu->csr.lpend0 = val;
      break;
    case CSR_COREV_LPCOUNT0:
      cpu->csr.lpcount0 = val;
      break;
    case CSR_COREV_LPSTART1:
      cpu->csr.lpstart1 = val;
      break;
    case CSR_COREV_LPEND1:
      cpu->csr.lpend1 = val;
      break;
    case CSR_COREV_LPCOUNT1:
      cpu->csr.lpcount1 = val;
      break;
      /* pulpv 3 */
    case CSR_PULPV3_LPSTART0:
      cpu->csr.lpstart0 = val;
      break;
    case CSR_PULPV3_LPEND0:
      cpu->csr.lpend0 = val;
      break;
    case CSR_PULPV3_LPCOUNT0:
      cpu->csr.lpcount0 = val;
      break;
    case CSR_PULPV3_LPSTART1:
      cpu->csr.lpstart1 = val;
      break;
    case CSR_PULPV3_LPEND1:
      cpu->csr.lpend1 = val;
      break;
    case CSR_PULPV3_LPCOUNT1:
      cpu->csr.lpcount1 = val;
      break;

    /* All the rest are immutable.  */
    default:
      val = *reg;
      break;
    }

  TRACE_REGISTER (cpu, "wrote CSR %s = %#" PRIxTW, name, val);
}

static inline unsigned_word
ashiftrt (unsigned_word val, unsigned_word shift)
{
  unsigned32 sign = (val & 0x80000000) ? ~(0xfffffffful >> shift) : 0;
  return (val >> shift) | sign;
}

static inline unsigned_word
ashiftrt64 (unsigned_word val, unsigned_word shift)
{
  unsigned64 sign = (val & 0x8000000000000000ull) ? ~(0xffffffffffffffffull >> shift) : 0;
  return (val >> shift) | sign;
}

static sim_cia
execute_d (SIM_CPU *cpu, unsigned_word iw, const struct riscv_opcode *op)
{
  SIM_DESC sd = CPU_STATE (cpu);
  unsigned int mask_arithmetic = MASK_FADD_D;
  unsigned int mask_mul_add = MASK_FMADD_S;
  unsigned int mask_convert = MASK_FCVT_S_W;

  static const int round_modes[] =
  {
      sim_fpu_round_near, sim_fpu_round_zero,
      sim_fpu_round_down, sim_fpu_round_up,
      sim_fpu_round_default, sim_fpu_round_default,
      sim_fpu_round_default
  };

  int rd = (iw >> OP_SH_RD) & OP_MASK_RD;
  int rs1 = (iw >> OP_SH_RS1) & OP_MASK_RS1;
  int rs2 = (iw >> OP_SH_RS2) & OP_MASK_RS2;
  int rs3 = (iw >> OP_SH_RS3) & OP_MASK_RS3;
  const char *frd_name = riscv_fpr_names_abi[rd];
  const char *frs1_name = riscv_fpr_names_abi[rs1];
  const char *frs2_name = riscv_fpr_names_abi[rs2];
  const char *frs3_name = riscv_fpr_names_abi[rs3];
  const char *rd_name = riscv_gpr_names_abi[rd];
  const char *rs1_name = riscv_gpr_names_abi[rs1];
  unsigned_word i_imm = EXTRACT_ITYPE_IMM (iw);
  unsigned_word s_imm = EXTRACT_STYPE_IMM (iw);
  uint32_t u32;
  int32_t i32;
  uint64_t u64;
  int64_t i64;
  sim_cia pc = cpu->pc + 4;

  /* Rounding mode.  */
  int rm = (iw >> OP_SH_RM) & OP_MASK_RM;
  int rounding = round_modes[rm];

  sim_fpu sft, sft2;
  sim_fpu sfa, sfb, sfc;
  sim_fpu_64to (&sfa, cpu->fpregs[rs1].v[0]);
  sim_fpu_64to (&sfb, cpu->fpregs[rs2].v[0]);

  switch (op->match & mask_mul_add)
    {
    case MATCH_FMADD_D:
      TRACE_INSN (cpu, "fmadd.d %s, %s, %s, %s",
		  frd_name, frs1_name, frs2_name, frs3_name);
      sim_fpu_64to (&sfc, cpu->fpregs[rs3].v[0]);
      sim_fpu_mul (&sft2, &sfa, &sfb);
      sim_fpu_add (&sft, &sfc, &sft2);
      sim_fpu_round_64 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to64 (&cpu->fpregs[rd].v[0], &sft);
      goto done;
    case MATCH_FMSUB_D:
      TRACE_INSN (cpu, "fmsub.d %s, %s, %s, %s",
		  frd_name, frs1_name, frs2_name, frs3_name);
      sim_fpu_64to (&sfc, cpu->fpregs[rs3].v[0]);
      sim_fpu_mul (&sft2, &sfa, &sfb);
      sim_fpu_sub (&sft, &sft2, &sfc);
      sim_fpu_round_64 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to64 (&cpu->fpregs[rd].v[0], &sft);
      goto done;
    case MATCH_FNMADD_D:
      TRACE_INSN (cpu, "fnmadd.d %s, %s, %s, %s",
		  frd_name, frs1_name, frs2_name, frs3_name);
      sim_fpu_64to (&sfc, cpu->fpregs[rs3].v[0]);
      sim_fpu_mul (&sft2, &sfa, &sfb);
      sim_fpu_neg (&sft2, &sft2);
      sim_fpu_sub (&sft, &sft2, &sfc);
      sim_fpu_round_64 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to64 (&cpu->fpregs[rd].v[0], &sft);
      goto done;
    case MATCH_FNMSUB_D:
      TRACE_INSN (cpu, "fnmsub.d %s, %s, %s, %s",
		  frd_name, frs1_name, frs2_name, frs3_name);
      sim_fpu_64to (&sfc, cpu->fpregs[rs3].v[0]);
      sim_fpu_mul (&sft2, &sfa, &sfb);
      sim_fpu_neg (&sft2, &sft2);
      sim_fpu_add (&sft, &sft2, &sfc);
      sim_fpu_round_64 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to64 (&cpu->fpregs[rd].v[0], &sft);
      goto done;
    }

  switch (op->match & mask_arithmetic)
    {
    case MATCH_FADD_D:
      TRACE_INSN (cpu, "fadd.d %s, %s, %s",
		  frd_name, frs1_name, frs2_name);
      sim_fpu_add (&sft, &sfa, &sfb);
      sim_fpu_round_64 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to64 (&cpu->fpregs[rd].v[0], &sft);
      goto done;
    case MATCH_FSUB_D:
      TRACE_INSN (cpu, "fsub.d %s, %s, %s",
		  frd_name, frs1_name, frs2_name);
      sim_fpu_sub (&sft, &sfa, &sfb);
      sim_fpu_round_64 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to64 (&cpu->fpregs[rd].v[0], &sft);
      goto done;
    case MATCH_FMUL_D:
      TRACE_INSN (cpu, "fmul.d %s, %s, %s",
		  frd_name, frs1_name, frs2_name);
      sim_fpu_mul (&sft, &sfa, &sfb);
      sim_fpu_round_64 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to64 (&cpu->fpregs[rd].v[0], &sft);
      goto done;
    case MATCH_FDIV_D:
      TRACE_INSN (cpu, "fdiv.d %s, %s, %s",
		  frd_name, frs1_name, frs2_name);
      sim_fpu_div (&sft, &sfa, &sfb);
      sim_fpu_round_64 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to64 (&cpu->fpregs[rd].v[0], &sft);
      goto done;
    case MATCH_FSQRT_D:
      TRACE_INSN (cpu, "fsqrt.d %s, %s",
		  frd_name, frs1_name);
      sim_fpu_sqrt (&sft, &sfa);
      sim_fpu_round_64 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to64 (&cpu->fpregs[rd].v[0], &sft);
      goto done;
    }

  switch (op->match & mask_convert)
    {
    case MATCH_FCVT_W_D:
      TRACE_INSN (cpu, "fcvt.w.d %s, %s",
		  rd_name, frs1_name);
      sim_fpu_to32i (&i32, &sfa, rounding);
      cpu->regs[rd] = i32;
      goto done;
    case MATCH_FCVT_WU_D:
      TRACE_INSN (cpu, "fcvt.wu.d %s, %s",
		  rd_name, frs1_name);
      sim_fpu_to32u (&u32, &sfa, rounding);
      i32 = u32;
      cpu->regs[rd] = i32;
      goto done;
    case MATCH_FCVT_D_W:
      TRACE_INSN (cpu, "fcvt.d.w %s, %s",
		  frd_name, rs1_name);
      sim_fpu_i32to (&sft, cpu->regs[rs1], rounding);
      sim_fpu_to64 ((unsigned64 *) (cpu->fpregs + rd), &sft);
      goto done;
    case MATCH_FCVT_D_WU:
      TRACE_INSN (cpu, "fcvt.d.wu %s, %s",
		  frd_name, rs1_name);
      sim_fpu_u32to (&sft, cpu->regs[rs1], rounding);
      sim_fpu_to64 ((unsigned64 *) (cpu->fpregs + rd), &sft);
      goto done;
    case MATCH_FCVT_S_D:
      TRACE_INSN (cpu, "fcvt.s.d %s, %s",
		  frd_name, frs1_name);
      sft = sfa;
      sim_fpu_round_32 (&sft, sim_fpu_round_near, sim_fpu_denorm_default);
      sim_fpu_to32 ((unsigned32 *) (cpu -> fpregs + rd), &sft);
      goto done;
    case MATCH_FCVT_D_S:
      TRACE_INSN (cpu, "fcvt.d.s %s, %s",
		  frd_name, frs1_name);
      sim_fpu_32to (&sft, cpu->fpregs[rs1].w[0]);
      sim_fpu_to64 (&cpu->fpregs[rd].v[0], &sft);
      goto done;
    case MATCH_FCVT_L_D:
      TRACE_INSN (cpu, "fcvt.l.d %s, %s",
		  rd_name, frs1_name);
      cpu->regs[rd] = (int64_t) cpu->fpregs[rs1].D[0];
      goto done;
    case MATCH_FCVT_LU_D:
      TRACE_INSN (cpu, "fcvt.lu.d %s, %s",
		  rd_name, frs1_name);
      cpu->regs[rd] = (uint64_t) cpu->fpregs[rs1].D[0];
      goto done;
    case MATCH_FCVT_D_L:
      TRACE_INSN (cpu, "fcvt.d.l %s, %s",
		  frd_name, rs1_name);
      cpu->fpregs[rd].D[0] = (double) ((int64_t) cpu->regs[rs1]);
      goto done;
    case MATCH_FCVT_D_LU:
      TRACE_INSN (cpu, "fcvt.d.lu %s, %s",
		  frd_name, rs1_name);
      cpu->fpregs[rd].D[0] = (double) cpu->regs[rs1];
      goto done;
    }

  switch (op->match)
    {
    case MATCH_FLD:
      TRACE_INSN (cpu, "fld %s, %" PRIiTW "(%s)",
		  frd_name, i_imm, rs1_name);
      store_frd64 (cpu, rd,
	sim_core_read_unaligned_8 (cpu, cpu->pc, read_map,
				   cpu->regs[rs1] + i_imm));
      break;
    case MATCH_FSD:
      TRACE_INSN (cpu, "fsd %s, %" PRIiTW "(%s)",
		  frs2_name, s_imm, rs1_name);
      sim_core_write_unaligned_8 (cpu, cpu->pc, write_map,
				  cpu->regs[rs1] + s_imm,
				  cpu->fpregs[rs2].v[0]);
      break;
    case MATCH_FSGNJ_D:
      TRACE_INSN (cpu, "fsgnj.d %s, %s, %s",
		  frd_name, frs1_name, frs2_name);
      u32 = cpu->fpregs[rs1].w[1] & 0x7fffffff;
      u32 |= cpu->fpregs[rs2].w[1] & 0x80000000;
      cpu->fpregs[rd].w[1] = u32;
      cpu->fpregs[rd].w[0] = cpu->fpregs[rs1].w[0];
      break;
    case MATCH_FSGNJN_D:
      TRACE_INSN (cpu, "fsgnjn.d %s, %s, %s",
		  frd_name, frs1_name, frs2_name);
      u32 = cpu->fpregs[rs1].w[1] & 0x7fffffff;
      u32 |= (cpu->fpregs[rs2].w[1] & 0x80000000) ^ 0x80000000;
      cpu->fpregs[rd].w[1] = u32;
      cpu->fpregs[rd].w[0] = cpu->fpregs[rs1].w[0];
      break;
    case MATCH_FSGNJX_D:
      TRACE_INSN (cpu, "fsgnjx.d %s, %s, %s",
		  frd_name, frs1_name, frs2_name);
      u32 = cpu->fpregs[rs1].w[1] & 0x7fffffff;
      u32 |= (cpu->fpregs[rs1].w[1] & 0x80000000) ^ (cpu->fpregs[rs2].w[1] & 0x80000000);
      cpu->fpregs[rd].w[1] = u32;
      cpu->fpregs[rd].w[0] = cpu->fpregs[rs1].w[0];
      break;
    case MATCH_FMIN_D:
      TRACE_INSN (cpu, "fmin.d %s, %s, %s",
		  frd_name, frs1_name, frs2_name);
      if (cpu->fpregs[rs1].D[0] < cpu->fpregs[rs2].D[0])
	cpu->fpregs[rd].D[0] = cpu->fpregs[rs1].D[0];
      else
	cpu->fpregs[rd].D[0] = cpu->fpregs[rs2].D[0];
      break;
    case MATCH_FMAX_D:
      TRACE_INSN (cpu, "fmax.d %s, %s, %s",
		  frd_name, frs1_name, frs2_name);
      if (cpu->fpregs[rs1].D[0] > cpu->fpregs[rs2].D[0])
	cpu->fpregs[rd].D[0] = cpu->fpregs[rs1].D[0];
      else
	cpu->fpregs[rd].D[0] = cpu->fpregs[rs2].D[0];
      break;
    case MATCH_FMV_X_D:
      TRACE_INSN (cpu, "fmv.x.d %s, %s",
		  rd_name, frs1_name);
      cpu->regs[rd] = cpu->fpregs[rs1].v[0];
      break;
    case MATCH_FMV_D_X:
      TRACE_INSN (cpu, "fmv.d.x %s, %s",
		  frd_name, frs1_name);
      cpu->fpregs[rd].v[0] = cpu->regs[rs1];
      break;
    case MATCH_FEQ_D:
      TRACE_INSN (cpu, "feq.d %s, %s, %s",
		  rd_name, frs1_name, frs2_name);
      cpu->regs[rd] = sim_fpu_is_eq (&sfa, &sfb);
      break;
    case MATCH_FLE_D:
      TRACE_INSN (cpu, "fle.d %s, %s, %s",
		  rd_name, frs1_name, frs2_name);
      cpu->regs[rd] = sim_fpu_is_le (&sfa, &sfb);
      break;
    case MATCH_FLT_D:
      TRACE_INSN (cpu, "flt.d %s, %s, %s",
		  rd_name, frs1_name, frs2_name);
      cpu->regs[rd] = sim_fpu_is_lt (&sfa, &sfb);
      break;
    case MATCH_FCLASS_D:
      TRACE_INSN (cpu, "fclass.d %s, %s",
		  rd_name, frs1_name);
      switch (sim_fpu_is (&sfa))
	{
	case SIM_FPU_IS_NINF:
	  cpu->regs[rd] = 1;
	  break;
	case SIM_FPU_IS_NNUMBER:
	  cpu->regs[rd] = 1 << 1;
	  break;
	case SIM_FPU_IS_NDENORM:
	  cpu->regs[rd] = 1 << 2;
	  break;
	case SIM_FPU_IS_NZERO:
	  cpu->regs[rd] = 1 << 3;
	  break;
	case SIM_FPU_IS_PZERO:
	  cpu->regs[rd] = 1 << 4;
	  break;
	case SIM_FPU_IS_PDENORM:
	  cpu->regs[rd] = 1 << 5;
	  break;
	case SIM_FPU_IS_PNUMBER:
	  cpu->regs[rd] = 1 << 6;
	  break;
	case SIM_FPU_IS_PINF:
	  cpu->regs[rd] = 1 << 7;
	  break;
	case SIM_FPU_IS_SNAN:
	  cpu->regs[rd] = 1 << 8;
	  break;
	case SIM_FPU_IS_QNAN:
	  cpu->regs[rd] = 1 << 9;
	  break;
	}
      break;
    default:
      TRACE_INSN (cpu, "UNHANDLED INSN: %s", op->name);
      sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL);
    }

 done:
  return pc;

}

static sim_cia
execute_f (SIM_CPU *cpu, unsigned_word iw, const struct riscv_opcode *op)
{
  SIM_DESC sd = CPU_STATE (cpu);
  unsigned int mask_arithmetic = MASK_FADD_S;
  unsigned int mask_mul_add = MASK_FMADD_S;
  unsigned int mask_convert = MASK_FCVT_S_W;

  static const int round_modes[] =
  {
      sim_fpu_round_near, sim_fpu_round_zero,
      sim_fpu_round_down, sim_fpu_round_up,
      sim_fpu_round_default, sim_fpu_round_default,
      sim_fpu_round_default
  };

  int rd = (iw >> OP_SH_RD) & OP_MASK_RD;
  int rs1 = (iw >> OP_SH_RS1) & OP_MASK_RS1;
  int rs2 = (iw >> OP_SH_RS2) & OP_MASK_RS2;
  int rs3 = (iw >> OP_SH_RS3) & OP_MASK_RS3;
  const char *frd_name = riscv_fpr_names_abi[rd];
  const char *frs1_name = riscv_fpr_names_abi[rs1];
  const char *frs2_name = riscv_fpr_names_abi[rs2];
  const char *frs3_name = riscv_fpr_names_abi[rs3];
  const char *rd_name = riscv_gpr_names_abi[rd];
  const char *rs1_name = riscv_gpr_names_abi[rs1];
  unsigned_word i_imm = EXTRACT_ITYPE_IMM (iw);
  unsigned_word s_imm = EXTRACT_STYPE_IMM (iw);
  uint32_t u32;
  int32_t i32;
  int64_t i64;
  uint64_t u64;
  sim_cia pc = cpu->pc + 4;

  /* Rounding mode.  */
  int rm = (iw >> OP_SH_RM) & OP_MASK_RM;
  int rounding = round_modes[rm];

  sim_fpu sft, sft2;
  sim_fpu sfa, sfb, sfc;
  sim_fpu_32to (&sfa, cpu->fpregs[rs1].w[0]);
  sim_fpu_32to (&sfb, cpu->fpregs[rs2].w[0]);

  switch (op->match & mask_mul_add)
    {
    case MATCH_FMADD_S:
      TRACE_INSN (cpu, "fmadd.s %s, %s, %s, %s",
		  frd_name, frs1_name, frs2_name, frs3_name);
      sim_fpu_32to (&sfc, cpu->fpregs[rs3].w[0]);
      sim_fpu_mul (&sft2, &sfa, &sfb);
      sim_fpu_add (&sft, &sfc, &sft2);
      sim_fpu_round_32 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to32 (&cpu->fpregs[rd].w[0], &sft);
      goto done;
    case MATCH_FMSUB_S:
      TRACE_INSN (cpu, "fmsub.s %s, %s, %s, %s",
		  frd_name, frs1_name, frs2_name, frs3_name);
      sim_fpu_32to (&sfc, cpu->fpregs[rs3].w[0]);
      sim_fpu_mul (&sft2, &sfa, &sfb);
      sim_fpu_sub (&sft, &sft2, &sfc);
      sim_fpu_round_32 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to32 (&cpu->fpregs[rd].w[0], &sft);
      goto done;
    case MATCH_FNMADD_S:
      TRACE_INSN (cpu, "fnmadd.s %s, %s, %s, %s",
		  frd_name, frs1_name, frs2_name, frs3_name);
      sim_fpu_32to (&sfc, cpu->fpregs[rs3].w[0]);
      sim_fpu_mul (&sft2, &sfa, &sfb);
      sim_fpu_neg (&sft2, &sft2);
      sim_fpu_sub (&sft, &sft2, &sfc);
      sim_fpu_round_32 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to32 (&cpu->fpregs[rd].w[0], &sft);
      goto done;
    case MATCH_FNMSUB_S:
      TRACE_INSN (cpu, "fnmsub.s %s, %s, %s, %s",
		  frd_name, frs1_name, frs2_name, frs3_name);
      sim_fpu_32to (&sfc, cpu->fpregs[rs3].w[0]);
      sim_fpu_mul (&sft2, &sfa, &sfb);
      sim_fpu_neg (&sft2, &sft2);
      sim_fpu_add (&sft, &sft2, &sfc);
      sim_fpu_round_32 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to32 (&cpu->fpregs[rd].w[0], &sft);
      goto done;
    }

  switch (op->match & mask_arithmetic)
    {
    case MATCH_FADD_S:
      TRACE_INSN (cpu, "fadd.s %s, %s, %s",
		  frd_name, frs1_name, frs2_name);
      sim_fpu_add (&sft, &sfa, &sfb);
      sim_fpu_round_32 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to32 (&cpu->fpregs[rd].w[0], &sft);
      goto done;
    case MATCH_FSUB_S:
      TRACE_INSN (cpu, "fsub.s %s, %s, %s",
		  frd_name, frs1_name, frs2_name);
      sim_fpu_sub (&sft, &sfa, &sfb);
      sim_fpu_round_32 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to32 (&cpu->fpregs[rd].w[0], &sft);
      goto done;
    case MATCH_FMUL_S:
      TRACE_INSN (cpu, "fmul.s %s, %s, %s",
		  frd_name, frs1_name, frs2_name);
      sim_fpu_mul (&sft, &sfa, &sfb);
      sim_fpu_round_64 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_round_32 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to32 (&cpu->fpregs[rd].w[0], &sft);
      goto done;
    case MATCH_FDIV_S:
      TRACE_INSN (cpu, "fdiv.s %s, %s, %s",
		  frd_name, frs1_name, frs2_name);
      sim_fpu_div (&sft, &sfa, &sfb);
      sim_fpu_round_32 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to32 (&cpu->fpregs[rd].w[0], &sft);
      goto done;
    case MATCH_FSQRT_S:
      TRACE_INSN (cpu, "fsqrt.s %s, %s, %s",
		  frd_name, frs1_name, frs2_name);
      sim_fpu_sqrt (&sft, &sfa);
      sim_fpu_to32 (&cpu->fpregs[rd].w[0], &sft);
      goto done;
    }

  switch (op->match & mask_convert)
    {
    case MATCH_FCVT_W_S:
      TRACE_INSN (cpu, "fcvt.w.s %s, %s",
		  rd_name, frs1_name);
      sim_fpu_to32i (&i32, &sfa, rounding);
      cpu->regs[rd] = i32;
      goto done;
    case MATCH_FCVT_WU_S:
      TRACE_INSN (cpu, "fcvt.wu.s %s, %s",
		  rd_name, frs1_name);
      sim_fpu_to32u (&u32, &sfa, rounding);
      i32 = u32;
      cpu->regs[rd] = i32;
      goto done;
    case MATCH_FCVT_S_W:
      TRACE_INSN (cpu, "fcvt.s.w %s, %s",
		  frd_name, rs1_name);
      sim_fpu_i32to (&sft, cpu->regs[rs1], rounding);
      sim_fpu_round_32 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to32 ((unsigned32 *) (cpu->fpregs + rd), &sft);
      goto done;
    case MATCH_FCVT_S_WU:
      TRACE_INSN (cpu, "fcvt.s.wu %s, %s",
		  frd_name, rs1_name);
      sim_fpu_u32to (&sft, cpu->regs[rs1], rounding);
      sim_fpu_round_32 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to32 ((unsigned32 *) (cpu->fpregs + rd), &sft);
      goto done;
    case MATCH_FCVT_L_S:
      TRACE_INSN (cpu, "fcvt.l.s %s, %s",
		  rd_name, frs1_name);
      cpu->regs[rd] = (int64_t) cpu->fpregs[rs1].S[0];
      goto done;
    case MATCH_FCVT_LU_S:
      TRACE_INSN (cpu, "fcvt.lu.s %s, %s",
		  rd_name, frs1_name);
      cpu->regs[rd] = (uint64_t) cpu->fpregs[rs1].S[0];
      goto done;
    case MATCH_FCVT_S_L:
      TRACE_INSN (cpu, "fcvt.s.l %s, %s",
		  frd_name, rs1_name);
      cpu->fpregs[rd].S[0] = (float) ((int64_t) cpu->regs[rs1]);
      goto done;
    case MATCH_FCVT_S_LU:
      TRACE_INSN (cpu, "fcvt.s.lu %s, %s",
		  frd_name, rs1_name);
      cpu->fpregs[rd].S[0] = (float) cpu->regs[rs1];
      goto done;
    }

  switch (op->match)
    {
    case MATCH_FLW:
      TRACE_INSN (cpu, "flw %s, %" PRIiTW "(%s)",
		  frd_name, i_imm, rs1_name);
      store_frd (cpu, rd, EXTEND32 (
	sim_core_read_unaligned_4 (cpu, cpu->pc, read_map,
				   cpu->regs[rs1] + i_imm)));
      break;
    case MATCH_FSW:
      TRACE_INSN (cpu, "fsw %s, %" PRIiTW "(%s)",
		  frs2_name, s_imm, rs1_name);
      sim_core_write_unaligned_4 (cpu, cpu->pc, write_map,
				  cpu->regs[rs1] + s_imm, cpu->fpregs[rs2].w[0]);
      break;
    case MATCH_FSGNJ_S:
      TRACE_INSN (cpu, "fsgnj.s %s, %s, %s",
		  frd_name, frs1_name, frs2_name);
      u32 = cpu->fpregs[rs1].w[0] & 0x7fffffff;
      u32 |= cpu->fpregs[rs2].w[0] & 0x80000000;
      cpu->fpregs[rd].w[0] = u32;
      break;
    case MATCH_FSGNJN_S:
      TRACE_INSN (cpu, "fsgnjn.s %s, %s, %s",
		  frd_name, frs1_name, frs2_name);
      u32 = cpu->fpregs[rs1].w[0] & 0x7fffffff;
      u32 |= (cpu->fpregs[rs2].w[0] & 0x80000000) ^ 0x80000000;
      cpu->fpregs[rd].w[0] = u32;
      break;
    case MATCH_FSGNJX_S:
      TRACE_INSN (cpu, "fsgnx.s %s, %s, %s",
		  frd_name, frs1_name, frs2_name);
      u32 = cpu->fpregs[rs1].w[0] & 0x7fffffff;
      u32 |= (cpu->fpregs[rs1].w[0] & 0x80000000) ^ (cpu->fpregs[rs2].w[0] & 0x80000000);
      cpu->fpregs[rd].w[0] = u32;
      break;
    case MATCH_FMIN_S:
      TRACE_INSN (cpu, "fmin.s %s, %s, %s",
		  frd_name, frs1_name, frs2_name);
      if (cpu->fpregs[rs1].S[0] < cpu->fpregs[rs2].S[0])
	cpu->fpregs[rd].S[0] = cpu->fpregs[rs1].S[0];
      else
	cpu->fpregs[rd].S[0] = cpu->fpregs[rs2].S[0];
      break;
    case MATCH_FMAX_S:
      TRACE_INSN (cpu, "fmax.s %s, %s, %s",
		  frd_name, frs1_name, frs2_name);
      if (cpu->fpregs[rs1].S[0] > cpu->fpregs[rs2].S[0])
	cpu->fpregs[rd].S[0] = cpu->fpregs[rs1].S[0];
      else
	cpu->fpregs[rd].S[0] = cpu->fpregs[rs2].S[0];
      break;
    case MATCH_FMV_X_S:
      TRACE_INSN (cpu, "fmv.x.s %s, %s",
		  rd_name, frs1_name);
      cpu->regs[rd] = cpu->fpregs[rs1].W[0];
      break;
    case MATCH_FMV_S_X:
      TRACE_INSN (cpu, "fmv.s.x %s, %s",
		  frd_name, rs1_name);
      cpu->fpregs[rd].w[0] = cpu->regs[rs1];
      break;
    case MATCH_FEQ_S:
      TRACE_INSN (cpu, "feq.s %s, %s, %s",
		  rd_name, frs1_name, frs2_name);
      cpu->regs[rd] = sim_fpu_is_eq (&sfa, &sfb);
      break;
    case MATCH_FLE_S:
      TRACE_INSN (cpu, "fle.s %s, %s, %s",
		  rd_name, frs1_name, frs2_name);
      cpu->regs[rd] = sim_fpu_is_le (&sfa, &sfb);
      break;
    case MATCH_FLT_S:
      TRACE_INSN (cpu, "flt.s %s, %s, %s",
		  rd_name, frs1_name, frs2_name);
      cpu->regs[rd] = sim_fpu_is_lt (&sfa, &sfb);
      break;
    case MATCH_FCLASS_S:
      TRACE_INSN (cpu, "fclass.s %s, %s",
		  rd_name, frs1_name);
      switch (sim_fpu_is (&sfa))
	{
	case SIM_FPU_IS_NINF:
	  cpu->regs[rd] = 1;
	  break;
	case SIM_FPU_IS_NNUMBER:
	  cpu->regs[rd] = 1 << 1;
	  break;
	case SIM_FPU_IS_NDENORM:
	  cpu->regs[rd] = 1 << 2;
	  break;
	case SIM_FPU_IS_NZERO:
	  cpu->regs[rd] = 1 << 3;
	  break;
	case SIM_FPU_IS_PZERO:
	  cpu->regs[rd] = 1 << 4;
	  break;
	case SIM_FPU_IS_PDENORM:
	  cpu->regs[rd] = 1 << 5;
	  break;
	case SIM_FPU_IS_PNUMBER:
	  cpu->regs[rd] = 1 << 6;
	  break;
	case SIM_FPU_IS_PINF:
	  cpu->regs[rd] = 1 << 7;
	  break;
	case SIM_FPU_IS_SNAN:
	  cpu->regs[rd] = 1 << 8;
	  break;
	case SIM_FPU_IS_QNAN:
	  cpu->regs[rd] = 1 << 9;
	  break;
	}
      break;
    case MATCH_FRCSR:
      TRACE_INSN (cpu, "frcsr %s",
		  rd_name);
      store_rd (cpu, rd, fetch_csr (cpu, "fcsr", CSR_FCSR, &cpu->csr.fcsr));
      break;
    case MATCH_FSCSR:
      TRACE_INSN (cpu, "fscsr %s, %sf",
		  rd_name, rs1_name);
      store_rd (cpu, rd, fetch_csr (cpu, "fcsr", CSR_FCSR, &cpu->csr.fcsr));
      store_csr (cpu, "fcsr", CSR_FCSR, &cpu->csr.fcsr, cpu->regs[rs1]);
      break;
    case MATCH_FRRM:
      TRACE_INSN (cpu, "frrm %s",
		  rd_name);
      store_rd (cpu, rd, fetch_csr (cpu, "frm", CSR_FRM, &cpu->csr.frm));
      break;
    case MATCH_FSRM:
      TRACE_INSN (cpu, "fsrm %s, %s",
		  rd_name, rs1_name);
      store_rd (cpu, rd, fetch_csr (cpu, "frm", CSR_FCSR, &cpu->csr.frm));
      store_csr (cpu, "frm", CSR_FCSR, &cpu->csr.frm, cpu->regs[rs1]);
      break;
    case MATCH_FRFLAGS:
      TRACE_INSN (cpu, "frflags %s",
		  rd_name);
      store_rd (cpu, rd, fetch_csr (cpu, "fflags", CSR_FFLAGS, &cpu->csr.fflags));
      break;
    case MATCH_FSFLAGS:
      TRACE_INSN (cpu, "fsflags %s, %s",
		  rd_name, frs1_name);
      store_rd (cpu, rd, fetch_csr (cpu, "fflags", CSR_FFLAGS, &cpu->csr.fflags));
      store_csr (cpu, "fflags", CSR_FFLAGS, &cpu->csr.fflags, cpu->regs[rs1]);
      break;
    default:
      TRACE_INSN (cpu, "UNHANDLED INSN: %s", op->name);
      sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL);
    }

 done:
  return pc;
}

static sim_cia
execute_zdinx (SIM_CPU *cpu, unsigned_word iw, const struct riscv_opcode *op)
{
  SIM_DESC sd = CPU_STATE (cpu);
  unsigned int mask_arithmetic = MASK_FADD_D;
  unsigned int mask_mul_add = MASK_FMADD_S;
  unsigned int mask_convert = MASK_FCVT_S_W;

  static const int round_modes[] =
  {
      sim_fpu_round_near, sim_fpu_round_zero,
      sim_fpu_round_down, sim_fpu_round_up,
      sim_fpu_round_default, sim_fpu_round_default,
      sim_fpu_round_default
  };

  int rd = (iw >> OP_SH_RD) & OP_MASK_RD;
  int rs1 = (iw >> OP_SH_RS1) & OP_MASK_RS1;
  int rs2 = (iw >> OP_SH_RS2) & OP_MASK_RS2;
  int rs3 = (iw >> OP_SH_RS3) & OP_MASK_RS3;
  const char *rd_name = riscv_gpr_names_abi[rd];
  const char *rs1_name = riscv_gpr_names_abi[rs1];
  const char *rs2_name = riscv_gpr_names_abi[rs2];
  const char *rs3_name = riscv_gpr_names_abi[rs3];
  unsigned_word i_imm = EXTRACT_ITYPE_IMM (iw);
  unsigned_word s_imm = EXTRACT_STYPE_IMM (iw);
  uint32_t u32;
  int32_t i32;
  uint64_t u64;
  int64_t i64;
  unsigned32 tmp32;
  unsigned64 tmp64;
  float tmpf;
  double tmpd;
  union conv64 fargrs1, fargrs2, fargrs3, frd;

  sim_cia pc = cpu->pc + 4;

  /* Rounding mode.  */
  int rm = (iw >> OP_SH_RM) & OP_MASK_RM;
  int rounding = round_modes[rm];

  sim_fpu sft, sft2;
  sim_fpu sfa, sfb, sfc;

  /* for rv32 we have to deal with register pairs */
  if (RISCV_XLEN (cpu) == 64)
    {
      fargrs1.v = cpu->regs[rs1];
      fargrs2.v = cpu->regs[rs2];
      fargrs3.v = cpu->regs[rs3];

      sim_fpu_64to (&sfa, cpu->regs[rs1]);
      sim_fpu_64to (&sfb, cpu->regs[rs2]);
    }
  else
    {
#if 0
      if ((rs1 % 2) != 0 || (rs2 % 2) != 0)
	{
	  TRACE_INSN (cpu, "ODD REGISTER ARGUMENT for: %s", op->name);
	  sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL)
	}
#endif
      /* access x0 means both x1 is not touched and the constant is zero */
      fargrs1.w[0] = rs1 ? cpu->regs[rs1]     : 0;
      fargrs1.w[1] = rs1 ? cpu->regs[rs1 + 1] : 0;
      fargrs2.w[0] = rs2 ? cpu->regs[rs2]     : 0;
      fargrs2.w[1] = rs2 ? cpu->regs[rs2 + 1] : 0;
      fargrs3.w[0] = rs3 ? cpu->regs[rs3]     : 0;
      fargrs3.w[1] = rs3 ? cpu->regs[rs3 + 1] : 0;

      sim_fpu_64to (&sfa, fargrs1.v);
      sim_fpu_64to (&sfb, fargrs2.v);
    }

  switch (op->match & mask_mul_add)
    {
    case MATCH_FMADD_D:
      TRACE_INSN (cpu, "fmadd.d %s, %s, %s, %s",
		  rd_name, rs1_name, rs2_name, rs3_name);
      sim_fpu_64to (&sfc, fargrs3.v);
      sim_fpu_mul (&sft2, &sfa, &sfb);
      sim_fpu_add (&sft, &sfc, &sft2);
      sim_fpu_round_64 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to64 (&tmp64, &sft);
      store_rd_dinx (cpu, rd, tmp64);
      goto done;
    case MATCH_FMSUB_D:
      TRACE_INSN (cpu, "fmsub.d %s, %s, %s, %s",
		  rd_name, rs1_name, rs2_name, rs3_name);
      sim_fpu_64to (&sfc, fargrs3.v);
      sim_fpu_mul (&sft2, &sfa, &sfb);
      sim_fpu_sub (&sft, &sft2, &sfc);
      sim_fpu_round_64 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to64 (&tmp64, &sft);
      store_rd_dinx (cpu, rd, tmp64);
      goto done;
    case MATCH_FNMADD_D:
      TRACE_INSN (cpu, "fnmadd.d %s, %s, %s, %s",
		  rd_name, rs1_name, rs2_name, rs3_name);
      sim_fpu_64to (&sfc, fargrs3.v);
      sim_fpu_mul (&sft2, &sfa, &sfb);
      sim_fpu_neg (&sft2, &sft2);
      sim_fpu_sub (&sft, &sft2, &sfc);
      sim_fpu_round_64 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to64 (&tmp64, &sft);
      store_rd_dinx (cpu, rd, tmp64);
      goto done;
    case MATCH_FNMSUB_D:
      TRACE_INSN (cpu, "fnmsub.d %s, %s, %s, %s",
		  rd_name, rs1_name, rs2_name, rs3_name);
      sim_fpu_64to (&sfc, fargrs3.v);
      sim_fpu_mul (&sft2, &sfa, &sfb);
      sim_fpu_neg (&sft2, &sft2);
      sim_fpu_add (&sft, &sft2, &sfc);
      sim_fpu_round_64 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to64 (&tmp64, &sft);
      store_rd_dinx (cpu, rd, tmp64);
      goto done;
    }

  switch (op->match & mask_arithmetic)
    {
    case MATCH_FADD_D:
      TRACE_INSN (cpu, "fadd.d %s, %s, %s",
		  rd_name, rs1_name, rs2_name);
      sim_fpu_add (&sft, &sfa, &sfb);
      sim_fpu_round_64 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to64 (&tmp64, &sft);
      store_rd_dinx (cpu, rd, tmp64);
      goto done;
    case MATCH_FSUB_D:
      TRACE_INSN (cpu, "fsub.d %s, %s, %s",
		  rd_name, rs1_name, rs2_name);
      sim_fpu_sub (&sft, &sfa, &sfb);
      sim_fpu_round_64 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to64 (&tmp64, &sft);
      store_rd_dinx (cpu, rd, tmp64);
      goto done;
    case MATCH_FMUL_D:
      TRACE_INSN (cpu, "fmul.d %s, %s, %s",
		  rd_name, rs1_name, rs2_name);
      sim_fpu_mul (&sft, &sfa, &sfb);
      sim_fpu_round_64 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to64 (&tmp64, &sft);
      store_rd_dinx (cpu, rd, tmp64);
      goto done;
    case MATCH_FDIV_D:
      TRACE_INSN (cpu, "fdiv.d %s, %s, %s",
		  rd_name, rs1_name, rs2_name);
      sim_fpu_div (&sft, &sfa, &sfb);
      sim_fpu_round_64 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to64 (&tmp64, &sft);
      store_rd_dinx (cpu, rd, tmp64);
      goto done;
    case MATCH_FSQRT_D:
      TRACE_INSN (cpu, "fsqrt.d %s, %s",
		  rd_name, rs1_name);
      sim_fpu_sqrt (&sft, &sfa);
      sim_fpu_round_64 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to64 (&tmp64, &sft);
      store_rd_dinx (cpu, rd, tmp64);
      goto done;
    }

  switch (op->match & mask_convert)
    {
    case MATCH_FCVT_W_D:
      TRACE_INSN (cpu, "fcvt.w.d %s, %s",
		  rd_name, rs1_name);
      sim_fpu_to32i (&i32, &sfa, rounding);
      cpu->regs[rd] = i32;
      goto done;
    case MATCH_FCVT_WU_D:
      TRACE_INSN (cpu, "fcvt.wu.d %s, %s",
		  rd_name, rs1_name);
      sim_fpu_to32u (&u32, &sfa, rounding);
      i32 = u32;
      cpu->regs[rd] = i32;
      goto done;
    case MATCH_FCVT_D_W:
      TRACE_INSN (cpu, "fcvt.d.w %s, %s",
		  rd_name, rs1_name);
      sim_fpu_i32to (&sft, cpu->regs[rs1], rounding);
      sim_fpu_to64 (&tmp64, &sft);
      store_rd_dinx (cpu, rd, tmp64);
      goto done;
    case MATCH_FCVT_D_WU:
      TRACE_INSN (cpu, "fcvt.d.wu %s, %s",
		  rd_name, rs1_name);
      sim_fpu_u32to (&sft, cpu->regs[rs1], rounding);
      sim_fpu_to64 (&tmp64, &sft);
      store_rd_dinx (cpu, rd, tmp64);
      goto done;
    case MATCH_FCVT_S_D:
      TRACE_INSN (cpu, "fcvt.s.d %s, %s",
		  rd_name, rs1_name);
      sft = sfa;
      sim_fpu_round_32 (&sft, sim_fpu_round_near, sim_fpu_denorm_default);
      sim_fpu_to32 (&tmp32, &sft);
      store_rd (cpu, rd, tmp32);
      goto done;
    case MATCH_FCVT_D_S:
      TRACE_INSN (cpu, "fcvt.d.s %s, %s",
		  rd_name, rs1_name);
      sim_fpu_32to (&sft, cpu->regs[rs1]);
      sim_fpu_to64 (&tmp64, &sft);
      store_rd_dinx (cpu, rd, tmp64);
      goto done;
    case MATCH_FCVT_L_D:
      TRACE_INSN (cpu, "fcvt.l.d %s, %s",
		  rd_name, rs1_name);
      RISCV_ASSERT_RV64 (cpu, "insn: %s", op->name);
      memcpy (&tmpd, &cpu->regs[rs1], sizeof (tmpd));
      cpu->regs[rd] = (int64_t) tmpd;
      goto done;
    case MATCH_FCVT_LU_D:
      TRACE_INSN (cpu, "fcvt.lu.d %s, %s",
		  rd_name, rs1_name);
      RISCV_ASSERT_RV64 (cpu, "insn: %s", op->name);
      memcpy (&tmpd, &cpu->regs[rs1], sizeof (tmpd));
      cpu->regs[rd] = (uint64_t) tmpd;
      goto done;
    case MATCH_FCVT_D_L:
      TRACE_INSN (cpu, "fcvt.d.l %s, %s",
		  rd_name, rs1_name);
      RISCV_ASSERT_RV64 (cpu, "insn: %s", op->name);
      tmpd = (double) ((int64_t) cpu->regs[rs1]);
      memcpy (&cpu->regs[rd], &tmpd, sizeof (cpu->regs[rd]));
      goto done;
    case MATCH_FCVT_D_LU:
      TRACE_INSN (cpu, "fcvt.d.lu %s, %s",
		  rd_name, rs1_name);
      RISCV_ASSERT_RV64 (cpu, "insn: %s", op->name);
      tmpd = (double) cpu->regs[rs1];
      memcpy (&cpu->regs[rd], &tmpd, sizeof (cpu->regs[rd]));
      goto done;
    }

  switch (op->match)
    {
    case MATCH_FSGNJ_D:
      TRACE_INSN (cpu, "fsgnj.d %s, %s, %s",
		  rd_name, rs1_name, rs2_name);
      u32 = fargrs1.w[1] & 0x7fffffff;
      u32 |= fargrs2.w[1] & 0x80000000;
      frd.w[1] = u32;
      frd.w[0] = fargrs1.w[0];
      store_rd_dinx (cpu, rd, frd.v);
      break;
    case MATCH_FSGNJN_D:
      TRACE_INSN (cpu, "fsgnjn.d %s, %s, %s",
		  rd_name, rs1_name, rs2_name);
      u32 = fargrs1.w[1] & 0x7fffffff;
      u32 |= (fargrs2.w[1] & 0x80000000) ^ 0x80000000;
      frd.w[1] = u32;
      frd.w[0] = fargrs1.w[0];
      store_rd_dinx (cpu, rd, frd.v);
      break;
    case MATCH_FSGNJX_D:
      TRACE_INSN (cpu, "fsgnjx.d %s, %s, %s",
		  rd_name, rs1_name, rs2_name);
      u32 = fargrs1.w[1] & 0x7fffffff;
      u32 |= (fargrs1.w[1] & 0x80000000) ^ (fargrs2.w[1] & 0x80000000);
      frd.w[1] = u32;
      frd.w[0] = fargrs1.w[0];
      store_rd_dinx (cpu, rd, frd.v);
      break;
    case MATCH_FMIN_D:
      TRACE_INSN (cpu, "fmin.d %s, %s, %s",
		  rd_name, rs1_name, rs2_name);
      if (fargrs1.d < fargrs2.d)
	store_rd_dinx (cpu, rd, fargrs1.v);
      else
	store_rd_dinx (cpu, rd, fargrs2.v);
      break;
    case MATCH_FMAX_D:
      TRACE_INSN (cpu, "fmax.d %s, %s, %s",
		  rd_name, rs1_name, rs2_name);
      if (fargrs1.d > fargrs2.d)
	store_rd_dinx (cpu, rd, fargrs1.v);
      else
	store_rd_dinx (cpu, rd, fargrs2.v);
      break;
    case MATCH_FEQ_D:
      TRACE_INSN (cpu, "feq.d %s, %s, %s",
		  rd_name, rs1_name, rs2_name);
      cpu->regs[rd] = sim_fpu_is_eq (&sfa, &sfb);
      break;
    case MATCH_FLE_D:
      TRACE_INSN (cpu, "fle.d %s, %s, %s",
		  rd_name, rs1_name, rs2_name);
      cpu->regs[rd] = sim_fpu_is_le (&sfa, &sfb);
      break;
    case MATCH_FLT_D:
      TRACE_INSN (cpu, "flt.d %s, %s, %s",
		  rd_name, rs1_name, rs2_name);
      cpu->regs[rd] = sim_fpu_is_lt (&sfa, &sfb);
      break;
    case MATCH_FCLASS_D:
      TRACE_INSN (cpu, "fclass.d %s, %s",
		  rd_name, rs1_name);
      switch (sim_fpu_is (&sfa))
	{
	case SIM_FPU_IS_NINF:
	  cpu->regs[rd] = 1;
	  break;
	case SIM_FPU_IS_NNUMBER:
	  cpu->regs[rd] = 1 << 1;
	  break;
	case SIM_FPU_IS_NDENORM:
	  cpu->regs[rd] = 1 << 2;
	  break;
	case SIM_FPU_IS_NZERO:
	  cpu->regs[rd] = 1 << 3;
	  break;
	case SIM_FPU_IS_PZERO:
	  cpu->regs[rd] = 1 << 4;
	  break;
	case SIM_FPU_IS_PDENORM:
	  cpu->regs[rd] = 1 << 5;
	  break;
	case SIM_FPU_IS_PNUMBER:
	  cpu->regs[rd] = 1 << 6;
	  break;
	case SIM_FPU_IS_PINF:
	  cpu->regs[rd] = 1 << 7;
	  break;
	case SIM_FPU_IS_SNAN:
	  cpu->regs[rd] = 1 << 8;
	  break;
	case SIM_FPU_IS_QNAN:
	  cpu->regs[rd] = 1 << 9;
	  break;
	}
      break;
    default:
      TRACE_INSN (cpu, "UNHANDLED INSN: %s", op->name);
      sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL);
    }

 done:
  return pc;

}

static sim_cia
execute_zfinx (SIM_CPU *cpu, unsigned_word iw, const struct riscv_opcode *op)
{
  SIM_DESC sd = CPU_STATE (cpu);
  unsigned int mask_arithmetic = MASK_FADD_S;
  unsigned int mask_mul_add = MASK_FMADD_S;
  unsigned int mask_convert = MASK_FCVT_S_W;

  static const int round_modes[] =
  {
      sim_fpu_round_near, sim_fpu_round_zero,
      sim_fpu_round_down, sim_fpu_round_up,
      sim_fpu_round_default, sim_fpu_round_default,
      sim_fpu_round_default
  };

  int rd = (iw >> OP_SH_RD) & OP_MASK_RD;
  int rs1 = (iw >> OP_SH_RS1) & OP_MASK_RS1;
  int rs2 = (iw >> OP_SH_RS2) & OP_MASK_RS2;
  int rs3 = (iw >> OP_SH_RS3) & OP_MASK_RS3;
  const char *rd_name = riscv_gpr_names_abi[rd];
  const char *rs1_name = riscv_gpr_names_abi[rs1];
  const char *rs2_name = riscv_gpr_names_abi[rs2];
  const char *rs3_name = riscv_gpr_names_abi[rs3];
  unsigned_word i_imm = EXTRACT_ITYPE_IMM (iw);
  unsigned_word s_imm = EXTRACT_STYPE_IMM (iw);
  uint32_t u32;
  int32_t i32;
  int64_t i64;
  uint64_t u64;
  unsigned_word tmp;
  float tmpf;
  union conv32 fargrs1, fargrs2;

  sim_cia pc = cpu->pc + 4;

  /* Rounding mode.  */
  int rm = (iw >> OP_SH_RM) & OP_MASK_RM;
  int rounding = round_modes[rm];

  sim_fpu sft, sft2;
  sim_fpu sfa, sfb, sfc;
  sim_fpu_32to (&sfa, cpu->regs[rs1]);
  sim_fpu_32to (&sfb, cpu->regs[rs2]);

  fargrs1.w = cpu->regs[rs1];
  fargrs2.w = cpu->regs[rs2];

  switch (op->match & mask_mul_add)
    {
    case MATCH_FMADD_S:
      TRACE_INSN (cpu, "fmadd.s %s, %s, %s, %s",
		  rd_name, rs1_name, rs2_name, rs3_name);
      sim_fpu_32to (&sfc, cpu->regs[rs3]);
      sim_fpu_mul (&sft2, &sfa, &sfb);
      sim_fpu_add (&sft, &sfc, &sft2);
      sim_fpu_round_32 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to32 (&tmp, &sft);
      store_rd (cpu, rd, tmp);
      goto done;
    case MATCH_FMSUB_S:
      TRACE_INSN (cpu, "fmsub.s %s, %s, %s, %s",
		  rd_name, rs1_name, rs2_name, rs3_name);
      sim_fpu_32to (&sfc, cpu->regs[rs3]);
      sim_fpu_mul (&sft2, &sfa, &sfb);
      sim_fpu_sub (&sft, &sft2, &sfc);
      sim_fpu_round_32 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to32 (&tmp, &sft);
      store_rd (cpu, rd, tmp);
      goto done;
    case MATCH_FNMADD_S:
      TRACE_INSN (cpu, "fnmadd.s %s, %s, %s, %s",
		  rd_name, rs1_name, rs2_name, rs3_name);
      sim_fpu_32to (&sfc, cpu->regs[rs3]);
      sim_fpu_mul (&sft2, &sfa, &sfb);
      sim_fpu_neg (&sft2, &sft2);
      sim_fpu_sub (&sft, &sft2, &sfc);
      sim_fpu_round_32 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to32 (&tmp, &sft);
      store_rd (cpu, rd, tmp);
      goto done;
    case MATCH_FNMSUB_S:
      TRACE_INSN (cpu, "fnmsub.s %s, %s, %s, %s",
		  rd_name, rs1_name, rs2_name, rs3_name);
      sim_fpu_32to (&sfc, cpu->regs[rs3]);
      sim_fpu_mul (&sft2, &sfa, &sfb);
      sim_fpu_neg (&sft2, &sft2);
      sim_fpu_add (&sft, &sft2, &sfc);
      sim_fpu_round_32 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to32 (&tmp, &sft);
      store_rd (cpu, rd, tmp);
      goto done;
    }

  switch (op->match & mask_arithmetic)
    {
    case MATCH_FADD_S:
      TRACE_INSN (cpu, "fadd.s %s, %s, %s",
		  rd_name, rs1_name, rs2_name);
      sim_fpu_add (&sft, &sfa, &sfb);
      sim_fpu_round_32 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to32 (&tmp, &sft);
      store_rd (cpu, rd, tmp);
      goto done;
    case MATCH_FSUB_S:
      TRACE_INSN (cpu, "fsub.s %s, %s, %s",
		  rd_name, rs1_name, rs2_name);
      sim_fpu_sub (&sft, &sfa, &sfb);
      sim_fpu_round_32 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to32 (&tmp, &sft);
      store_rd (cpu, rd, tmp);
      goto done;
    case MATCH_FMUL_S:
      TRACE_INSN (cpu, "fmul.s %s, %s, %s",
		  rd_name, rs1_name, rs2_name);
      sim_fpu_mul (&sft, &sfa, &sfb);
      sim_fpu_round_64 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_round_32 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to32 (&tmp, &sft);
      store_rd (cpu, rd, tmp);
      goto done;
    case MATCH_FDIV_S:
      TRACE_INSN (cpu, "fdiv.s %s, %s, %s",
		  rd_name, rs1_name, rs2_name);
      sim_fpu_div (&sft, &sfa, &sfb);
      sim_fpu_round_32 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to32 (&tmp, &sft);
      store_rd (cpu, rd, tmp);
      goto done;
    case MATCH_FSQRT_S:
      TRACE_INSN (cpu, "fsqrt.s %s, %s, %s",
		  rd_name, rs1_name, rs2_name);
      sim_fpu_sqrt (&sft, &sfa);
      sim_fpu_to32 (&tmp, &sft);
      store_rd (cpu, rd, tmp);
      goto done;
    }

  switch (op->match & mask_convert)
    {
    case MATCH_FCVT_W_S:
      TRACE_INSN (cpu, "fcvt.w.s %s, %s",
		  rd_name, rs1_name);
      sim_fpu_to32i (&i32, &sfa, rounding);
      cpu->regs[rd] = i32;
      goto done;
    case MATCH_FCVT_WU_S:
      TRACE_INSN (cpu, "fcvt.wu.s %s, %s",
		  rd_name, rs1_name);
      sim_fpu_to32u (&u32, &sfa, rounding);
      i32 = u32;
      cpu->regs[rd] = i32;
      goto done;
    case MATCH_FCVT_S_W:
      TRACE_INSN (cpu, "fcvt.s.w %s, %s",
		  rd_name, rs1_name);
      sim_fpu_i32to (&sft, cpu->regs[rs1], rounding);
      sim_fpu_round_32 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to32 (&tmp, &sft);
      store_rd (cpu, rd, tmp);
      goto done;
    case MATCH_FCVT_S_WU:
      TRACE_INSN (cpu, "fcvt.s.wu %s, %s",
		  rd_name, rs1_name);
      sim_fpu_u32to (&sft, cpu->regs[rs1], rounding);
      sim_fpu_round_32 (&sft, rounding, sim_fpu_denorm_default);
      sim_fpu_to32 (&tmp, &sft);
      store_rd (cpu, rd, tmp);
      goto done;
    case MATCH_FCVT_L_S:
      TRACE_INSN (cpu, "fcvt.l.s %s, %s",
		  rd_name, rs1_name);
      RISCV_ASSERT_RV64 (cpu, "insn: %s", op->name);
      memcpy (&tmpf, &cpu->regs[rs1], sizeof (tmpf));
      cpu->regs[rd] = (int64_t) tmpf;
      goto done;
    case MATCH_FCVT_LU_S:
      TRACE_INSN (cpu, "fcvt.lu.s %s, %s",
		  rd_name, rs1_name);
      RISCV_ASSERT_RV64 (cpu, "insn: %s", op->name);
      memcpy (&tmpf, &cpu->regs[rs1], sizeof (tmpf));
      cpu->regs[rd] = (uint64_t) tmpf;
      goto done;
    case MATCH_FCVT_S_L:
      TRACE_INSN (cpu, "fcvt.s.l %s, %s",
		  rd_name, rs1_name);
      RISCV_ASSERT_RV64 (cpu, "insn: %s", op->name);
      tmpf = (float) ((int64_t) cpu->regs[rs1]);
      memcpy (&cpu->regs[rd], &tmpf, sizeof (cpu->regs[rd]));
      goto done;
    case MATCH_FCVT_S_LU:
      TRACE_INSN (cpu, "fcvt.s.lu %s, %s",
		  rd_name, rs1_name);
      RISCV_ASSERT_RV64 (cpu, "insn: %s", op->name);
      tmpf = (float) cpu->regs[rs1];
      memcpy (&cpu->regs[rd], &tmpf, sizeof (cpu->regs[rd]));
      goto done;
    }

  switch (op->match)
    {
    case MATCH_FSGNJ_S:
      TRACE_INSN (cpu, "fsgnj.s %s, %s, %s",
		  rd_name, rs1_name, rs2_name);
      u32 = cpu->regs[rs1] & 0x7fffffff;
      u32 |= cpu->regs[rs2] & 0x80000000;
      cpu->regs[rd] = u32;
      break;
    case MATCH_FSGNJN_S:
      TRACE_INSN (cpu, "fsgnjn.s %s, %s, %s",
		  rd_name, rs1_name, rs2_name);
      u32 = cpu->regs[rs1] & 0x7fffffff;
      u32 |= (cpu->regs[rs2] & 0x80000000) ^ 0x80000000;
      cpu->regs[rd] = u32;
      break;
    case MATCH_FSGNJX_S:
      TRACE_INSN (cpu, "fsgnx.s %s, %s, %s",
		  rd_name, rs1_name, rs2_name);
      u32 = cpu->regs[rs1] & 0x7fffffff;
      u32 |= (cpu->regs[rs1] & 0x80000000) ^ (cpu->regs[rs2] & 0x80000000);
      cpu->regs[rd] = u32;
      break;
    case MATCH_FMIN_S:
      TRACE_INSN (cpu, "fmin.s %s, %s, %s",
		  rd_name, rs1_name, rs2_name);
      if (fargrs1.s < fargrs2.s)
	cpu->regs[rd] = fargrs1.w;
      else
	cpu->regs[rd] = fargrs2.w;
      break;
    case MATCH_FMAX_S:
      TRACE_INSN (cpu, "fmax.s %s, %s, %s",
		  rd_name, rs1_name, rs2_name);
      if (fargrs1.s > fargrs2.s)
	cpu->regs[rd] = fargrs1.w;
      else
	cpu->regs[rd] = fargrs2.w;
      break;
    case MATCH_FEQ_S:
      TRACE_INSN (cpu, "feq.s %s, %s, %s",
		  rd_name, rs1_name, rs2_name);
      cpu->regs[rd] = sim_fpu_is_eq (&sfa, &sfb);
      break;
    case MATCH_FLE_S:
      TRACE_INSN (cpu, "fle.s %s, %s, %s",
		  rd_name, rs1_name, rs2_name);
      cpu->regs[rd] = sim_fpu_is_le (&sfa, &sfb);
      break;
    case MATCH_FLT_S:
      TRACE_INSN (cpu, "flt.s %s, %s, %s",
		  rd_name, rs1_name, rs2_name);
      cpu->regs[rd] = sim_fpu_is_lt (&sfa, &sfb);
      break;
    case MATCH_FCLASS_S:
      TRACE_INSN (cpu, "fclass.s %s, %s",
		  rd_name, rs1_name);
      switch (sim_fpu_is (&sfa))
	{
	case SIM_FPU_IS_NINF:
	  cpu->regs[rd] = 1;
	  break;
	case SIM_FPU_IS_NNUMBER:
	  cpu->regs[rd] = 1 << 1;
	  break;
	case SIM_FPU_IS_NDENORM:
	  cpu->regs[rd] = 1 << 2;
	  break;
	case SIM_FPU_IS_NZERO:
	  cpu->regs[rd] = 1 << 3;
	  break;
	case SIM_FPU_IS_PZERO:
	  cpu->regs[rd] = 1 << 4;
	  break;
	case SIM_FPU_IS_PDENORM:
	  cpu->regs[rd] = 1 << 5;
	  break;
	case SIM_FPU_IS_PNUMBER:
	  cpu->regs[rd] = 1 << 6;
	  break;
	case SIM_FPU_IS_PINF:
	  cpu->regs[rd] = 1 << 7;
	  break;
	case SIM_FPU_IS_SNAN:
	  cpu->regs[rd] = 1 << 8;
	  break;
	case SIM_FPU_IS_QNAN:
	  cpu->regs[rd] = 1 << 9;
	  break;
	}
      break;
    case MATCH_FRCSR:
      TRACE_INSN (cpu, "frcsr %s",
		  rd_name);
      store_rd (cpu, rd, fetch_csr (cpu, "fcsr", CSR_FCSR, &cpu->csr.fcsr));
      break;
    case MATCH_FSCSR:
      TRACE_INSN (cpu, "fscsr %s, %sf",
		  rd_name, rs1_name);
      store_rd (cpu, rd, fetch_csr (cpu, "fcsr", CSR_FCSR, &cpu->csr.fcsr));
      store_csr (cpu, "fcsr", CSR_FCSR, &cpu->csr.fcsr, cpu->regs[rs1]);
      break;
    case MATCH_FRRM:
      TRACE_INSN (cpu, "frrm %s",
		  rd_name);
      store_rd (cpu, rd, fetch_csr (cpu, "frm", CSR_FRM, &cpu->csr.frm));
      break;
    case MATCH_FSRM:
      TRACE_INSN (cpu, "fsrm %s, %s",
		  rd_name, rs1_name);
      store_rd (cpu, rd, fetch_csr (cpu, "frm", CSR_FCSR, &cpu->csr.frm));
      store_csr (cpu, "frm", CSR_FCSR, &cpu->csr.frm, cpu->regs[rs1]);
      break;
    case MATCH_FRFLAGS:
      TRACE_INSN (cpu, "frflags %s",
		  rd_name);
      store_rd (cpu, rd, fetch_csr (cpu, "fflags", CSR_FFLAGS, &cpu->csr.fflags));
      break;
    case MATCH_FSFLAGS:
      TRACE_INSN (cpu, "fsflags %s, %s",
		  rd_name, rs1_name);
      store_rd (cpu, rd, fetch_csr (cpu, "fflags", CSR_FFLAGS, &cpu->csr.fflags));
      store_csr (cpu, "fflags", CSR_FFLAGS, &cpu->csr.fflags, cpu->regs[rs1]);
      break;
    default:
      TRACE_INSN (cpu, "UNHANDLED INSN: %s", op->name);
      sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL);
    }

 done:
  return pc;
}

static sim_cia
execute_c (SIM_CPU *cpu, unsigned_word iw, const struct riscv_opcode *op)
{
  SIM_DESC sd = CPU_STATE (cpu);
  const int mask_group_op = 0x3;
  const int mask_mv_jr = 0xf003;
  const int match_mv_jr = 0x8002;
  const int mask_ebk_jalr_add = 0xf003;
  const int match_ebk_jalr_add = 0x9002;

  int rd = (iw >> OP_SH_RD) & OP_MASK_RD;
  int crs2 = (iw >> OP_SH_CRS2) & OP_MASK_CRS2;
  int crs1s = ((iw >> OP_SH_CRS1S) & OP_MASK_CRS1S) | 0x8;
  int crs2s = ((iw >> OP_SH_CRS2S) & OP_MASK_CRS2S) | 0x8;
  int ciw_rd = crs2s;
  unsigned_word rvc_imm = EXTRACT_RVC_IMM (iw);
  unsigned_word tmp;
  sim_cia pc = cpu->pc + 2;

  const char *rd_name = riscv_gpr_names_abi[rd];
  const char *crs2_name = riscv_gpr_names_abi[crs2];
  const char *crs1s_name = riscv_gpr_names_abi[crs1s];
  const char *crs2s_name = riscv_gpr_names_abi[crs2s];
  const char *ciw_rd_name = crs2s_name;

  const char *frd_name = riscv_fpr_names_abi[rd];
  const char *fcrs2_name = riscv_fpr_names_abi[crs2];
  const char *fcrs1s_name = riscv_fpr_names_abi[crs1s];
  const char *fcrs2s_name = riscv_fpr_names_abi[crs2s];
  const char *fciw_rd_name = fcrs2s_name;

  /* Deal with c.mv, c.jr instructons.  */
  if ((op->match & mask_mv_jr) == match_mv_jr)
    {
      if (crs2 != 0)
	{
	  /* c.mv */
	  TRACE_INSN (cpu, "c.mv %s, %s // %s = %s",
		      rd_name, crs2_name, rd_name, crs2_name);
	  cpu->regs[rd] = cpu->regs[crs2];
	}
      else
	{
	  /* c.jr */
	  TRACE_INSN (cpu, "c.jr %s", rd_name);
	  pc = cpu->regs[rd];
	}
      return pc;
    }

  /* Deal with c.ebreak, c.jalr, c.add instructions.  */
  if ((op->match & mask_ebk_jalr_add) == match_ebk_jalr_add)
    {
      if (iw == MATCH_C_EBREAK)
	{
	  /* c.ebreak */
	  TRACE_INSN (cpu, "c.break");
	  sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_stopped, SIM_SIGTRAP);
	}
      else if (crs2 == 0)
	{
	  /* c.jalr */
	  TRACE_INSN (cpu, "c.jalr %s", rd_name);
	  pc = cpu->regs[rd];
	  store_rd (cpu, X_RA, cpu->pc + 2);
	}
      else
	{
	  /* c.add */
	  TRACE_INSN (cpu, "c.add %s, %s // %s += %s",
		      rd_name, crs2_name, rd_name, crs2_name);
	  store_rd (cpu, rd, cpu->regs[rd] + cpu->regs[crs2]);
	}
      return pc;
    }

  switch (op->match & mask_group_op)
    {
    case 0:
      switch (op->match)
	{
	case MATCH_C_LW:
	  TRACE_INSN (cpu, "c.lw %s, %" PRIiTW "(%s);"
			   " // %s = *(%s + %" PRIiTW ")",
		      crs2s_name, EXTRACT_RVC_LW_IMM (iw), crs1s_name,
		      crs2s_name, crs1s_name, EXTRACT_RVC_LW_IMM (iw));
	  store_rd (cpu, crs2s, EXTEND32 (
	    sim_core_read_unaligned_4 (cpu, cpu->pc, read_map,
				       cpu->regs[crs1s]
				       + EXTRACT_RVC_LW_IMM (iw))));
	  return pc;
	case MATCH_C_SW:
	  TRACE_INSN (cpu, "c.sw %s, %" PRIiTW "(%s);"
			   " // *(%s + %" PRIiTW ") = %s",
		      crs2s_name, EXTRACT_RVC_LW_IMM (iw), crs1s_name,
		      crs1s_name, EXTRACT_RVC_LW_IMM (iw), crs2s_name);
	  sim_core_write_unaligned_4 (cpu, cpu->pc, write_map,
				      (cpu->regs[crs1s]
				       + EXTRACT_RVC_LW_IMM (iw)),
				      cpu->regs[crs2s]);
	  return pc;
	case MATCH_C_ADDI4SPN:
	  TRACE_INSN (cpu, "c.addi4spn %s, %" PRIiTW
			   " // %s = sp + %" PRIiTW,
		      ciw_rd_name, EXTRACT_RVC_ADDI4SPN_IMM (iw),
		      ciw_rd_name, EXTRACT_RVC_ADDI4SPN_IMM (iw));
	  store_rd (cpu, ciw_rd, cpu->sp + EXTRACT_RVC_ADDI4SPN_IMM (iw));
	  return pc;
	case MATCH_C_FLD:
	  if (RISCV_XLEN (cpu) <= 64)
	    {
	      TRACE_INSN (cpu, "c.fld %s, %" PRIiTW "(%s);"
			       " // %s = *(%s + %" PRIiTW ")",
			  fcrs2s_name, EXTRACT_RVC_LD_IMM (iw), fcrs1s_name,
			  fcrs2s_name, fcrs1s_name, EXTRACT_RVC_LD_IMM (iw));
	      /* rv32/64, c.fld instruction.  */
	      store_frd64 (cpu, crs2s,
		sim_core_read_unaligned_8 (cpu, cpu->pc, read_map,
					   cpu->regs[crs1s]
					   + EXTRACT_RVC_LD_IMM (iw)));
	      return pc;
	    }
	  else
	    {
	      /* rv128, c.lq instruction.  */
	      TRACE_INSN (cpu, "UNHANDLED RV128 INSN: %s", op->name);
	      sim_engine_halt (sd, cpu, NULL, cpu->pc,
			       sim_signalled, SIM_SIGILL);
	    }
	case MATCH_C_FLW:
	  /* rv32: c.flw, rv64: c.ld.  */
	  if (RISCV_XLEN (cpu) == 32)
	    {
	      TRACE_INSN (cpu, "c.flw %s, %" PRIiTW "(%s);"
			       " // *(%s + %" PRIiTW ") = %s",
			  fcrs2s_name, EXTRACT_RVC_LW_IMM (iw), crs1s_name,
			  crs1s_name, EXTRACT_RVC_LW_IMM (iw), fcrs2s_name);
	      store_frd (cpu, crs2s, EXTEND32 (
		sim_core_read_unaligned_4 (cpu, cpu->pc, read_map,
					   cpu->regs[crs1s]
					   + EXTRACT_RVC_LW_IMM (iw))));
	    }
	  else
	    {
	      TRACE_INSN (cpu, "c.ld %s, %" PRIiTW "(%s);"
			       " // *(%s + %" PRIiTW ") = %s",
			  crs2s_name, EXTRACT_RVC_LD_IMM (iw), crs1s_name,
			  crs1s_name, EXTRACT_RVC_LD_IMM (iw), crs2s_name);
	      store_rd (cpu, crs2s,
		sim_core_read_unaligned_8 (cpu, cpu->pc, read_map,
					   cpu->regs[crs1s]
					   + EXTRACT_RVC_LD_IMM (iw)));
	    }
	  return pc;
	case MATCH_C_FSD:
	  if (RISCV_XLEN (cpu) <= 64)
	    {
	      /* rv32/64, c.fsd instruction.  */
	      TRACE_INSN (cpu, "c.fsd %s, %" PRIiTW "(%s);"
			       " // *(%s + %" PRIiTW ") = %s",
			  fcrs2s_name, EXTRACT_RVC_LD_IMM (iw), crs1s_name,
			  crs1s_name, EXTRACT_RVC_LD_IMM (iw), fcrs2s_name);
	      sim_core_write_unaligned_8 (cpu, cpu->pc, write_map,
					  cpu->regs[crs1s]
					  + EXTRACT_RVC_LD_IMM (iw),
					  cpu->fpregs[crs2s].v[0]);
	      return pc;
	    }
	  else
	    {
	      /* rv128, c.sq instruction.  */
	      TRACE_INSN (cpu, "UNHANDLED RV128 INSN: %s", op->name);
	      sim_engine_halt (sd, cpu, NULL, cpu->pc,
			       sim_signalled, SIM_SIGILL);
	    }
	case MATCH_C_FSW:
	  /* rv32: c.fsw, rv64: c.sd.  */
	  if (RISCV_XLEN (cpu) == 32)
	    {
	      TRACE_INSN (cpu, "c.fsw %s, %" PRIiTW "(%s);"
			       " // *(%s + %" PRIiTW ") = %s",
			  fcrs2s_name, EXTRACT_RVC_LW_IMM (iw), crs1s_name,
			  crs1s_name, EXTRACT_RVC_LW_IMM (iw), fcrs2s_name);
	      sim_core_write_unaligned_4 (cpu, cpu->pc, write_map,
					  cpu->regs[crs1s]
					  + EXTRACT_RVC_LW_IMM (iw),
					  cpu->fpregs[crs2s].w[0]);
	    }
	  else
	    {
	      TRACE_INSN (cpu, "c.sd %s, %" PRIiTW "(%s);"
			       " // *(%s + %" PRIiTW ") = %s",
			  crs2s_name, EXTRACT_RVC_LD_IMM (iw), crs1s_name,
			  crs1s_name, EXTRACT_RVC_LD_IMM (iw), crs2s_name);
	      sim_core_write_unaligned_8 (cpu, cpu->pc, write_map,
					  cpu->regs[crs1s]
					  + EXTRACT_RVC_LD_IMM (iw),
					  cpu->regs[crs2s]);
	    }
	  return pc;
	default:
	  TRACE_INSN (cpu, "UNHANDLED INSN: %s", op->name);
	  sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL);
	}
    case 1:
      switch (op->match)
	{
	case MATCH_C_ADDI:
	  if (rd != 0)
	    {
	      /* c.addi */
	      TRACE_INSN (cpu, "c.addi %s, %" PRIiTW " // %s += %" PRIiTW,
			  rd_name, rvc_imm, rd_name, rvc_imm);
	      store_rd (cpu, rd, cpu->regs[rd] + rvc_imm);
	      return pc;
	    }
	  else
	    {
	      /* c.nop */
	      TRACE_INSN (cpu, "c.nop");
	      return pc;
	    }
	case MATCH_C_JAL:
	  /* In rv32 is c.jal, rv64 c.addiw.  */
	  if (RISCV_XLEN (cpu) == 32)
	    {
	      TRACE_INSN (cpu, "c.jal %" PRIiTW, EXTRACT_RVC_J_IMM (iw));
	      store_rd (cpu, X_RA, cpu->pc + 2);
	      pc = cpu->pc + EXTRACT_RVC_J_IMM (iw);
	    }
	  else
	    {
	      TRACE_INSN (cpu, "c.addiw %s, %" PRIiTW " // %s += %" PRIiTW,
			  rd_name, rvc_imm, rd_name, rvc_imm);
	      store_rd (cpu, rd, EXTEND32 (cpu->regs[rd] + rvc_imm));
	    }
	  return pc;
	case MATCH_C_LI:
	  TRACE_INSN (cpu, "c.li %s, %" PRIiTW " // %s = %" PRIiTW,
		      rd_name, rvc_imm, rd_name, rvc_imm);
	  store_rd (cpu, rd, rvc_imm);
	  return pc;
	case MATCH_C_ADDI16SP:
	  TRACE_INSN (cpu, "c.addi16sp %s, %" PRIiTW,
		      rd_name, rvc_imm);
	  store_rd (cpu, rd, cpu->sp + EXTRACT_RVC_ADDI16SP_IMM (iw));
	  return pc;
	case MATCH_C_SRLI:
	  /* rv32: c.srli, rv128: c.srli64.  */
	  TRACE_INSN (cpu, "c.srli %s, %" PRIiTW,
		      crs1s_name, EXTRACT_RVC_IMM (iw));
	  if (RISCV_XLEN (cpu) == 32 && EXTRACT_RVC_IMM (iw) > 0x1f)
	    sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL);
	  store_rd (cpu, crs1s, cpu->regs[crs1s] >> EXTRACT_RVC_IMM (iw));
	  return pc;
	case MATCH_C_SRAI:
	  /* rv32: c.srli, rv128: c.srli64.  */
	  TRACE_INSN (cpu, "c.srai %s, %" PRIiTW,
		      crs1s_name, EXTRACT_RVC_IMM (iw));
	  if (RISCV_XLEN (cpu) == 32)
	    {
	      if (EXTRACT_RVC_IMM (iw) > 0x1f)
		sim_engine_halt (sd, cpu, NULL, cpu->pc,
				 sim_signalled, SIM_SIGILL);
	      tmp = ashiftrt (cpu->regs[crs1s], EXTRACT_RVC_IMM (iw));
	    }
	  else
	    tmp = ashiftrt64 (cpu->regs[crs1s], EXTRACT_RVC_IMM (iw));
	  store_rd (cpu, crs1s, tmp);
	  return pc;
	case MATCH_C_ANDI:
	  TRACE_INSN (cpu, "c.andi %s, %" PRIiTW,
		      crs1s_name, EXTRACT_RVC_IMM (iw));
	  store_rd (cpu, crs1s, cpu->regs[crs1s] & EXTRACT_RVC_IMM (iw));
	  return pc;
	case MATCH_C_SUB:
	  TRACE_INSN (cpu, "c.sub %s, %s",
		      crs1s_name, crs2s_name);
	  store_rd (cpu, crs1s, cpu->regs[crs1s] - cpu->regs[crs2s]);
	  return pc;
	case MATCH_C_XOR:
	  TRACE_INSN (cpu, "c.xor %s, %s",
		      crs1s_name, crs2s_name);
	  store_rd (cpu, crs1s, cpu->regs[crs1s] ^ cpu->regs[crs2s]);
	  return pc;
	case MATCH_C_OR:
	  TRACE_INSN (cpu, "c.or %s, %s",
		      crs1s_name, crs2s_name);
	  store_rd (cpu, crs1s, cpu->regs[crs1s] | cpu->regs[crs2s]);
	  return pc;
	case MATCH_C_AND:
	  TRACE_INSN (cpu, "c.and %s, %s",
		      crs1s_name, crs2s_name);
	  store_rd (cpu, crs1s, cpu->regs[crs1s] & cpu->regs[crs2s]);
	  return pc;
	case MATCH_C_SUBW:
	  TRACE_INSN (cpu, "c.subw %s, %s",
		      crs1s_name, crs2s_name);
	  RISCV_ASSERT_RV64 (cpu, "insn: %s", op->name);
	  store_rd (cpu, crs1s, EXTEND32 (cpu->regs[crs1s] - cpu->regs[crs2s]));
	  return pc;
	case MATCH_C_ADDW:
	  TRACE_INSN (cpu, "c.addw %s, %s",
		      crs1s_name, crs2s_name);
	  RISCV_ASSERT_RV64 (cpu, "insn: %s", op->name);
	  store_rd (cpu, crs1s, EXTEND32 (cpu->regs[crs1s] + cpu->regs[crs2s]));
	  return pc;
	case MATCH_C_BEQZ:
	  TRACE_INSN (cpu, "c.beqz %s, %" PRIiTW,
		      crs1s_name, cpu->pc + EXTRACT_RVC_B_IMM (iw));
	  if (cpu->regs[crs1s] == 0)
	    pc = cpu->pc + EXTRACT_RVC_B_IMM (iw);
	  return pc;
	case MATCH_C_BNEZ:
	  TRACE_INSN (cpu, "c.bnez %s, %" PRIiTW,
		      crs1s_name, cpu->pc + EXTRACT_RVC_B_IMM (iw));
	  if (cpu->regs[crs1s] != 0)
	    pc = cpu->pc + EXTRACT_RVC_B_IMM (iw);
	  return pc;
	case MATCH_C_LUI:
	  TRACE_INSN (cpu, "c.lui %s, %" PRIiTW,
		      rd_name, EXTRACT_RVC_LUI_IMM (iw));
	  store_rd (cpu, rd, EXTRACT_RVC_LUI_IMM (iw));
	  return pc;
	case MATCH_C_J:
	  TRACE_INSN (cpu, "c.j %" PRIiTW,
		      cpu->pc + EXTRACT_RVC_B_IMM (iw));
	  pc = cpu->pc + EXTRACT_RVC_J_IMM (iw);
	  return pc;
	default:
	  TRACE_INSN (cpu, "UNHANDLED INSN: %s", op->name);
	  sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL);
	}
    case 2:
      switch (op->match)
	{
	case MATCH_C_SLLI:
	  TRACE_INSN (cpu, "c.slli %s, %" PRIiTW,
		      rd_name, rvc_imm);
	  /* rv32: c.slli, rv128: c.slli64.  */
	  if (RISCV_XLEN (cpu) == 32 && rvc_imm > 0x1f)
	    sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL);
	  store_rd (cpu, rd, cpu->regs[rd] << rvc_imm);
	  return pc;
	case MATCH_C_LWSP:
	  TRACE_INSN (cpu, "c.lwsp %s, %" PRIiTW "(sp);"
			   " // %s = *(sp + %" PRIiTW ")",
		      rd_name, EXTRACT_RVC_LWSP_IMM (iw),
		      rd_name, EXTRACT_RVC_LWSP_IMM (iw));
	  store_rd (cpu, rd, EXTEND32 (
	    sim_core_read_unaligned_4 (cpu, cpu->pc, read_map,
				       cpu->sp
				       + EXTRACT_RVC_LWSP_IMM (iw))));
	  return pc;
	case MATCH_C_SWSP:
	  TRACE_INSN (cpu, "c.swsp %s, %" PRIiTW "(sp);"
			   " // *(sp + %" PRIiTW ") = %s",
		      rd_name, EXTRACT_RVC_SWSP_IMM (iw),
		      EXTRACT_RVC_SWSP_IMM (iw), rd_name);
	  sim_core_write_unaligned_4 (cpu, cpu->pc, write_map,
				      (cpu->sp + EXTRACT_RVC_SWSP_IMM (iw)),
				      cpu->regs[crs2]);
	  return pc;
	case MATCH_C_ADD:
	  TRACE_INSN (cpu, "c.add %s, %s // %s += %s",
		      rd_name, crs2_name,
		      rd_name, crs2_name);
	  store_rd (cpu, rd, cpu->regs[rd] + cpu->regs[crs2]);
	  return pc;
	case MATCH_C_FLDSP:
	  /* rv32/64: c.fldsp, rv128: c.flqsp.  */
	  if (RISCV_XLEN (cpu) <= 64)
	    {
	      TRACE_INSN (cpu, "c.fldsp %s, %" PRIiTW "(sp);"
			       " // %s = *(sp + %" PRIiTW ")",
			  frd_name, EXTRACT_RVC_LDSP_IMM (iw),
			  frd_name, EXTRACT_RVC_LDSP_IMM (iw));
	      store_frd64 (cpu, rd,
		sim_core_read_unaligned_8 (cpu, cpu->pc, read_map,
					   cpu->sp
					   + EXTRACT_RVC_LDSP_IMM (iw)));
	      return pc;
	    }
	  else
	    {
	      TRACE_INSN (cpu, "UNHANDLED RV128 INSN: %s", op->name);
	      sim_engine_halt (sd, cpu, NULL, cpu->pc,
			       sim_signalled, SIM_SIGILL);
	    }
	case MATCH_C_FLWSP:
	  /* rv32: c.flwsp, rv64: c.ldsp.  */
	  if (RISCV_XLEN (cpu) == 32)
	    {
	      TRACE_INSN (cpu, "c.flwsp %s, %" PRIiTW "(sp);"
			       " // %s = *(sp + %" PRIiTW ")",
			  frd_name, EXTRACT_RVC_LWSP_IMM (iw),
			  frd_name, EXTRACT_RVC_LWSP_IMM (iw));
	      store_frd (cpu, rd, EXTEND32 (
		sim_core_read_unaligned_4 (cpu, cpu->pc, read_map,
					   cpu->sp
					   + EXTRACT_RVC_LWSP_IMM (iw))));
	    }
	  else
	    {
	      TRACE_INSN (cpu, "c.ldsp %s, %" PRIiTW "(sp);"
			       " // %s = *(sp + %" PRIiTW ")",
			  rd_name, EXTRACT_RVC_LDSP_IMM (iw),
			  rd_name, EXTRACT_RVC_LDSP_IMM (iw));
	      store_rd (cpu, rd,
		sim_core_read_unaligned_8 (cpu, cpu->pc, read_map,
					   cpu->sp
					   + EXTRACT_RVC_LDSP_IMM (iw)));
	    }
	  return pc;
	case MATCH_C_FSDSP:
	  /* rv32/64: c.fsdsp, rv128: c.fsqsp.  */
	  if (RISCV_XLEN (cpu) <= 64)
	    {
	      TRACE_INSN (cpu, "c.fsdsp %s, %" PRIiTW "(sp);"
			       " // *(sp + %" PRIiTW ") = %s",
			  fcrs2_name, EXTRACT_RVC_LDSP_IMM (iw),
			  EXTRACT_RVC_LDSP_IMM (iw), fcrs2_name);
	      sim_core_write_unaligned_8 (cpu, cpu->pc, write_map,
					  cpu->sp + EXTRACT_RVC_SDSP_IMM (iw),
					  cpu->fpregs[crs2].v[0]);
	      return pc;
	    }
	  else
	    {
	      TRACE_INSN (cpu, "UNHANDLED RV128 INSN: %s", op->name);
	      sim_engine_halt (sd, cpu, NULL, cpu->pc,
			       sim_signalled, SIM_SIGILL);
	    }
	case MATCH_C_FSWSP:
	  /* rv32: c.fswsp, rv64: c.sdsp.  */
	  if (RISCV_XLEN (cpu) == 32)
	    {
	      TRACE_INSN (cpu, "c.fswsp %s, %" PRIiTW "(sp);"
			       " // *(sp + %" PRIiTW ") = %s",
			  fcrs2_name, EXTRACT_RVC_SWSP_IMM (iw),
			  EXTRACT_RVC_SWSP_IMM (iw), fcrs2_name);
	      sim_core_write_unaligned_4 (cpu, cpu->pc, write_map,
					  cpu->sp
					  + EXTRACT_RVC_SWSP_IMM (iw),
					  cpu->fpregs[crs2].w[0]);
	    }
	  else
	    {
	      TRACE_INSN (cpu, "c.sdsp %s, %" PRIiTW "(sp);"
			       " // *(sp + %" PRIiTW ") = %s",
			  crs2_name, EXTRACT_RVC_SDSP_IMM (iw),
			  EXTRACT_RVC_SDSP_IMM (iw), crs2_name);
	      sim_core_write_unaligned_8 (cpu, cpu->pc, write_map,
					  cpu->sp + EXTRACT_RVC_SDSP_IMM (iw),
					  cpu->regs[crs2]);
	    }
	  return pc;
	default:
	  TRACE_INSN (cpu, "UNHANDLED INSN: %s", op->name);
	  sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL);
	}
    default:
      TRACE_INSN (cpu, "UNHANDLED INSN: %s", op->name);
      sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL);
    }

  return pc;
}

static sim_cia
execute_i (SIM_CPU *cpu, unsigned_word iw, const struct riscv_opcode *op)
{
  SIM_DESC sd = CPU_STATE (cpu);
  int rd = (iw >> OP_SH_RD) & OP_MASK_RD;
  int rs1 = (iw >> OP_SH_RS1) & OP_MASK_RS1;
  int rs2 = (iw >> OP_SH_RS2) & OP_MASK_RS2;
  const char *rd_name = riscv_gpr_names_abi[rd];
  const char *rs1_name = riscv_gpr_names_abi[rs1];
  const char *rs2_name = riscv_gpr_names_abi[rs2];
  unsigned int csr = (iw >> OP_SH_CSR) & OP_MASK_CSR;
  unsigned_word i_imm = EXTRACT_ITYPE_IMM (iw);
  unsigned_word u_imm = EXTRACT_UTYPE_IMM ((unsigned64) iw);
  unsigned_word s_imm = EXTRACT_STYPE_IMM (iw);
  unsigned_word sb_imm = EXTRACT_SBTYPE_IMM (iw);
  unsigned_word shamt_imm = ((iw >> OP_SH_SHAMT) & OP_MASK_SHAMT);
  unsigned_word tmp;
  sim_cia pc = cpu->pc + 4;

  TRACE_EXTRACT (cpu, "rd:%-2i:%-4s  rs1:%-2i:%-4s %0*"PRIxTW"  rs2:%-2i:%-4s %0*"PRIxTW"  match:%#x mask:%#x",
		 rd, rd_name,
		 rs1, rs1_name, (int)sizeof (unsigned_word) * 2, cpu->regs[rs1],
		 rs2, rs2_name, (int)sizeof (unsigned_word) * 2, cpu->regs[rs2],
		 (unsigned) op->match, (unsigned) op->mask);

  switch (op->match)
    {
    case MATCH_ADD:
      TRACE_INSN (cpu, "add %s, %s, %s;  // %s = %s + %s",
		  rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
      store_rd (cpu, rd, cpu->regs[rs1] + cpu->regs[rs2]);
      break;
    case MATCH_ADDW:
      TRACE_INSN (cpu, "addw %s, %s, %s;  // %s = %s + %s",
		  rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
      RISCV_ASSERT_RV64 (cpu, "insn: %s", op->name);
      store_rd (cpu, rd, EXTEND32 (cpu->regs[rs1] + cpu->regs[rs2]));
      break;
    case MATCH_ADDI:
      TRACE_INSN (cpu, "addi %s, %s, %#"PRIxTW";  // %s = %s + %#" PRIxTW,
		  rd_name, rs1_name, i_imm, rd_name, rs1_name, i_imm);
      store_rd (cpu, rd, cpu->regs[rs1] + i_imm);
      break;
    case MATCH_ADDIW:
      TRACE_INSN (cpu, "addiw %s, %s, %#" PRIxTW ";  // %s = %s + %#" PRIxTW,
		  rd_name, rs1_name, i_imm, rd_name, rs1_name, i_imm);
      RISCV_ASSERT_RV64 (cpu, "insn: %s", op->name);
      store_rd (cpu, rd, EXTEND32 (cpu->regs[rs1] + i_imm));
      break;
    case MATCH_AND:
      TRACE_INSN (cpu, "and %s, %s, %s;  // %s = %s & %s",
		  rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
      store_rd (cpu, rd, cpu->regs[rs1] & cpu->regs[rs2]);
      break;
    case MATCH_ANDI:
      TRACE_INSN (cpu, "andi %s, %s, %" PRIiTW ";  // %s = %s & %#" PRIxTW,
		  rd_name, rs1_name, i_imm, rd_name, rs1_name, i_imm);
      store_rd (cpu, rd, cpu->regs[rs1] & i_imm);
      break;
    case MATCH_OR:
      TRACE_INSN (cpu, "or %s, %s, %s;  // %s = %s | %s",
		  rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
      store_rd (cpu, rd, cpu->regs[rs1] | cpu->regs[rs2]);
      break;
    case MATCH_ORI:
      TRACE_INSN (cpu, "ori %s, %s, %" PRIiTW ";  // %s = %s | %#" PRIxTW,
		  rd_name, rs1_name, i_imm, rd_name, rs1_name, i_imm);
      store_rd (cpu, rd, cpu->regs[rs1] | i_imm);
      break;
    case MATCH_XOR:
      TRACE_INSN (cpu, "xor %s, %s, %s;  // %s = %s ^ %s",
		  rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
      store_rd (cpu, rd, cpu->regs[rs1] ^ cpu->regs[rs2]);
      break;
    case MATCH_XORI:
      TRACE_INSN (cpu, "xori %s, %s, %" PRIiTW ";  // %s = %s ^ %#" PRIxTW,
		  rd_name, rs1_name, i_imm, rd_name, rs1_name, i_imm);
      store_rd (cpu, rd, cpu->regs[rs1] ^ i_imm);
      break;
    case MATCH_SUB:
      TRACE_INSN (cpu, "sub %s, %s, %s;  // %s = %s - %s",
		  rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
      store_rd (cpu, rd, cpu->regs[rs1] - cpu->regs[rs2]);
      break;
    case MATCH_SUBW:
      TRACE_INSN (cpu, "subw %s, %s, %s;  // %s = %s - %s",
		  rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
      RISCV_ASSERT_RV64 (cpu, "insn: %s", op->name);
      store_rd (cpu, rd, EXTEND32 (cpu->regs[rs1] - cpu->regs[rs2]));
      break;
    case MATCH_LUI:
      TRACE_INSN (cpu, "lui %s, %#"PRIxTW";", rd_name, u_imm);
      store_rd (cpu, rd, u_imm);
      break;
    case MATCH_SLL:
      TRACE_INSN (cpu, "sll %s, %s, %s;  // %s = %s << %s",
		  rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
      u_imm = RISCV_XLEN (cpu) == 32 ? 0x1f : 0x3f;
      store_rd (cpu, rd, cpu->regs[rs1] << (cpu->regs[rs2] & u_imm));
      break;
    case MATCH_SLLW:
      TRACE_INSN (cpu, "sllw %s, %s, %s;  // %s = %s << %s",
		  rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
      RISCV_ASSERT_RV64 (cpu, "insn: %s", op->name);
      store_rd (cpu, rd, EXTEND32 ((unsigned32)cpu->regs[rs1] << (cpu->regs[rs2] & 0x1f)));
      break;
    case MATCH_SLLI:
      TRACE_INSN (cpu, "slli %s, %s, %" PRIiTW ";  // %s = %s << %#" PRIxTW,
		  rd_name, rs1_name, shamt_imm, rd_name, rs1_name, shamt_imm);
      if (RISCV_XLEN (cpu) == 32 && shamt_imm > 0x1f)
	sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL);
      store_rd (cpu, rd, cpu->regs[rs1] << shamt_imm);
      break;
    case MATCH_SLLIW:
      TRACE_INSN (cpu, "slliw %s, %s, %" PRIiTW ";  // %s = %s << %#" PRIxTW,
		  rd_name, rs1_name, shamt_imm, rd_name, rs1_name, shamt_imm);
      RISCV_ASSERT_RV64 (cpu, "insn: %s", op->name);
      store_rd (cpu, rd, EXTEND32 ((unsigned32)cpu->regs[rs1] << shamt_imm));
      break;
    case MATCH_SRL:
      TRACE_INSN (cpu, "srl %s, %s, %s;  // %s = %s >> %s",
		  rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
      u_imm = RISCV_XLEN (cpu) == 32 ? 0x1f : 0x3f;
      store_rd (cpu, rd, cpu->regs[rs1] >> (cpu->regs[rs2] & u_imm));
      break;
    case MATCH_SRLW:
      TRACE_INSN (cpu, "srlw %s, %s, %s;  // %s = %s >> %s",
		  rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
      RISCV_ASSERT_RV64 (cpu, "insn: %s", op->name);
      store_rd (cpu, rd, EXTEND32 ((unsigned32)cpu->regs[rs1] >> (cpu->regs[rs2] & 0x1f)));
      break;
    case MATCH_SRLI:
      TRACE_INSN (cpu, "srli %s, %s, %" PRIiTW ";  // %s = %s >> %#" PRIxTW,
		  rd_name, rs1_name, shamt_imm, rd_name, rs1_name, shamt_imm);
      if (RISCV_XLEN (cpu) == 32 && shamt_imm > 0x1f)
	sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL);
      store_rd (cpu, rd, cpu->regs[rs1] >> shamt_imm);
      break;
    case MATCH_SRLIW:
      TRACE_INSN (cpu, "srliw %s, %s, %" PRIiTW ";  // %s = %s >> %#" PRIxTW,
		  rd_name, rs1_name, shamt_imm, rd_name, rs1_name, shamt_imm);
      RISCV_ASSERT_RV64 (cpu, "insn: %s", op->name);
      store_rd (cpu, rd, EXTEND32 ((unsigned32)cpu->regs[rs1] >> shamt_imm));
      break;
    case MATCH_SRA:
      TRACE_INSN (cpu, "sra %s, %s, %s;  // %s = %s >>> %s",
		  rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
      if (RISCV_XLEN (cpu) == 32)
	tmp = ashiftrt (cpu->regs[rs1], cpu->regs[rs2] & 0x1f);
      else
	tmp = ashiftrt64 (cpu->regs[rs1], cpu->regs[rs2] & 0x3f);
      store_rd (cpu, rd, tmp);
      break;
    case MATCH_SRAW:
      TRACE_INSN (cpu, "sraw %s, %s, %s;  // %s = %s >>> %s",
		  rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
      RISCV_ASSERT_RV64 (cpu, "insn: %s", op->name);
      store_rd (cpu, rd, EXTEND32 (ashiftrt ((signed32)cpu->regs[rs1], cpu->regs[rs2] & 0x1f)));
      break;
    case MATCH_SRAI:
      TRACE_INSN (cpu, "srai %s, %s, %" PRIiTW ";  // %s = %s >>> %#" PRIxTW,
		  rd_name, rs1_name, shamt_imm, rd_name, rs1_name, shamt_imm);
      if (RISCV_XLEN (cpu) == 32)
	{
	  if (shamt_imm > 0x1f)
	    sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL);
	  tmp = ashiftrt (cpu->regs[rs1], shamt_imm);
	}
      else
	tmp = ashiftrt64 (cpu->regs[rs1], shamt_imm);
      store_rd (cpu, rd, tmp);
      break;
    case MATCH_SRAIW:
      TRACE_INSN (cpu, "sraiw %s, %s, %" PRIiTW ";  // %s = %s >>> %#" PRIxTW,
		  rd_name, rs1_name, shamt_imm, rd_name, rs1_name, shamt_imm);
      RISCV_ASSERT_RV64 (cpu, "insn: %s", op->name);
      store_rd (cpu, rd,
		EXTEND32 (ashiftrt ((signed32)cpu->regs[rs1], shamt_imm)));
      break;
    case MATCH_SLT:
      TRACE_INSN (cpu, "slt %s, %s, %s", rd_name, rs1_name, rs2_name);
      store_rd (cpu, rd,
		!!((signed_word)cpu->regs[rs1] < (signed_word)cpu->regs[rs2]));
      break;
    case MATCH_SLTU:
      TRACE_INSN (cpu, "sltu %s, %s, %s", rd_name, rs1_name, rs2_name);
      store_rd (cpu, rd,
		!!((unsigned_word)cpu->regs[rs1]
		   < (unsigned_word)cpu->regs[rs2]));
      break;
    case MATCH_SLTI:
      TRACE_INSN (cpu, "slti %s, %s, %" PRIiTW,
		  rd_name, rs1_name, i_imm);
      store_rd (cpu, rd, !!((signed_word)cpu->regs[rs1] < (signed_word)i_imm));
      break;
    case MATCH_SLTIU:
      TRACE_INSN (cpu, "sltiu %s, %s, %" PRIiTW,
		  rd_name, rs1_name, i_imm);
      store_rd (cpu, rd,
		!!((unsigned_word)cpu->regs[rs1] < (unsigned_word)i_imm));
      break;
    case MATCH_AUIPC:
      TRACE_INSN (cpu, "auipc %s, %" PRIiTW ";  // %s = pc + %" PRIiTW,
		  rd_name, u_imm, rd_name, u_imm);
      store_rd (cpu, rd, cpu->pc + u_imm);
      break;
    case MATCH_BEQ:
      TRACE_INSN (cpu, "beq %s, %s, %#" PRIxTW ";  // if (%s == %s) goto %#" PRIxTW,
		  rs1_name, rs2_name, sb_imm, rs1_name, rs2_name, cpu->pc + sb_imm);
      if (cpu->regs[rs1] == cpu->regs[rs2])
	{
	  pc = cpu->pc + sb_imm;
	  TRACE_BRANCH (cpu, "to %#" PRIxTW, pc);
	}
      break;
    case MATCH_BLT:
      TRACE_INSN (cpu, "blt %s, %s, %#" PRIxTW ";  // if (%s < %s) goto %#" PRIxTW,
		  rs1_name, rs2_name, sb_imm, rs1_name, rs2_name, cpu->pc + sb_imm);
      if ((signed_word)cpu->regs[rs1] < (signed_word)cpu->regs[rs2])
	{
	  pc = cpu->pc + sb_imm;
	  TRACE_BRANCH (cpu, "to %#" PRIxTW, pc);
	}
      break;
    case MATCH_BLTU:
      TRACE_INSN (cpu, "bltu %s, %s, %#" PRIxTW ";  // if (%s < %s) goto %#" PRIxTW,
		  rs1_name, rs2_name, sb_imm, rs1_name, rs2_name, cpu->pc + sb_imm);
      if ((unsigned_word)cpu->regs[rs1] < (unsigned_word)cpu->regs[rs2])
	{
	  pc = cpu->pc + sb_imm;
	  TRACE_BRANCH (cpu, "to %#" PRIxTW, pc);
	}
      break;
    case MATCH_BGE:
      TRACE_INSN (cpu, "bge %s, %s, %#" PRIxTW ";  // if (%s >= %s) goto %#" PRIxTW,
		  rs1_name, rs2_name, sb_imm, rs1_name, rs2_name, cpu->pc + sb_imm);
      if ((signed_word)cpu->regs[rs1] >= (signed_word)cpu->regs[rs2])
	{
	  pc = cpu->pc + sb_imm;
	  TRACE_BRANCH (cpu, "to %#" PRIxTW, pc);
	}
      break;
    case MATCH_BGEU:
      TRACE_INSN (cpu, "bgeu %s, %s, %#" PRIxTW ";  // if (%s >= %s) goto %#" PRIxTW,
		  rs1_name, rs2_name, sb_imm, rs1_name, rs2_name, cpu->pc + sb_imm);
      if ((unsigned_word)cpu->regs[rs1] >= (unsigned_word)cpu->regs[rs2])
	{
	  pc = cpu->pc + sb_imm;
	  TRACE_BRANCH (cpu, "to %#" PRIxTW, pc);
	}
      break;
    case MATCH_BNE:
      TRACE_INSN (cpu, "bne %s, %s, %#" PRIxTW ";  // if (%s != %s) goto %#" PRIxTW,
		  rs1_name, rs2_name, sb_imm, rs1_name, rs2_name, cpu->pc + sb_imm);
      if (cpu->regs[rs1] != cpu->regs[rs2])
	{
	  pc = cpu->pc + sb_imm;
	  TRACE_BRANCH (cpu, "to %#" PRIxTW, pc);
	}
      break;
    case MATCH_JAL:
      TRACE_INSN (cpu, "jal %s, %" PRIiTW ";", rd_name, EXTRACT_UJTYPE_IMM (iw));
      pc = cpu->pc + EXTRACT_UJTYPE_IMM (iw);
      store_rd (cpu, rd, cpu->pc + 4);
      TRACE_BRANCH (cpu, "to %#" PRIxTW, pc);
      break;
    case MATCH_JALR:
      TRACE_INSN (cpu, "jalr %s, %s, %" PRIiTW ";", rd_name, rs1_name, i_imm);
      pc = cpu->regs[rs1] + i_imm;
      store_rd (cpu, rd, cpu->pc + 4);
      TRACE_BRANCH (cpu, "to %#" PRIxTW, pc);
      break;

    case MATCH_LD:
      TRACE_INSN (cpu, "ld %s, %" PRIiTW "(%s); // ",
		  rd_name, i_imm, rs1_name);
      RISCV_ASSERT_RV64 (cpu, "insn: %s", op->name);
      store_rd (cpu, rd,
	sim_core_read_unaligned_8 (cpu, cpu->pc, read_map,
				   cpu->regs[rs1] + i_imm));
      break;
    case MATCH_LW:
      TRACE_INSN (cpu, "lw %s, %" PRIiTW "(%s); // ",
		  rd_name, i_imm, rs1_name);
      store_rd (cpu, rd, EXTEND32 (
	sim_core_read_unaligned_4 (cpu, cpu->pc, read_map,
				   cpu->regs[rs1] + i_imm)));
      break;
    case MATCH_LWU:
      TRACE_INSN (cpu, "lwu %s, %" PRIiTW "(%s); // ",
		  rd_name, i_imm, rs1_name);
      store_rd (cpu, rd,
	sim_core_read_unaligned_4 (cpu, cpu->pc, read_map,
				   cpu->regs[rs1] + i_imm));
      break;
    case MATCH_LH:
      TRACE_INSN (cpu, "lh %s, %" PRIiTW "(%s); // ",
		  rd_name, i_imm, rs1_name);
      store_rd (cpu, rd, EXTEND16 (
	sim_core_read_unaligned_2 (cpu, cpu->pc, read_map,
				   cpu->regs[rs1] + i_imm)));
      break;
    case MATCH_LHU:
      TRACE_INSN (cpu, "lhu %s, %" PRIiTW "(%s); // ",
		  rd_name, i_imm, rs1_name);
      store_rd (cpu, rd,
	sim_core_read_unaligned_2 (cpu, cpu->pc, read_map,
				   cpu->regs[rs1] + i_imm));
      break;
    case MATCH_LB:
      TRACE_INSN (cpu, "lb %s, %" PRIiTW "(%s); // ",
		  rd_name, i_imm, rs1_name);
      store_rd (cpu, rd, EXTEND8 (
	sim_core_read_unaligned_1 (cpu, cpu->pc, read_map,
				   cpu->regs[rs1] + i_imm)));
      break;
    case MATCH_LBU:
      TRACE_INSN (cpu, "lbu %s, %" PRIiTW "(%s); // ",
		  rd_name, i_imm, rs1_name);
      store_rd (cpu, rd,
	sim_core_read_unaligned_1 (cpu, cpu->pc, read_map,
				   cpu->regs[rs1] + i_imm));
      break;
    case MATCH_SD:
      TRACE_INSN (cpu, "sd %s, %" PRIiTW "(%s); // ",
		  rs2_name, s_imm, rs1_name);
      RISCV_ASSERT_RV64 (cpu, "insn: %s", op->name);
      sim_core_write_unaligned_8 (cpu, cpu->pc, write_map,
				  cpu->regs[rs1] + s_imm, cpu->regs[rs2]);
      break;
    case MATCH_SW:
      TRACE_INSN (cpu, "sw %s, %" PRIiTW "(%s); // ",
		  rs2_name, s_imm, rs1_name);
      sim_core_write_unaligned_4 (cpu, cpu->pc, write_map,
				  cpu->regs[rs1] + s_imm, cpu->regs[rs2]);
      break;
    case MATCH_SH:
      TRACE_INSN (cpu, "sh %s, %" PRIiTW "(%s); // ",
		  rs2_name, s_imm, rs1_name);
      sim_core_write_unaligned_2 (cpu, cpu->pc, write_map,
				  cpu->regs[rs1] + s_imm, cpu->regs[rs2]);
      break;
    case MATCH_SB:
      TRACE_INSN (cpu, "sb %s, %" PRIiTW "(%s); // ",
		  rs2_name, s_imm, rs1_name);
      sim_core_write_unaligned_1 (cpu, cpu->pc, write_map,
				  cpu->regs[rs1] + s_imm, cpu->regs[rs2]);
      break;

    case MATCH_CSRRC:
      TRACE_INSN (cpu, "csrrc");
      switch (csr)
	{
#define DECLARE_CSR(name, num) \
	case num: \
	  store_rd (cpu, rd, fetch_csr (cpu, #name, num, &cpu->csr.name)); \
	  store_csr (cpu, #name, num, &cpu->csr.name, \
		     cpu->csr.name & !cpu->regs[rs1]); \
	  break;
#include "opcode/riscv-opc.h"
#undef DECLARE_CSR
	}
      break;
    case MATCH_CSRRCI:
      TRACE_INSN (cpu, "csrrci");
      switch (csr)
	{
#define DECLARE_CSR(name, num) \
	case num: \
	  store_rd (cpu, rd, fetch_csr (cpu, #name, num, &cpu->csr.name)); \
	  store_csr (cpu, #name, num, &cpu->csr.name, \
		     cpu->csr.name & !rs1); \
	  break;
#include "opcode/riscv-opc.h"
#undef DECLARE_CSR
	}
      break;
    case MATCH_CSRRS:
      TRACE_INSN (cpu, "csrrs");
      switch (csr)
	{
#define DECLARE_CSR(name, num) \
	case num: \
	  store_rd (cpu, rd, fetch_csr (cpu, #name, num, &cpu->csr.name)); \
	  store_csr (cpu, #name, num, &cpu->csr.name, \
		     cpu->csr.name | cpu->regs[rs1]); \
	  break;
#include "opcode/riscv-opc.h"
#undef DECLARE_CSR
	}
      break;
    case MATCH_CSRRSI:
      TRACE_INSN (cpu, "csrrsi");
      switch (csr)
	{
#define DECLARE_CSR(name, num) \
	case num: \
	  store_rd (cpu, rd, fetch_csr (cpu, #name, num, &cpu->csr.name)); \
	  store_csr (cpu, #name, num, &cpu->csr.name, \
		     cpu->csr.name | rs1); \
	  break;
#include "opcode/riscv-opc.h"
#undef DECLARE_CSR
	}
      break;
    case MATCH_CSRRW:
      TRACE_INSN (cpu, "csrrw");
      switch (csr)
	{
#define DECLARE_CSR(name, num) \
	case num: \
	  store_rd (cpu, rd, fetch_csr (cpu, #name, num, &cpu->csr.name)); \
	  store_csr (cpu, #name, num, &cpu->csr.name, cpu->regs[rs1]); \
	  break;
#include "opcode/riscv-opc.h"
#undef DECLARE_CSR
	}
      break;
    case MATCH_CSRRWI:
      TRACE_INSN (cpu, "csrrwi");
      switch (csr)
	{
#define DECLARE_CSR(name, num) \
	case num: \
	  store_rd (cpu, rd, fetch_csr (cpu, #name, num, &cpu->csr.name)); \
	  store_csr (cpu, #name, num, &cpu->csr.name, rs1); \
	  break;
#include "opcode/riscv-opc.h"
#undef DECLARE_CSR
	}
      break;

    case MATCH_RDCYCLE:
      TRACE_INSN (cpu, "rdcycle %s;", rd_name);
      store_rd (cpu, rd, fetch_csr (cpu, "cycle", CSR_CYCLE, &cpu->csr.cycle));
      break;
    case MATCH_RDCYCLEH:
      TRACE_INSN (cpu, "rdcycleh %s;", rd_name);
      RISCV_ASSERT_RV32 (cpu, "insn: %s", op->name);
      store_rd (cpu, rd, fetch_csr (cpu, "cycleh", CSR_CYCLEH, &cpu->csr.cycleh));
      break;
    case MATCH_RDINSTRET:
      TRACE_INSN (cpu, "rdinstret %s;", rd_name);
      store_rd (cpu, rd, fetch_csr (cpu, "instret", CSR_INSTRET, &cpu->csr.instret));
      break;
    case MATCH_RDINSTRETH:
      TRACE_INSN (cpu, "rdinstreth %s;", rd_name);
      RISCV_ASSERT_RV32 (cpu, "insn: %s", op->name);
      store_rd (cpu, rd, fetch_csr (cpu, "instreth", CSR_INSTRETH, &cpu->csr.instreth));
      break;
    case MATCH_RDTIME:
      TRACE_INSN (cpu, "rdtime %s;", rd_name);
      store_rd (cpu, rd, fetch_csr (cpu, "time", CSR_TIME, &cpu->csr.cycle));
      break;
    case MATCH_RDTIMEH:
      TRACE_INSN (cpu, "rdtimeh %s;", rd_name);
      RISCV_ASSERT_RV32 (cpu, "insn: %s", op->name);
      store_rd (cpu, rd, fetch_csr (cpu, "timeh", CSR_TIMEH, &cpu->csr.cycleh));
      break;

    case MATCH_FENCE:
      TRACE_INSN (cpu, "fence;");
      break;
    case MATCH_FENCE_I:
      TRACE_INSN (cpu, "fence.i;");
      break;
    case MATCH_SBREAK:
      TRACE_INSN (cpu, "sbreak;");
      /* GDB expects us to step over SBREAK.  */
      sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_stopped, SIM_SIGTRAP);
      break;
    case MATCH_ECALL:
      TRACE_INSN (cpu, "ecall;");
      if (cb_target_to_host_syscall (STATE_CALLBACK (sd), cpu->a7) == -1)
	{
	  host_callback *cb = STATE_CALLBACK (sd);
	  CB_SYSCALL sc;

	  CB_SYSCALL_INIT (&sc);

	  sc.arg1 = cpu->a0;
	  sc.arg2 = cpu->a1;
	  sc.arg3 = cpu->a2;
	  sc.arg4 = cpu->a3;
	  sc.func = cpu->a7;

	  sc.p1 = (PTR) sd;
	  sc.p2 = (PTR) cpu;
	  sc.read_mem = sim_syscall_read_mem;
	  sc.write_mem = sim_syscall_write_mem;

	  switch (cpu->a7)
	    {
#ifndef __MINGW32__
	    case TARGET_SYS_link:
	      {
		char oldpath[1024], newpath[1024];
		cb_get_string (cb, &sc, oldpath, sizeof (oldpath), sc.arg1);
		cb_get_string (cb, &sc, newpath, sizeof (newpath), sc.arg2);
		cpu->a0 = link (oldpath, newpath);
		break;
	      }
#endif
	    case TARGET_SYS_brk:
	      {
		/* FIXME: Check the invalid access.  */
		if (cpu->a0 == 0)
		  cpu->a0 = cpu->endbrk;
		else
		  {
		    if (cpu->a0 >= DEFAULT_MEM_SIZE)
		      cpu->a0 = -1;
		    else
		      cpu->endbrk = cpu->a0;
		  }
		break;
	      }
	    case TARGET_SYS_gettimeofday:
	      {
		int rv;
		struct timeval tv;

		rv = gettimeofday (&tv, 0);
		if (RISCV_XLEN (cpu) == 32)
		  {
		    sim_core_write_unaligned_4 (cpu, cpu->pc, write_map,
						cpu->a0, tv.tv_sec);
		    sim_core_write_unaligned_4 (cpu, cpu->pc, write_map,
						cpu->a0 + 4,
						tv.tv_usec);
		  }
		else
		  {
		    sim_core_write_unaligned_8 (cpu, cpu->pc, write_map,
						cpu->a0, tv.tv_sec);
		    sim_core_write_unaligned_8 (cpu, cpu->pc, write_map,
						cpu->a0 + 8,
						tv.tv_usec);
		  }

		cpu->a0 = rv;
		break;
	      }
	    default:
	      cpu->a0 = sim_syscall (cpu, cpu->a7, cpu->a0,
				     cpu->a1, cpu->a2, cpu->a3);
	      break;
	    }
	}
      else
	cpu->a0 = sim_syscall (cpu, cpu->a7, cpu->a0, cpu->a1, cpu->a2, cpu->a3);
      break;
    default:
      TRACE_INSN (cpu, "UNHANDLED INSN: %s", op->name);
      sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL);
    }

  return pc;
}

static unsigned64
mulhu (unsigned64 a, unsigned64 b)
{
  uint64_t t;
  uint32_t y1, y2, y3;
  uint64_t a0 = (uint32_t)a, a1 = a >> 32;
  uint64_t b0 = (uint32_t)b, b1 = b >> 32;

  t = a1*b0 + ((a0*b0) >> 32);
  y1 = t;
  y2 = t >> 32;

  t = a0*b1 + y1;
  y1 = t;

  t = a1*b1 + y2 + (t >> 32);
  y2 = t;
  y3 = t >> 32;

  return ((uint64_t)y3 << 32) | y2;
}

static unsigned64
mulh (signed64 a, signed64 b)
{
  int negate = (a < 0) != (b < 0);
  uint64_t res = mulhu (a < 0 ? -a : a, b < 0 ? -b : b);
  return negate ? ~res + (a * b == 0) : res;
}

static unsigned64
mulhsu (signed64 a, unsigned64 b)
{
  int negate = a < 0;
  uint64_t res = mulhu (a < 0 ? -a : a, b);
  return negate ? ~res + (a * b == 0) : res;
}

static sim_cia
execute_m (SIM_CPU *cpu, unsigned_word iw, const struct riscv_opcode *op)
{
  SIM_DESC sd = CPU_STATE (cpu);
  int rd = (iw >> OP_SH_RD) & OP_MASK_RD;
  int rs1 = (iw >> OP_SH_RS1) & OP_MASK_RS1;
  int rs2 = (iw >> OP_SH_RS2) & OP_MASK_RS2;
  const char *rd_name = riscv_gpr_names_abi[rd];
  const char *rs1_name = riscv_gpr_names_abi[rs1];
  const char *rs2_name = riscv_gpr_names_abi[rs2];
  unsigned_word tmp, dividend_max;
  signed_word dividend32_max;
  sim_cia pc = cpu->pc + 4;

  dividend_max = -((unsigned_word)1 << (WITH_TARGET_WORD_BITSIZE - 1));
  dividend32_max = INT32_MIN;

  switch (op->match)
    {
    case MATCH_DIV:
      TRACE_INSN (cpu, "div %s, %s, %s;  // %s = %s / %s",
		  rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
      if (cpu->regs[rs1] == dividend_max && cpu->regs[rs2] == -1)
	tmp = dividend_max;
      else if (cpu->regs[rs2])
	tmp = (signed_word)cpu->regs[rs1] / (signed_word)cpu->regs[rs2];
      else
	tmp = -1;
      store_rd (cpu, rd, tmp);
      break;
    case MATCH_DIVW:
      TRACE_INSN (cpu, "divw %s, %s, %s;  // %s = %s / %s",
		  rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
      RISCV_ASSERT_RV64 (cpu, "insn: %s", op->name);
      if (EXTEND32 (cpu->regs[rs1]) == dividend32_max
	  && EXTEND32 (cpu->regs[rs2]) == -1)
	tmp = 1 << 31;
      else if (EXTEND32 (cpu->regs[rs2]))
	tmp = EXTEND32 (cpu->regs[rs1]) / EXTEND32 (cpu->regs[rs2]);
      else
	tmp = -1;
      store_rd (cpu, rd, EXTEND32 (tmp));
      break;
    case MATCH_DIVU:
      TRACE_INSN (cpu, "divu %s, %s, %s;  // %s = %s / %s",
		  rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
      if (cpu->regs[rs2])
	store_rd (cpu, rd, (unsigned_word)cpu->regs[rs1]
			   / (unsigned_word)cpu->regs[rs2]);
      else
	store_rd (cpu, rd, -1);
      break;
    case MATCH_DIVUW:
      TRACE_INSN (cpu, "divuw %s, %s, %s;  // %s = %s / %s",
		  rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
      RISCV_ASSERT_RV64 (cpu, "insn: %s", op->name);
      if ((unsigned32)cpu->regs[rs2])
	tmp = (unsigned32)cpu->regs[rs1] / (unsigned32)cpu->regs[rs2];
      else
	tmp = -1;
      store_rd (cpu, rd, EXTEND32 (tmp));
      break;
    case MATCH_MUL:
      TRACE_INSN (cpu, "mul %s, %s, %s;  // %s = %s * %s",
		  rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
      store_rd (cpu, rd, cpu->regs[rs1] * cpu->regs[rs2]);
      break;
    case MATCH_MULW:
      TRACE_INSN (cpu, "mulw %s, %s, %s;  // %s = %s * %s",
		  rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
      RISCV_ASSERT_RV64 (cpu, "insn: %s", op->name);
      store_rd (cpu, rd, EXTEND32 ((signed32)cpu->regs[rs1]
				   * (signed32)cpu->regs[rs2]));
      break;
    case MATCH_MULH:
      TRACE_INSN (cpu, "mulh %s, %s, %s;  // %s = %s * %s",
		  rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
      if (RISCV_XLEN (cpu) == 32)
	store_rd (cpu, rd, ((signed64)(signed_word)cpu->regs[rs1]
			    * (signed64)(signed_word)cpu->regs[rs2]) >> 32);
      else
	store_rd (cpu, rd, mulh (cpu->regs[rs1], cpu->regs[rs2]));
      break;
    case MATCH_MULHU:
      TRACE_INSN (cpu, "mulhu %s, %s, %s;  // %s = %s * %s",
		  rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
      if (RISCV_XLEN (cpu) == 32)
	store_rd (cpu, rd, ((unsigned64)cpu->regs[rs1]
			    * (unsigned64)cpu->regs[rs2]) >> 32);
      else
	store_rd (cpu, rd, mulhu (cpu->regs[rs1], cpu->regs[rs2]));
      break;
    case MATCH_MULHSU:
      TRACE_INSN (cpu, "mulhsu %s, %s, %s;  // %s = %s * %s",
		  rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
      if (RISCV_XLEN (cpu) == 32)
	store_rd (cpu, rd, ((signed64)(signed_word)cpu->regs[rs1]
			    * (unsigned64)cpu->regs[rs2]) >> 32);
      else
	store_rd (cpu, rd, mulhsu (cpu->regs[rs1], cpu->regs[rs2]));
      break;
    case MATCH_REM:
      TRACE_INSN (cpu, "rem %s, %s, %s;  // %s = %s %% %s",
		  rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
      if (cpu->regs[rs1] == dividend_max && cpu->regs[rs2] == -1)
	tmp = 0;
      else if (cpu->regs[rs2])
	tmp = (signed_word)cpu->regs[rs1] % (signed_word)cpu->regs[rs2];
      else
	tmp = cpu->regs[rs1];
      store_rd (cpu, rd, tmp);
      break;
    case MATCH_REMW:
      TRACE_INSN (cpu, "remw %s, %s, %s;  // %s = %s %% %s",
		  rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
      RISCV_ASSERT_RV64 (cpu, "insn: %s", op->name);
      if (EXTEND32 (cpu->regs[rs1]) == dividend32_max
	  && EXTEND32 (cpu->regs[rs2]) == -1)
	tmp = 0;
      else if (EXTEND32 (cpu->regs[rs2]))
	tmp = EXTEND32 (cpu->regs[rs1]) % EXTEND32 (cpu->regs[rs2]);
      else
	tmp = cpu->regs[rs1];
      store_rd (cpu, rd, EXTEND32 (tmp));
      break;
    case MATCH_REMU:
      TRACE_INSN (cpu, "remu %s, %s, %s;  // %s = %s %% %s",
		  rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
      if (cpu->regs[rs2])
	store_rd (cpu, rd, cpu->regs[rs1] % cpu->regs[rs2]);
      else
	store_rd (cpu, rd, cpu->regs[rs1]);
      break;
    case MATCH_REMUW:
      TRACE_INSN (cpu, "remuw %s, %s, %s;  // %s = %s %% %s",
		  rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
      RISCV_ASSERT_RV64 (cpu, "insn: %s", op->name);
      if ((unsigned32)cpu->regs[rs2])
	tmp = (unsigned32)cpu->regs[rs1] % (unsigned32)cpu->regs[rs2];
      else
	tmp = cpu->regs[rs1];
      store_rd (cpu, rd, EXTEND32 (tmp));
      break;
    default:
      TRACE_INSN (cpu, "UNHANDLED INSN: %s", op->name);
      sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL);
    }

  return pc;
}

#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))

static sim_cia
execute_a (SIM_CPU *cpu, unsigned_word iw, const struct riscv_opcode *op)
{
  SIM_DESC sd = CPU_STATE (cpu);
  int rd = (iw >> OP_SH_RD) & OP_MASK_RD;
  int rs1 = (iw >> OP_SH_RS1) & OP_MASK_RS1;
  int rs2 = (iw >> OP_SH_RS2) & OP_MASK_RS2;
  const char *rd_name = riscv_gpr_names_abi[rd];
  const char *rs1_name = riscv_gpr_names_abi[rs1];
  const char *rs2_name = riscv_gpr_names_abi[rs2];
  struct atomic_mem_reserved_list *amo_prev, *amo_curr;
  insn_t aqrl_mask = (OP_MASK_AQ << OP_SH_AQ) | (OP_MASK_RL << OP_SH_RL);
  unsigned_word tmp;
  unsigned_word rs2_val = cpu->regs[rs2];
  sim_cia pc = cpu->pc + 4;

  /* Handle these two load/store operations specifically.  */
  switch (op->match & ~aqrl_mask)
    {
    case MATCH_LR_W:
      TRACE_INSN (cpu, "%s %s, (%s);", op->name, rd_name, rs1_name);
      store_rd (cpu, rd,
	sim_core_read_unaligned_4 (cpu, cpu->pc, read_map, cpu->regs[rs1]));

      /* Walk the reservation list to find an existing match.  */
      amo_curr = sd->amo_reserved_list;
      while (amo_curr)
	{
	  if (amo_curr->addr == cpu->regs[rs1])
	    goto done;
	  amo_curr = amo_curr->next;
	}

      /* No reservation exists, so add one.  */
      amo_curr = xmalloc (sizeof (*amo_curr));
      amo_curr->addr = cpu->regs[rs1];
      amo_curr->next = sd->amo_reserved_list;
      sd->amo_reserved_list = amo_curr;
      goto done;
    case MATCH_SC_W:
      TRACE_INSN (cpu, "%s %s, %s, (%s);", op->name, rd_name, rs2_name, rs1_name);

      /* Walk the reservation list to find a match.  */
      amo_curr = amo_prev = sd->amo_reserved_list;
      while (amo_curr)
	{
	  if (amo_curr->addr == cpu->regs[rs1])
	    {
	      /* We found a reservation, so operate it.  */
	      sim_core_write_unaligned_4 (cpu, cpu->pc, write_map,
					  cpu->regs[rs1], cpu->regs[rs2]);
	      store_rd (cpu, rd, 0);
	      if (amo_curr == sd->amo_reserved_list)
		sd->amo_reserved_list = amo_curr->next;
	      else
		amo_prev->next = amo_curr->next;
	      free (amo_curr);
	      goto done;
	    }
	  amo_prev = amo_curr;
	  amo_curr = amo_curr->next;
	}

      /* If we're still here, then no reservation exists, so mark as failed.  */
      store_rd (cpu, rd, 1);
      goto done;
    }

  /* Handle the rest of the atomic insns with common code paths.  */
  TRACE_INSN (cpu, "%s %s, %s, (%s);",
	      op->name, rd_name, rs2_name, rs1_name);
  if (op->xlen_requirement == 64)
    tmp = sim_core_read_unaligned_8 (cpu, cpu->pc, read_map, cpu->regs[rs1]);
  else
    tmp = EXTEND32 (sim_core_read_unaligned_4 (cpu, cpu->pc,
					       read_map, cpu->regs[rs1]));

  store_rd (cpu, rd, tmp);

  switch (op->match & ~aqrl_mask)
    {
    case MATCH_AMOADD_D:
    case MATCH_AMOADD_W:
      tmp = tmp + cpu->regs[rs2];
      break;
    case MATCH_AMOAND_D:
    case MATCH_AMOAND_W:
      tmp = tmp & cpu->regs[rs2];
      break;
    case MATCH_AMOMAX_D:
    case MATCH_AMOMAX_W:
      tmp = MAX ((signed_word)tmp, (signed_word)cpu->regs[rs2]);
      break;
    case MATCH_AMOMAXU_D:
    case MATCH_AMOMAXU_W:
      tmp = MAX ((unsigned_word)tmp, (unsigned_word)cpu->regs[rs2]);
      break;
    case MATCH_AMOMIN_D:
    case MATCH_AMOMIN_W:
      tmp = MIN ((signed_word)tmp, (signed_word)cpu->regs[rs2]);
      break;
    case MATCH_AMOMINU_D:
    case MATCH_AMOMINU_W:
      tmp = MIN ((unsigned_word)tmp, (unsigned_word)cpu->regs[rs2]);
      break;
    case MATCH_AMOOR_D:
    case MATCH_AMOOR_W:
      tmp = tmp | cpu->regs[rs2];
      break;
    case MATCH_AMOSWAP_D:
    case MATCH_AMOSWAP_W:
      tmp = rs2_val;
      break;
    case MATCH_AMOXOR_D:
    case MATCH_AMOXOR_W:
      tmp = tmp ^ cpu->regs[rs2];
      break;
    default:
      TRACE_INSN (cpu, "UNHANDLED INSN: %s", op->name);
      sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL);
    }

  if (op->xlen_requirement == 64)
    sim_core_write_unaligned_8 (cpu, cpu->pc, write_map, cpu->regs[rs1], tmp);
  else
    sim_core_write_unaligned_4 (cpu, cpu->pc, write_map, cpu->regs[rs1], tmp);

 done:
  return pc;
}

static INLINE unsigned_word
sext (unsigned_word w, int bit)
{
  const int shift = sizeof(unsigned_word) * CHAR_BIT - bit;
  return ((signed_word)w << shift) >> shift;
}

static INLINE unsigned_word
zext (unsigned_word w, int bit)
{
  const int shift = sizeof(unsigned_word) * CHAR_BIT - bit;
  return ((unsigned_word)w << shift) >> shift;
}

static int
clip (long long int low, long long int x, long long int high)
{
  if (x >= high)
    return high;
  else if (x <= low)
    return low;
  else
    return x;
}

static INLINE unsigned_word
round_u (unsigned_word v, int shift)
{
  if (shift > 0)
    return (v + (1 << (shift - 1))) >> shift;
  else
    return v;
}

static INLINE unsigned_word
round_s (unsigned_word v, int shift)
{
  if (shift > 0)
    return (signed_word)(v + (1 << (shift - 1))) >> shift;
  else
    return v;
}

static sim_cia
execute_xpulp (SIM_CPU *cpu, unsigned_word iw, const struct riscv_opcode *op)
{
  SIM_DESC sd = CPU_STATE (cpu);
  int rd = (iw >> OP_SH_RD) & OP_MASK_RD;
  int rs1 = (iw >> OP_SH_RS1) & OP_MASK_RS1;
  int rs2 = (iw >> OP_SH_RS2) & OP_MASK_RS2;
  int rs3 = rd; /* PULP sometimes uses rd as rs3 */
  int imm5 = ((((signed_word)iw >> OP_SH_RS2) & OP_MASK_RS2) << 27) >> 27; /* rs2, but sign extended */
  int uimm5 = (iw >> OP_SH_RS2) & OP_MASK_RS2; /* rs2 */
  int luimm5 = (iw >> OP_SH_RS3I) & OP_MASK_RS3I;
  int uimms = (iw >> OP_SH_RS1) & OP_MASK_RS1; /* loop immediate */
  int loopindex = (iw >> 7) & 1;

  const char *rd_name = riscv_gpr_names_abi[rd];
  const char *rs1_name = riscv_gpr_names_abi[rs1];
  const char *rs2_name = riscv_gpr_names_abi[rs2];
  const char *rs3_name = riscv_gpr_names_abi[rs3];

  unsigned int csr = (iw >> OP_SH_CSR) & OP_MASK_CSR;
  unsigned_word i_imm = EXTRACT_ITYPE_IMM (iw);
  unsigned_word u_imm = EXTRACT_UTYPE_IMM ((unsigned64) iw);
  unsigned_word s_imm = EXTRACT_STYPE_IMM (iw);
  unsigned_word sb_imm = EXTRACT_SBTYPE_IMM (iw);
  unsigned_word shamt_imm = ((iw >> OP_SH_SHAMT) & OP_MASK_SHAMT);
  unsigned_word vsimm6 = sext(EXTRACT_I6TYPE_IMM(iw), 6);

  unsigned_word tmp;
  unsigned_word b0, b1, b2, b3, low, high;
  int shift;
  sim_cia pc = cpu->pc + 4;

  TRACE_EXTRACT (cpu, "rd:%-2i:%-4s  rs1:%-2i:%-4s %0*"PRIxTW"  rs2:%-2i:%-4s %0*"PRIxTW"  match:%#x mask:%#x",
		 rd, rd_name,
		 rs1, rs1_name, (int)sizeof (unsigned_word) * 2, cpu->regs[rs1],
		 rs2, rs2_name, (int)sizeof (unsigned_word) * 2, cpu->regs[rs2],
		 (unsigned) op->match, (unsigned) op->mask);

  /* PULP branches */
  if ((iw & MASK_BEQM1) == MATCH_BEQM1)
    {
      TRACE_INSN (cpu, "p.beqimm %s, %d, %#" PRIxTW ";  // if (%s == %d) goto %#" PRIxTW,
		  rs1_name, imm5, sb_imm, rs1_name, imm5, cpu->pc + sb_imm);
      if(cpu->regs[rs1] == imm5)
	{
	  pc = cpu->pc + sb_imm;
	  TRACE_BRANCH (cpu, "to %#" PRIxTW, pc);
	}
      goto done;
    }

  if ((iw & MASK_BNEM1) == MATCH_BNEM1)
    {
      TRACE_INSN (cpu, "p.bneimm %s, %d, %#" PRIxTW ";  // if (%s != %d) goto %#" PRIxTW,
		  rs1_name, imm5, sb_imm, rs1_name, imm5, cpu->pc + sb_imm);
      if(cpu->regs[rs1] != imm5)
	{
	  pc = cpu->pc + sb_imm;
	  TRACE_BRANCH (cpu, "to %#" PRIxTW, pc);
	}
      goto done;
    }

  /* PULP clip and bit manipulation */

  switch (iw & MASK_PALU1)
    {
    case MATCH_CLIP:
      {
	signed_word high = (1 << MAX (uimm5 - 1, 0)) - 1;
	signed_word low = -(1 << MAX (uimm5 - 1, 0));
	TRACE_INSN (cpu, "p.clip %s, %s, %"PRIiTW";  // %s = clip(%"PRIiTW", %s, %"PRIiTW")",
		    rd_name, rs1_name, uimm5, rd_name, low, rs1_name, high);
	store_rd (cpu, rd, clip (low, (signed_word)cpu->regs[rs1], high));
      }
      goto done;
    case MATCH_CLIPU:
      {
	signed_word high = (1 << MAX (uimm5 - 1, 0)) - 1;
	signed_word low = 0;
	TRACE_INSN (cpu, "p.clipu %s, %s, %"PRIiTW";  // %s = clip(%"PRIiTW", %s, %"PRIiTW")",
		    rd_name, rs1_name, uimm5, rd_name, low, rs1_name, high);
	store_rd (cpu, rd, clip (low, (signed_word)cpu->regs[rs1], high));
      }
      goto done;
    }

  switch (iw & MASK_PALU)
    {
    case MATCH_CLIPR:
      {
	signed_word high = cpu->regs[rs2];
	signed_word low = -cpu->regs[rs2] - 1;
	TRACE_INSN (cpu, "p.clipr %s, %s, %s;  // %s = clip(-%s-1, %s, %s)",
		    rd_name, rs1_name, rs2_name, rd_name, rs2_name, rs1_name, rs2_name);
	store_rd (cpu, rd, clip (low, (signed_word)cpu->regs[rs1], high));
      }
      goto done;
    case MATCH_CLIPUR:
      {
	signed_word high = cpu->regs[rs2];
	signed_word low = 0;
	TRACE_INSN (cpu, "p.clipur %s, %s, %s;  // %s = clip(-%"PRIiTW", %s, %s)",
		    rd_name, rs1_name, rs2_name, rd_name, low, rs1_name, rs2_name);
	store_rd (cpu, rd, clip (low, (signed_word)cpu->regs[rs1], high));
      }
      goto done;
    case MATCH_EXTRACTR:
      {
	unsigned_word width = (cpu->regs[rs2] >> 5 & 0x1f) + 1;
	unsigned_word shift = cpu->regs[rs2] & 0x1f;
	unsigned_word mask = width == 32 ? -1 : ((1 << width) - 1);
	if ((width - 1) + shift > 32)
	  {
	    TRACE_INSN (cpu, "UNHANDLED INSN: %s", op->name);
	    sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL);
	  }
	TRACE_INSN (cpu, "p.extract %s, %s, %s;  // %s = sext(extract(%s, %s[9:5], %s[4:0]))",
		    rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name, rs2_name);
	store_rd (cpu, rd, sext ((cpu->regs[rs1] & (mask << shift)) >> shift, width));
      }
      goto done;
    case MATCH_EXTRACTUR:
      {
	unsigned_word width = (cpu->regs[rs2] >> 5 & 0x1f) + 1;
	unsigned_word shift = cpu->regs[rs2] & 0x1f;
	unsigned_word mask = width == 32 ? -1 : ((1 << width) - 1);
	if ((width - 1) + shift > 32)
	  {
	    TRACE_INSN (cpu, "UNHANDLED INSN: %s", op->name);
	    sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL);
	  }
	TRACE_INSN (cpu, "p.extract %s, %s, %s;  // %s = (extract(%s, %s[9:5], %s[4:0]))",
		    rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name, rs2_name);
	store_rd (cpu, rd, zext ((cpu->regs[rs1] & (mask << shift)) >> shift, width));
      }
      goto done;
    case MATCH_INSERTR:
      {
	unsigned_word width = (cpu->regs[rs2] >> 5 & 0x1f) + 1;
	unsigned_word shift = cpu->regs[rs2] & 0x1f;
	unsigned_word mask = width == 32 ? -1 : ((1 << width) - 1);
	if ((width - 1) + shift > 32)
	  {
	    TRACE_INSN (cpu, "UNHANDLED INSN: %s", op->name);
	    sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL);
	  }
	TRACE_INSN (cpu,  "p.insertr %s, %s, %s;  // %s = insert(%s, %s[9:5], %s[4:0])",
		    rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name, rs2_name);
	store_rd (cpu, rd, (cpu->regs[rd] & ~(mask << shift))
		  | ((cpu->regs[rs1] & mask) << shift));
      }
      goto done;
    case MATCH_BSETR:
      {
	unsigned_word width = (cpu->regs[rs2] >> 5 & 0x1f) + 1;
	unsigned_word shift = cpu->regs[rs2] & 0x1f;
	unsigned_word mask = width == 32 ? -1 : ((1 << width) - 1);
	if ((width - 1) + shift > 32)
	  {
	    TRACE_INSN (cpu, "UNHANDLED INSN: %s", op->name);
	    sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL);
	  }
	TRACE_INSN (cpu,  "p.bsetr %s, %s, %s;  // %s = bsetr(%s, %s[9:5], %s[4:0])",
		    rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name, rs2_name);
	store_rd (cpu, rd, cpu->regs[rs1] | (mask << shift));
      }
      goto done;
    case MATCH_BCLRR:
      {
	unsigned_word width = (cpu->regs[rs2] >> 5 & 0x1f) + 1;
	unsigned_word shift = cpu->regs[rs2] & 0x1f;
	unsigned_word mask = width == 32 ? -1 : ((1 << width) - 1);
	if ((width - 1) + shift > 32)
	  {
	    TRACE_INSN (cpu, "UNHANDLED INSN: %s", op->name);
	    sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL);
	  }
	TRACE_INSN (cpu,  "p.bclrr %s, %s, %s;  // %s = bclrr(%s, %s[9:5], %s[4:0])",
		    rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name, rs2_name);
	store_rd (cpu, rd, cpu->regs[rs1] & ~(mask << shift));
      }
      goto done;
    }

  switch (iw & MASK_PALU2)
    {
    case MATCH_EXTRACT:
      {
	unsigned_word width = luimm5 + 1;
	unsigned_word shift = uimm5;
	unsigned_word mask = width == 32 ? -1 : ((1 << width) - 1);
	if (luimm5 + uimm5 > 32)
	  {
	    TRACE_INSN (cpu, "UNHANDLED INSN: %s", op->name);
	    sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL);
	  }
	TRACE_INSN (cpu, "p.extract %s, %s, %d, %d;  // %s = sext(extract(%s, %d, %d))",
		    rd_name, rs1_name, luimm5, uimm5, rd_name, rs1_name, luimm5, uimm5);
	store_rd (cpu, rd, sext ((cpu->regs[rs1] & (mask << shift)) >> shift, width));
      }
      goto done;
    case MATCH_EXTRACTU:
      {
	unsigned_word width = luimm5 + 1;
	unsigned_word shift = uimm5;
	unsigned_word mask = width == 32 ? -1 : ((1 << width) - 1);
	if (luimm5 + uimm5 > 32)
	  {
	    TRACE_INSN (cpu, "UNHANDLED INSN: %s", op->name);
	    sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL);
	  }
	TRACE_INSN (cpu, "p.extract %s, %s, %d, %d;  // %s = extract(%s, %d, %d)",
		    rd_name, rs1_name, luimm5, uimm5, rd_name, rs1_name, luimm5, uimm5);
	store_rd (cpu, rd, zext ((cpu->regs[rs1] & (mask << shift)) >> shift, width));
      }
      goto done;
    case MATCH_INSERT:
      {
	unsigned_word width = luimm5 + 1;
	unsigned_word shift = uimm5;
	unsigned_word mask = width == 32 ? -1 : ((1 << width) - 1);
	if (luimm5 + uimm5 > 32)
	  {
	    TRACE_INSN (cpu, "UNHANDLED INSN: %s", op->name);
	    sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL);
	  }
	TRACE_INSN (cpu, "p.insert %s, %s, %d, %d;  // %s = insert(%s, %d, %d)",
		    rd_name, rs1_name, luimm5, uimm5, rd_name, rs1_name, luimm5, uimm5);
	store_rd (cpu, rd, (cpu->regs[rd] & ~(mask << shift))
		  | ((cpu->regs[rs1] & mask) << shift));
      }
      goto done;
    case MATCH_BSET:
      {
	unsigned_word width = luimm5 + 1;
	unsigned_word shift = uimm5;
	unsigned_word mask = width == 32 ? -1 : ((1 << width) - 1);
	if (luimm5 + uimm5 > 32)
	  {
	    TRACE_INSN (cpu, "UNHANDLED INSN: %s", op->name);
	    sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL);
	  }
	TRACE_INSN (cpu, "p.bset %s, %s, %d, %d;  // %s = bset(%s, %d, %d)",
		    rd_name, rs1_name, luimm5, uimm5, rd_name, rs1_name, luimm5, uimm5);
	store_rd (cpu, rd, cpu->regs[rs1] | (mask << shift));
      }
      goto done;
    case MATCH_BCLR:
      {
	unsigned_word width = luimm5 + 1;
	unsigned_word shift = uimm5;
	unsigned_word mask = width == 32 ? -1 : ((1 << width) - 1);
	if (luimm5 + uimm5 > 32)
	  {
	    TRACE_INSN (cpu, "UNHANDLED INSN: %s", op->name);
	    sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL);
	  }
	TRACE_INSN (cpu, "p.bclr %s, %s, %d, %d;  // %s = bclr(%s, %d, %d)",
		    rd_name, rs1_name, luimm5, uimm5, rd_name, rs1_name, luimm5, uimm5);
	store_rd (cpu, rd, cpu->regs[rs1] & ~(mask << shift));
      }
      goto done;
    }

  /* PULP additional ALU operations with only a single source operand */
  switch (iw & MASK_PALUS)
    {
    case MATCH_FF1:
      TRACE_INSN (cpu, "p.ff1 %s, %s;  // %s = ffs(%s)",
		  rd_name, rs1_name, rd_name, rs1_name);
      store_rd (cpu, rd, cpu->regs[rs1] == 0 ? 32 : ffs(cpu->regs[rs1]) - 1);
      goto done;
    case MATCH_FL1:
      {
	unsigned_word t;
	TRACE_INSN (cpu, "p.fl1 %s, %s;  // %s = fls(%s)",
		    rd_name, rs1_name, rd_name, rs1_name);
	t = cpu->regs[rs1];
	/* reverse bits and ffs */
	t = (((t & 0xaaaaaaaa) >> 1) | ((t & 0x55555555) << 1));
	t = (((t & 0xcccccccc) >> 2) | ((t & 0x33333333) << 2));
	t = (((t & 0xf0f0f0f0) >> 4) | ((t & 0x0f0f0f0f) << 4));
	t = (((t & 0xff00ff00) >> 8) | ((t & 0x00ff00ff) << 8));
	t = ffs ((t >> 16) | (t << 16));
	store_rd (cpu, rd, (0 == t ? 32 : 32 - t));
      }
      goto done;
    case MATCH_CLB:
      TRACE_INSN (cpu, "p.clb %s, %s;  // %s = count_leading_bits(%s)",
		  rd_name, rs1_name, rd_name, rs1_name);
      store_rd (cpu, rd, cpu->regs[rs1] == 0 ?
		0 : __builtin_clrsb(cpu->regs[rs1]));
      goto done;
    case MATCH_CNT:
      TRACE_INSN (cpu, "p.cnt %s, %s;  // %s = popcount(%s)",
		  rd_name, rs1_name, rd_name, rs1_name);
      store_rd (cpu, rd, __builtin_popcount(cpu->regs[rs1]));
      goto done;
    case MATCH_EXTHS:
      TRACE_INSN (cpu, "p.exths %s, %s;  // %s = sext(%s[15:0])",
		  rd_name, rs1_name, rd_name, rs1_name);
      store_rd (cpu, rd, sext(cpu->regs[rs1], 16));
      goto done;
    case MATCH_EXTHZ:
      TRACE_INSN (cpu, "p.exthz %s, %s;  // %s = zext(%s[15:0])",
		  rd_name, rs1_name, rd_name, rs1_name);
      store_rd (cpu, rd, zext(cpu->regs[rs1], 16));
      goto done;
    case MATCH_EXTBS:
      TRACE_INSN (cpu, "p.extbs %s, %s;  // %s = sext(%s[7:0])",
		  rd_name, rs1_name, rd_name, rs1_name);
      store_rd (cpu, rd, sext(cpu->regs[rs1], 8));
      goto done;
    case MATCH_EXTBZ:
      TRACE_INSN (cpu, "p.extbz %s, %s;  // %s = zext(%s[7:0])",
		  rd_name, rs1_name, rd_name, rs1_name);
      store_rd (cpu, rd, zext(cpu->regs[rs1], 8));
      goto done;
    case MATCH_AVG: /* TODO: binutils says MATCH_AVG, weird. Fix binutils */
      TRACE_INSN (cpu, "p.abs %s, %s;  // %s = abs(%s)",
		  rd_name, rs1_name, rd_name, rs1_name);
      store_rd (cpu, rd, (signed_word)cpu->regs[rs1] < 0 ?
		-cpu->regs[rs1] : cpu->regs[rs1]);
      goto done;
    }

  /* PULP additional ALU operations */
  switch (iw & MASK_PALU)
    {
    case MATCH_SLET:
      {
	signed_word gpr1 = cpu->regs[rs1];
	signed_word gpr2 = cpu->regs[rs2];
	TRACE_INSN (cpu, "p.slet %s, %s, %s;  // %s = slet(%s, %s)",
		    rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
	store_rd (cpu, rd, gpr1 <= gpr2 ? 1 : 0);
      }
      goto done;
    case MATCH_SLETU:
      {
	unsigned_word gpr1 = cpu->regs[rs1];
	unsigned_word gpr2 = cpu->regs[rs2];
	TRACE_INSN (cpu, "p.sletu %s, %s, %s;  // %s = sletu(%s, %s)",
		    rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
	store_rd (cpu, rd, gpr1 <= gpr2 ? 1 : 0);
      }
      goto done;
    case MATCH_MIN:
      {
	signed_word gpr1 = cpu->regs[rs1];
	signed_word gpr2 = cpu->regs[rs2];
	TRACE_INSN (cpu, "p.min %s, %s, %s;  // %s = min(%s, %s)",
		    rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
	store_rd (cpu, rd, MIN (gpr1, gpr2));
      }
      goto done;
    case MATCH_MINU:
      {
	unsigned_word gpr1 = cpu->regs[rs1];
	unsigned_word gpr2 = cpu->regs[rs2];
	TRACE_INSN (cpu, "p.minu %s, %s, %s;  // %s = minu(%s, %s)",
		    rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
	store_rd (cpu, rd, MIN (gpr1, gpr2));
      }
      goto done;
    case MATCH_MAX:
      {
	signed_word gpr1 = cpu->regs[rs1];
	signed_word gpr2 = cpu->regs[rs2];
	TRACE_INSN (cpu, "p.max %s, %s, %s;  // %s = max(%s, %s)",
		    rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
	store_rd (cpu, rd, MAX (gpr1, gpr2));
      }
      goto done;
    case MATCH_MAXU:
      {
	unsigned_word gpr1 = cpu->regs[rs1];
	unsigned_word gpr2 = cpu->regs[rs2];
	TRACE_INSN (cpu, "p.maxu %s, %s, %s;  // %s = maxu(%s, %s)",
		    rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
	store_rd (cpu, rd, MAX (gpr1, gpr2));
      }
      goto done;
    case MATCH_ROR:
      {
	unsigned_word gpr1 = cpu->regs[rs1];
	unsigned_word gpr2 = cpu->regs[rs2];
	unsigned_word tmp1;
	unsigned_word tmp2;
	if (gpr2 >= 32 || gpr2 < 0)
	  {
	    TRACE_INSN (cpu, "UNHANDLED INSN: %s", op->name);
	    sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL);
	  }

	TRACE_INSN (cpu, "p.ror %s, %s, %s;  // %s = ror(%s, %s)",
		    rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
	tmp1 = gpr2 == 32 ? 0 : gpr1 >> gpr2;
	tmp2 = gpr2 == 0 ? 0 : gpr1 << (32 - gpr2);
	store_rd (cpu, rd, tmp1 | tmp2);
      }
      goto done;
    }

  /* PULP post-increment and reg-reg stores */

  switch (iw & MASK_SPOST)
    {
    case MATCH_SBPOST:
      TRACE_INSN (cpu, "p.sb %s, %" PRIiTW "(%s!); // ",
		  rs2_name, s_imm, rs1_name);
      tmp = cpu->regs[rs1] + s_imm;
      sim_core_write_unaligned_1 (cpu, cpu->pc, write_map,
				  cpu->regs[rs1], cpu->regs[rs2]);
      store_rd (cpu, rs1, tmp);
      goto done;
    case MATCH_SHPOST:
      TRACE_INSN (cpu, "p.sh %s, %" PRIiTW "(%s!); // ",
		  rs2_name, s_imm, rs1_name);
      tmp = cpu->regs[rs1] + s_imm;
      sim_core_write_unaligned_2 (cpu, cpu->pc, write_map,
				  cpu->regs[rs1], cpu->regs[rs2]);
      store_rd (cpu, rs1, tmp);
      goto done;
    case MATCH_SWPOST:
      TRACE_INSN (cpu, "p.sw %s, %" PRIiTW "(%s!); // ",
		  rs2_name, s_imm, rs1_name);
      tmp = cpu->regs[rs1] + s_imm;
      sim_core_write_unaligned_4 (cpu, cpu->pc, write_map,
				  cpu->regs[rs1], cpu->regs[rs2]);
      store_rd (cpu, rs1, tmp);
      goto done;
    }

  switch (iw & MASK_PALU)
    {
    case MATCH_SBRR:
      TRACE_INSN (cpu, "p.sb %s, %s(%s); // ", rs2_name, rs3_name, rs1_name);
      sim_core_write_unaligned_1 (cpu, cpu->pc, write_map,
				  cpu->regs[rs1] + cpu->regs[rs3],
				  cpu->regs[rs2]);
      goto done;
    case MATCH_SHRR:
      TRACE_INSN (cpu, "p.sh %s, %s(%s); // ", rs2_name, rs3_name, rs1_name);
      sim_core_write_unaligned_2 (cpu, cpu->pc, write_map,
				  cpu->regs[rs1] + cpu->regs[rs3],
				  cpu->regs[rs2]);
      goto done;
    case MATCH_SWRR:
      TRACE_INSN (cpu, "p.sw %s, %s(%s); // ", rs2_name, rs3_name, rs1_name);
      sim_core_write_unaligned_4 (cpu, cpu->pc, write_map,
				  cpu->regs[rs1] + cpu->regs[rs3],
				  cpu->regs[rs2]);
      goto done;
    case MATCH_SBRRPOST:
      TRACE_INSN (cpu, "p.sb %s, %s(%s!); // ",
		  rs2_name, rs3_name, rs1_name);
      tmp = cpu->regs[rs1] + cpu->regs[rs3];
      sim_core_write_unaligned_1 (cpu, cpu->pc, write_map,
				  cpu->regs[rs1], cpu->regs[rs2]);
      store_rd (cpu, rs1, tmp);
      goto done;
    case MATCH_SHRRPOST:
      TRACE_INSN (cpu, "p.sh %s, %s(%s!); // ",
		  rs2_name, rs3_name, rs1_name);
      tmp = cpu->regs[rs1] + cpu->regs[rs3];
      sim_core_write_unaligned_2 (cpu, cpu->pc, write_map,
				  cpu->regs[rs1], cpu->regs[rs2]);
      store_rd (cpu, rs1, tmp);
      goto done;
    case MATCH_SWRRPOST:
      TRACE_INSN (cpu, "p.sw %s, %s(%s!); // ",
		  rs2_name, rs3_name, rs1_name);
      tmp = cpu->regs[rs1] + cpu->regs[rs3];
      sim_core_write_unaligned_4 (cpu, cpu->pc, write_map,
				  cpu->regs[rs1], cpu->regs[rs2]);
      store_rd (cpu, rs1, tmp);
      goto done;
    }

  /* PULP post-increment and reg-reg loads */

  switch (iw & MASK_LPOST)
    {
    case MATCH_LBPOST:
      TRACE_INSN (cpu, "p.lb %s, %" PRIiTW "(%s!); // ",
		  rd_name, i_imm, rs1_name);
      tmp = cpu->regs[rs1] + i_imm;
      store_rd (cpu, rd, EXTEND8 (
	sim_core_read_unaligned_1 (cpu, cpu->pc, read_map,
				   cpu->regs[rs1])));
      store_rd (cpu, rs1, tmp);
      goto done;
    case MATCH_LBUPOST:
      TRACE_INSN (cpu, "p.lbu %s, %" PRIiTW "(%s!); // ",
		  rd_name, i_imm, rs1_name);
      tmp = cpu->regs[rs1] + i_imm;
      store_rd (cpu, rd,
	sim_core_read_unaligned_1 (cpu, cpu->pc, read_map,
				   cpu->regs[rs1]));
      store_rd (cpu, rs1, tmp);
      goto done;
    case MATCH_LHPOST:
      TRACE_INSN (cpu, "p.lh %s, %" PRIiTW "(%s!); // ",
		  rd_name, i_imm, rs1_name);
      tmp = cpu->regs[rs1] + i_imm;
      store_rd (cpu, rd, EXTEND16 (
	sim_core_read_unaligned_2 (cpu, cpu->pc, read_map,
				   cpu->regs[rs1])));
      store_rd (cpu, rs1, tmp);
      goto done;
    case MATCH_LHUPOST:
      TRACE_INSN (cpu, "p.lhu %s, %" PRIiTW "(%s!); // ",
		  rd_name, i_imm, rs1_name);
      tmp = cpu->regs[rs1] + i_imm;
      store_rd (cpu, rd,
	sim_core_read_unaligned_2 (cpu, cpu->pc, read_map,
				   cpu->regs[rs1]));
      store_rd (cpu, rs1, tmp);
      goto done;
    case MATCH_LWPOST:
      TRACE_INSN (cpu, "p.lw %s, %" PRIiTW "(%s!); // ",
		  rd_name, i_imm, rs1_name);
      tmp = cpu->regs[rs1] + i_imm;
      store_rd (cpu, rd,
	sim_core_read_unaligned_4 (cpu, cpu->pc, read_map,
				   cpu->regs[rs1]));
      store_rd (cpu, rs1, tmp);
      goto done;
    }

  switch (iw & MASK_LRR)
    {
    case MATCH_LBRR:
      TRACE_INSN (cpu, "p.lb %s, %s(%s); // ",
		  rd_name, rs2_name, rs1_name);
      store_rd (cpu, rd, EXTEND8 (
	sim_core_read_unaligned_1 (cpu, cpu->pc, read_map,
				   cpu->regs[rs1] + cpu->regs[rs2])));
      goto done;
    case MATCH_LBURR:
      TRACE_INSN (cpu, "p.lbu %s, %s(%s); // ",
		  rd_name, rs2_name, rs1_name);
      store_rd (cpu, rd,
	sim_core_read_unaligned_1 (cpu, cpu->pc, read_map,
				   cpu->regs[rs1] + cpu->regs[rs2]));
      goto done;
    case MATCH_LHRR:
      TRACE_INSN (cpu, "p.lh %s, %s(%s); // ",
		  rd_name, rs2_name, rs1_name);
      store_rd (cpu, rd, EXTEND16 (
	sim_core_read_unaligned_2 (cpu, cpu->pc, read_map,
				   cpu->regs[rs1] + cpu->regs[rs2])));
      goto done;
    case MATCH_LHURR:
      TRACE_INSN (cpu, "p.lhu %s, %s(%s); // ",
		  rd_name, rs2_name, rs1_name);
      store_rd (cpu, rd,
	sim_core_read_unaligned_2 (cpu, cpu->pc, read_map,
				   cpu->regs[rs1] + cpu->regs[rs2]));
      goto done;
    case MATCH_LWRR:
      TRACE_INSN (cpu, "p.lw %s, %s(%s); // ",
		  rd_name, rs2_name, rs1_name);
      store_rd (cpu, rd,
	sim_core_read_unaligned_4 (cpu, cpu->pc, read_map,
				   cpu->regs[rs1] + cpu->regs[rs2]));
      goto done;
    }

  switch (iw & MASK_LRRPOST)
    {
    case MATCH_LBRRPOST:
      TRACE_INSN (cpu, "p.lb %s, %s(%s!); // ",
		  rd_name, rs2_name, rs1_name);
      tmp = cpu->regs[rs1] + cpu->regs[rs2];
      store_rd (cpu, rd, EXTEND8 (
	sim_core_read_unaligned_1 (cpu, cpu->pc, read_map,
				   cpu->regs[rs1])));
      store_rd (cpu, rs1, tmp);
      goto done;
    case MATCH_LBURRPOST:
      TRACE_INSN (cpu, "p.lbu %s, %s(%s!); // ",
		  rd_name, rs2_name, rs1_name);
      tmp = cpu->regs[rs1] + cpu->regs[rs2];
      store_rd (cpu, rd,
	sim_core_read_unaligned_1 (cpu, cpu->pc, read_map,
				   cpu->regs[rs1]));
      store_rd (cpu, rs1, tmp);
      goto done;
    case MATCH_LHRRPOST:
      TRACE_INSN (cpu, "p.lh %s, %s(%s!); // ",
		  rd_name, rs2_name, rs1_name);
      tmp = cpu->regs[rs1] + cpu->regs[rs2];
      store_rd (cpu, rd, EXTEND16 (
	sim_core_read_unaligned_2 (cpu, cpu->pc, read_map,
				   cpu->regs[rs1])));
      store_rd (cpu, rs1, tmp);
      goto done;
    case MATCH_LHURRPOST:
      TRACE_INSN (cpu, "p.lhu %s, %s(%s!); // ",
		  rd_name, rs2_name, rs1_name);
      tmp = cpu->regs[rs1] + cpu->regs[rs2];
      store_rd (cpu, rd,
	sim_core_read_unaligned_2 (cpu, cpu->pc, read_map,
				   cpu->regs[rs1]));
      store_rd (cpu, rs1, tmp);
      goto done;
    case MATCH_LWRRPOST:
      TRACE_INSN (cpu, "p.lw %s, %s(%s!); // ",
		  rd_name, rs2_name, rs1_name);
      tmp = cpu->regs[rs1] + cpu->regs[rs2];
      store_rd (cpu, rd,
	sim_core_read_unaligned_4 (cpu, cpu->pc, read_map,
				   cpu->regs[rs1]));
      store_rd (cpu, rs1, tmp);
      goto done;
    }

  /* PULP hardware loops */

  if ((iw & MASK_HWLP_STARTI) == MATCH_HWLP_STARTI)
    {
      TRACE_INSN (cpu, "lp.starti %"PRIiTW", %"PRIiTW"; // start at %#"PRIxTW"",
		  loopindex, i_imm, cpu->pc + (i_imm << 1));
      if (loopindex)
	cpu->csr.lpstart1 = cpu->pc + (i_imm << 1);
      else
	cpu->csr.lpstart0 = cpu->pc + (i_imm << 1);
      goto done;
    }
  else if ((iw & MASK_HWLP_ENDI) == MATCH_HWLP_ENDI)
    {
      TRACE_INSN (cpu, "lp.endi %"PRIiTW", %"PRIiTW";  // end loop at %#"PRIxTW"",
		  loopindex, i_imm, cpu->pc + (i_imm << 1));
      if (loopindex)
	cpu->csr.lpend1 = cpu->pc + (i_imm << 1);
      else
	cpu->csr.lpend0 = cpu->pc + (i_imm << 1);
      goto done;
    }
  else if ((iw & MASK_HWLP_COUNT) == MATCH_HWLP_COUNT)
    {
      TRACE_INSN (cpu, "lp.count %"PRIiTW", %s;  // iterate %s",
		  loopindex, rs1_name, rs1_name);
      if (loopindex)
	cpu->csr.lpcount1 = cpu->regs[rs1];
      else
	cpu->csr.lpcount0 = cpu->regs[rs1];
      goto done;
    }
  else if ((iw & MASK_HWLP_COUNTI) == MATCH_HWLP_COUNTI)
    {
      TRACE_INSN (cpu, "lp.counti %"PRIiTW", %"PRIiTW";  // iterate %"PRIiTW"",
		  loopindex, i_imm, i_imm);
      if (loopindex)
	cpu->csr.lpcount1 = i_imm;
      else
	cpu->csr.lpcount0 = i_imm;
      goto done;
    }
  else if ((iw & MASK_HWLP_SETUP) == MATCH_HWLP_SETUP)
    {
      TRACE_INSN (cpu, "lp.setup %"PRIiTW", %s, %"PRIiTW";  "
		  "// start at %#"PRIxTW", end at %#"PRIxTW", count %s",
		  loopindex, rs1_name, i_imm,
		  cpu->pc + 4, cpu->pc + (i_imm << 1), rs1_name);
      if (loopindex)
	{
	  cpu->csr.lpstart1 = cpu->pc + 4;
	  cpu->csr.lpend1 = cpu->pc + (i_imm << 1);
	  cpu->csr.lpcount1 = cpu->regs[rs1];
	}
      else
	{
	  cpu->csr.lpstart0 = cpu->pc + 4;
	  cpu->csr.lpend0 = cpu->pc + (i_imm << 1);
	  cpu->csr.lpcount0 = cpu->regs[rs1];
	}
      goto done;
    }
  else if ((iw & MASK_HWLP_SETUPI) == MATCH_HWLP_SETUPI)
    {
      TRACE_INSN (cpu, "lp.setupi %"PRIiTW", %"PRIiTW", %"PRIiTW";  "
		  "// start at %#"PRIxTW", end at %#"PRIxTW", count %"PRIiTW"",
		  loopindex, i_imm, uimms,
		  cpu->pc + 4, cpu->pc + (uimms << 1), i_imm);
      if (loopindex)
	{
	  cpu->csr.lpstart1 = cpu->pc + 4;
	  cpu->csr.lpend1 = cpu->pc + (uimms << 1);
	  cpu->csr.lpcount1 = i_imm;
	}
      else
	{
	  cpu->csr.lpstart0 = cpu->pc + 4;
	  cpu->csr.lpend0 = cpu->pc + (uimms << 1);
	  cpu->csr.lpcount0 = i_imm;
	}
      goto done;
    }

  /* PULP mac operations */

  switch (iw & MASK_MACMSU32)
    {
    case MATCH_MAC32:
      TRACE_INSN (cpu, "p.mac %s, %s, %s;  // %s += %s * %s",
		  rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
      store_rd (cpu, rd, cpu->regs[rd] + cpu->regs[rs1] * cpu->regs[rs2]);
      goto done;
    case MATCH_MSU32:
      TRACE_INSN (cpu, "p.msu %s, %s, %s;  // %s -= %s * %s",
		  rd_name, rs1_name, rs2_name, rd_name, rs1_name, rs2_name);
      store_rd (cpu, rd, cpu->regs[rd] - cpu->regs[rs1] * cpu->regs[rs2]);
      goto done;
    }

#define RISCV_SL(reg) (EXTEND16 (cpu->regs[reg] & 0xffff))
#define RISCV_UL(reg) (cpu->regs[reg] & 0xffff)
#define RISCV_SH(reg) (EXTEND16 (cpu->regs[reg] >> 16 & 0xffff))
#define RISCV_UH(reg) (cpu->regs[reg] >> 16 & 0xffff)

  switch (iw & MASK_MACMUL)
    {
    case MATCH_MULS:
      TRACE_INSN (cpu, "p.muls %s, %s, %s;  //",
		  rd_name, rs1_name, rs2_name);
      store_rd (cpu, rd, RISCV_SL (rs1) * RISCV_SL (rs2));
      goto done;
    case MATCH_MULHHS:
      TRACE_INSN (cpu, "p.mulhhs %s, %s, %s;  //",
		  rd_name, rs1_name, rs2_name);
      store_rd (cpu, rd, RISCV_SH (rs1) * RISCV_SH (rs2));
      goto done;
    case MATCH_MULU:
      TRACE_INSN (cpu, "p.mulu %s, %s, %s;  //",
		  rd_name, rs1_name, rs2_name);
      store_rd (cpu, rd, RISCV_UL (rs1) * RISCV_UL (rs2));
      goto done;
    case MATCH_MULHHU:
      TRACE_INSN (cpu, "p.mulhhu %s, %s, %s;  //",
		  rd_name, rs1_name, rs2_name);
      store_rd (cpu, rd, RISCV_UH (rs1) * RISCV_UH (rs2));
      goto done;
    case MATCH_MACS:
      TRACE_INSN (cpu, "p.macs %s, %s, %s;  //",
		  rd_name, rs1_name, rs2_name);
      store_rd (cpu, rd, cpu->regs[rd] + (RISCV_SL (rs1) * RISCV_SL (rs2)));
      goto done;
    case MATCH_MACHHS:
      TRACE_INSN (cpu, "p.machhs %s, %s, %s;  //",
		  rd_name, rs1_name, rs2_name);
      store_rd (cpu, rd, cpu->regs[rd] + (RISCV_SH (rs1) * RISCV_SH (rs2)));
      goto done;
    case MATCH_MACU:
      TRACE_INSN (cpu, "p.macu %s, %s, %s;  //",
		  rd_name, rs1_name, rs2_name);
      store_rd (cpu, rd, cpu->regs[rd] + (RISCV_UL (rs1) * RISCV_UL (rs2)));
      goto done;
    case MATCH_MACHHU:
      TRACE_INSN (cpu, "p.machhu %s, %s, %s;  //",
		  rd_name, rs1_name, rs2_name);
      store_rd (cpu, rd, cpu->regs[rd] + (RISCV_UH (rs1) * RISCV_UH (rs2)));
      goto done;
    }

  switch (iw & MASK_MACMULNR)
    {
    case MATCH_MULSN:
      TRACE_INSN (cpu, "p.mulsn %s, %s, %s, %"PRIiTW";  //",
		  rd_name, rs1_name, rs2_name, luimm5);
      store_rd (cpu, rd,
		(signed_word)(RISCV_SL (rs1) * RISCV_SL (rs2)) >> luimm5);
      goto done;
    case MATCH_MULHHSN:
      TRACE_INSN (cpu, "p.mulhhsn %s, %s, %s, %"PRIiTW";  //",
		  rd_name, rs1_name, rs2_name, luimm5);
      store_rd (cpu, rd,
		(signed_word)(RISCV_SH (rs1) * RISCV_SH (rs2)) >> luimm5);
      goto done;
    case MATCH_MULSRN:
      TRACE_INSN (cpu, "p.mulsrn %s, %s, %s, %"PRIiTW";  //",
		  rd_name, rs1_name, rs2_name, luimm5);
      tmp = RISCV_SL (rs1) * RISCV_SL (rs2);
      tmp = round_s (tmp, luimm5);
      store_rd (cpu, rd, tmp);
      goto done;
    case MATCH_MULHHSRN:
      TRACE_INSN (cpu, "p.mulhhsrn %s, %s, %s, %"PRIiTW";  //",
		  rd_name, rs1_name, rs2_name, luimm5);
      tmp = RISCV_SH (rs1) * RISCV_SH (rs2);
      tmp = round_s (tmp, luimm5);
      store_rd (cpu, rd, tmp);
      goto done;
    case MATCH_MULUN:
      TRACE_INSN (cpu, "p.mulun %s, %s, %s, %"PRIiTW";  //",
		  rd_name, rs1_name, rs2_name, luimm5);
      store_rd (cpu, rd, (RISCV_UL (rs1) * RISCV_UL (rs2)) >> luimm5);
      goto done;
    case MATCH_MULHHUN:
      TRACE_INSN (cpu, "p.mulhhun %s, %s, %s, %"PRIiTW";  //",
		  rd_name, rs1_name, rs2_name, luimm5);
      store_rd (cpu, rd, (RISCV_UH (rs1) * RISCV_UH (rs2)) >> luimm5);
      goto done;
    case MATCH_MULURN:
      TRACE_INSN (cpu, "p.mulurn %s, %s, %s, %"PRIiTW";  //",
		  rd_name, rs1_name, rs2_name, luimm5);
      tmp = RISCV_UL (rs1) * RISCV_UL (rs2);
      tmp = round_u (tmp, luimm5);
      store_rd (cpu, rd, tmp);
      goto done;
    case MATCH_MULHHURN:
      TRACE_INSN (cpu, "p.mulhhurn %s, %s, %s, %"PRIiTW";  //",
		  rd_name, rs1_name, rs2_name, luimm5);
      tmp = RISCV_UH (rs1) * RISCV_UH (rs2);
      tmp = round_u (tmp, luimm5);
      store_rd (cpu, rd, tmp);
      goto done;
    case MATCH_MACSN:
      TRACE_INSN (cpu, "p.macsn %s, %s, %s, %"PRIiTW";  //",
		  rd_name, rs1_name, rs2_name, luimm5);
      tmp = cpu->regs[rd] + (RISCV_SL (rs1) * RISCV_SL (rs2));
      store_rd (cpu, rd, (signed_word)tmp >> luimm5);
      goto done;
    case MATCH_MACHHSN:
      TRACE_INSN (cpu, "p.machhsn %s, %s, %s, %"PRIiTW";  //",
		  rd_name, rs1_name, rs2_name, luimm5);
      tmp = cpu->regs[rd] + (RISCV_SH (rs1) * RISCV_SH (rs2));
      store_rd (cpu, rd, (signed_word)tmp >> luimm5);
      goto done;
    case MATCH_MACSRN:
      TRACE_INSN (cpu, "p.macsrn %s, %s, %s, %"PRIiTW";  //",
		  rd_name, rs1_name, rs2_name, luimm5);
      tmp = cpu->regs[rd] + (RISCV_SL (rs1) * RISCV_SL (rs2));
      tmp = round_s (tmp, luimm5);
      store_rd (cpu, rd, tmp);
      goto done;
    case MATCH_MACHHSRN:
      TRACE_INSN (cpu, "p.machhsrn %s, %s, %s, %"PRIiTW";  //",
		  rd_name, rs1_name, rs2_name, luimm5);
      tmp = cpu->regs[rd] + (RISCV_SH (rs1) * RISCV_SH (rs2));
      tmp = round_s (tmp, luimm5);
      store_rd (cpu, rd, tmp);
      goto done;
    case MATCH_MACUN:
      TRACE_INSN (cpu, "p.macun %s, %s, %s, %"PRIiTW";  //",
		  rd_name, rs1_name, rs2_name, luimm5);
      tmp = cpu->regs[rd] + (RISCV_UL (rs1) * RISCV_UL (rs2));
      store_rd (cpu, rd, tmp >> luimm5);
      goto done;
    case MATCH_MACHHUN:
      TRACE_INSN (cpu, "p.machhun %s, %s, %s, %"PRIiTW";  //",
		  rd_name, rs1_name, rs2_name, luimm5);
      tmp = cpu->regs[rd] + (RISCV_UH (rs1) * RISCV_UH (rs2));
      store_rd (cpu, rd, tmp >> luimm5);
      goto done;
    case MATCH_MACURN:
      TRACE_INSN (cpu, "p.macurn %s, %s, %s, %"PRIiTW";  //",
		  rd_name, rs1_name, rs2_name, luimm5);
      tmp = cpu->regs[rd] + (RISCV_UL (rs1) * RISCV_UL (rs2));
      tmp = round_u (tmp, luimm5);
      store_rd (cpu, rd, tmp);
      goto done;
    case MATCH_MACHHURN:
      TRACE_INSN (cpu, "p.machhurn %s, %s, %s, %"PRIiTW";  //",
		  rd_name, rs1_name, rs2_name, luimm5);
      tmp = cpu->regs[rd] + (RISCV_UH (rs1) * RISCV_UH (rs2));
      tmp = round_u (tmp, luimm5);
      store_rd (cpu, rd, tmp);
      goto done;
    }

  /* PULP add and sub with norm and rounding */

  switch (iw & MASK_MACMULNR)
    {
    case MATCH_ADDN:
      TRACE_INSN (cpu, "p.addn %s, %s, %s, %"PRIiTW";  //",
		  rd_name, rs1_name, rs2_name, luimm5);
      store_rd (cpu, rd,
		(signed_word)(cpu->regs[rs1] + cpu->regs[rs2]) >> luimm5);
      goto done;
    case MATCH_ADDUN:
      TRACE_INSN (cpu, "p.addn %s, %s, %s, %"PRIiTW";  //",
		  rd_name, rs1_name, rs2_name, luimm5);
      store_rd (cpu, rd, (cpu->regs[rs1] + cpu->regs[rs2]) >> luimm5);
      goto done;
    case MATCH_ADDRN:
      TRACE_INSN (cpu, "p.addrn %s, %s, %s, %"PRIiTW";  //",
		  rd_name, rs1_name, rs2_name, luimm5);
      tmp = cpu->regs[rs1] + cpu->regs[rs2];
      tmp = round_s (tmp, luimm5);
      store_rd (cpu, rd, tmp);
      goto done;
    case MATCH_ADDURN:
      TRACE_INSN (cpu, "p.addurn %s, %s, %s, %"PRIiTW";  //",
		  rd_name, rs1_name, rs2_name, luimm5);
      tmp = cpu->regs[rs1] + cpu->regs[rs2];
      tmp = round_u (tmp, luimm5);
      store_rd (cpu, rd, tmp);
      goto done;
    case MATCH_SUBN:
      TRACE_INSN (cpu, "p.subn %s, %s, %s, %"PRIiTW";  //",
		  rd_name, rs1_name, rs2_name, luimm5);
      store_rd (cpu, rd,
		(signed_word)(cpu->regs[rs1] - cpu->regs[rs2]) >> luimm5);
      goto done;
    case MATCH_SUBUN:
      TRACE_INSN (cpu, "p.subun %s, %s, %s, %"PRIiTW";  //",
		  rd_name, rs1_name, rs2_name, luimm5);
      store_rd (cpu, rd, (cpu->regs[rs1] - cpu->regs[rs2]) >> luimm5);
      goto done;
    case MATCH_SUBRN:
      TRACE_INSN (cpu, "p.subrn %s, %s, %s, %"PRIiTW";  //",
		  rd_name, rs1_name, rs2_name, luimm5);
      tmp = cpu->regs[rs1] - cpu->regs[rs2];
      tmp = round_s (tmp, luimm5);
      store_rd (cpu, rd, tmp);
      goto done;
    case MATCH_SUBURN:
      TRACE_INSN (cpu, "p.suburn %s, %s, %s, %"PRIiTW";  //",
		  rd_name, rs1_name, rs2_name, luimm5);
      tmp = cpu->regs[rs1] - cpu->regs[rs2];
      tmp = round_u (tmp, luimm5);
      store_rd (cpu, rd, tmp);
      goto done;
  }

  switch (iw & MASK_PALU)
    {
    case MATCH_ADDNR:
      TRACE_INSN (cpu, "p.addnr %s, %s, %s;  //",
		    rd_name, rs1_name, rs2_name);
      tmp = cpu->regs[rd] + cpu->regs[rs1];
      store_rd (cpu, rd, (signed_word)tmp >> (cpu->regs[rs2] & 0x1f));
      goto done;
    case MATCH_ADDUNR:
      TRACE_INSN (cpu, "p.addunr %s, %s, %s;  //",
		    rd_name, rs1_name, rs2_name);
      tmp = cpu->regs[rd] + cpu->regs[rs1];
      store_rd (cpu, rd, tmp >> (cpu->regs[rs2] & 0x1f));
      goto done;
    case MATCH_ADDRNR:
      TRACE_INSN (cpu, "p.addrnr %s, %s, %s;  //",
		    rd_name, rs1_name, rs2_name);
      tmp = cpu->regs[rd] + cpu->regs[rs1];
      shift = cpu->regs[rs2] & 0x1f;
      tmp = round_s (tmp, shift);
      store_rd (cpu, rd, tmp);
      goto done;
    case MATCH_ADDURNR:
      TRACE_INSN (cpu, "p.addurnr %s, %s, %s;  //",
		    rd_name, rs1_name, rs2_name);
      tmp = cpu->regs[rd] + cpu->regs[rs1];
      shift = cpu->regs[rs2] & 0x1f;
      tmp = round_u (tmp, shift);
      store_rd (cpu, rd, tmp);
      goto done;
    case MATCH_SUBNR:
      TRACE_INSN (cpu, "p.subnr %s, %s, %s;  //",
		    rd_name, rs1_name, rs2_name);
      tmp = cpu->regs[rd] - cpu->regs[rs1];
      store_rd (cpu, rd, (signed_word)tmp >> (cpu->regs[rs2] & 0x1f));
      goto done;
    case MATCH_SUBUNR:
      TRACE_INSN (cpu, "p.subunr %s, %s, %s;  //",
		    rd_name, rs1_name, rs2_name);
      tmp = cpu->regs[rd] - cpu->regs[rs1];
      store_rd (cpu, rd, tmp >> (cpu->regs[rs2] & 0x1f));
      goto done;
    case MATCH_SUBRNR:
      TRACE_INSN (cpu, "p.subrnr %s, %s, %s;  //",
		    rd_name, rs1_name, rs2_name);
      tmp = cpu->regs[rd] - cpu->regs[rs1];
      shift = cpu->regs[rs2] & 0x1f;
      tmp = round_s (tmp, shift);
      store_rd (cpu, rd, tmp);
      goto done;
    case MATCH_SUBURNR:
      TRACE_INSN (cpu, "p.suburnr %s, %s, %s;  //",
		    rd_name, rs1_name, rs2_name);
      tmp = cpu->regs[rd] - cpu->regs[rs1];
      shift = cpu->regs[rs2] & 0x1f;
      tmp = round_u (tmp, shift);
      store_rd (cpu, rd, tmp);
      goto done;
  }

#define RS1V (cpu->regs[rs1])
#define RS2V (cpu->regs[rs2])
#define RDV (cpu->regs[rd])

  /* PULP bitrev */

  if ((iw & (MASK_BITREV)) == (MATCH_BITREV))
    {
      /* d,s,bi,b5 */
      unsigned_word bits = uimm5;
      unsigned_word group = luimm5 & 3;
      unsigned_word x = RS1V << bits;
      TRACE_INSN (cpu, "pv.bitrev %s, %s, %"PRIiTW", %"PRIiTW"; //",
		  rd_name, rs1_name, group, bits);
      switch (group)
	{
	case 2: /* radix 8 special case */
	  {
	    int s = 30;
	    tmp = x;
	    /* since 32 is not divisible by 3, we drop the first two bits */
	    for (tmp >>= 2; tmp; tmp >>= 3)
	      {
		x <<= 3;
		x |= tmp & 7;
		s -= 3;
	      }
	    if (s > 0)
	      x <<= s;
	  }
	  break;
	case 0:
	  x = (((x & 0xaaaaaaaa) >> 1) | ((x & 0x55555555) << 1));
	case 1:
	  x = (((x & 0xcccccccc) >> 2) | ((x & 0x33333333) << 2));
	  x = (((x & 0xf0f0f0f0) >> 4) | ((x & 0x0f0f0f0f) << 4));
	case 3: /* doesn't actually exist */
	  x = (((x & 0xff00ff00) >> 8) | ((x & 0x00ff00ff) << 8));
	  x = ((x >> 16) | (x << 16));
	}
      store_rd (cpu, rd, x);
      goto done;
    }

  /* PULP SIMD operations */

  /* These refer to an external OP macro that defines the operation for this
     particular insn. The arguments passed to it are of the correct SIMD size
     and zero extended by default. Use sext() in the OP macro to produce a sign
     extended version. */

#define PULP_VECTOR_HI_V(INSN, MATCH, MASK)                                   \
  if ((iw & (MASK)) == (MATCH))                                               \
    {                                                                         \
      TRACE_INSN (cpu, INSN " %s, %s, %s;  //", rd_name, rs1_name, rs2_name); \
      low = OP (zext (RS1V, 16), zext (RS2V, 16)) & 0xffff;                   \
      high = OP (zext (RS1V >> 16, 16), zext (RS2V >> 16, 16)) & 0xffff;      \
      store_rd (cpu, rd, low | (high << 16));                                 \
      goto done;                                                              \
    }

#define PULP_VECTOR_HI_SC(INSN, MATCH, MASK)                                  \
  if ((iw & (MASK)) == (MATCH))                                               \
    {                                                                         \
      TRACE_INSN (cpu, INSN " %s, %s, %s;  //", rd_name, rs1_name, rs2_name); \
      low = OP (zext (RS1V, 16), zext (RS2V, 16)) & 0xffff;                   \
      high = OP (zext (RS1V >> 16, 16), zext (RS2V, 16)) & 0xffff;            \
      store_rd (cpu, rd, low | (high << 16));                                 \
      goto done;                                                              \
    }

#define PULP_VECTOR_HI_SCI(INSN, MATCH, MASK)                                 \
  if ((iw & (MASK)) == (MATCH))                                               \
    {                                                                         \
      TRACE_INSN (cpu, INSN " %s, %s, %" PRIiTW "; //", rd_name, rs1_name,    \
                  vsimm6);                                                    \
      low = OP (zext (RS1V, 16), vsimm6) & 0xffff;                            \
      high = OP (zext (RS1V >> 16, 16), vsimm6) & 0xffff;                     \
      store_rd (cpu, rd, low | (high << 16));                                 \
      goto done;                                                              \
    }

#define PULP_VECTOR_HI_SCI_U(INSN, MATCH, MASK)                               \
  if ((iw & (MASK)) == (MATCH))                                               \
    {                                                                         \
      TRACE_INSN (cpu, INSN " %s, %s, %" PRIiTW "; //", rd_name, rs1_name,    \
                  vsimm6);                                                    \
      low = OP (zext (RS1V, 16), zext (vsimm6, 6)) & 0xffff;                  \
      high = OP (zext (RS1V >> 16, 16), zext (vsimm6, 6)) & 0xffff;           \
      store_rd (cpu, rd, low | (high << 16));                                 \
      goto done;                                                              \
    }

#define PULP_VECTOR_QI_V(INSN, MATCH, MASK)                                   \
  if ((iw & (MASK)) == (MATCH))                                               \
    {                                                                         \
      TRACE_INSN (cpu, INSN " %s, %s, %s;  //", rd_name, rs1_name, rs2_name); \
      b0 = OP (zext (RS1V, 8), zext (RS2V, 8)) & 0xff;                        \
      b1 = OP (zext (RS1V >> 8, 8), zext (RS2V >> 8, 8)) & 0xff;              \
      b2 = OP (zext (RS1V >> 16, 8), zext (RS2V >> 16, 8)) & 0xff;            \
      b3 = OP (zext (RS1V >> 24, 8), zext (RS2V >> 24, 8)) & 0xff;            \
      store_rd (cpu, rd, b0 | (b1 << 8) | (b2 << 16) | (b3 << 24));           \
      goto done;                                                              \
    }

#define PULP_VECTOR_QI_SC(INSN, MATCH, MASK)                                  \
  if ((iw & (MASK)) == (MATCH))                                               \
    {                                                                         \
      TRACE_INSN (cpu, INSN " %s, %s, %s;  //", rd_name, rs1_name, rs2_name); \
      b0 = OP (zext (RS1V, 8), zext (RS2V, 8)) & 0xff;                        \
      b1 = OP (zext (RS1V >> 8, 8), zext (RS2V, 8)) & 0xff;                   \
      b2 = OP (zext (RS1V >> 16, 8), zext (RS2V, 8)) & 0xff;                  \
      b3 = OP (zext (RS1V >> 24, 8), zext (RS2V, 8)) & 0xff;                  \
      store_rd (cpu, rd, b0 | (b1 << 8) | (b2 << 16) | (b3 << 24));           \
      goto done;                                                              \
    }

#define PULP_VECTOR_QI_SCI(INSN, MATCH, MASK)                                 \
  if ((iw & (MASK)) == (MATCH))                                               \
    {                                                                         \
      TRACE_INSN (cpu, INSN " %s, %s, %" PRIiTW "; //", rd_name, rs1_name,    \
                  vsimm6);                                                    \
      b0 = OP (zext (RS1V, 8), vsimm6) & 0xff;                                \
      b1 = OP (zext (RS1V >> 8, 8), vsimm6) & 0xff;                           \
      b2 = OP (zext (RS1V >> 16, 8), vsimm6) & 0xff;                          \
      b3 = OP (zext (RS1V >> 24, 8), vsimm6) & 0xff;                          \
      store_rd (cpu, rd, b0 | (b1 << 8) | (b2 << 16) | (b3 << 24));           \
      goto done;                                                              \
    }

#define PULP_VECTOR_QI_SCI_U(INSN, MATCH, MASK)                               \
  if ((iw & (MASK)) == (MATCH))                                               \
    {                                                                         \
      TRACE_INSN (cpu, INSN " %s, %s, %" PRIiTW "; //", rd_name, rs1_name,    \
                  vsimm6);                                                    \
      b0 = OP (zext (RS1V, 8), zext (vsimm6, 6)) & 0xff;                      \
      b1 = OP (zext (RS1V >> 8, 8), zext (vsimm6, 6)) & 0xff;                 \
      b2 = OP (zext (RS1V >> 16, 8), zext (vsimm6, 6)) & 0xff;                \
      b3 = OP (zext (RS1V >> 24, 8), zext (vsimm6, 6)) & 0xff;                \
      store_rd (cpu, rd, b0 | (b1 << 8) | (b2 << 16) | (b3 << 24));           \
      goto done;                                                              \
    }

  /* Similar to the previous macros, but instead of concatenating the resulting
     words we can freely chose the function */

#define PULP_VECTOR_HI_V_FOLD(INSN, MATCH, MASK)                              \
  if ((iw & (MASK)) == (MATCH))                                               \
    {                                                                         \
      TRACE_INSN (cpu, INSN " %s, %s, %s;  //", rd_name, rs1_name, rs2_name); \
      low = OP (zext (RS1V, 16), zext (RS2V, 16));                            \
      high = OP (zext (RS1V >> 16, 16), zext (RS2V >> 16, 16));               \
      store_rd (cpu, rd, FOLD2 (low, high, RDV));                             \
      goto done;                                                              \
    }

#define PULP_VECTOR_HI_SC_FOLD(INSN, MATCH, MASK)                             \
  if ((iw & (MASK)) == (MATCH))                                               \
    {                                                                         \
      TRACE_INSN (cpu, INSN " %s, %s, %s;  //", rd_name, rs1_name, rs2_name); \
      low = OP (zext (RS1V, 16), zext (RS2V, 16));                            \
      high = OP (zext (RS1V >> 16, 16), zext (RS2V, 16));                     \
      store_rd (cpu, rd, FOLD2 (low, high, RDV));                             \
      goto done;                                                              \
    }

#define PULP_VECTOR_HI_SCI_FOLD(INSN, MATCH, MASK)                            \
  if ((iw & (MASK)) == (MATCH))                                               \
    {                                                                         \
      TRACE_INSN (cpu, INSN " %s, %s, %" PRIiTW "; //", rd_name, rs1_name,    \
                  vsimm6);                                                    \
      low = OP (zext (RS1V, 16), vsimm6);                                     \
      high = OP (zext (RS1V >> 16, 16), vsimm6);                              \
      store_rd (cpu, rd, FOLD2 (low, high, RDV));                             \
      goto done;                                                              \
    }

#define PULP_VECTOR_HI_SCI_U_FOLD(INSN, MATCH, MASK)                          \
  if ((iw & (MASK)) == (MATCH))                                               \
    {                                                                         \
      TRACE_INSN (cpu, INSN " %s, %s, %" PRIiTW "; //", rd_name, rs1_name,    \
                  vsimm6);                                                    \
      low = OP (zext (RS1V, 16), zext (vsimm6, 6));                           \
      high = OP (zext (RS1V >> 16, 16), zext (vsimm6, 6));                    \
      store_rd (cpu, rd, FOLD2 (low, high, RDV));                             \
      goto done;                                                              \
    }

#define PULP_VECTOR_QI_V_FOLD(INSN, MATCH, MASK)                              \
  if ((iw & (MASK)) == (MATCH))                                               \
    {                                                                         \
      TRACE_INSN (cpu, INSN " %s, %s, %s;  //", rd_name, rs1_name, rs2_name); \
      b0 = OP (zext (RS1V, 8), zext (RS2V, 8));                               \
      b1 = OP (zext (RS1V >> 8, 8), zext (RS2V >> 8, 8));                     \
      b2 = OP (zext (RS1V >> 16, 8), zext (RS2V >> 16, 8));                   \
      b3 = OP (zext (RS1V >> 24, 8), zext (RS2V >> 24, 8));                   \
      store_rd (cpu, rd, FOLD4 (b0, b1, b2, b3, RDV));                        \
      goto done;                                                              \
    }

#define PULP_VECTOR_QI_SC_FOLD(INSN, MATCH, MASK)                             \
  if ((iw & (MASK)) == (MATCH))                                               \
    {                                                                         \
      TRACE_INSN (cpu, INSN " %s, %s, %s;  //", rd_name, rs1_name, rs2_name); \
      b0 = OP (zext (RS1V, 8), zext (RS2V, 8));                               \
      b1 = OP (zext (RS1V >> 8, 8), zext (RS2V, 8));                          \
      b2 = OP (zext (RS1V >> 16, 8), zext (RS2V, 8));                         \
      b3 = OP (zext (RS1V >> 24, 8), zext (RS2V, 8));                         \
      store_rd (cpu, rd, FOLD4 (b0, b1, b2, b3, RDV));                        \
      goto done;                                                              \
    }

#define PULP_VECTOR_QI_SCI_FOLD(INSN, MATCH, MASK)                            \
  if ((iw & (MASK)) == (MATCH))                                               \
    {                                                                         \
      TRACE_INSN (cpu, INSN " %s, %s, %" PRIiTW "; //", rd_name, rs1_name,    \
                  vsimm6);                                                    \
      b0 = OP (zext (RS1V, 8), vsimm6);                                       \
      b1 = OP (zext (RS1V >> 8, 8), vsimm6);                                  \
      b2 = OP (zext (RS1V >> 16, 8), vsimm6);                                 \
      b3 = OP (zext (RS1V >> 24, 8), vsimm6);                                 \
      store_rd (cpu, rd, FOLD4 (b0, b1, b2, b3, RDV));                        \
      goto done;                                                              \
    }

#define PULP_VECTOR_QI_SCI_U_FOLD(INSN, MATCH, MASK)                          \
  if ((iw & (MASK)) == (MATCH))                                               \
    {                                                                         \
      TRACE_INSN (cpu, INSN " %s, %s, %" PRIiTW "; //", rd_name, rs1_name,    \
                  vsimm6);                                                    \
      b0 = OP (zext (RS1V, 8), zext (vsimm6, 6));                             \
      b1 = OP (zext (RS1V >> 8, 8), zext (vsimm6, 6));                        \
      b2 = OP (zext (RS1V >> 16, 8), zext (vsimm6, 6));                       \
      b3 = OP (zext (RS1V >> 24, 8), zext (vsimm6, 6));                       \
      store_rd (cpu, rd, FOLD4 (b0, b1, b2, b3, RDV));                        \
      goto done;                                                              \
    }

#define OP(A, B) ((A) + (B))
  PULP_VECTOR_HI_V  ("pv.add.h",     MATCH_V_OP_ADD|MATCH_V_OP_H_VV, MASK_V_OP  );
  PULP_VECTOR_HI_SC ("pv.add.sc.h",  MATCH_V_OP_ADD|MATCH_V_OP_H_VR, MASK_V_OP  );
  PULP_VECTOR_HI_SCI("pv.add.sci.h", MATCH_V_OP_ADD|MATCH_V_OP_H_VI, MASK_V_OP1 );
  PULP_VECTOR_QI_V  ("pv.add.b",     MATCH_V_OP_ADD|MATCH_V_OP_B_VV, MASK_V_OP  );
  PULP_VECTOR_QI_SC ("pv.add.sc.b",  MATCH_V_OP_ADD|MATCH_V_OP_B_VR, MASK_V_OP  );
  PULP_VECTOR_QI_SCI("pv.add.sci.b", MATCH_V_OP_ADD|MATCH_V_OP_B_VI, MASK_V_OP1 );
#undef OP

#define OP(A, B) ((signed_word)sext ((A) + (B), 16) >> 1)
  PULP_VECTOR_HI_V  ("pv.add.h.div2", MATCH_V_OP_ADD_DIV|MATCH_V_OP_H_VV_S1, MASK_V_OP  );
#undef OP
#define OP(A, B) ((signed_word)sext ((A) + (B), 16) >> 2)
  PULP_VECTOR_HI_V  ("pv.add.h.div4", MATCH_V_OP_ADD_DIV|MATCH_V_OP_H_VV_S2, MASK_V_OP  );
#undef OP
#define OP(A, B) ((signed_word)sext ((A) + (B), 16) >> 3)
  PULP_VECTOR_HI_V  ("pv.add.h.div8", MATCH_V_OP_ADD_DIV|MATCH_V_OP_H_VV_S3, MASK_V_OP  );
#undef OP

#define OP(A, B) ((A) - (B))
  PULP_VECTOR_HI_V  ("pv.sub.h",     MATCH_V_OP_SUB|MATCH_V_OP_H_VV, MASK_V_OP  );
  PULP_VECTOR_HI_SC ("pv.sub.sc.h",  MATCH_V_OP_SUB|MATCH_V_OP_H_VR, MASK_V_OP  );
  PULP_VECTOR_HI_SCI("pv.sub.sci.h", MATCH_V_OP_SUB|MATCH_V_OP_H_VI, MASK_V_OP1 );
  PULP_VECTOR_QI_V  ("pv.sub.b",     MATCH_V_OP_SUB|MATCH_V_OP_B_VV, MASK_V_OP  );
  PULP_VECTOR_QI_SC ("pv.sub.sc.b",  MATCH_V_OP_SUB|MATCH_V_OP_B_VR, MASK_V_OP  );
  PULP_VECTOR_QI_SCI("pv.sub.sci.b", MATCH_V_OP_SUB|MATCH_V_OP_B_VI, MASK_V_OP1 );
#undef OP

#define OP(A, B) ((signed_word)sext ((A) - (B), 16) >> 1)
  PULP_VECTOR_HI_V  ("pv.sub.h.div2", MATCH_V_OP_SUB_DIV|MATCH_V_OP_H_VV_S1, MASK_V_OP  );
#undef OP
#define OP(A, B) ((signed_word)sext ((A) - (B), 16) >> 2)
  PULP_VECTOR_HI_V  ("pv.sub.h.div4", MATCH_V_OP_SUB_DIV|MATCH_V_OP_H_VV_S2, MASK_V_OP  );
#undef OP
#define OP(A, B) ((signed_word)sext ((A) - (B), 16) >> 3)
  PULP_VECTOR_HI_V  ("pv.sub.h.div8", MATCH_V_OP_SUB_DIV|MATCH_V_OP_H_VV_S3, MASK_V_OP  );
#undef OP

#define OP(A, B) (((signed_word)sext((A) + (B), 16)) >> 1)
  PULP_VECTOR_HI_V  ("pv.avg.h",     MATCH_V_OP_AVG|MATCH_V_OP_H_VV, MASK_V_OP  );
  PULP_VECTOR_HI_SC ("pv.avg.sc.h",  MATCH_V_OP_AVG|MATCH_V_OP_H_VR, MASK_V_OP  );
  PULP_VECTOR_HI_SCI("pv.avg.sci.h", MATCH_V_OP_AVG|MATCH_V_OP_H_VI, MASK_V_OP1 );
#undef OP
#define OP(A, B) (((signed_word)sext((A) + (B), 8)) >> 1)
  PULP_VECTOR_QI_V  ("pv.avg.b",     MATCH_V_OP_AVG|MATCH_V_OP_B_VV, MASK_V_OP  );
  PULP_VECTOR_QI_SC ("pv.avg.sc.b",  MATCH_V_OP_AVG|MATCH_V_OP_B_VR, MASK_V_OP  );
  PULP_VECTOR_QI_SCI("pv.avg.sci.b", MATCH_V_OP_AVG|MATCH_V_OP_B_VI, MASK_V_OP1 );
#undef OP

#define OP(A, B) (zext((A) + (B), 16) >> 1)
  PULP_VECTOR_HI_V  ("pv.avgu.h",     MATCH_V_OP_AVGU|MATCH_V_OP_H_VV, MASK_V_OP  );
  PULP_VECTOR_HI_SC ("pv.avgu.sc.h",  MATCH_V_OP_AVGU|MATCH_V_OP_H_VR, MASK_V_OP  );
  PULP_VECTOR_HI_SCI_U("pv.avgu.sci.h", MATCH_V_OP_AVGU|MATCH_V_OP_H_VI, MASK_V_OP1 );
#undef OP
#define OP(A, B) (zext((A) + (B), 8) >> 1)
  PULP_VECTOR_QI_V  ("pv.avgu.b",     MATCH_V_OP_AVGU|MATCH_V_OP_B_VV, MASK_V_OP  );
  PULP_VECTOR_QI_SC ("pv.avgu.sc.b",  MATCH_V_OP_AVGU|MATCH_V_OP_B_VR, MASK_V_OP  );
  PULP_VECTOR_QI_SCI_U("pv.avgu.sci.b", MATCH_V_OP_AVGU|MATCH_V_OP_B_VI, MASK_V_OP1 );
#undef OP

#define OP(A, B) (((signed_word)sext(A, 16)) < ((signed_word)sext(B, 16)) ? A : B)
  PULP_VECTOR_HI_V  ("pv.min.h",     MATCH_V_OP_MIN|MATCH_V_OP_H_VV, MASK_V_OP  );
  PULP_VECTOR_HI_SC ("pv.min.sc.h",  MATCH_V_OP_MIN|MATCH_V_OP_H_VR, MASK_V_OP  );
  PULP_VECTOR_HI_SCI("pv.min.sci.h", MATCH_V_OP_MIN|MATCH_V_OP_H_VI, MASK_V_OP1 );
#undef OP
#define OP(A, B) (((signed_word)sext(A, 8)) < ((signed_word)sext(B, 8)) ? A : B)
  PULP_VECTOR_QI_V  ("pv.min.b",     MATCH_V_OP_MIN|MATCH_V_OP_B_VV, MASK_V_OP  );
  PULP_VECTOR_QI_SC ("pv.min.sc.b",  MATCH_V_OP_MIN|MATCH_V_OP_B_VR, MASK_V_OP  );
  PULP_VECTOR_QI_SCI("pv.min.sci.b", MATCH_V_OP_MIN|MATCH_V_OP_B_VI, MASK_V_OP1 );
#undef OP

/* TODO: immediate is zero extended */
#define OP(A, B) (((unsigned_word)A) < ((unsigned_word)B) ? A : B)
  PULP_VECTOR_HI_V  ("pv.minu.h",     MATCH_V_OP_MINU|MATCH_V_OP_H_VV, MASK_V_OP  );
  PULP_VECTOR_HI_SC ("pv.minu.sc.h",  MATCH_V_OP_MINU|MATCH_V_OP_H_VR, MASK_V_OP  );
  PULP_VECTOR_HI_SCI_U("pv.minu.sci.h", MATCH_V_OP_MINU|MATCH_V_OP_H_VI, MASK_V_OP1 );
  PULP_VECTOR_QI_V  ("pv.minu.b",     MATCH_V_OP_MINU|MATCH_V_OP_B_VV, MASK_V_OP  );
  PULP_VECTOR_QI_SC ("pv.minu.sc.b",  MATCH_V_OP_MINU|MATCH_V_OP_B_VR, MASK_V_OP  );
  PULP_VECTOR_QI_SCI_U("pv.minu.sci.b", MATCH_V_OP_MINU|MATCH_V_OP_B_VI, MASK_V_OP1 );
#undef OP

#define OP(A, B) (((signed_word)sext(A, 16)) > ((signed_word)sext(B, 16)) ? A : B)
  PULP_VECTOR_HI_V  ("pv.max.h",     MATCH_V_OP_MAX|MATCH_V_OP_H_VV, MASK_V_OP  );
  PULP_VECTOR_HI_SC ("pv.max.sc.h",  MATCH_V_OP_MAX|MATCH_V_OP_H_VR, MASK_V_OP  );
  PULP_VECTOR_HI_SCI("pv.max.sci.h", MATCH_V_OP_MAX|MATCH_V_OP_H_VI, MASK_V_OP1 );
#undef OP
#define OP(A, B) (((signed_word)sext(A, 8)) > ((signed_word)sext(B, 8)) ? A : B)
  PULP_VECTOR_QI_V  ("pv.max.b",     MATCH_V_OP_MAX|MATCH_V_OP_B_VV, MASK_V_OP  );
  PULP_VECTOR_QI_SC ("pv.max.sc.b",  MATCH_V_OP_MAX|MATCH_V_OP_B_VR, MASK_V_OP  );
  PULP_VECTOR_QI_SCI("pv.max.sci.b", MATCH_V_OP_MAX|MATCH_V_OP_B_VI, MASK_V_OP1 );
#undef OP

/* TODO: immediate is zero extended */
#define OP(A, B) (((unsigned_word)A) > ((unsigned_word)B) ? A : B)
  PULP_VECTOR_HI_V  ("pv.maxu.h",     MATCH_V_OP_MAXU|MATCH_V_OP_H_VV, MASK_V_OP  );
  PULP_VECTOR_HI_SC ("pv.maxu.sc.h",  MATCH_V_OP_MAXU|MATCH_V_OP_H_VR, MASK_V_OP  );
  PULP_VECTOR_HI_SCI_U("pv.maxu.sci.h", MATCH_V_OP_MAXU|MATCH_V_OP_H_VI, MASK_V_OP1 );
  PULP_VECTOR_QI_V  ("pv.maxu.b",     MATCH_V_OP_MAXU|MATCH_V_OP_B_VV, MASK_V_OP  );
  PULP_VECTOR_QI_SC ("pv.maxu.sc.b",  MATCH_V_OP_MAXU|MATCH_V_OP_B_VR, MASK_V_OP  );
  PULP_VECTOR_QI_SCI_U("pv.maxu.sci.b", MATCH_V_OP_MAXU|MATCH_V_OP_B_VI, MASK_V_OP1 );
#undef OP

/* TODO: immediate is zero extended */
#define OP(A, B) (((unsigned_word)A) >> ((unsigned_word)B))
  PULP_VECTOR_HI_V  ("pv.srl.h",     MATCH_V_OP_SRL|MATCH_V_OP_H_VV, MASK_V_OP  );
  PULP_VECTOR_HI_SC ("pv.srl.sc.h",  MATCH_V_OP_SRL|MATCH_V_OP_H_VR, MASK_V_OP  );
  PULP_VECTOR_HI_SCI_U("pv.srl.sci.h", MATCH_V_OP_SRL|MATCH_V_OP_H_VI, MASK_V_OP1 );
  PULP_VECTOR_QI_V  ("pv.srl.b",     MATCH_V_OP_SRL|MATCH_V_OP_B_VV, MASK_V_OP  );
  PULP_VECTOR_QI_SC ("pv.srl.sc.b",  MATCH_V_OP_SRL|MATCH_V_OP_B_VR, MASK_V_OP  );
  PULP_VECTOR_QI_SCI_U("pv.srl.sci.b", MATCH_V_OP_SRL|MATCH_V_OP_B_VI, MASK_V_OP1 );
#undef OP

/* TODO: immediate is zero extended */
#define OP(A, B) ((signed_word)sext(A, 16) >> ((signed_word)B))
  PULP_VECTOR_HI_V  ("pv.sra.h",     MATCH_V_OP_SRA|MATCH_V_OP_H_VV, MASK_V_OP  );
  PULP_VECTOR_HI_SC ("pv.sra.sc.h",  MATCH_V_OP_SRA|MATCH_V_OP_H_VR, MASK_V_OP  );
  PULP_VECTOR_HI_SCI_U("pv.sra.sci.h", MATCH_V_OP_SRA|MATCH_V_OP_H_VI, MASK_V_OP1 );
#undef OP
#define OP(A, B) ((signed_word)sext(A, 8) >> ((signed_word)B))
  PULP_VECTOR_QI_V  ("pv.sra.b",     MATCH_V_OP_SRA|MATCH_V_OP_B_VV, MASK_V_OP  );
  PULP_VECTOR_QI_SC ("pv.sra.sc.b",  MATCH_V_OP_SRA|MATCH_V_OP_B_VR, MASK_V_OP  );
  PULP_VECTOR_QI_SCI_U("pv.sra.sci.b", MATCH_V_OP_SRA|MATCH_V_OP_B_VI, MASK_V_OP1 );
#undef OP

/* TODO: immediate is zero extended */
#define OP(A, B) (((unsigned_word)A) << ((unsigned_word)B))
  PULP_VECTOR_HI_V  ("pv.sll.h",     MATCH_V_OP_SLL|MATCH_V_OP_H_VV, MASK_V_OP  );
  PULP_VECTOR_HI_SC ("pv.sll.sc.h",  MATCH_V_OP_SLL|MATCH_V_OP_H_VR, MASK_V_OP  );
  PULP_VECTOR_HI_SCI_U("pv.sll.sci.h", MATCH_V_OP_SLL|MATCH_V_OP_H_VI, MASK_V_OP1 );
  PULP_VECTOR_QI_V  ("pv.sll.b",     MATCH_V_OP_SLL|MATCH_V_OP_B_VV, MASK_V_OP  );
  PULP_VECTOR_QI_SC ("pv.sll.sc.b",  MATCH_V_OP_SLL|MATCH_V_OP_B_VR, MASK_V_OP  );
  PULP_VECTOR_QI_SCI_U("pv.sll.sci.b", MATCH_V_OP_SLL|MATCH_V_OP_B_VI, MASK_V_OP1 );
#undef OP

#define OP(A, B) ((A) | (B))
  PULP_VECTOR_HI_V  ("pv.or.h",     MATCH_V_OP_OR|MATCH_V_OP_H_VV, MASK_V_OP  );
  PULP_VECTOR_HI_SC ("pv.or.sc.h",  MATCH_V_OP_OR|MATCH_V_OP_H_VR, MASK_V_OP  );
  PULP_VECTOR_HI_SCI("pv.or.sci.h", MATCH_V_OP_OR|MATCH_V_OP_H_VI, MASK_V_OP1 );
  PULP_VECTOR_QI_V  ("pv.or.b",     MATCH_V_OP_OR|MATCH_V_OP_B_VV, MASK_V_OP  );
  PULP_VECTOR_QI_SC ("pv.or.sc.b",  MATCH_V_OP_OR|MATCH_V_OP_B_VR, MASK_V_OP  );
  PULP_VECTOR_QI_SCI("pv.or.sci.b", MATCH_V_OP_OR|MATCH_V_OP_B_VI, MASK_V_OP1 );
#undef OP

#define OP(A, B) ((A) ^ (B))
  PULP_VECTOR_HI_V  ("pv.xor.h",     MATCH_V_OP_XOR|MATCH_V_OP_H_VV, MASK_V_OP  );
  PULP_VECTOR_HI_SC ("pv.xor.sc.h",  MATCH_V_OP_XOR|MATCH_V_OP_H_VR, MASK_V_OP  );
  PULP_VECTOR_HI_SCI("pv.xor.sci.h", MATCH_V_OP_XOR|MATCH_V_OP_H_VI, MASK_V_OP1 );
  PULP_VECTOR_QI_V  ("pv.xor.b",     MATCH_V_OP_XOR|MATCH_V_OP_B_VV, MASK_V_OP  );
  PULP_VECTOR_QI_SC ("pv.xor.sc.b",  MATCH_V_OP_XOR|MATCH_V_OP_B_VR, MASK_V_OP  );
  PULP_VECTOR_QI_SCI("pv.xor.sci.b", MATCH_V_OP_XOR|MATCH_V_OP_B_VI, MASK_V_OP1 );
#undef OP

#define OP(A, B) ((A) & (B))
  PULP_VECTOR_HI_V  ("pv.and.h",     MATCH_V_OP_AND|MATCH_V_OP_H_VV, MASK_V_OP  );
  PULP_VECTOR_HI_SC ("pv.and.sc.h",  MATCH_V_OP_AND|MATCH_V_OP_H_VR, MASK_V_OP  );
  PULP_VECTOR_HI_SCI("pv.and.sci.h", MATCH_V_OP_AND|MATCH_V_OP_H_VI, MASK_V_OP1 );
  PULP_VECTOR_QI_V  ("pv.and.b",     MATCH_V_OP_AND|MATCH_V_OP_B_VV, MASK_V_OP  );
  PULP_VECTOR_QI_SC ("pv.and.sc.b",  MATCH_V_OP_AND|MATCH_V_OP_B_VR, MASK_V_OP  );
  PULP_VECTOR_QI_SCI("pv.and.sci.b", MATCH_V_OP_AND|MATCH_V_OP_B_VI, MASK_V_OP1 );
#undef OP

#define OP(A, B) ((signed_word)sext(A, 16) < 0 ? (-sext(A, 16)) : A)
  PULP_VECTOR_HI_V  ("pv.abs.h", MATCH_V_OP_ABS|MATCH_V_OP_H_VV, MASK_V_OP2  );
#undef OP
#define OP(A, B) ((signed_word)sext(A, 8) < 0 ? (-sext(A, 8)) : (A))
  PULP_VECTOR_QI_V  ("pv.abs.b", MATCH_V_OP_ABS|MATCH_V_OP_B_VV, MASK_V_OP2  );
#undef OP

  /* these don't operate on h or b wide words so we handle them manually */

  if ((iw & (MASK_V_OP1)) == (MATCH_V_OP_EXTRACT|MATCH_V_OP_H_VI))
    {
      TRACE_INSN (cpu, "pv.extract.h"" %s, %s, %"PRIiTW"; //",
		  rd_name, rs1_name, vsimm6);
      tmp = sext(RS1V >> (16 * (vsimm6 & 1)), 16);
      store_rd (cpu, rd, tmp);
      goto done;
    }

  if ((iw & (MASK_V_OP1)) == (MATCH_V_OP_EXTRACT|MATCH_V_OP_B_VI))
    {
      TRACE_INSN (cpu, "pv.extract.b"" %s, %s, %"PRIiTW"; //",
		  rd_name, rs1_name, vsimm6);
      tmp = sext(RS1V >> (8 * (vsimm6 & 3)), 8);
      store_rd (cpu, rd, tmp);
      goto done;
    }

  /* yes these masks are weird */

  if ((iw & (MASK_V_OP1)) == (MATCH_V_OP_DOTSP|MATCH_V_OP_H_VI))
    {
      TRACE_INSN (cpu, "pv.extractu.h"" %s, %s, %"PRIiTW"; //",
		  rd_name, rs1_name, vsimm6);
      tmp = zext(RS1V >> (16 * (vsimm6 & 1)), 16);
      store_rd (cpu, rd, tmp);
      goto done;
    }

  if ((iw & (MASK_V_OP1)) == (MATCH_V_OP_DOTSP|MATCH_V_OP_B_VI))
    {
      TRACE_INSN (cpu, "pv.extractu.b"" %s, %s, %"PRIiTW"; //",
		  rd_name, rs1_name, vsimm6);
      tmp = zext(RS1V >> (8 * (vsimm6 & 3)), 8);
      store_rd (cpu, rd, tmp);
      goto done;
    }

  if ((iw & (MASK_V_OP1)) == (MATCH_V_OP_SDOTUP|MATCH_V_OP_H_VI))
    {
      TRACE_INSN (cpu, "pv.insert.h"" %s, %s, %"PRIiTW"; //",
		  rd_name, rs1_name, vsimm6);
      tmp = RDV & ~(0xffff << (16 * (vsimm6 & 1)));
      tmp |=  (zext(RS1V, 16) << (16 * (vsimm6 & 1)));
      store_rd (cpu, rd, tmp);
      goto done;
    }

  if ((iw & (MASK_V_OP1)) == (MATCH_V_OP_SDOTUP|MATCH_V_OP_B_VI))
    {
      TRACE_INSN (cpu, "pv.insert.b"" %s, %s, %"PRIiTW"; //",
		  rd_name, rs1_name, vsimm6);
      tmp = RDV & ~(0xff << (8 * (vsimm6 & 3)));
      tmp |=  (zext(RS1V, 8) << (8 * (vsimm6 & 3)));
      store_rd (cpu, rd, tmp);
      goto done;
    }


#define OP(A, B) ((A) == (B) ? 0xffffffff : 0)
  PULP_VECTOR_HI_V  ("pv.cmpeq.h",     MATCH_V_OP_CMPEQ|MATCH_V_OP_H_VV, MASK_V_OP  );
  PULP_VECTOR_HI_SC ("pv.cmpeq.sc.h",  MATCH_V_OP_CMPEQ|MATCH_V_OP_H_VR, MASK_V_OP  );
  PULP_VECTOR_HI_SCI("pv.cmpeq.sci.h", MATCH_V_OP_CMPEQ|MATCH_V_OP_H_VI, MASK_V_OP1 );
  PULP_VECTOR_QI_V  ("pv.cmpeq.b",     MATCH_V_OP_CMPEQ|MATCH_V_OP_B_VV, MASK_V_OP  );
  PULP_VECTOR_QI_SC ("pv.cmpeq.sc.b",  MATCH_V_OP_CMPEQ|MATCH_V_OP_B_VR, MASK_V_OP  );
  PULP_VECTOR_QI_SCI("pv.cmpeq.sci.b", MATCH_V_OP_CMPEQ|MATCH_V_OP_B_VI, MASK_V_OP1 );
#undef OP

#define OP(A, B) ((A) != (B) ? 0xffffffff : 0)
  PULP_VECTOR_HI_V  ("pv.cmpne.h",     MATCH_V_OP_CMPNE|MATCH_V_OP_H_VV, MASK_V_OP  );
  PULP_VECTOR_HI_SC ("pv.cmpne.sc.h",  MATCH_V_OP_CMPNE|MATCH_V_OP_H_VR, MASK_V_OP  );
  PULP_VECTOR_HI_SCI("pv.cmpne.sci.h", MATCH_V_OP_CMPNE|MATCH_V_OP_H_VI, MASK_V_OP1 );
  PULP_VECTOR_QI_V  ("pv.cmpne.b",     MATCH_V_OP_CMPNE|MATCH_V_OP_B_VV, MASK_V_OP  );
  PULP_VECTOR_QI_SC ("pv.cmpne.sc.b",  MATCH_V_OP_CMPNE|MATCH_V_OP_B_VR, MASK_V_OP  );
  PULP_VECTOR_QI_SCI("pv.cmpne.sci.b", MATCH_V_OP_CMPNE|MATCH_V_OP_B_VI, MASK_V_OP1 );
#undef OP

#define OP(A, B) ((signed_word)sext(A, 16) > (signed_word)sext(B, 16) ? 0xffffffff : 0)
  PULP_VECTOR_HI_V  ("pv.cmpgt.h",     MATCH_V_OP_CMPGT|MATCH_V_OP_H_VV, MASK_V_OP  );
  PULP_VECTOR_HI_SC ("pv.cmpgt.sc.h",  MATCH_V_OP_CMPGT|MATCH_V_OP_H_VR, MASK_V_OP  );
  PULP_VECTOR_HI_SCI("pv.cmpgt.sci.h", MATCH_V_OP_CMPGT|MATCH_V_OP_H_VI, MASK_V_OP1 );
#undef OP
#define OP(A, B) ((signed_word)sext(A, 8) > (signed_word)sext(B, 8) ? 0xffffffff : 0)
  PULP_VECTOR_QI_V  ("pv.cmpgt.b",     MATCH_V_OP_CMPGT|MATCH_V_OP_B_VV, MASK_V_OP  );
  PULP_VECTOR_QI_SC ("pv.cmpgt.sc.b",  MATCH_V_OP_CMPGT|MATCH_V_OP_B_VR, MASK_V_OP  );
  PULP_VECTOR_QI_SCI("pv.cmpgt.sci.b", MATCH_V_OP_CMPGT|MATCH_V_OP_B_VI, MASK_V_OP1 );
#undef OP

#define OP(A, B) ((signed_word)sext(A, 16) >= (signed_word)sext(B, 16) ? 0xffffffff : 0)
  PULP_VECTOR_HI_V  ("pv.cmpge.h",     MATCH_V_OP_CMPGE|MATCH_V_OP_H_VV, MASK_V_OP  );
  PULP_VECTOR_HI_SC ("pv.cmpge.sc.h",  MATCH_V_OP_CMPGE|MATCH_V_OP_H_VR, MASK_V_OP  );
  PULP_VECTOR_HI_SCI("pv.cmpge.sci.h", MATCH_V_OP_CMPGE|MATCH_V_OP_H_VI, MASK_V_OP1 );
#undef OP
#define OP(A, B) ((signed_word)sext(A, 8) >= (signed_word)sext(B, 8) ? 0xffffffff : 0)
  PULP_VECTOR_QI_V  ("pv.cmpge.b",     MATCH_V_OP_CMPGE|MATCH_V_OP_B_VV, MASK_V_OP  );
  PULP_VECTOR_QI_SC ("pv.cmpge.sc.b",  MATCH_V_OP_CMPGE|MATCH_V_OP_B_VR, MASK_V_OP  );
  PULP_VECTOR_QI_SCI("pv.cmpge.sci.b", MATCH_V_OP_CMPGE|MATCH_V_OP_B_VI, MASK_V_OP1 );
#undef OP

#define OP(A, B) ((signed_word)sext(A, 16) < (signed_word)sext(B, 16) ? 0xffffffff : 0)
  PULP_VECTOR_HI_V  ("pv.cmplt.h",     MATCH_V_OP_CMPLT|MATCH_V_OP_H_VV, MASK_V_OP  );
  PULP_VECTOR_HI_SC ("pv.cmplt.sc.h",  MATCH_V_OP_CMPLT|MATCH_V_OP_H_VR, MASK_V_OP  );
  PULP_VECTOR_HI_SCI("pv.cmplt.sci.h", MATCH_V_OP_CMPLT|MATCH_V_OP_H_VI, MASK_V_OP1 );
#undef OP
#define OP(A, B) ((signed_word)sext(A, 8) < (signed_word)sext(B, 8) ? 0xffffffff : 0)
  PULP_VECTOR_QI_V  ("pv.cmplt.b",     MATCH_V_OP_CMPLT|MATCH_V_OP_B_VV, MASK_V_OP  );
  PULP_VECTOR_QI_SC ("pv.cmplt.sc.b",  MATCH_V_OP_CMPLT|MATCH_V_OP_B_VR, MASK_V_OP  );
  PULP_VECTOR_QI_SCI("pv.cmplt.sci.b", MATCH_V_OP_CMPLT|MATCH_V_OP_B_VI, MASK_V_OP1 );
#undef OP

#define OP(A, B) ((signed_word)sext(A, 16) <= (signed_word)sext(B, 16) ? 0xffffffff : 0)
  PULP_VECTOR_HI_V  ("pv.cmple.h",     MATCH_V_OP_CMPLE|MATCH_V_OP_H_VV, MASK_V_OP  );
  PULP_VECTOR_HI_SC ("pv.cmple.sc.h",  MATCH_V_OP_CMPLE|MATCH_V_OP_H_VR, MASK_V_OP  );
  PULP_VECTOR_HI_SCI("pv.cmple.sci.h", MATCH_V_OP_CMPLE|MATCH_V_OP_H_VI, MASK_V_OP1 );
#undef OP
#define OP(A, B) ((signed_word)sext(A, 8) <= (signed_word)sext(B, 8) ? 0xffffffff : 0)
  PULP_VECTOR_QI_V  ("pv.cmple.b",     MATCH_V_OP_CMPLE|MATCH_V_OP_B_VV, MASK_V_OP  );
  PULP_VECTOR_QI_SC ("pv.cmple.sc.b",  MATCH_V_OP_CMPLE|MATCH_V_OP_B_VR, MASK_V_OP  );
  PULP_VECTOR_QI_SCI("pv.cmple.sci.b", MATCH_V_OP_CMPLE|MATCH_V_OP_B_VI, MASK_V_OP1 );
#undef OP

#define OP(A, B) ((A) > (B) ? 0xffffffff : 0)
  PULP_VECTOR_HI_V  ("pv.cmpgtu.h",     MATCH_V_OP_CMPGTU|MATCH_V_OP_H_VV, MASK_V_OP  );
  PULP_VECTOR_HI_SC ("pv.cmpgtu.sc.h",  MATCH_V_OP_CMPGTU|MATCH_V_OP_H_VR, MASK_V_OP  );
  PULP_VECTOR_HI_SCI("pv.cmpgtu.sci.h", MATCH_V_OP_CMPGTU|MATCH_V_OP_H_VI, MASK_V_OP1 );
  PULP_VECTOR_QI_V  ("pv.cmpgtu.b",     MATCH_V_OP_CMPGTU|MATCH_V_OP_B_VV, MASK_V_OP  );
  PULP_VECTOR_QI_SC ("pv.cmpgtu.sc.b",  MATCH_V_OP_CMPGTU|MATCH_V_OP_B_VR, MASK_V_OP  );
  PULP_VECTOR_QI_SCI("pv.cmpgtu.sci.b", MATCH_V_OP_CMPGTU|MATCH_V_OP_B_VI, MASK_V_OP1 );
#undef OP

#define OP(A, B) ((A) >= (B) ? 0xffffffff : 0)
  PULP_VECTOR_HI_V  ("pv.cmpgeu.h",     MATCH_V_OP_CMPGEU|MATCH_V_OP_H_VV, MASK_V_OP  );
  PULP_VECTOR_HI_SC ("pv.cmpgeu.sc.h",  MATCH_V_OP_CMPGEU|MATCH_V_OP_H_VR, MASK_V_OP  );
  PULP_VECTOR_HI_SCI("pv.cmpgeu.sci.h", MATCH_V_OP_CMPGEU|MATCH_V_OP_H_VI, MASK_V_OP1 );
  PULP_VECTOR_QI_V  ("pv.cmpgeu.b",     MATCH_V_OP_CMPGEU|MATCH_V_OP_B_VV, MASK_V_OP  );
  PULP_VECTOR_QI_SC ("pv.cmpgeu.sc.b",  MATCH_V_OP_CMPGEU|MATCH_V_OP_B_VR, MASK_V_OP  );
  PULP_VECTOR_QI_SCI("pv.cmpgeu.sci.b", MATCH_V_OP_CMPGEU|MATCH_V_OP_B_VI, MASK_V_OP1 );
#undef OP

#define OP(A, B) ((A) < (B) ? 0xffffffff : 0)
  PULP_VECTOR_HI_V  ("pv.cmpltu.h",     MATCH_V_OP_CMPLTU|MATCH_V_OP_H_VV, MASK_V_OP  );
  PULP_VECTOR_HI_SC ("pv.cmpltu.sc.h",  MATCH_V_OP_CMPLTU|MATCH_V_OP_H_VR, MASK_V_OP  );
  PULP_VECTOR_HI_SCI("pv.cmpltu.sci.h", MATCH_V_OP_CMPLTU|MATCH_V_OP_H_VI, MASK_V_OP1 );
  PULP_VECTOR_QI_V  ("pv.cmpltu.b",     MATCH_V_OP_CMPLTU|MATCH_V_OP_B_VV, MASK_V_OP  );
  PULP_VECTOR_QI_SC ("pv.cmpltu.sc.b",  MATCH_V_OP_CMPLTU|MATCH_V_OP_B_VR, MASK_V_OP  );
  PULP_VECTOR_QI_SCI("pv.cmpltu.sci.b", MATCH_V_OP_CMPLTU|MATCH_V_OP_B_VI, MASK_V_OP1 );
#undef OP

#define OP(A, B) ((A) <= (B) ? 0xffffffff : 0)
  PULP_VECTOR_HI_V  ("pv.cmpleu.h",     MATCH_V_OP_CMPLEU|MATCH_V_OP_H_VV, MASK_V_OP  );
  PULP_VECTOR_HI_SC ("pv.cmpleu.sc.h",  MATCH_V_OP_CMPLEU|MATCH_V_OP_H_VR, MASK_V_OP  );
  PULP_VECTOR_HI_SCI("pv.cmpleu.sci.h", MATCH_V_OP_CMPLEU|MATCH_V_OP_H_VI, MASK_V_OP1 );
  PULP_VECTOR_QI_V  ("pv.cmpleu.b",     MATCH_V_OP_CMPLEU|MATCH_V_OP_B_VV, MASK_V_OP  );
  PULP_VECTOR_QI_SC ("pv.cmpleu.sc.b",  MATCH_V_OP_CMPLEU|MATCH_V_OP_B_VR, MASK_V_OP  );
  PULP_VECTOR_QI_SCI("pv.cmpleu.sci.b", MATCH_V_OP_CMPLEU|MATCH_V_OP_B_VI, MASK_V_OP1 );
#undef OP

#define OP(A, B) ((signed_word)sext (A, 16) * (signed_word)sext (B, 16))
#define FOLD2(A, B, ACC) ((signed_word)A + (signed_word)B)
  PULP_VECTOR_HI_V_FOLD  ("pv.dotsp.h",     MATCH_V_OP_DOTUP|MATCH_V_OP_H_VV, MASK_V_OP  );
  PULP_VECTOR_HI_SC_FOLD ("pv.dotsp.sc.h",  MATCH_V_OP_DOTUP|MATCH_V_OP_H_VR, MASK_V_OP  );
  PULP_VECTOR_HI_SCI_FOLD("pv.dotsp.sci.h", MATCH_V_OP_DOTUP|MATCH_V_OP_H_VI, MASK_V_OP1 );
#undef OP
#define OP(A, B) ((signed_word)sext (A, 8) * (signed_word)sext (B, 8))
#define FOLD4(A, B, C, D, ACC) ((signed_word)A + (signed_word)B	\
			   + (signed_word)C + (signed_word)D)
  PULP_VECTOR_QI_V_FOLD  ("pv.dotsp.b",     MATCH_V_OP_DOTUP|MATCH_V_OP_B_VV, MASK_V_OP  );
  PULP_VECTOR_QI_SC_FOLD ("pv.dotsp.sc.b",  MATCH_V_OP_DOTUP|MATCH_V_OP_B_VR, MASK_V_OP  );
  PULP_VECTOR_QI_SCI_FOLD("pv.dotsp.sci.b", MATCH_V_OP_DOTUP|MATCH_V_OP_B_VI, MASK_V_OP1 );
#undef FOLD4
#undef FOLD2
#undef OP

#define OP(A, B) (A * B)
#define FOLD2(A, B, ACC) (A + B)
#define FOLD4(A, B, C, D, ACC) (A + B + C + D)
  PULP_VECTOR_HI_V_FOLD  ("pv.dotup.h",     MATCH_V_OP_EXTRACTU|MATCH_V_OP_H_VV, MASK_V_OP  );
  PULP_VECTOR_HI_SC_FOLD ("pv.dotup.sc.h",  MATCH_V_OP_EXTRACTU|MATCH_V_OP_H_VR, MASK_V_OP  );
  PULP_VECTOR_HI_SCI_U_FOLD("pv.dotup.sci.h", MATCH_V_OP_EXTRACTU|MATCH_V_OP_H_VI, MASK_V_OP1 );
  PULP_VECTOR_QI_V_FOLD  ("pv.dotup.b",     MATCH_V_OP_EXTRACTU|MATCH_V_OP_B_VV, MASK_V_OP  );
  PULP_VECTOR_QI_SC_FOLD ("pv.dotup.sc.b",  MATCH_V_OP_EXTRACTU|MATCH_V_OP_B_VR, MASK_V_OP  );
  PULP_VECTOR_QI_SCI_U_FOLD("pv.dotup.sci.b", MATCH_V_OP_EXTRACTU|MATCH_V_OP_B_VI, MASK_V_OP1 );
#undef FOLD4
#undef FOLD2
#undef OP

#define OP(A, B) (A * (signed_word)sext (B, 16))
#define FOLD2(A, B, ACC) (A + B)
  PULP_VECTOR_HI_V_FOLD  ("pv.dotusp.h",     MATCH_V_OP_INSERT|MATCH_V_OP_H_VV, MASK_V_OP  );
  PULP_VECTOR_HI_SC_FOLD ("pv.dotusp.sc.h",  MATCH_V_OP_INSERT|MATCH_V_OP_H_VR, MASK_V_OP  );
  PULP_VECTOR_HI_SCI_FOLD("pv.dotusp.sci.h", MATCH_V_OP_INSERT|MATCH_V_OP_H_VI, MASK_V_OP1 );
#undef OP
#define OP(A, B) (A * (signed_word)sext (B, 8))
#define FOLD4(A, B, C, D, ACC) (A + B + C + D)
  PULP_VECTOR_QI_V_FOLD  ("pv.dotusp.b",     MATCH_V_OP_INSERT|MATCH_V_OP_B_VV, MASK_V_OP  );
  PULP_VECTOR_QI_SC_FOLD ("pv.dotusp.sc.b",  MATCH_V_OP_INSERT|MATCH_V_OP_B_VR, MASK_V_OP  );
  PULP_VECTOR_QI_SCI_FOLD("pv.dotusp.sci.b", MATCH_V_OP_INSERT|MATCH_V_OP_B_VI, MASK_V_OP1 );
#undef FOLD4
#undef FOLD2
#undef OP

#define OP(A, B) ((signed_word)sext (A, 16) * (signed_word)sext (B, 16))
#define FOLD2(A, B, ACC) ((signed_word)ACC + (signed_word)A + (signed_word)B)
  PULP_VECTOR_HI_V_FOLD  ("pv.sdotsp.h",     MATCH_V_OP_SDOTUSP|MATCH_V_OP_H_VV, MASK_V_OP  );
  PULP_VECTOR_HI_SC_FOLD ("pv.sdotsp.sc.h",  MATCH_V_OP_SDOTUSP|MATCH_V_OP_H_VR, MASK_V_OP  );
  PULP_VECTOR_HI_SCI_FOLD("pv.sdotsp.sci.h", MATCH_V_OP_SDOTUSP|MATCH_V_OP_H_VI, MASK_V_OP1 );
#undef OP
#define OP(A, B) ((signed_word)sext (A, 8) * (signed_word)sext (B, 8))
#define FOLD4(A, B, C, D, ACC) ((signed_word)ACC + (signed_word)A \
				+ (signed_word)B + (signed_word)C + (signed_word)D)
  PULP_VECTOR_QI_V_FOLD  ("pv.sdotsp.b",     MATCH_V_OP_SDOTUSP|MATCH_V_OP_B_VV, MASK_V_OP  );
  PULP_VECTOR_QI_SC_FOLD ("pv.sdotsp.sc.b",  MATCH_V_OP_SDOTUSP|MATCH_V_OP_B_VR, MASK_V_OP  );
  PULP_VECTOR_QI_SCI_FOLD("pv.sdotsp.sci.b", MATCH_V_OP_SDOTUSP|MATCH_V_OP_B_VI, MASK_V_OP1 );
#undef FOLD4
#undef FOLD2
#undef OP

#define OP(A, B) (A * B)
#define FOLD2(A, B, ACC) (ACC + A + B)
#define FOLD4(A, B, C, D, ACC) (ACC + A + B + C + D)
  PULP_VECTOR_HI_V_FOLD  ("pv.sdotup.h",     MATCH_V_OP_DOTUSP|MATCH_V_OP_H_VV, MASK_V_OP  );
  PULP_VECTOR_HI_SC_FOLD ("pv.sdotup.sc.h",  MATCH_V_OP_DOTUSP|MATCH_V_OP_H_VR, MASK_V_OP  );
  PULP_VECTOR_HI_SCI_U_FOLD("pv.sdotup.sci.h", MATCH_V_OP_DOTUSP|MATCH_V_OP_H_VI, MASK_V_OP1 );
  PULP_VECTOR_QI_V_FOLD  ("pv.sdotup.b",     MATCH_V_OP_DOTUSP|MATCH_V_OP_B_VV, MASK_V_OP  );
  PULP_VECTOR_QI_SC_FOLD ("pv.sdotup.sc.b",  MATCH_V_OP_DOTUSP|MATCH_V_OP_B_VR, MASK_V_OP  );
  PULP_VECTOR_QI_SCI_U_FOLD("pv.sdotup.sci.b", MATCH_V_OP_DOTUSP|MATCH_V_OP_B_VI, MASK_V_OP1 );
#undef FOLD4
#undef FOLD2
#undef OP

#define OP(A, B) (A * (signed_word)sext (B, 16))
#define FOLD2(A, B, ACC) (A + B + ACC)
  PULP_VECTOR_HI_V_FOLD  ("pv.sdotusp.h",     MATCH_V_OP_SDOTSP|MATCH_V_OP_H_VV, MASK_V_OP  );
  PULP_VECTOR_HI_SC_FOLD ("pv.sdotusp.sc.h",  MATCH_V_OP_SDOTSP|MATCH_V_OP_H_VR, MASK_V_OP  );
  PULP_VECTOR_HI_SCI_FOLD("pv.sdotusp.sci.h", MATCH_V_OP_SDOTSP|MATCH_V_OP_H_VI, MASK_V_OP1 );
#undef OP
#define OP(A, B) (A * (signed_word)sext (B, 8))
#define FOLD4(A, B, C, D, ACC) (A + B + C + D + ACC)
  PULP_VECTOR_QI_V_FOLD  ("pv.sdotusp.b",     MATCH_V_OP_SDOTSP|MATCH_V_OP_B_VV, MASK_V_OP  );
  PULP_VECTOR_QI_SC_FOLD ("pv.sdotusp.sc.b",  MATCH_V_OP_SDOTSP|MATCH_V_OP_B_VR, MASK_V_OP  );
  PULP_VECTOR_QI_SCI_FOLD("pv.sdotusp.sci.b", MATCH_V_OP_SDOTSP|MATCH_V_OP_B_VI, MASK_V_OP1 );
#undef FOLD4
#undef FOLD2
#undef OP

  if ((iw & (MASK_V_OP)) == (MATCH_V_OP_SHUFFLE|MATCH_V_OP_H_VV))
    {
      TRACE_INSN (cpu, "pv.shuffle.h %s, %s, %s;  //", rd_name, rs1_name, rs2_name);
      low = (RS1V >> (16 * (RS2V & 1))) & 0xffff;
      high = (RS1V >> (16 * ((RS2V >> 16) & 1))) & 0xffff;
      store_rd (cpu, rd, low | (high << 16));
      goto done;
    }

  if ((iw & (MASK_V_OP1)) == (MATCH_V_OP_SHUFFLE|MATCH_V_OP_H_VI))
    {
      TRACE_INSN (cpu, "pv.shuffle.sci.h %s, %s, %"PRIiTW";  //", rd_name, rs1_name, vsimm6);
      low = (RS1V >> (16 * (vsimm6 & 1))) & 0xffff;
      high = (RS1V >> (16 * ((vsimm6 >> 1) & 1))) & 0xffff;
      store_rd (cpu, rd, low | (high << 16));
      goto done;
    }

  if ((iw & (MASK_V_OP)) == (MATCH_V_OP_SHUFFLE|MATCH_V_OP_B_VV))
    {
      TRACE_INSN (cpu, "pv.shuffle.b %s, %s, %s;  //", rd_name, rs1_name, rs2_name);
      b0 = (RS1V >> (8 * (RS2V & 3))) & 0xff;
      b1 = (RS1V >> (8 * ((RS2V >> 8) & 3))) & 0xff;
      b2 = (RS1V >> (8 * ((RS2V >> 16) & 3))) & 0xff;
      b3 = (RS1V >> (8 * ((RS2V >> 24) & 3))) & 0xff;
      store_rd (cpu, rd, b0 | (b1 << 8) | (b2 << 16) | (b3 << 24));
      goto done;
    }

#define PULP_VECTOR_QI_SHUFFLE_SCI(INSN, MATCH, MASK, INDX)                   \
  if ((iw & (MASK)) == (MATCH))                                               \
    {                                                                         \
      TRACE_INSN (cpu, INSN " %s, %s, %" PRIiTW ";  //", rd_name, rs1_name,   \
                  vsimm6);                                                    \
      b0 = (RS1V >> (8 * (vsimm6 & 3))) & 0xff;                               \
      b1 = (RS1V >> (8 * ((vsimm6 >> 2) & 3))) & 0xff;                        \
      b2 = (RS1V >> (8 * ((vsimm6 >> 4) & 3))) & 0xff;                        \
      b3 = (RS1V >> (8 * INDX)) & 0xff;                                       \
      store_rd (cpu, rd, b0 | (b1 << 8) | (b2 << 16) | (b3 << 24));           \
      goto done;                                                              \
    }

  PULP_VECTOR_QI_SHUFFLE_SCI("pv.shufflei0.sci.b", MATCH_V_OP_SHUFFLE|MATCH_V_OP_B_VI, MASK_V_OP1, 0)
  PULP_VECTOR_QI_SHUFFLE_SCI("pv.shufflei1.sci.b", MATCH_V_OP_SHUFFLE|MATCH_V_OP_B_VI, MASK_V_OP1, 1)
  PULP_VECTOR_QI_SHUFFLE_SCI("pv.shufflei2.sci.b", MATCH_V_OP_SHUFFLE|MATCH_V_OP_B_VI, MASK_V_OP1, 2)
  PULP_VECTOR_QI_SHUFFLE_SCI("pv.shufflei3.sci.b", MATCH_V_OP_SHUFFLE|MATCH_V_OP_B_VI, MASK_V_OP1, 3)

  if ((iw & (MASK_V_OP)) == (MATCH_V_OP_SHUFFLE2 | MATCH_V_OP_H_VV))
    {
      TRACE_INSN (cpu, "pv.shuffle2.h %s, %s, %s;  //", rd_name, rs1_name, rs2_name);
      low = ((((RS2V >> 1) & 1) ? RS1V : RDV) >> (16 * (RS2V & 1))) & 0xffff;
      high = ((((RS2V >> 17) & 1) ? RS1V : RDV) >> (16 * ((RS2V >> 16) & 1))) & 0xffff;
      store_rd (cpu, rd, low | (high << 16));
      goto done;
    }

  if ((iw & (MASK_V_OP)) == (MATCH_V_OP_SHUFFLE2 | MATCH_V_OP_B_VV))
    {
      TRACE_INSN (cpu, "pv.shuffle2.b %s, %s, %s;  //", rd_name, rs1_name, rs2_name);
      b0 = ((((RS2V >> 2) & 1) ? RS1V : RDV) >> (8 * (RS2V & 3))) & 0xff;
      b1 = ((((RS2V >> 10) & 1) ? RS1V : RDV) >> (8 * ((RS2V >> 8) & 3))) & 0xff;
      b2 = ((((RS2V >> 18) & 1) ? RS1V : RDV) >> (8 * ((RS2V >> 16) & 3))) & 0xff;
      b3 = ((((RS2V >> 26) & 1) ? RS1V : RDV) >> (8 * ((RS2V >> 24) & 3))) & 0xff;
      store_rd (cpu, rd, b0 | (b1 << 8) | (b2 << 16) | (b3 << 24));
      goto done;
    }

  if ((iw & (MASK_V_OP)) == (MATCH_V_OP_PACK | MATCH_V_OP_H_VV))
    {
      TRACE_INSN (cpu, "pv.pack %s, %s, %s;  //", rd_name, rs1_name, rs2_name);
      low = RS2V & 0xffff;
      high = RS1V & 0xffff;
      store_rd (cpu, rd, low | (high << 16));
      goto done;
    }

  if ((iw & (MASK_V_OP)) == (MATCH_V_OP_PACKH | MATCH_V_OP_H_VV))
    {
      TRACE_INSN (cpu, "pv.pack.h %s, %s, %s;  //", rd_name, rs1_name,
                  rs2_name);
      low = (RS2V >> 16) & 0xffff;
      high = (RS1V >> 16) & 0xffff;
      store_rd (cpu, rd, low | (high << 16));
      goto done;
    }

  if ((iw & (MASK_V_OP)) == (MATCH_V_OP_PACKHI | MATCH_V_OP_B_VV))
    {
      TRACE_INSN (cpu, "pv.packhi.b %s, %s, %s;  //", rd_name, rs1_name,
                  rs2_name);
      b2 = RS2V & 0xff;
      b3 = RS1V & 0xff;
      store_rd (cpu, rd, (RDV & 0xffff) | (b2 << 16) | (b3 << 24));
      goto done;
    }

  if ((iw & (MASK_V_OP)) == (MATCH_V_OP_PACKLO | MATCH_V_OP_B_VV))
    {
      TRACE_INSN (cpu, "pv.packlo.b %s, %s, %s;  //", rd_name, rs1_name,
                  rs2_name);
      b0 = RS2V & 0xff;
      b1 = RS1V & 0xff;
      store_rd (cpu, rd, b0 | (b1 << 8) | (RDV & 0xffff0000));
      goto done;
    }

#define PULP_VECTOR_HI_CPLXMUL_R(INSN, MATCH, MASK, DIV)                      \
  if ((iw & (MASK)) == (MATCH))                                               \
    {                                                                         \
      TRACE_INSN (cpu, INSN " %s, %s, %s;  //", rd_name, rs1_name, rs2_name); \
      low = (sext (RS1V, 16) * sext (RS2V, 16))                               \
            - ((sext (RS1V >> 16, 16)) * (sext (RS2V >> 16, 16)));            \
      low = ((signed_word)low >> DIV) & 0xffff;                               \
      store_rd (cpu, rd, low | (RDV & 0xffff0000));                           \
      goto done;                                                              \
    }

  PULP_VECTOR_HI_CPLXMUL_R("pv.cplxmul.h.r"     , MATCH_V_OP_CPLXMULR|MATCH_V_OP_H_VV_S0, MASK_V_OP11, 15);
  PULP_VECTOR_HI_CPLXMUL_R("pv.cplxmul.h.r.div2", MATCH_V_OP_CPLXMULR|MATCH_V_OP_H_VV_S1, MASK_V_OP11, 16);
  PULP_VECTOR_HI_CPLXMUL_R("pv.cplxmul.h.r.div4", MATCH_V_OP_CPLXMULR|MATCH_V_OP_H_VV_S2, MASK_V_OP11, 17);
  PULP_VECTOR_HI_CPLXMUL_R("pv.cplxmul.h.r.div8", MATCH_V_OP_CPLXMULR|MATCH_V_OP_H_VV_S3, MASK_V_OP11, 18);

#define PULP_VECTOR_HI_CPLXMUL_I(INSN, MATCH, MASK, DIV)                      \
  if ((iw & (MASK)) == (MATCH))                                               \
    {                                                                         \
      TRACE_INSN (cpu, INSN " %s, %s, %s;  //", rd_name, rs1_name, rs2_name); \
      high = (sext (RS1V, 16) * (sext (RS2V >> 16, 16)))                      \
             + ((sext (RS1V >> 16, 16)) * sext (RS2V, 16));                   \
      high = ((signed_word)high >> DIV) & 0xffff;                             \
      store_rd (cpu, rd, (high << 16) | (RDV & 0xffff));                      \
      goto done;                                                              \
    }

  PULP_VECTOR_HI_CPLXMUL_I("pv.cplxmul.h.i"     , MATCH_V_OP_CPLXMULI|MATCH_V_OP_H_VV_S0, MASK_V_OP11, 15);
  PULP_VECTOR_HI_CPLXMUL_I("pv.cplxmul.h.i.div2", MATCH_V_OP_CPLXMULI|MATCH_V_OP_H_VV_S1, MASK_V_OP11, 16);
  PULP_VECTOR_HI_CPLXMUL_I("pv.cplxmul.h.i.div4", MATCH_V_OP_CPLXMULI|MATCH_V_OP_H_VV_S2, MASK_V_OP11, 17);
  PULP_VECTOR_HI_CPLXMUL_I("pv.cplxmul.h.i.div8", MATCH_V_OP_CPLXMULI|MATCH_V_OP_H_VV_S3, MASK_V_OP11, 18);

#define PULP_VECTOR_HI_SUBROT(INSN, MATCH, MASK, DIV)                         \
  if ((iw & (MASK)) == (MATCH))                                               \
    {                                                                         \
      TRACE_INSN (cpu, INSN " %s, %s, %s;  //", rd_name, rs1_name, rs2_name); \
      low = ((sext (RS1V >> 16, 16)) - (sext (RS2V >> 16, 16))) & 0xffff;     \
      high = (sext (RS2V, 16) - sext (RS1V, 16)) & 0xffff;                    \
      low = ((signed_word)sext (low, 16) >> DIV) & 0xffff;                    \
      high = ((signed_word)sext (high, 16) >> DIV) & 0xffff;                  \
      store_rd (cpu, rd, low | (high << 16));                                 \
      goto done;                                                              \
    }

  PULP_VECTOR_HI_SUBROT("pv.subrotmj.h"     , MATCH_V_OP_SUBROTMJ|MATCH_V_OP_H_VV_S0, MASK_V_OP, 0);
  PULP_VECTOR_HI_SUBROT("pv.subrotmj.h.div2", MATCH_V_OP_SUBROTMJ|MATCH_V_OP_H_VV_S1, MASK_V_OP, 1);
  PULP_VECTOR_HI_SUBROT("pv.subrotmj.h.div4", MATCH_V_OP_SUBROTMJ|MATCH_V_OP_H_VV_S2, MASK_V_OP, 2);
  PULP_VECTOR_HI_SUBROT("pv.subrotmj.h.div8", MATCH_V_OP_SUBROTMJ|MATCH_V_OP_H_VV_S3, MASK_V_OP, 3);

  if ((iw & (MASK_V_OP2)) == (MATCH_V_OP_CPLXCONJ|MATCH_V_OP_H_VV))
    {
      TRACE_INSN (cpu, "pv.cplxconj.h %s, %s;  //", rd_name, rs1_name);
      low = RS1V & 0xffff;
      high = -sext (RS1V >> 16, 16) & 0xffff;
      store_rd (cpu, rd, low | (high << 16));
      goto done;
    }

  /* trap unimplemented insn */

  if (op->match_func (op, iw))
    {
      TRACE_INSN (cpu, "UNHANDLED INSN: %s", op->name);
      sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL);
    }

 done:
  return pc;
}



static sim_cia
execute_one (SIM_CPU *cpu, unsigned_word iw, const struct riscv_opcode *op)
{
  SIM_DESC sd = CPU_STATE (cpu);
  int index;

  if (op->xlen_requirement == 32)
    RISCV_ASSERT_RV32 (cpu, "insn: %s", op->name);
  else if (op->xlen_requirement == 64)
    RISCV_ASSERT_RV64 (cpu, "insn: %s", op->name);

  switch (op->insn_class)
    {
    case INSN_CLASS_A:
      return execute_a (cpu, iw, op);
    case INSN_CLASS_C:
    case INSN_CLASS_F_AND_C:
    case INSN_CLASS_D_AND_C:
      /* todo compressed ZFINX insn */
      return execute_c (cpu, iw, op);
    case INSN_CLASS_D:
      return execute_d (cpu, iw, op);
    case INSN_CLASS_F:
      return execute_f (cpu, iw, op);
    case INSN_CLASS_I:
      return execute_i (cpu, iw, op);
    case INSN_CLASS_M:
      return execute_m (cpu, iw, op);
    case INSN_CLASS_ZFINX:
      return execute_zfinx (cpu, iw, op);
    case INSN_CLASS_ZDINX:
      return execute_zdinx (cpu, iw, op);

    /* case INSN_CLASS_XPULP_V0: */
    /* case INSN_CLASS_XPULP_V1: */

    /* pulpv0 and pulpv1 compatibility */
#define INSN_CLASS(NAME, ARCH)			\
    case INSN_CLASS_XPULP_##NAME:

      /* from opcodes/riscv.h */
      PULP_EXTENSION_COMPAT_MAP

      TRACE_INSN (cpu, "PULP v0/v1 compatibility insns not supported: %d",
		  op->insn_class);
      sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL);
#undef INSN_CLASS

    /* pulpv2 onwards */
#define INSN_CLASS(NAME, ARCH)						\
    case INSN_CLASS_XPULP_##NAME:

      /* from opcode/riscv.h */
      PULP_EXTENSION_MAP

      return execute_xpulp (cpu, iw, op);
#undef INSN_CLASS

    case_default:
    default:
      TRACE_INSN (cpu, "UNHANDLED EXTENSION: %d", op->insn_class);
      sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL);
    }

  return cpu->pc + riscv_insn_length (iw);
}

/* Decode & execute a single instruction.  */
void step_once (SIM_CPU *cpu)
{
  SIM_DESC sd = CPU_STATE (cpu);
  unsigned_word iw;
  unsigned int len;
  sim_cia pc = cpu->pc;
  sim_cia new_pc = pc;
  const struct riscv_opcode *op;
  int xlen = RISCV_XLEN (cpu);

  if (TRACE_ANY_P (cpu))
    trace_prefix (sd, cpu, NULL_CIA, pc, TRACE_LINENUM_P (cpu),
		  NULL, 0, " "); /* Use a space for gcc warnings.  */

  iw = sim_core_read_aligned_2 (cpu, pc, exec_map, pc);

  len = riscv_insn_length (iw);

  if (len == 4)
    iw |= ((unsigned_word)sim_core_read_aligned_2 (cpu, pc, exec_map, pc + 2) << 16);

  TRACE_CORE (cpu, "0x%08" PRIxTW, iw);

  op = riscv_hash[OP_HASH_IDX (iw)];
  if (!op)
    sim_engine_halt (sd, cpu, NULL, pc, sim_signalled, SIM_SIGILL);

  for (; op->name; op++)
    {
      /* Does the opcode match?  */
      if (!(op->match_func) (op, iw))
	continue;
      /* Is this a pseudo-instruction?  */
      if ((op->pinfo & INSN_ALIAS))
	continue;
      /* Is this instruction restricted to a certain value of XLEN?  */
      if (op->xlen_requirement != 0 && op->xlen_requirement != xlen)
	continue;
      /* is this supported by the current subsets? */
      if (!riscv_multi_subset_supports (op->insn_class))
	continue;

      /* It's a match.  */
      new_pc = execute_one (cpu, iw, op);
      /* hardware loop specific next pc update */
      /* TODO: verify hwloop constraints */
      if (cpu->csr.lpcount0 && cpu->csr.lpend0 == pc)
	{
	  cpu->csr.lpcount0--;
	  if (cpu->csr.lpcount0)
	    {
	      pc = cpu->csr.lpstart0;
	      break;
	    }
	}

      if (cpu->csr.lpcount1 && cpu->csr.lpend1 == pc)
	{
	  cpu->csr.lpcount1--;
	  if (cpu->csr.lpcount1)
	    {
	      pc = cpu->csr.lpstart1;
	      break;
	    }
	}
      /* if we didn't update the pc yet */
      pc = new_pc;
      break;
    }

  if (op->name == NULL)
    {
      TRACE_INSN (cpu, "INSN NOT FOUND IN OPC TABLE");
      sim_engine_halt (sd, cpu, NULL, cpu->pc, sim_signalled, SIM_SIGILL);
    }

  /* TODO: Try to use a common counter and only update on demand (reads).  */
  if (RISCV_XLEN (cpu) == 32)
    {
      unsigned_word old_cycle = cpu->csr.cycle++;

      /* Increase cycleh if cycle is overflowed.  */
      if (old_cycle > cpu->csr.cycle)
	cpu->csr.cycleh++;
    }
  else
    ++cpu->csr.cycle;

  cpu->csr.instret = cpu->csr.cycle;
  cpu->csr.instreth = cpu->csr.cycleh;

  cpu->pc = pc;
}

/* Return the program counter for this cpu. */
static sim_cia
pc_get (sim_cpu *cpu)
{
  return cpu->pc;
}

/* Set the program counter for this cpu to the new pc value. */
static void
pc_set (sim_cpu *cpu, sim_cia pc)
{
  cpu->pc = pc;
}

static int
reg_fetch (sim_cpu *cpu, int rn, unsigned char *buf, int len)
{
  if (len <= 0 || len > sizeof (unsigned_word))
    return -1;

  switch (rn)
    {
    case SIM_RISCV_RA_REGNUM ... SIM_RISCV_T6_REGNUM:
      memcpy (buf, &cpu->regs[rn], len);
      return len;
    case SIM_RISCV_FIRST_FP_REGNUM ... SIM_RISCV_LAST_FP_REGNUM:
      rn -= SIM_RISCV_FIRST_FP_REGNUM;
      memcpy (buf, &cpu->fpregs[rn], len);
      return len;
    case SIM_RISCV_PC_REGNUM:
      memcpy (buf, &cpu->pc, len);
      return len;

#define DECLARE_CSR(name, num) \
    case SIM_RISCV_ ## num ## _REGNUM: \
      memcpy (buf, &cpu->csr.name, len); \
      return len;
#include "opcode/riscv-opc.h"
#undef DECLARE_CSR

    default:
      return -1;
    }
}

static int
reg_store (sim_cpu *cpu, int rn, unsigned char *buf, int len)
{
  if (len <= 0 || len > sizeof (unsigned_word))
    return -1;

  switch (rn)
    {
    case SIM_RISCV_ZERO_REGNUM:
      /* Always return len to avoid warning/error in gdbsim_store_register.  */
      return len;
    case SIM_RISCV_RA_REGNUM ... SIM_RISCV_T6_REGNUM:
      memcpy (&cpu->regs[rn], buf, len);
      return len;
    case SIM_RISCV_FIRST_FP_REGNUM ... SIM_RISCV_LAST_FP_REGNUM:
      rn -= SIM_RISCV_FIRST_FP_REGNUM;
      memcpy (&cpu->fpregs[rn], buf, len);
      return len;
    case SIM_RISCV_PC_REGNUM:
      memcpy (&cpu->pc, buf, len);
      return len;

#define DECLARE_CSR(name, num) \
    case SIM_RISCV_ ## num ## _REGNUM: \
      memcpy (&cpu->csr.name, buf, len); \
      return len;
#include "opcode/riscv-opc.h"
#undef DECLARE_CSR

    default:
      return -1;
    }
}

/* Initialize the state for a single cpu.  Usuaully this involves clearing all
   registers back to their reset state.  Should also hook up the fetch/store
   helper functions too.  */
void initialize_cpu (SIM_DESC sd, SIM_CPU *cpu, int mhartid, char* attr_arch)
{
  const char *extensions;
  int i;
  char *p, *low_ext; /* lower cased extension string */

  memset (cpu->regs, 0, sizeof (cpu->regs));

  CPU_PC_FETCH (cpu) = pc_get;
  CPU_PC_STORE (cpu) = pc_set;
  CPU_REG_FETCH (cpu) = reg_fetch;
  CPU_REG_STORE (cpu) = reg_store;

  p = xmalloc (strlen (MODEL_NAME (CPU_MODEL (cpu))));
  strcpy (p, MODEL_NAME (CPU_MODEL (cpu)));
  low_ext = p;
  for ( ; *p; ++p)
    *p = tolower (*p);

  /* To determine the arch string we do the following with decreasing priority:
     1. Take arch attribute from elf file
     2. use --model MODEL

     TODO: ideally we would let --model take preference, but we can't determine
     whether --model was given by the user or the default value taken */
  if (attr_arch)
    {
      riscv_set_arch (attr_arch);
      free (attr_arch);
    }
  else
    riscv_set_arch (low_ext);

  riscv_opts.rvc = TRUE;

  if (!riscv_hash[0])
    {
      const struct riscv_opcode *op;

      for (op = riscv_opcodes; op->name; op++)
	/* only add requested insn subsets, because they overlap */
        if (riscv_multi_subset_supports (op->insn_class))
	  if (!riscv_hash[OP_HASH_IDX (op->match)])
	    riscv_hash[OP_HASH_IDX (op->match)] = op;
    }

  cpu->csr.misa = 0;
  /* RV32 sets this field to 0, and we don't really support RV128 yet.  */
  if (RISCV_XLEN (cpu) == 64)
    cpu->csr.misa |= (unsigned64)2 << 62;

  /* Skip the leading "rv" prefix and the two numbers.  */
  extensions = MODEL_NAME (CPU_MODEL (cpu)) + 4;
  for (i = 0; i < 26; ++i)
    {
      char ext = 'A' + i;

      if (ext == 'X')
	continue;
      else if (strchr (extensions, ext) != NULL)
	{
	  if (ext == 'G')
	    cpu->csr.misa |= 0x1129;  /* G = IMAFD.  */
	  else
	    cpu->csr.misa |= (1 << i);
	}
    }

  cpu->csr.mimpid = 0x8000;
  cpu->csr.mhartid = mhartid;
  cpu->csr.cycle = 0;
  cpu->csr.cycleh = 0;
  cpu->csr.instret = 0;
  cpu->csr.instreth = 0;

  free (low_ext);
}

/* Some utils don't like having a NULL environ.  */
static const char * const simple_env[] = { "HOME=/", "PATH=/bin", NULL };

/* Count the number of arguments in an argv.  */
static int
count_argv (const char * const *argv)
{
  int i;

  if (!argv)
    return -1;

  for (i = 0; argv[i] != NULL; ++i)
    continue;
  return i;
}

void initialize_env (SIM_DESC sd, const char * const *argv,
		     const char * const *env)
{
  SIM_CPU *cpu = STATE_CPU (sd, 0);
  int i;
  int argc, argv_flat;
  int envc, env_flat;
  address_word sp, sp_flat;
  unsigned char null[8] = { 0, 0, 0, 0, 0, 0, 0, 0, };

  /* Figure out how many bytes the argv strings take up.  */
  argc = count_argv (argv);
  if (argc == -1)
    argc = 0;
  argv_flat = argc; /* NUL bytes.  */
  for (i = 0; i < argc; ++i)
    argv_flat += strlen (argv[i]);

  /* Figure out how many bytes the environ strings take up.  */
  if (!env)
    env = simple_env;
  envc = count_argv (env);
  env_flat = envc; /* NUL bytes.  */
  for (i = 0; i < envc; ++i)
    env_flat += strlen (env[i]);

  /* Make space for the strings themselves.  */
  sp_flat = (DEFAULT_MEM_SIZE - argv_flat - env_flat) & -sizeof (address_word);
  /* Then the pointers to the strings.  */
  sp = sp_flat - ((argc + 1 + envc + 1) * sizeof (address_word));
  /* Then the argc.  */
  sp -= sizeof (unsigned_word);
  /* Synchronize sp alignment with GCC's STACK_BOUNDARY.  */
  sp = ALIGN_16 (sp - 15);

  /* Set up the regs the libgloss crt0 expects.  */
  cpu->a0 = argc;
  cpu->sp = sp;

  /* First push the argc value.  */
  sim_write (sd, sp, (void *)&argc, sizeof (unsigned_word));
  sp += sizeof (unsigned_word);

  /* Then the actual argv strings so we know where to point argv[].  */
  for (i = 0; i < argc; ++i)
    {
      unsigned len = strlen (argv[i]) + 1;
      sim_write (sd, sp_flat, (void *)argv[i], len);
      sim_write (sd, sp, (void *)&sp_flat, sizeof (address_word));
      sp_flat += len;
      sp += sizeof (address_word);
    }
  sim_write (sd, sp, null, sizeof (address_word));
  sp += sizeof (address_word);

  /* Then the actual env strings so we know where to point env[].  */
  for (i = 0; i < envc; ++i)
    {
      unsigned len = strlen (env[i]) + 1;
      sim_write (sd, sp_flat, (void *)env[i], len);
      sim_write (sd, sp, (void *)&sp_flat, sizeof (address_word));
      sp_flat += len;
      sp += sizeof (address_word);
    }
}
