/* RISC-V disassembler
   Copyright (C) 2011-2020 Free Software Foundation, Inc.

   Contributed by Andrew Waterman (andrew@sifive.com).
   Based on MIPS target.

   PULP family support contributed by Eric Flamand (eflamand@iis.ee.ethz.ch) at ETH-Zurich
   and Greenwaves Technologies (eric.flamand@greenwaves-technologies.com)

   This file is part of the GNU opcodes library.

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   It is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; see the file COPYING3. If not,
   see <http://www.gnu.org/licenses/>.  */

#include "sysdep.h"
#include "disassemble.h"
#include "libiberty.h"
#include "opcode/riscv.h"
#include "opintl.h"
#include "elf-bfd.h"
#include "elf/riscv.h"
#include "elfxx-riscv.h"

#include "bfd_stdint.h"
#include <ctype.h>

#define RISCV_FALLBACK_MARCH "rv32imafdcxpulpv3"

#ifndef DEFAULT_RISCV_ATTR
#define DEFAULT_RISCV_ATTR 0
#endif

struct riscv_private_data
{
  bfd_vma gp;
  bfd_vma print_addr;
  bfd_vma hi_addr[OP_MASK_RD + 1];
};

static const char * const *riscv_gpr_names;
static const char * const *riscv_fpr_names;

/* Other options.  */
static int no_aliases;	/* If set disassemble as most general inst.  */

/* Indicate arch attribute is explictly set.  */
static bfd_boolean explicit_arch_attr = FALSE;

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
	    opcodes_error_handler ("internal error: too many custom pulp extensions");
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
      /* as_fatal ("Unreachable"); */
      opcodes_error_handler ("Unreachable");
      return FALSE;
    }
}

/* Set which ISA and extensions are available.  */

static void
riscv_set_arch (const char *s)
{
  riscv_parse_subset_t rps;
  rps.subset_list = &riscv_subsets;
  rps.error_handler = opcodes_error_handler;
  rps.xlen = &gxlen;

  riscv_release_subset_list (&riscv_subsets);
  riscv_parse_subset (&rps, s);
}

/* Parse a .attribute directive.  */

static void
parse_riscv_attribute (bfd *abfd)
{
  obj_attribute *attr;
  attr = elf_known_obj_attributes_proc (abfd);
  if (attr && attr[Tag_RISCV_arch].s)
    {
      explicit_arch_attr = TRUE;
      riscv_set_arch (attr[Tag_RISCV_arch].s);
    }
  else
    riscv_set_arch (RISCV_FALLBACK_MARCH);
}

/* Set default options except for arch */

static void
set_default_riscv_dis_options (void)
{
  riscv_gpr_names = riscv_gpr_names_abi;
  riscv_fpr_names = riscv_fpr_names_abi;
  no_aliases = 0;
}

static void
parse_riscv_dis_option (const char *option)
{
  const char *where=NULL;
  if (strcmp (option, "no-aliases") == 0)
    no_aliases = 1;
  else if (strcmp (option, "numeric") == 0)
    {
      riscv_gpr_names = riscv_gpr_names_numeric;
      riscv_fpr_names = riscv_fpr_names_numeric;
    }
  else if ((where = strstr(option, "march="))&&(where==option))
    {
      char *comma;
      where = option+6;
      comma = strstr(where, ",");
      if (comma) *comma = '\0';
      riscv_set_arch(where);
      if (comma) *comma = ',';
    }
  else if ((where = strstr(option, "mchip="))&&(where==option))
    {
      int i;
      char *uppercase;
      char *comma;
      where = option+6;
      comma = strstr(where, ",");
      if (comma) *comma = '\0';
      uppercase = xstrdup (where);
      for (i = 0; uppercase[i]; i++) uppercase[i] = toupper (uppercase[i]);
      if (strstr(uppercase, "PULPINO")) riscv_set_arch ("rv32imcxpulpv1");
      else if (strstr(uppercase, "HONEY")) riscv_set_arch ("rv32imcxpulpv0");
      else if (strstr(uppercase, "GAP8")) riscv_set_arch ("rv32imcxgap8");
      else if (strstr(uppercase, "GAP9")) riscv_set_arch ("rv32imcxgap9");
      else if (strstr(uppercase, "HUA20")) riscv_set_arch ("rv32imcxgap9");
      else fprintf (stderr, _("Unrecognized mchip= : %s\n"), uppercase);
      if (comma) *comma = ',';
    }
  else
    {
      /* Invalid option.  */
      /* xgettext:c-format */
      opcodes_error_handler (_("unrecognized disassembler option: %s"), option);
    }
}

static void
parse_riscv_dis_options (const char *opts_in)
{
  char *opts = xstrdup (opts_in), *opt = opts, *opt_end = opts;

  set_default_riscv_dis_options ();

  for ( ; opt_end != NULL; opt = opt_end + 1)
    {
      if ((opt_end = strchr (opt, ',')) != NULL)
	*opt_end = 0;
      parse_riscv_dis_option (opt);
    }

  free (opts);
}

/* Print one argument from an array.  */

static void
arg_print (struct disassemble_info *info, unsigned long val,
	   const char* const* array, size_t size)
{
  const char *s = val >= size || array[val] == NULL ? "unknown" : array[val];
  (*info->fprintf_func) (info->stream, "%s", s);
}

static void
maybe_print_address (struct riscv_private_data *pd, int base_reg, int offset)
{
  if (base_reg == 0) { /* %tiny(Symbol_Expr)(x0) */
    pd->print_addr = offset;
  } else if (pd->hi_addr[base_reg] != (bfd_vma)-1)
    {
      pd->print_addr = (base_reg != 0 ? pd->hi_addr[base_reg] : 0) + offset;
      pd->hi_addr[base_reg] = -1;
    }
  else if (base_reg == X_GP && pd->gp != (bfd_vma)-1)
    pd->print_addr = pd->gp + offset;
  else if (base_reg == X_TP || base_reg == 0)
    pd->print_addr = offset;
}

/* Print insn arguments for 32/64-bit code.  */

static void
print_insn_args (const char *d, insn_t l, bfd_vma pc, disassemble_info *info)
{
  struct riscv_private_data *pd = info->private_data;
  int rs1 = (l >> OP_SH_RS1) & OP_MASK_RS1;
  int rd = (l >> OP_SH_RD) & OP_MASK_RD;
  fprintf_ftype print = info->fprintf_func;

  if (*d != '\0')
    print (info->stream, "\t");

  for (; *d != '\0'; d++)
    {
      switch (*d)
	{
	case 'C': /* RVC */
	  switch (*++d)
	    {
	    case 's': /* RS1 x8-x15 */
	    case 'w': /* RS1 x8-x15 */
	      print (info->stream, "%s",
		     riscv_gpr_names[EXTRACT_OPERAND (CRS1S, l) + 8]);
	      break;
	    case 't': /* RS2 x8-x15 */
	    case 'x': /* RS2 x8-x15 */
	      print (info->stream, "%s",
		     riscv_gpr_names[EXTRACT_OPERAND (CRS2S, l) + 8]);
	      break;
	    case 'U': /* RS1, constrained to equal RD */
	      print (info->stream, "%s", riscv_gpr_names[rd]);
	      break;
	    case 'c': /* RS1, constrained to equal sp */
	      print (info->stream, "%s", riscv_gpr_names[X_SP]);
	      break;
	    case 'V': /* RS2 */
	      print (info->stream, "%s",
		     riscv_gpr_names[EXTRACT_OPERAND (CRS2, l)]);
	      break;
	    case 'i':
	      print (info->stream, "%d", (int)EXTRACT_RVC_SIMM3 (l));
	      break;
	    case 'o':
	    case 'j':
	      print (info->stream, "%d", (int)EXTRACT_RVC_IMM (l));
	      break;
	    case 'k':
	      print (info->stream, "%d", (int)EXTRACT_RVC_LW_IMM (l));
	      break;
	    case 'l':
	      print (info->stream, "%d", (int)EXTRACT_RVC_LD_IMM (l));
	      break;
	    case 'm':
	      print (info->stream, "%d", (int)EXTRACT_RVC_LWSP_IMM (l));
	      break;
	    case 'n':
	      print (info->stream, "%d", (int)EXTRACT_RVC_LDSP_IMM (l));
	      break;
	    case 'K':
	      print (info->stream, "%d", (int)EXTRACT_RVC_ADDI4SPN_IMM (l));
	      break;
	    case 'L':
	      print (info->stream, "%d", (int)EXTRACT_RVC_ADDI16SP_IMM (l));
	      break;
	    case 'M':
	      print (info->stream, "%d", (int)EXTRACT_RVC_SWSP_IMM (l));
	      break;
	    case 'N':
	      print (info->stream, "%d", (int)EXTRACT_RVC_SDSP_IMM (l));
	      break;
	    case 'p':
	      info->target = EXTRACT_RVC_B_IMM (l) + pc;
	      (*info->print_address_func) (info->target, info);
	      break;
	    case 'a':
	      info->target = EXTRACT_RVC_J_IMM (l) + pc;
	      (*info->print_address_func) (info->target, info);
	      break;
	    case 'u':
	      print (info->stream, "0x%x",
		     (int)(EXTRACT_RVC_IMM (l) & (RISCV_BIGIMM_REACH-1)));
	      break;
	    case '>':
	      print (info->stream, "0x%x", (int)EXTRACT_RVC_IMM (l) & 0x3f);
	      break;
	    case '<':
	      print (info->stream, "0x%x", (int)EXTRACT_RVC_IMM (l) & 0x1f);
	      break;
	    case 'T': /* floating-point RS2 */
	      print (info->stream, "%s",
		     riscv_fpr_names[EXTRACT_OPERAND (CRS2, l)]);
	      break;
	    case 'D': /* floating-point RS2 x8-x15 */
	      print (info->stream, "%s",
		     riscv_fpr_names[EXTRACT_OPERAND (CRS2S, l) + 8]);
	      break;
	    }
	  break;

	case ',':
        case '!':
	case '(':
	case ')':
	case '[':
	case ']':
	  print (info->stream, "%c", *d);
	  break;

	case '0':
	  /* Only print constant 0 if it is the last argument */
	  if (!d[1])
	    print (info->stream, "0");
	  break;

	case 'b':
          if (d[1]=='1') {
             info->target = (EXTRACT_ITYPE_IMM (l)<<1) + pc; ++d;
             (*info->print_address_func) (info->target, info);
          } else if (d[1]=='2') {
             info->target = (EXTRACT_I1TYPE_UIMM (l)<<1) + pc; ++d;
             (*info->print_address_func) (info->target, info);
          } else if (d[1]=='3') {
             print (info->stream, "%d", (int) EXTRACT_I1TYPE_UIMM (l)); ++d;
          } else if (d[1]=='5') {
             print (info->stream, "%d", ((int) EXTRACT_I5TYPE_UIMM (l))&0x1F); ++d;
          } else if (d[1]=='I') {
             print (info->stream, "%d", ((int) EXTRACT_I5_1_TYPE_IMM (l)<<27)>>27); ++d;
          } else if (d[1]=='i') {
             print (info->stream, "%d", ((int) EXTRACT_I5_1_TYPE_UIMM (l))&0x1F); ++d;
          } else if (d[1]=='s') {
             print (info->stream, "%d", ((int) EXTRACT_I6TYPE_IMM (l)<<26)>>26); ++d;
          } else if (d[1]=='u') {
             print (info->stream, "%d", ((int) EXTRACT_I6TYPE_IMM (l))&0x1F); ++d;
          } else if (d[1]=='U') {
             print (info->stream, "%d", ((int) EXTRACT_I6TYPE_IMM (l))&0x0F); ++d;
          } else if (d[1]=='f') {
             print (info->stream, "%d", ((int) EXTRACT_I6TYPE_IMM (l))&0x01); ++d;
          } else if (d[1]=='F') {
             print (info->stream, "%d", ((int) EXTRACT_I6TYPE_IMM (l))&0x03); ++d;
          } else (*info->print_address_func) (info->target, info);
          break;

	case 's':
	  if ((l & MASK_JALR) == MATCH_JALR)
	    maybe_print_address (pd, rs1, 0);
	  print (info->stream, "%s", riscv_gpr_names[rs1]);
	  break;

        case 'y':
          print (info->stream, "%s", riscv_gpr_names[EXTRACT_OPERAND (PULP_RS3, l)]);
          break;

        case 'e':
          print (info->stream, "%s", riscv_gpr_names[EXTRACT_OPERAND (RS3, l)]);
          break;

	case 'w':
	  print (info->stream, "%s", riscv_gpr_names[EXTRACT_OPERAND (RS1, l)]);
	case 't':
	  print (info->stream, "%s",
		 riscv_gpr_names[EXTRACT_OPERAND (RS2, l)]);
	  break;

	case 'u':
	  print (info->stream, "0x%x",
		 (unsigned)EXTRACT_UTYPE_IMM (l) >> RISCV_IMM_BITS);
	  break;

	case 'm':
	  arg_print (info, EXTRACT_OPERAND (RM, l),
		     riscv_rm, ARRAY_SIZE (riscv_rm));
	  break;

	case 'P':
	  arg_print (info, EXTRACT_OPERAND (PRED, l),
		     riscv_pred_succ, ARRAY_SIZE (riscv_pred_succ));
	  break;

	case 'Q':
	  arg_print (info, EXTRACT_OPERAND (SUCC, l),
		     riscv_pred_succ, ARRAY_SIZE (riscv_pred_succ));
	  break;

	case 'o':
	  maybe_print_address (pd, rs1, EXTRACT_ITYPE_IMM (l));
	  /* Fall through.  */
	case 'j':
          if (d[1]=='i') {
             ++d;
             print (info->stream, "%d", (int)ENCODE_LOOP_UIMM(l));
             break;
          }
	  if (((l & MASK_ADDI) == MATCH_ADDI && rs1 != 0)
	      || (l & MASK_JALR) == MATCH_JALR)
	    maybe_print_address (pd, rs1, EXTRACT_ITYPE_IMM (l));
	  print (info->stream, "%d", (int)EXTRACT_ITYPE_IMM (l));
	  break;

	case 'q':
	  maybe_print_address (pd, rs1, EXTRACT_STYPE_IMM (l));
	  print (info->stream, "%d", (int)EXTRACT_STYPE_IMM (l));
	  break;

	case 'a':
	  info->target = EXTRACT_UJTYPE_IMM (l) + pc;
	  (*info->print_address_func) (info->target, info);
	  break;

	case 'p':
	  info->target = EXTRACT_SBTYPE_IMM (l) + pc;
	  (*info->print_address_func) (info->target, info);
	  break;

	case 'd':
	  if ((l & MASK_AUIPC) == MATCH_AUIPC)
	    pd->hi_addr[rd] = pc + EXTRACT_UTYPE_IMM (l);
	  else if ((l & MASK_LUI) == MATCH_LUI)
	    pd->hi_addr[rd] = EXTRACT_UTYPE_IMM (l);
	  else if ((l & MASK_C_LUI) == MATCH_C_LUI)
	    pd->hi_addr[rd] = EXTRACT_RVC_LUI_IMM (l);
          if (d[1]=='i') {
             ++d;
             print (info->stream, "x%d", (int) rd);
          } else print (info->stream, "%s", riscv_gpr_names[rd]);

	  break;

	case 'z':
	  print (info->stream, "%s", riscv_gpr_names[0]);
	  break;

	case '>':
	  print (info->stream, "0x%x", (int)EXTRACT_OPERAND (SHAMT, l));
	  break;

	case '<':
	  print (info->stream, "0x%x", (int)EXTRACT_OPERAND (SHAMTW, l));
	  break;

	case 'S':
	case 'U':
	  print (info->stream, "%s", riscv_fpr_names[rs1]);
	  break;

	case 'T':
	  print (info->stream, "%s", riscv_fpr_names[EXTRACT_OPERAND (RS2, l)]);
	  break;

	case 'D':
	  print (info->stream, "%s", riscv_fpr_names[rd]);
	  break;

	case 'R':
	  print (info->stream, "%s", riscv_fpr_names[EXTRACT_OPERAND (RS3, l)]);
	  break;

	case 'E':
	  {
	    const char* csr_name = NULL;
	    unsigned int csr = EXTRACT_OPERAND (CSR, l);
	    switch (csr)
	      {
#define DECLARE_CSR(name, num) case num: csr_name = #name; break;
#include "opcode/riscv-opc.h"
#undef DECLARE_CSR
	      }
	    if (csr_name)
	      print (info->stream, "%s", csr_name);
	    else
	      print (info->stream, "0x%x", csr);
	    break;
	  }

	case 'Z':
	  print (info->stream, "%d", rs1);
	  break;

	default:
	  /* xgettext:c-format */
	  print (info->stream, _("# internal error, undefined modifier (%c)"),
		 *d);
	  return;
	}
    }
}

/* Print the RISC-V instruction at address MEMADDR in debugged memory,
   on using INFO.  Returns length of the instruction, in bytes.
   BIGENDIAN must be 1 if this is big-endian code, 0 if
   this is little-endian code.  */

static int
riscv_disassemble_insn (bfd_vma memaddr, insn_t word, disassemble_info *info)
{
  const struct riscv_opcode *op;
  static bfd_boolean init = 0;
  static const struct riscv_opcode *riscv_hash[OP_MASK_OP + 1];
  struct riscv_private_data *pd;
  int insnlen;

#define OP_HASH_IDX(i) ((i) & (riscv_insn_length (i) == 2 ? 0x3 : OP_MASK_OP))

  /* Build a hash table to shorten the search time.  */
  if (! init)
    {
      for (op = riscv_opcodes; op->name; op++)
	/* only add requested insn subsets, because they overlap */
        if (riscv_multi_subset_supports (op->insn_class))
	  if (!riscv_hash[OP_HASH_IDX (op->match)])
	    riscv_hash[OP_HASH_IDX (op->match)] = op;

      init = 1;
    }

  if (info->private_data == NULL)
    {
      int i;

      pd = info->private_data = xcalloc (1, sizeof (struct riscv_private_data));
      pd->gp = -1;
      pd->print_addr = -1;
      for (i = 0; i < (int)ARRAY_SIZE (pd->hi_addr); i++)
	pd->hi_addr[i] = -1;

      for (i = 0; i < info->symtab_size; i++)
	if (strcmp (bfd_asymbol_name (info->symtab[i]), RISCV_GP_SYMBOL) == 0)
	  pd->gp = bfd_asymbol_value (info->symtab[i]);
    }
  else
    pd = info->private_data;

  insnlen = riscv_insn_length (word);

  /* RISC-V instructions are always little-endian.  */
  info->endian_code = BFD_ENDIAN_LITTLE;

  info->bytes_per_chunk = insnlen % 4 == 0 ? 4 : 2;
  info->bytes_per_line = 8;
  /* We don't support constant pools, so this must be code.  */
  info->display_endian = info->endian_code;
  info->insn_info_valid = 1;
  info->branch_delay_insns = 0;
  info->data_size = 0;
  info->insn_type = dis_nonbranch;
  info->target = 0;
  info->target2 = 0;

  op = riscv_hash[OP_HASH_IDX (word)];
  if (op != NULL)
    {
      unsigned xlen = 0;

      /* If XLEN is not known, get its value from the ELF class.  */
      if (info->mach == bfd_mach_riscv64)
	xlen = 64;
      else if (info->mach == bfd_mach_riscv32)
	xlen = 32;
      else if (info->section != NULL)
	{
	  Elf_Internal_Ehdr *ehdr = elf_elfheader (info->section->owner);
	  xlen = ehdr->e_ident[EI_CLASS] == ELFCLASS64 ? 64 : 32;
	}

      for (; op->name; op++)
	{
	  /* is this instruction really supported by the current subset (due to
	     overlapping pulp subsets) */
	  if (!riscv_multi_subset_supports (op->insn_class))
            continue;
	  /* Does the opcode match?  */
	  if (! (op->match_func) (op, word))
	    continue;
	  /* Is this a pseudo-instruction and may we print it as such?  */
	  if (no_aliases && (op->pinfo & INSN_ALIAS))
	    continue;
	  /* Is this instruction restricted to a certain value of XLEN?  */
	  if ((op->xlen_requirement != 0) && (op->xlen_requirement != xlen))
	    continue;

	  /* It's a match.  */
	  (*info->fprintf_func) (info->stream, "%s", op->name);
	  print_insn_args (op->args, word, memaddr, info);

	  /* Try to disassemble multi-instruction addressing sequences.  */
	  if (pd->print_addr != (bfd_vma)-1)
	    {
	      info->target = pd->print_addr;
	      (*info->fprintf_func) (info->stream, " # ");
	      (*info->print_address_func) (info->target, info);
	      pd->print_addr = -1;
	    }

	  /* Finish filling out insn_info fields.  */
	  switch (op->pinfo & INSN_TYPE)
	    {
	    case INSN_BRANCH:
	      info->insn_type = dis_branch;
	      break;
	    case INSN_CONDBRANCH:
	      info->insn_type = dis_condbranch;
	      break;
	    case INSN_JSR:
	      info->insn_type = dis_jsr;
	      break;
	    case INSN_DREF:
	      info->insn_type = dis_dref;
	      break;
	    default:
	      break;
	    }

	  if (op->pinfo & INSN_DATA_SIZE)
	    {
	      int size = ((op->pinfo & INSN_DATA_SIZE)
			  >> INSN_DATA_SIZE_SHIFT);
	      info->data_size = 1 << (size - 1);
	    }

	  return insnlen;
	}
    }

  /* We did not find a match, so just print the instruction bits.  */
  info->insn_type = dis_noninsn;
  (*info->fprintf_func) (info->stream, "0x%llx", (unsigned long long)word);
  return insnlen;
}

int
print_insn_riscv (bfd_vma memaddr, struct disassemble_info *info)
{
  bfd_byte packet[2];
  insn_t insn = 0;
  bfd_vma n;
  int status;

  if (info->disassembler_options != NULL)
    {
      parse_riscv_dis_options (info->disassembler_options);
      /* Avoid repeatedly parsing the options.  */
      info->disassembler_options = NULL;
    }
  else if (riscv_gpr_names == NULL)
    set_default_riscv_dis_options ();

  /* Instructions are a sequence of 2-byte packets in little-endian order.  */
  for (n = 0; n < sizeof (insn) && n < riscv_insn_length (insn); n += 2)
    {
      status = (*info->read_memory_func) (memaddr + n, packet, 2, info);
      if (status != 0)
	{
	  /* Don't fail just because we fell off the end.  */
	  if (n > 0)
	    break;
	  (*info->memory_error_func) (status, memaddr, info);
	  return status;
	}

      insn |= ((insn_t) bfd_getl16 (packet)) << (8 * n);
    }

  return riscv_disassemble_insn (memaddr, insn, info);
}

/* Prevent use of the fake labels that are generated as part of the DWARF
   and for relaxable relocations in the assembler.  */

bfd_boolean
riscv_symbol_is_valid (asymbol * sym,
                       struct disassemble_info * info ATTRIBUTE_UNUSED)
{
  const char * name;

  if (sym == NULL)
    return FALSE;

  name = bfd_asymbol_name (sym);

  return (strcmp (name, RISCV_FAKE_LABEL_NAME) != 0);
}

disassembler_ftype
riscv_get_disassembler (bfd *abfd)
{
  /* BFD my be absent, if opcodes is invoked from the debugger that
     has connected to remote target and doesn't have an ELF file.  */
  if (abfd != NULL)
    {
      /* Set arch depending on attribute values. */
      parse_riscv_attribute (abfd);
    }
  else
    riscv_set_arch (RISCV_FALLBACK_MARCH);

  /* Enable rvc regardless of -march to support .option rvc. */
  riscv_opts.rvc = TRUE;

  return print_insn_riscv;
}

void
print_riscv_disassembler_options (FILE *stream)
{
  fprintf (stream, _("\n\
The following RISC-V-specific disassembler options are supported for use\n\
with the -M switch (multiple options should be separated by commas):\n"));

  fprintf (stream, _("\n\
  numeric       Print numeric register names, rather than ABI names.\n"));

  fprintf (stream, _("\n\
  no-aliases    Disassemble only into canonical instructions, rather\n\
                than into pseudoinstructions.\n"));

  fprintf (stream, _("\n\
  march         Disassemble assuming the given extensions are \n\
                available. By default they are derived from the \n\
                RISCV_arch attribute, if available.\n"));

  fprintf (stream, _("\n"));
}
