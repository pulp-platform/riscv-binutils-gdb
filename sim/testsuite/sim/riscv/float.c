/*
# mach: riscv
# cc: -march=rv32imfc
# output: == test: fadd32 -> success, nr. of errors: 0\n
# output: == test: fsub32 -> success, nr. of errors: 0\n
# output: == test: fmul32 -> success, nr. of errors: 0\n
# output: == test: fdiv32 -> success, nr. of errors: 0\n
# output: == test: fsqrt32 -> success, nr. of errors: 0\n
# output: == test: feq32 -> success, nr. of errors: 0\n
# output: == test: flt32 -> success, nr. of errors: 0\n
# output: == test: fle32 -> success, nr. of errors: 0\n
# output: ==== SUMMARY: SUCCESS\n
*/

/* Copyright 2021 ETH Zurich and University of Bologna.
   Copyright and related rights are licensed under the Solderpad Hardware
   License, Version 0.51 (the “License”); you may not use this file except in
   compliance with the License.  You may obtain a copy of the License at
   http://solderpad.org/licenses/SHL-0.51. Unless required by applicable law
   or agreed to in writing, software, hardware and materials distributed under
   this License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR
   CONDITIONS OF ANY KIND, either express or implied. See the License for the
   specific language governing permissions and limitations under the License.
 */

#include "bench.h"
#include "fadd32.h"
#include "fmul32.h"
#include "fsub32.h"
/* #include "fmuladd32.h" /* /* too large */
#include "fdiv32.h"
#include "feq32.h"
#include "fle32.h"
#include "flt32.h"
#include "fsqrt32.h"

#include <stdio.h>

void check_fadd (testresult_t *result, void (*start) (), void (*stop) ());
void check_fsub (testresult_t *result, void (*start) (), void (*stop) ());
void check_fmul (testresult_t *result, void (*start) (), void (*stop) ());
void check_fdiv (testresult_t *result, void (*start) (), void (*stop) ());
void check_frem (testresult_t *result, void (*start) (), void (*stop) ());
void check_fsqrt (testresult_t *result, void (*start) (), void (*stop) ());
void check_feq (testresult_t *result, void (*start) (), void (*stop) ());
void check_flt (testresult_t *result, void (*start) (), void (*stop) ());
void check_fle (testresult_t *result, void (*start) (), void (*stop) ());

testcase_t testcases[] = { { .name = "fadd32", .test = check_fadd },
                           { .name = "fsub32", .test = check_fsub },
                           { .name = "fmul32", .test = check_fmul },
                           { .name = "fdiv32", .test = check_fdiv },
                           { .name = "fsqrt32", .test = check_fsqrt },
                           { .name = "feq32", .test = check_feq },
                           { .name = "flt32", .test = check_flt },
                           { .name = "fle32", .test = check_fle },
                           { 0, 0 } };

union conv
{
  float hf;
  uint32_t h;
};

#define ARRAY_SIZE(x) ((sizeof (x)) / sizeof (x[0]))

void
check_fadd (testresult_t *result, void (*start) (), void (*stop) ())
{
  unsigned int i;
  union conv arg0, arg1, tmp0, tmp1;
  float out;

  for (i = 0; i < ARRAY_SIZE (fadd32) / 4; i += 4)
    {
      arg0.h = fadd32[i + 0];
      arg1.h = fadd32[i + 1];
      asm volatile("fadd.s %[c], %[a], %[b]\n"
                   : [c] "=f"(out)
                   : [a] "f"(arg0.hf), [b] "f"(arg1.hf));
      tmp0.hf = out;
      tmp1.h = fadd32[i + 2];
      if (tmp0.h != fadd32[i + 2])
        {
          printf ("\t\tdebug: %08lx+%08lx -> %08lx (expected=%08lx)\n", arg0.h,
                  arg1.h, tmp0.h, tmp1.h);
          printf ("\t\tdebug: %f+%f -> %f (expected=%f)\n", arg0.hf, arg1.hf,
                  tmp0.hf, tmp1.hf);
        }
      check_uint32 (result, "fadd.s", tmp0.h, fadd32[i + 2]);
    }
}

void
check_fsub (testresult_t *result, void (*start) (), void (*stop) ())
{
  unsigned int i;
  union conv arg0, arg1, tmp0, tmp1;
  float out;

  for (i = 0; i < ARRAY_SIZE (fsub32) / 4; i += 4)
    {
      arg0.h = fsub32[i + 0];
      arg1.h = fsub32[i + 1];
      asm volatile("fsub.s %[c], %[a], %[b]\n"
                   : [c] "=f"(out)
                   : [a] "f"(arg0.hf), [b] "f"(arg1.hf));
      tmp0.hf = out;
      tmp1.h = fsub32[i + 2];
      if (tmp0.h != fsub32[i + 2])
        {
          printf ("\t\tdebug: %08lx-%08lx -> %08lx (expected=%08lx)\n", arg0.h,
                  arg1.h, tmp0.h, tmp1.h);
          printf ("\t\tdebug: %f-%f -> %f (expected=%f)\n", arg0.hf, arg1.hf,
                  tmp0.hf, tmp1.hf);
        }
      check_uint32 (result, "fsub.s", tmp0.h, fsub32[i + 2]);
    }
}

void
check_fmul (testresult_t *result, void (*start) (), void (*stop) ())
{
  unsigned int i;
  union conv arg0, arg1, tmp0, tmp1;
  float out;

  for (i = 0; i < ARRAY_SIZE (fmul32) / 4; i += 4)
    {
      arg0.h = fmul32[i + 0];
      arg1.h = fmul32[i + 1];
      asm volatile("fmul.s %[c], %[a], %[b]\n"
                   : [c] "=f"(out)
                   : [a] "f"(arg0.hf), [b] "f"(arg1.hf));
      tmp0.hf = out;
      tmp1.h = fmul32[i + 2];
      if (tmp0.h != fmul32[i + 2])
        {
          printf ("\t\tdebug: %08lx*%08lx -> %08lx (expected=%08lx)\n", arg0.h,
                  arg1.h, tmp0.h, tmp1.h);
          printf ("\t\tdebug: %f*%f -> %f (expected=%f)\n", arg0.hf, arg1.hf,
                  tmp0.hf, tmp1.hf);
        }
      check_uint32 (result, "fmul.s", tmp0.h, fmul32[i + 2]);
    }
}

void
check_fdiv (testresult_t *result, void (*start) (), void (*stop) ())
{
  unsigned int i;
  union conv arg0, arg1, tmp0, tmp1;
  float out;

  for (i = 0; i < ARRAY_SIZE (fdiv32) / 4; i += 4)
    {
      arg0.h = fdiv32[i + 0];
      arg1.h = fdiv32[i + 1];
      asm volatile("fdiv.s %[c], %[a], %[b]\n"
                   : [c] "=f"(out)
                   : [a] "f"(arg0.hf), [b] "f"(arg1.hf));
      tmp0.hf = out;
      tmp1.h = fdiv32[i + 2];
      if (tmp0.h != fdiv32[i + 2])
        {
          printf ("\t\tdebug: %08lx/%08lx -> %08lx (expected=%08lx)\n", arg0.h,
                  arg1.h, tmp0.h, tmp1.h);
          printf ("\t\tdebug: %f/%f -> %f (expected=%f)\n", arg0.hf, arg1.hf,
                  tmp0.hf, tmp1.hf);
        }
      check_uint32 (result, "fdiv.s", tmp0.h, fdiv32[i + 2]);
    }
}

void
check_fsqrt (testresult_t *result, void (*start) (), void (*stop) ())
{
  unsigned int i;
  union conv arg0, arg1, tmp0, tmp1;
  float out;

  for (i = 0; i < ARRAY_SIZE (fsqrt32) / 3; i += 3)
    {
      arg0.h = fsqrt32[i + 0];
      asm volatile("fsqrt.s %[c], %[a]\n" : [c] "=f"(out) : [a] "f"(arg0.hf));
      tmp0.hf = out;
      tmp1.h = fsqrt32[i + 1];
      if (tmp0.h != fsqrt32[i + 1])
        {
          printf ("\t\tdebug: sqrt(%08lx) -> %08lx (expected=%08lx)\n", arg0.h,
                  tmp0.h, tmp1.h);
          printf ("\t\tdebug: sqrt(%f) -> %f (expected=%f)\n", arg0.hf,
                  tmp0.hf, tmp1.hf);
        }
      check_uint32 (result, "fsqrt.s", tmp0.h, fsqrt32[i + 1]);
    }
}

void
check_feq (testresult_t *result, void (*start) (), void (*stop) ())
{
  unsigned int i;
  union conv arg0, arg1, tmp0, tmp1;
  int out;

  for (i = 0; i < ARRAY_SIZE (feq32) / 4; i += 4)
    {
      arg0.h = feq32[i + 0];
      arg1.h = feq32[i + 1];
      asm volatile("feq.s %[c], %[a], %[b]\n"
                   : [c] "=r"(out)
                   : [a] "f"(arg0.hf), [b] "f"(arg1.hf));
      tmp0.h = out;
      tmp1.h = feq32[i + 2];
      if (tmp0.h != feq32[i + 2])
        {
          printf ("\t\tdebug: %08lx==%08lx -> %08lx (expected=%08lx)\n",
                  arg0.h, arg1.h, tmp0.h, tmp1.h);
          printf ("\t\tdebug: %f==%f -> %ld (expected=%ld)\n", arg0.hf,
                  arg1.hf, tmp0.h, tmp1.h);
        }
      check_uint32 (result, "feq.s", tmp0.h, feq32[i + 2]);
    }
}

void
check_fle (testresult_t *result, void (*start) (), void (*stop) ())
{
  unsigned int i;
  union conv arg0, arg1, tmp0, tmp1;
  int out;

  for (i = 0; i < ARRAY_SIZE (fle32) / 4; i += 4)
    {
      arg0.h = fle32[i + 0];
      arg1.h = fle32[i + 1];
      asm volatile("fle.s %[c], %[a], %[b]\n"
                   : [c] "=r"(out)
                   : [a] "f"(arg0.hf), [b] "f"(arg1.hf));
      tmp0.h = out;
      tmp1.h = fle32[i + 2];
      if (tmp0.h != fle32[i + 2])
        {
          printf ("\t\tdebug: %08lx<=%08lx -> %08lx (expected=%08lx)\n",
                  arg0.h, arg1.h, tmp0.h, tmp1.h);
          printf ("\t\tdebug: %f<=%f -> %ld (expected=%ld)\n", arg0.hf,
                  arg1.hf, tmp0.h, tmp1.h);
        }
      check_uint32 (result, "fle.s", tmp0.h, fle32[i + 2]);
    }
}

void
check_flt (testresult_t *result, void (*start) (), void (*stop) ())
{
  unsigned int i;
  union conv arg0, arg1, tmp0, tmp1;
  int out;

  for (i = 0; i < ARRAY_SIZE (flt32) / 4; i += 4)
    {
      arg0.h = flt32[i + 0];
      arg1.h = flt32[i + 1];
      asm volatile("flt.s %[c], %[a], %[b]\n"
                   : [c] "=r"(out)
                   : [a] "f"(arg0.hf), [b] "f"(arg1.hf));
      tmp0.h = out;
      tmp1.h = flt32[i + 2];
      if (tmp0.h != flt32[i + 2])
        {
          printf ("\t\tdebug: %08lx<%08lx -> %08lx (expected=%08lx)\n", arg0.h,
                  arg1.h, tmp0.h, tmp1.h);
          printf ("\t\tdebug: %f<%f -> %ld (expected=%ld)\n", arg0.hf, arg1.hf,
                  tmp0.h, tmp1.h);
        }
      check_uint32 (result, "flt.s", tmp0.h, flt32[i + 2]);
    }
}

int
main (void)
{
  return run_suite (testcases);
}
