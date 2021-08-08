/*
# mach: riscv
# cc: -march=rv32imfc_xfhalf_xfhalfwithf
# output: == test: fadd16 -> success, nr. of errors: 0\n
# output: == test: fsub16 -> success, nr. of errors: 0\n
# output: == test: fmul16 -> success, nr. of errors: 0\n
# output: == test: fdiv16 -> success, nr. of errors: 0\n
# output: == test: fsqrt16 -> success, nr. of errors: 0\n
# output: == test: feq16 -> success, nr. of errors: 0\n
# output: == test: flt16 -> success, nr. of errors: 0\n
# output: == test: fle16 -> success, nr. of errors: 0\n
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
#include "fadd16.h"
#include "fmul16.h"
#include "fsub16.h"
// #include "fmuladd16.h" /* too large */
#include "fdiv16.h"
#include "feq16.h"
#include "fle16.h"
#include "flt16.h"
#include "fsqrt16.h"

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

testcase_t testcases[] = { { .name = "fadd16", .test = check_fadd },
                           { .name = "fsub16", .test = check_fsub },
                           { .name = "fmul16", .test = check_fmul },
                           { .name = "fdiv16", .test = check_fdiv },
                           { .name = "fsqrt16", .test = check_fsqrt },
                           { .name = "feq16", .test = check_feq },
                           { .name = "flt16", .test = check_flt },
                           { .name = "fle16", .test = check_fle },
                           { 0, 0 } };

union conv
{
  float16 hf;
  uint16_t h;
};

#define ARRAY_SIZE(x) ((sizeof (x)) / sizeof (x[0]))

void
check_fadd (testresult_t *result, void (*start) (), void (*stop) ())
{
  unsigned int i;
  union conv arg0, arg1, tmp0, tmp1;
  float16 out;

  for (i = 0; i < ARRAY_SIZE (fadd16) / 4; i += 4)
    {
      arg0.h = fadd16[i + 0];
      arg1.h = fadd16[i + 1];
      asm volatile("fadd.h %[c], %[a], %[b]\n"
                   : [c] "=f"(out)
                   : [a] "f"(arg0.hf), [b] "f"(arg1.hf));
      tmp0.hf = out;
      tmp1.h = fadd16[i + 2];
      if (tmp0.h != fadd16[i + 2])
        {
          printf ("\t\tdebug: %04x+%04x -> %04x (expected=%04x)\n", arg0.h,
                  arg1.h, tmp0.h, tmp1.h);
          printf ("\t\tdebug: %f+%f -> %f (expected=%f)\n", arg0.hf, arg1.hf,
                  tmp0.hf, tmp1.hf);
        }
      check_uint32 (result, "fadd.h", tmp0.h, fadd16[i + 2]);
    }
}

void
check_fsub (testresult_t *result, void (*start) (), void (*stop) ())
{
  unsigned int i;
  union conv arg0, arg1, tmp0, tmp1;
  float16 out;

  for (i = 0; i < ARRAY_SIZE (fsub16) / 4; i += 4)
    {
      arg0.h = fsub16[i + 0];
      arg1.h = fsub16[i + 1];
      asm volatile("fsub.h %[c], %[a], %[b]\n"
                   : [c] "=f"(out)
                   : [a] "f"(arg0.hf), [b] "f"(arg1.hf));
      tmp0.hf = out;
      tmp1.h = fsub16[i + 2];
      if (tmp0.h != fsub16[i + 2])
        {
          printf ("\t\tdebug: %04x-%04x -> %04x (expected=%04x)\n", arg0.h,
                  arg1.h, tmp0.h, tmp1.h);
          printf ("\t\tdebug: %f-%f -> %f (expected=%f)\n", arg0.hf, arg1.hf,
                  tmp0.hf, tmp1.hf);
        }
      check_uint32 (result, "fsub.h", tmp0.h, fsub16[i + 2]);
    }
}

void
check_fmul (testresult_t *result, void (*start) (), void (*stop) ())
{
  unsigned int i;
  union conv arg0, arg1, tmp0, tmp1;
  float16 out;

  for (i = 0; i < ARRAY_SIZE (fmul16) / 4; i += 4)
    {
      arg0.h = fmul16[i + 0];
      arg1.h = fmul16[i + 1];
      asm volatile("fmul.h %[c], %[a], %[b]\n"
                   : [c] "=f"(out)
                   : [a] "f"(arg0.hf), [b] "f"(arg1.hf));
      tmp0.hf = out;
      tmp1.h = fmul16[i + 2];
      if (tmp0.h != fmul16[i + 2])
        {
          printf ("\t\tdebug: %04x*%04x -> %04x (expected=%04x)\n", arg0.h,
                  arg1.h, tmp0.h, tmp1.h);
          printf ("\t\tdebug: %f*%f -> %f (expected=%f)\n", arg0.hf, arg1.hf,
                  tmp0.hf, tmp1.hf);
        }
      check_uint32 (result, "fmul.h", tmp0.h, fmul16[i + 2]);
    }
}

void
check_fdiv (testresult_t *result, void (*start) (), void (*stop) ())
{
  unsigned int i;
  union conv arg0, arg1, tmp0, tmp1;
  float16 out;

  for (i = 0; i < ARRAY_SIZE (fdiv16) / 4; i += 4)
    {
      arg0.h = fdiv16[i + 0];
      arg1.h = fdiv16[i + 1];
      asm volatile("fdiv.h %[c], %[a], %[b]\n"
                   : [c] "=f"(out)
                   : [a] "f"(arg0.hf), [b] "f"(arg1.hf));
      tmp0.hf = out;
      tmp1.h = fdiv16[i + 2];
      if (tmp0.h != fdiv16[i + 2])
        {
          printf ("\t\tdebug: %04x/%04x -> %04x (expected=%04x)\n", arg0.h,
                  arg1.h, tmp0.h, tmp1.h);
          printf ("\t\tdebug: %f/%f -> %f (expected=%f)\n", arg0.hf, arg1.hf,
                  tmp0.hf, tmp1.hf);
        }
      check_uint32 (result, "fdiv.h", tmp0.h, fdiv16[i + 2]);
    }
}

void
check_fsqrt (testresult_t *result, void (*start) (), void (*stop) ())
{
  unsigned int i;
  union conv arg0, arg1, tmp0, tmp1;
  float16 out;

  for (i = 0; i < ARRAY_SIZE (fsqrt16) / 3; i += 3)
    {
      arg0.h = fsqrt16[i + 0];
      asm volatile("fsqrt.h %[c], %[a]\n" : [c] "=f"(out) : [a] "f"(arg0.hf));
      tmp0.hf = out;
      tmp1.h = fsqrt16[i + 1];
      if (tmp0.h != fsqrt16[i + 1])
        {
          printf ("\t\tdebug: sqrt(%04x) -> %04x (expected=%04x)\n", arg0.h,
                  tmp0.h, tmp1.h);
          printf ("\t\tdebug: sqrt(%f) -> %f (expected=%f)\n", arg0.hf,
                  tmp0.hf, tmp1.hf);
        }
      check_uint32 (result, "fsqrt.h", tmp0.h, fsqrt16[i + 1]);
    }
}

void
check_feq (testresult_t *result, void (*start) (), void (*stop) ())
{
  unsigned int i;
  union conv arg0, arg1, tmp0, tmp1;
  int out;

  for (i = 0; i < ARRAY_SIZE (feq16) / 4; i += 4)
    {
      arg0.h = feq16[i + 0];
      arg1.h = feq16[i + 1];
      asm volatile("feq.h %[c], %[a], %[b]\n"
                   : [c] "=r"(out)
                   : [a] "f"(arg0.hf), [b] "f"(arg1.hf));
      tmp0.h = out;
      tmp1.h = feq16[i + 2];
      if (tmp0.h != feq16[i + 2])
        {
          printf ("\t\tdebug: %04x==%04x -> %04x (expected=%04x)\n", arg0.h,
                  arg1.h, tmp0.h, tmp1.h);
          printf ("\t\tdebug: %f==%f -> %d (expected=%d)\n", arg0.hf, arg1.hf,
                  tmp0.h, tmp1.h);
        }
      check_uint32 (result, "feq.h", tmp0.h, feq16[i + 2]);
    }
}

void
check_fle (testresult_t *result, void (*start) (), void (*stop) ())
{
  unsigned int i;
  union conv arg0, arg1, tmp0, tmp1;
  int out;

  for (i = 0; i < ARRAY_SIZE (fle16) / 4; i += 4)
    {
      arg0.h = fle16[i + 0];
      arg1.h = fle16[i + 1];
      asm volatile("fle.h %[c], %[a], %[b]\n"
                   : [c] "=r"(out)
                   : [a] "f"(arg0.hf), [b] "f"(arg1.hf));
      tmp0.h = out;
      tmp1.h = fle16[i + 2];
      if (tmp0.h != fle16[i + 2])
        {
          printf ("\t\tdebug: %04x<=%04x -> %04x (expected=%04x)\n", arg0.h,
                  arg1.h, tmp0.h, tmp1.h);
          printf ("\t\tdebug: %f<=%f -> %d (expected=%d)\n", arg0.hf, arg1.hf,
                  tmp0.h, tmp1.h);
        }
      check_uint32 (result, "fle.h", tmp0.h, fle16[i + 2]);
    }
}

void
check_flt (testresult_t *result, void (*start) (), void (*stop) ())
{
  unsigned int i;
  union conv arg0, arg1, tmp0, tmp1;
  int out;

  for (i = 0; i < ARRAY_SIZE (flt16) / 4; i += 4)
    {
      arg0.h = flt16[i + 0];
      arg1.h = flt16[i + 1];
      asm volatile("flt.h %[c], %[a], %[b]\n"
                   : [c] "=r"(out)
                   : [a] "f"(arg0.hf), [b] "f"(arg1.hf));
      tmp0.h = out;
      tmp1.h = flt16[i + 2];
      if (tmp0.h != flt16[i + 2])
        {
          printf ("\t\tdebug: %04x<%04x -> %04x (expected=%04x)\n", arg0.h,
                  arg1.h, tmp0.h, tmp1.h);
          printf ("\t\tdebug: %f<%f -> %d (expected=%d)\n", arg0.hf, arg1.hf,
                  tmp0.h, tmp1.h);
        }
      check_uint32 (result, "flt.h", tmp0.h, flt16[i + 2]);
    }
}

int
main (void)
{
  return run_suite (testcases);
}
