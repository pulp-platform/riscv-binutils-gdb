/*
# mach: riscv
# cc: -march=rv32imfcxcorev
# output: == test: check_pv_addsubdiv -> success, nr. of errors: 0\n
# output: ==== SUMMARY: SUCCESS\n
*/

// Copyright 2021 ETH Zurich and University of Bologna.
// Copyright and related rights are licensed under the Solderpad Hardware
// License, Version 0.51 (the “License”); you may not use this file except in
// compliance with the License.  You may obtain a copy of the License at
// http://solderpad.org/licenses/SHL-0.51. Unless required by applicable law
// or agreed to in writing, software, hardware and materials distributed under
// this License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR
// CONDITIONS OF ANY KIND, either express or implied. See the License for the
// specific language governing permissions and limitations under the License.

#include "bench.h"
#include <stdio.h>

#include "cplxbitman_stimuli.h"
#include "cplxbitrev_stimuli.h"

#include "pv_complex_stimuli.h"
#include "pv_complex_stimuli_div2.h"
#include "pv_complex_stimuli_div4.h"
#include "pv_complex_stimuli_div8.h"
#include "pv_cplxmul_img_stimuli.h"
#include "pv_cplxmul_img_stimuli_div2.h"
#include "pv_cplxmul_img_stimuli_div4.h"
#include "pv_cplxmul_img_stimuli_div8.h"
#include "pv_cplxmul_real_stimuli.h"
#include "pv_cplxmul_real_stimuli_div2.h"
#include "pv_cplxmul_real_stimuli_div4.h"
#include "pv_cplxmul_real_stimuli_div8.h"

#ifndef NUM_STIM
#define NUM_STIM
#define N 100
#endif

int tmp_result[N];

void check_pv_addsubdiv (testresult_t *result, void (*start) (), void (*stop) ());

testcase_t testcases[]
    = { { .name = "check_pv_addsubdiv", .test = check_pv_addsubdiv },
        { 0, 0 } };

int
main ()
{
  return run_suite (testcases);
}

void
check_pv_addsubdiv (testresult_t *result, void (*start) (), void (*stop) ())
{
  //-----------------------------------------------------------------
  // Check pv.add.h.div2 equivalent to pv.avg
  //-----------------------------------------------------------------
  int i;

  for (i = 0; i < N; i++)
    {
      asm volatile("pv.add.h.div2 %[c], %[a], %[b]\n"
                   : [c] "=r"(tmp_result[i])
                   : [a] "r"(cplx_opAdiv2[i]), [b] "r"(cplx_opBdiv2[i]));

      check_uint32 (result, "pv.add.h.div2", tmp_result[i],
                    cplxadd_opCdiv2[i]);
    }

  //-----------------------------------------------------------------
  // Check pv.add.h.div4
  //-----------------------------------------------------------------

  for (i = 0; i < N; i++)
    {
      asm volatile("pv.add.h.div4 %[c], %[a], %[b]\n"
                   : [c] "=r"(tmp_result[i])
                   : [a] "r"(cplx_opAdiv4[i]), [b] "r"(cplx_opBdiv4[i]));

      check_uint32 (result, "pv.add.h.div4", tmp_result[i],
                    cplxadd_opCdiv4[i]);
    }

  //-----------------------------------------------------------------
  // Check pv.add.h.div8
  //-----------------------------------------------------------------

  for (i = 0; i < N; i++)
    {
      asm volatile("pv.add.h.div8 %[c], %[a], %[b]\n"
                   : [c] "=r"(tmp_result[i])
                   : [a] "r"(cplx_opAdiv8[i]), [b] "r"(cplx_opBdiv8[i]));

      check_uint32 (result, "pv.add.h.div8", tmp_result[i],
                    cplxadd_opCdiv8[i]);
    }

  //-----------------------------------------------------------------
  // Check pv.sub.h.div2
  //-----------------------------------------------------------------

  for (i = 0; i < N; i++)
    {
      asm volatile("pv.sub.h.div2 %[c], %[a], %[b]\n"
                   : [c] "=r"(tmp_result[i])
                   : [a] "r"(cplx_opAdiv2[i]), [b] "r"(cplx_opBdiv2[i]));

      check_uint32 (result, "pv.sub.h.div2", tmp_result[i],
                    cplxsub_opCdiv2[i]);
    }

  //-----------------------------------------------------------------
  // Check pv.sub.h.div4
  //-----------------------------------------------------------------

  for (i = 0; i < N; i++)
    {
      asm volatile("pv.sub.h.div4 %[c], %[a], %[b]\n"
                   : [c] "=r"(tmp_result[i])
                   : [a] "r"(cplx_opAdiv4[i]), [b] "r"(cplx_opBdiv4[i]));

      check_uint32 (result, "pv.sub.h.div4", tmp_result[i],
                    cplxsub_opCdiv4[i]);
    }

  //-----------------------------------------------------------------
  // Check pv.sub.h.div8
  //-----------------------------------------------------------------

  for (i = 0; i < N; i++)
    {
      asm volatile("pv.sub.h.div8 %[c], %[a], %[b]\n"
                   : [c] "=r"(tmp_result[i])
                   : [a] "r"(cplx_opAdiv8[i]), [b] "r"(cplx_opBdiv8[i]));

      check_uint32 (result, "pv.sub.h.div8", tmp_result[i],
                    cplxsub_opCdiv8[i]);
    }
}
