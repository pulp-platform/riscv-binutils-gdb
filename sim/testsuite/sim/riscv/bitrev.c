/*
# mach: riscv
# cc: -march=rv32imfcxcorev
# output:  == test: check_pv_bitman -> success, nr. of errors: 0\n
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

void check_pv_bitman (testresult_t *result, void (*start) (), void (*stop) ());

testcase_t testcases[]
    = { { .name = "check_pv_bitman", .test = check_pv_bitman }, { 0, 0 } };

int
main ()
{
  return run_suite (testcases);
}

//################################################################################
//# T E S T    check_pv_bitman
//################################################################################

void
check_pv_bitman (testresult_t *result, void (*start) (), void (*stop) ())
{
  unsigned int i;
  unsigned int res;

#include "bitrev-radix2.h"

  for (i = 0; i < NumberOfStimuli; i++)
    check_uint32 (result, "p.bitrev_radix2", tmp_result[i],
                  cplxbitrev_opC_radix2[i]);

#include "bitrev-radix4.h"

  for (i = 0; i < NumberOfStimuli; i++)
    check_uint32 (result, "p.bitrev_radix4", tmp_result[i],
                  cplxbitrev_opC_radix4[i]);

#include "bitrev-radix8.h"

  for (i = 0; i < NumberOfStimuli; i++)
    check_uint32 (result, "p.bitrev_radix8", tmp_result[i],
                  cplxbitrev_opC_radix8[i]);
}
