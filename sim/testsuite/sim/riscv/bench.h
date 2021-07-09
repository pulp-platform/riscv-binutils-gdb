/*
 * Copyright (C) 2021 ETH Zurich and University of Bologna
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifndef BENCH_H
#define BENCH_H

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

static inline void
start_timer ()
{
}

static inline void
stop_timer ()
{
}

static inline void
reset_timer ()
{
}

static inline int
get_time ()
{
  return 0;
}

typedef struct _testresult_t
{
  int time;
  int errors;
} testresult_t;

typedef struct _testcase_t
{
  char *name;
  void (*test) (testresult_t *result, void (*start) (), void (*stop) ());
} testcase_t;

int bench_cluster_exec (int cid, int (*entry) ());

/**
 * @brief Disables the printf ouput of the function print_summary() and
 * run_suite(). These functions are required to run the bench suite and write
 * the result status. Disabling the output of these functions makes it easier
 * for the testing a chip on the tester setup.
 */
void bench_disable_printf (void);

/**
 * @brief Prints the information and the result (success/fail, time elapsed)
 * of a benchmark.
 * @param[in] test the test.
 * @param[in] result the result of the test.
 */
void print_result (testcase_t *test, testresult_t *result);

void print_summary (unsigned int errors);

/**
 * @brief Runs a single benchmark.
 * @param[in] test the benchmark to run.
 * @param[out] result the result of the benchmark.
 */
void run_benchmark (testcase_t *test, testresult_t *result);

/**
 * @brief Runs a series of benchmarks and prints the results.
 * @param[in] tests an array with the benchmarks to run.
 */
int run_suite (testcase_t *tests);

/**
 * @brief Checks if actual == expected and if not, prints fail_msg and
 * increases the error counter in the testresult struct.
 * This function should be used when a fine grained error reporting is needed
 */
void check_uint32 (testresult_t *result, const char *fail_msg, uint32_t actual,
                   uint32_t expected);

/**
 * @brief Starts all performance counters
 */
static inline void
perf_start (void)
{
  /* cpu_perf_start(); */
}

/**
 * @brief Stops all performance counters
 */
static inline void
perf_stop (void)
{
  /* cpu_perf_stop(); */
}

/**
 * @brief Resets all performance counters to 0 without stopping them
 */
static inline void
perf_reset (void)
{
  /* perf_stop(); */
  /* cpu_perf_setall(0); */
  /* perf_start(); */
}

/**
 * @brief Enable a specific performance counter
 */
static inline void perf_enable_id (int eventid){};

/**
 * @brief Prints all performance counters
 */
void perf_print_all (void);

static inline void
plp_power_init ()
{
}

static inline void
plp_power_start ()
{
}

static inline void
plp_power_stop ()
{
}

static inline void
rt_bench_power_start ()
{
}

static inline void
rt_bench_power_stop ()
{
}

int bench_cluster_forward (int cid);

int bench_cluster_exec (int cid, int (*entry) ());

// Flag for disabling the printf output
uint32_t enable_printf = 1;

void
bench_disable_printf (void)
{
  enable_printf = 0;
}

void
bench_timer_start (void)
{
  start_timer ();
}

void
bench_timer_stop (void)
{
  stop_timer ();
}

void
bench_timer_reset (void)
{
  stop_timer ();
  reset_timer ();
}

void
print_result (testcase_t *test, testresult_t *result)
{
  printf ("== test: %s -> ", test->name);

  if (result->errors == 0)
    printf ("success, ");
  else
    printf ("fail, ");

  printf ("nr. of errors: %d", result->errors);

  if (result->time == 0)
    printf ("\n");
  else
    printf (", execution time: %d\n", result->time);
}

void
print_summary (unsigned int errors)
{

  if (enable_printf)
    {
      printf ("==== SUMMARY: ");

      if (errors == 0)
        {
          printf ("SUCCESS\n");
        }
      else
        {
          printf ("FAIL\n");
        }
    }
}

void
run_benchmark (testcase_t *test, testresult_t *result)
{
  result->errors = 0;

  bench_timer_reset ();

  test->test (result, bench_timer_start, bench_timer_stop);

  result->time = get_time ();
}

int
run_suite (testcase_t *tests)
{
  // figure out how many tests should be run
  int num = 0;
  while (tests[num].name != 0)
    num++;

  unsigned int errors = 0;
  int i;

  // perform the tests
  for (i = 0; i < num; i++)
    {
      testresult_t result;
      run_benchmark (&tests[i], &result);
      if (enable_printf)
        print_result (&tests[i], &result);

      errors += result.errors;
    }

  print_summary (errors);

  return errors;
}

void
check_uint32 (testresult_t *result, const char *fail_msg, uint32_t actual,
              uint32_t expected)
{
  if (actual != expected)
    {
      result->errors += 1;
      if (enable_printf)
        printf ("%s: Actual %X, expected %X\n", fail_msg, (unsigned int)actual,
                (unsigned int)expected);
    }
}

void
perf_print_all (void)
{
#ifdef __ibex__
  printf ("Perf CYCLES: %d\n", cpu_perf_get (CSR_PCER_CYCLES));
  printf ("Perf INSTR: %d\n", cpu_perf_get (CSR_PCER_INSTR));
  printf ("Perf CINSTR: %d\n", cpu_perf_get (CSR_PCER_RVC));
  printf ("Perf LD_STALL: %d\n", cpu_perf_get (CSR_PCER_LD_STALL));
  printf ("Perf JR_STALL: [Not Implemented]\n");
  printf ("Perf IMISS: %d\n", cpu_perf_get (CSR_PCER_IMISS));
  printf ("Perf LD: %d\n", cpu_perf_get (CSR_PCER_LD));
  printf ("Perf ST: %d\n", cpu_perf_get (CSR_PCER_ST));
  printf ("Perf JUMP: %d\n", cpu_perf_get (CSR_PCER_JUMP));
  printf ("Perf BRANCH: %d\n", cpu_perf_get (CSR_PCER_BRANCH));
  printf ("Perf BTAKEN: %d\n", cpu_perf_get (CSR_PCER_TAKEN_BRANCH));
  printf ("Perf LD EXT: %d\n", cpu_perf_get (CSR_PCER_LD_EXT));
  printf ("Perf ST EXT: %d\n", cpu_perf_get (CSR_PCER_ST_EXT));
  printf ("Perf LD EXT CYC: %d\n", cpu_perf_get (CSR_PCER_LD_EXT_CYC));
  printf ("Perf ST EXT CYC: %d\n", cpu_perf_get (CSR_PCER_ST_EXT_CYC));
  printf ("Perf TCDM CONT: %d\n", cpu_perf_get (CSR_PCER_TCDM_CONT));
  printf ("Perf CSR HAZARD: [Not Implemented]\n");
#elif defined(__riscv__)
  printf ("Perf CYCLES: %d\n", cpu_perf_get (0));
  printf ("Perf INSTR: %d\n", cpu_perf_get (1));
  printf ("Perf CINSTR: %d\n", cpu_perf_get (10));
  printf ("Perf LD_STALL: %d\n", cpu_perf_get (2));
  printf ("Perf JR_STALL: %d\n", cpu_perf_get (3));
  printf ("Perf IMISS: %d\n", cpu_perf_get (4));
  printf ("Perf LD: %d\n", cpu_perf_get (5));
  printf ("Perf ST: %d\n", cpu_perf_get (6));
  printf ("Perf JUMP: %d\n", cpu_perf_get (7));
  printf ("Perf BRANCH: %d\n", cpu_perf_get (8));
  printf ("Perf BTAKEN: %d\n", cpu_perf_get (9));
  printf ("Perf LD EXT: %d\n", cpu_perf_get (11));
  printf ("Perf ST EXT: %d\n", cpu_perf_get (12));
  printf ("Perf LD EXT CYC: %d\n", cpu_perf_get (13));
  printf ("Perf ST EXT CYC: %d\n", cpu_perf_get (14));
  printf ("Perf TCDM CONT: %d\n", cpu_perf_get (15));
  printf ("Perf CSR HAZARD: %d\n", cpu_perf_get (16));
#ifdef HARD_FLOAT
  if (!rt_is_fc ())
    {
      printf ("Perf APU_TY_CONF: %d\n", cpu_perf_get (17));
      printf ("Perf APU_CONT: %d\n", cpu_perf_get (18));
      printf ("Perf APU_DEP: %d\n", cpu_perf_get (19));
      printf ("Perf APU_WB: %d\n", cpu_perf_get (20));
    }
#endif
#else
#ifdef CSR_PCER_ALL_EVENTS_MASK
  printf ("Perf CYCLES: %d\n", cpu_perf_get (SPR_PCER_CYCLES));
  printf ("Perf INSTR: %d\n", cpu_perf_get (SPR_PCER_INSTR));
  printf ("Perf LD_STALL: %d\n", cpu_perf_get (SPR_PCER_LD_STALL));
  printf ("Perf JMP_STALL: %d\n", cpu_perf_get (SPR_PCER_JMP_STALL));
  printf ("Perf IMISS: %d\n", cpu_perf_get (SPR_PCER_IMISS));
  printf ("Perf WBRANCH: %d\n", cpu_perf_get (SPR_PCER_WBRANCH));
  printf ("Perf WBRANCH_CYC: %d\n", cpu_perf_get (SPR_PCER_WBRANCH_CYC));
  printf ("Perf LD: %d\n", cpu_perf_get (SPR_PCER_LD));
  printf ("Perf ST: %d\n", cpu_perf_get (SPR_PCER_ST));
  printf ("Perf JUMP: %d\n", cpu_perf_get (SPR_PCER_JUMP));
  printf ("Perf BRANCH: %d\n", cpu_perf_get (SPR_PCER_BRANCH));
  printf ("Perf DELAY NOP: %d\n", cpu_perf_get (SPR_PCER_DELAY_NOP));
  printf ("Perf LD EXT: %d\n", cpu_perf_get (SPR_PCER_LD_EXT));
  printf ("Perf ST EXT: %d\n", cpu_perf_get (SPR_PCER_ST_EXT));
  printf ("Perf LD EXT CYC: %d\n", cpu_perf_get (SPR_PCER_LD_EXT_CYC));
  printf ("Perf ST EXT CYC: %d\n", cpu_perf_get (SPR_PCER_ST_EXT_CYC));
  printf ("Perf TCDM CONT: %d\n", cpu_perf_get (SPR_PCER_TCDM_CONT));
#endif
#endif
}

#endif
