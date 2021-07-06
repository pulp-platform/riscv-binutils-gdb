#ifndef __RISCV_TESTS_H__
#define __RISCV_TESTS_H__
#include <stdint.h>
#include <stdio.h>

#define SR_OVERFLOW 0x800
#define SR_CARRY    0x400

unsigned int g_num_checks = 0;
unsigned int g_errors = 0;

#define test_init() test_init_int (__FILE__)

/* since the global variables are not initialized we have to do that in a
   special function here */
void
test_init_int (const char *filename)
{
  g_num_checks = 0;
  g_errors = 0;
}

void
test_check (const char *str, uint32_t act, uint32_t exp)
{
  g_num_checks++;

  if (act != exp)
    {
      g_errors++;
      printf ("%s: Is %X, expected %X\n", str, act, exp);
    }
}

void
test_check_flags (const char *str, uint32_t spr, uint32_t exp, uint32_t flags)
{
  g_num_checks++;

  if ((spr & flags) != (exp & flags))
    {
      g_errors++;
      printf ("%s: SPR is %X, is %X, expected %X\n", str, spr, spr & flags,
              exp & flags);
    }
}

void
test_check_overflow (const char *str, uint32_t spr, uint32_t exp)
{
  g_num_checks++;

  if ((spr & SR_OVERFLOW) != (exp & SR_OVERFLOW))
    {
      g_errors++;
      if (exp & SR_OVERFLOW)
        printf ("%s: Overflow should have been set\n", str);
      else
        printf ("%s: Overflow should not have been set\n", str);
    }
}

void
test_check_carry (const char *str, uint32_t spr, uint32_t exp)
{
  g_num_checks++;

  if ((spr & SR_CARRY) != (exp & SR_CARRY))
    {
      g_errors++;
      if (exp & SR_CARRY)
        printf ("%s: Carry should have been set\n", str);
      else
        printf ("%s: Carry should not have been set\n", str);
    }
}

unsigned int
test_get_num_errors (void)
{
  return g_errors;
}

void
test_report (void)
{
  printf ("%d of %d checks failed\n", g_errors, g_num_checks);
}

#endif
