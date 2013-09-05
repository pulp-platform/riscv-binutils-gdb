/* Common definitions for remote server for GDB.
   Copyright (C) 1993-2013 Free Software Foundation, Inc.

   This file is part of GDB.

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

#ifndef SERVER_H
#define SERVER_H

#include "config.h"
#include "build-gnulib-gdbserver/config.h"

#ifdef __MINGW32CE__
#include "wincecompat.h"
#endif

#include "libiberty.h"
#include "ansidecl.h"
#include "version.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#include <setjmp.h>

/* For gnulib's PATH_MAX.  */
#include "pathmax.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
/* On some systems such as MinGW, alloca is declared in malloc.h
   (there is no alloca.h).  */
#if HAVE_MALLOC_H
#include <malloc.h>
#endif

#if !HAVE_DECL_STRERROR
#ifndef strerror
extern char *strerror (int);	/* X3.159-1989  4.11.6.2 */
#endif
#endif

#if !HAVE_DECL_PERROR
#ifndef perror
extern void perror (const char *);
#endif
#endif

#if !HAVE_DECL_VASPRINTF
extern int vasprintf(char **strp, const char *fmt, va_list ap);
#endif
#if !HAVE_DECL_VSNPRINTF
int vsnprintf(char *str, size_t size, const char *format, va_list ap);
#endif

#ifdef IN_PROCESS_AGENT
#  define PROG "ipa"
#else
#  define PROG "gdbserver"
#endif

/* A type used for binary buffers.  */
typedef unsigned char gdb_byte;

#include "ptid.h"
#include "buffer.h"
#include "xml-utils.h"
#include "gdb_locale.h"

/* FIXME: This should probably be autoconf'd for.  It's an integer type at
   least the size of a (void *).  */
typedef long long CORE_ADDR;

typedef long long LONGEST;
typedef unsigned long long ULONGEST;

/* Generic information for tracking a list of ``inferiors'' - threads,
   processes, etc.  */
struct inferior_list
{
  struct inferior_list_entry *head;
  struct inferior_list_entry *tail;
};
struct inferior_list_entry
{
  ptid_t id;
  struct inferior_list_entry *next;
};

struct thread_info;
struct process_info;
struct regcache;
struct target_desc;

#include "regcache.h"
#include "gdb/signals.h"
#include "gdb_signals.h"
#include "target.h"
#include "mem-break.h"
#include "gdbthread.h"

struct dll_info
{
  struct inferior_list_entry entry;
  char *name;
  CORE_ADDR base_addr;
};

struct sym_cache;
struct breakpoint;
struct raw_breakpoint;
struct fast_tracepoint_jump;
struct process_info_private;

struct process_info
{
  struct inferior_list_entry head;

  /* Nonzero if this child process was attached rather than
     spawned.  */
  int attached;

  /* True if GDB asked us to detach from this process, but we remained
     attached anyway.  */
  int gdb_detached;

  /* The symbol cache.  */
  struct sym_cache *symbol_cache;

  /* The list of memory breakpoints.  */
  struct breakpoint *breakpoints;

  /* The list of raw memory breakpoints.  */
  struct raw_breakpoint *raw_breakpoints;

  /* The list of installed fast tracepoints.  */
  struct fast_tracepoint_jump *fast_tracepoint_jumps;

  const struct target_desc *tdesc;

  /* Private target data.  */
  struct process_info_private *private;
};

/* Return a pointer to the process that corresponds to the current
   thread (current_inferior).  It is an error to call this if there is
   no current thread selected.  */

struct process_info *current_process (void);
struct process_info *get_thread_process (struct thread_info *);

/* Target-specific functions */

void initialize_low ();

/* From inferiors.c.  */

extern struct inferior_list all_processes;
extern struct inferior_list all_dlls;
extern int dlls_changed;
extern void clear_dlls (void);

void add_inferior_to_list (struct inferior_list *list,
			   struct inferior_list_entry *new_inferior);
void for_each_inferior (struct inferior_list *list,
			void (*action) (struct inferior_list_entry *));

extern struct thread_info *current_inferior;
void remove_inferior (struct inferior_list *list,
		      struct inferior_list_entry *entry);

struct process_info *add_process (int pid, int attached);
void remove_process (struct process_info *process);
struct process_info *find_process_pid (int pid);
int have_started_inferiors_p (void);
int have_attached_inferiors_p (void);

ptid_t thread_id_to_gdb_id (ptid_t);
ptid_t thread_to_gdb_id (struct thread_info *);
ptid_t gdb_id_to_thread_id (ptid_t);

void clear_inferiors (void);
struct inferior_list_entry *find_inferior
     (struct inferior_list *,
      int (*func) (struct inferior_list_entry *,
		   void *),
      void *arg);
struct inferior_list_entry *find_inferior_id (struct inferior_list *list,
					      ptid_t id);
void *inferior_target_data (struct thread_info *);
void set_inferior_target_data (struct thread_info *, void *);
void *inferior_regcache_data (struct thread_info *);
void set_inferior_regcache_data (struct thread_info *, void *);

void loaded_dll (const char *name, CORE_ADDR base_addr);
void unloaded_dll (const char *name, CORE_ADDR base_addr);

/* Public variables in server.c */

extern ptid_t cont_thread;
extern ptid_t general_thread;

extern int server_waiting;
extern int debug_threads;
extern int debug_hw_points;
extern int pass_signals[];
extern int program_signals[];
extern int program_signals_p;

extern jmp_buf toplevel;

extern int disable_packet_vCont;
extern int disable_packet_Tthread;
extern int disable_packet_qC;
extern int disable_packet_qfThreadInfo;

extern int run_once;
extern int multi_process;
extern int non_stop;

extern int disable_randomization;

#if USE_WIN32API
#include <winsock2.h>
typedef SOCKET gdb_fildes_t;
#else
typedef int gdb_fildes_t;
#endif

/* Functions from event-loop.c.  */
typedef void *gdb_client_data;
typedef int (handler_func) (int, gdb_client_data);
typedef int (callback_handler_func) (gdb_client_data);

extern void delete_file_handler (gdb_fildes_t fd);
extern void add_file_handler (gdb_fildes_t fd, handler_func *proc,
			      gdb_client_data client_data);
extern int append_callback_event (callback_handler_func *proc,
				   gdb_client_data client_data);
extern void delete_callback_event (int id);

extern void start_event_loop (void);
extern void initialize_event_loop (void);

/* Functions from server.c.  */
extern int handle_serial_event (int err, gdb_client_data client_data);
extern int handle_target_event (int err, gdb_client_data client_data);

/* Functions from hostio.c.  */
extern int handle_vFile (char *, int, int *);

/* Functions from hostio-errno.c.  */
extern void hostio_last_error_from_errno (char *own_buf);

#include "remote-utils.h"

#include "common-utils.h"
#include "utils.h"

#include "gdb_assert.h"

/* Maximum number of bytes to read/write at once.  The value here
   is chosen to fill up a packet (the headers account for the 32).  */
#define MAXBUFBYTES(N) (((N)-32)/2)

/* Buffer sizes for transferring memory, registers, etc.   Set to a constant
   value to accomodate multiple register formats.  This value must be at least
   as large as the largest register set supported by gdbserver.  */
#define PBUFSIZ 16384

/* Bytecode compilation function vector.  */

struct emit_ops
{
  void (*emit_prologue) (void);
  void (*emit_epilogue) (void);
  void (*emit_add) (void);
  void (*emit_sub) (void);
  void (*emit_mul) (void);
  void (*emit_lsh) (void);
  void (*emit_rsh_signed) (void);
  void (*emit_rsh_unsigned) (void);
  void (*emit_ext) (int arg);
  void (*emit_log_not) (void);
  void (*emit_bit_and) (void);
  void (*emit_bit_or) (void);
  void (*emit_bit_xor) (void);
  void (*emit_bit_not) (void);
  void (*emit_equal) (void);
  void (*emit_less_signed) (void);
  void (*emit_less_unsigned) (void);
  void (*emit_ref) (int size);
  void (*emit_if_goto) (int *offset_p, int *size_p);
  void (*emit_goto) (int *offset_p, int *size_p);
  void (*write_goto_address) (CORE_ADDR from, CORE_ADDR to, int size);
  void (*emit_const) (LONGEST num);
  void (*emit_call) (CORE_ADDR fn);
  void (*emit_reg) (int reg);
  void (*emit_pop) (void);
  void (*emit_stack_flush) (void);
  void (*emit_zero_ext) (int arg);
  void (*emit_swap) (void);
  void (*emit_stack_adjust) (int n);

  /* Emit code for a generic function that takes one fixed integer
     argument and returns a 64-bit int (for instance, tsv getter).  */
  void (*emit_int_call_1) (CORE_ADDR fn, int arg1);

  /* Emit code for a generic function that takes one fixed integer
     argument and a 64-bit int from the top of the stack, and returns
     nothing (for instance, tsv setter).  */
  void (*emit_void_call_2) (CORE_ADDR fn, int arg1);

  /* Emit code specialized for common combinations of compare followed
     by a goto.  */
  void (*emit_eq_goto) (int *offset_p, int *size_p);
  void (*emit_ne_goto) (int *offset_p, int *size_p);
  void (*emit_lt_goto) (int *offset_p, int *size_p);
  void (*emit_le_goto) (int *offset_p, int *size_p);
  void (*emit_gt_goto) (int *offset_p, int *size_p);
  void (*emit_ge_goto) (int *offset_p, int *size_p);
};

extern CORE_ADDR current_insn_ptr;
extern int emit_error;

#endif /* SERVER_H */
