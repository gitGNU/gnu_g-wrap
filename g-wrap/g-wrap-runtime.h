/**********************************************************************
Copyright (C) 2003 Andreas Rottmann
 
This program is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 2.1, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.
 
You should have received a copy of the GNU Lesser General Public
License along with this software; see the file COPYING.  If not, write
to the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139,
USA.
**********************************************************************/

#ifndef __G_WRAP_RUNTIME_H__
#define __G_WRAP_RUNTIME_H__

#include <libguile.h>
#include <ffi.h>

#ifdef __cplusplus
extern "C" {
#endif

/* FFI has messed-up type #defines, see
 * http://gcc.gnu.org/bugzilla/show_bug.cgi?id=12782 */
#if !defined(ffi_type_ulong_long)

#undef ffi_type_ulong
#undef ffi_type_slong

#if SIZEOF_LONG == 4

#define ffi_type_ulong ffi_type_uint32
#define ffi_type_slong ffi_type_sint32

#elif SIZEOF_LONG == 8

#define ffi_type_ulong ffi_type_uint64
#define ffi_type_slong ffi_type_sint64

#endif

/* any machines with 128bit long longs yet? */
#define ffi_type_ulong_long ffi_type_uint64
#define ffi_type_slong_long ffi_type_sint64

#endif /* !defined(ffi_type_ulong_long) */

typedef struct _GWEnumPair GWEnumPair;
typedef struct _GWFunctionInfo GWFunctionInfo;
typedef enum _GWErrorStatus GWErrorStatus;
typedef struct _GWTypeInfo GWTypeInfo;
typedef struct _GWWrapSet GWWrapSet;
typedef struct _GWError GWError;

enum _GWErrorStatus
{
  GW_ERR_NONE,
  GW_ERR_MISC,
  GW_ERR_MEMORY,
  GW_ERR_RANGE,
  GW_ERR_TYPE,
  GW_ERR_ARGC,
  GW_ERR_ARG_RANGE,
  GW_ERR_ARG_TYPE
};

struct _GWError
{
    GWErrorStatus status;
    const char *message;
    SCM data;
};

struct _GWEnumPair
{
    int val;
    const char *sym;
};

typedef unsigned long GWTypeSpec;
typedef void (*GWFromScmFunc)(void *instance,
                              const GWTypeSpec *ts,
                              SCM val,
                              GWError *error);
typedef SCM (*GWToScmFunc)(void *instance,
                           const GWTypeSpec *ts,
                           GWError *error);
typedef void (*GWDestructorFunc)(void *instance,
                                 const GWTypeSpec *ts,
                                 int force,
                                 GWError *error);

enum
{
  GW_TYPESPEC_CALLER_OWNED = 0x01,
  GW_TYPESPEC_CALLEE_OWNED = 0x02
};

#define GW_TYPESPEC_USER_SHIFT 8

struct _GWTypeInfo
{
    const char *name;
    const char *class_name;

    ffi_type *type; /* if non-NULL, this type may passed dynamically */
    
    GWFromScmFunc from_scm;
    GWToScmFunc to_scm;
    GWDestructorFunc destructor;
};

struct _GWFunctionInfo
{
    int dynamic; /* set if all arg types are dynamic and we should
                  * create the functions dynamically. */
    
    void *proc;            /* Wrapper function (if !dynamic) or real C
                            * function */
    int n_required_args;
    int n_optional_args;
    int use_extra_args;

    GWTypeInfo *ret_type;
    GWTypeSpec ret_typespec;
    
    int nargs;
    GWTypeInfo **arg_types; /* array */
    GWTypeSpec *arg_typespecs;
    
    const char *proc_name;
    const char *generic_name;

    ffi_cif *cif;
    unsigned int data_area_size; /* Size needed for the dynamic-call info */
};

struct _GWWrapSet
{
    const char *name;
    
    int ndependencies;
    GWWrapSet **dependencies;
    
    int ntypes;
    GWTypeInfo *types;
    
    int nfunctions;
    GWFunctionInfo *functions;

    /* private */
    int ntypes_allocated;
    int types_sorted;
    int nfuncs_allocated;
};

void gw_runtime_init (void);
void gw_runtime_get_version_info(int *major, int *revision, int *age);

SCM gw_enum_val2sym(GWEnumPair enum_pairs[], SCM scm_val, SCM scm_show_all_p);
SCM gw_enum_val2int(GWEnumPair enum_pairs[], SCM scm_val);

void gw_handle_wrapper_error(GWError *error,
                             const char *func_name,
                             unsigned int arg_pos) __attribute__ ((noreturn));

/* Standard g-wrap helper functions. Strings arguments must be static. */

GWWrapSet *gw_wrapset_new(const char *name, const char *dependency, ...);
void       gw_wrapset_add_type(GWWrapSet *ws,
                               const char *name,
                               const char *class_name,
                               ffi_type *type,
                               const char **subtypes,
                               GWFromScmFunc from_scm,
                               GWToScmFunc to_scm,
                               GWDestructorFunc destructor);
GWTypeInfo *gw_wrapset_lookup_type(GWWrapSet *ws, const char *name);

void 	   gw_wrapset_add_function(GWWrapSet *ws,
                                   int dynamic,
                                   void *proc,
                                   int n_req_args,
                                   int n_opt_args,
                                   int use_extra_args,
                                   const char *ret_type,
                                   GWTypeSpec ret_typespec,
                                   const char **arg_types,
                                   GWTypeSpec *arg_typespecs,
                                   const char *proc_name,
                                   const char *generic_name);

void 	   gw_wrapset_register(GWWrapSet *ws);

#ifdef __cplusplus
}
#endif

#endif
