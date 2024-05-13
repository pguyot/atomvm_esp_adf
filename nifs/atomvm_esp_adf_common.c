/* SPDX-License-Identifier: MIT */
#include <sdkconfig.h>

#include <stdbool.h>

#include <context.h>
#include <defaultatoms.h>
#include <interop.h>
#include <nifs.h>
#include <term.h>

#include "atomvm_esp_adf_common.h"

bool get_integer_parameter(Context *ctx, term argv[], AtomString key, int *value, bool required)
{
    term parameter_term = interop_kv_get_value(argv[0], key, ctx->global);
    if (term_is_invalid_term(parameter_term)) {
        if (required) {
            RAISE_ERROR(BADARG_ATOM);
            return false;
        }
        return true;
    }
    if (UNLIKELY(!term_is_integer(parameter_term))) {
        RAISE_ERROR(BADARG_ATOM);
        return false;
    }
    *value = term_to_int(parameter_term);
    return true;
}

bool get_bool_parameter(Context *ctx, term argv[], AtomString key, bool *value)
{
    term parameter_term = interop_kv_get_value(argv[0], key, ctx->global);
    if (term_is_invalid_term(parameter_term)) {
        return true;
    }
    if (UNLIKELY(!term_is_atom(parameter_term) || (parameter_term != TRUE_ATOM && parameter_term != FALSE_ATOM))) {
        RAISE_ERROR(BADARG_ATOM);
        return false;
    }
    *value = parameter_term == TRUE_ATOM;
    return true;
}

bool get_enum_parameter(Context *ctx, term argv[], AtomString key, const AtomStringIntPair *table, int *value)
{
    term parameter_term = interop_kv_get_value(argv[0], key, ctx->global);
    if (term_is_invalid_term(parameter_term)) {
        return true;
    }
    if (UNLIKELY(!term_is_atom(parameter_term))) {
        RAISE_ERROR(BADARG_ATOM);
        return false;
    }
    int enum_value = interop_atom_term_select_int(table, parameter_term, ctx->global);
    if (enum_value < 0) {
        return false;
    }
    *value = enum_value;
    return true;
}
