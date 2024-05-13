/* SPDX-License-Identifier: MIT */
#include <sdkconfig.h>

#include <stdbool.h>

#include <context.h>
#include <interop.h>
#include <term.h>

bool get_integer_parameter(Context *ctx, term argv[], AtomString key, int *value, bool required);
bool get_bool_parameter(Context *ctx, term argv[], AtomString key, bool *value);
bool get_enum_parameter(Context *ctx, term argv[], AtomString key, const AtomStringIntPair *table, int *value);
