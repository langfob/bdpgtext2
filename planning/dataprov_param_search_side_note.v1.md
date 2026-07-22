# Side note: could dataprov gain a "search by stored parameters" function?

**Scope:** this is a musing only, prompted by a side question. It is NOT a task for the current
work, and nothing here should be acted on without a separate, explicit decision.

## The gap

`prov_list()` filters on **tags**, **parent UUID**, and **date** only. The stored
`parameters` list (now the full params set) is recorded verbatim in each sidecar but is **not**
queryable. So switching from a curated subset to the full params improves what you can *read
back* from a located file, but does not let you *find* files by param values. That capability
gap is unchanged by the switch.

## Why a param-search function could be useful

Today, if you want "all records where `some_param == X`", you cannot ask the registry directly.
You would have to list records and then open each sidecar to inspect its `parameters` — doable,
but manual and O(n). A first-class filter would make questions like "which outputs were
generated with setting Y?" a one-liner, which is squarely in the spirit of a provenance tool.

## Design considerations (if it were ever pursued)

- **Interface.** Mirror the existing `tags` idiom, e.g. a `parameters = list(key = value)`
  argument on `prov_list()` (AND-combined, like tags), rather than a separate function — keeps
  the mental model consistent.  
- **Matching semantics.** Decide exact-match vs. predicate/range matching, and how to handle
  missing keys (a record that never recorded that param) and type coercion (YAML strings vs.
  numbers/booleans).  
- **Nested/complex values.** `parameters` can hold lists or nested structures; searching those
  needs a rule (top-level scalar keys only, or a path/JSONPath-style selector).  
- **Where the data lives.** The registry is described as a rebuildable index over the sidecars.
  Efficient param search likely means indexing selected parameter keys into the registry DB at
  write time (or a `prov_registry_rebuild()` that extracts them), rather than scanning JSON on
  every query. That is a schema/indexing decision, not just an API addition.  
- **Tags as the escape hatch.** In the meantime, the existing mechanism already covers the most
  important case: if you know in advance you will want to *search* on a given knob, record it as
  a **tag** as well as in `parameters`. That is exactly what the current plan does for
  `gurobi_problem_filter` and `exclude_imperfect_wraps`. Tags are the "queryable projection" of
  the parameters; the full `parameters` list is the complete record.  

## Bottom line

The full-params change is the right call for completeness of the record, and it does not
regress anything. A param-aware search would be a reasonable future enhancement to dataprov,
but it is a non-trivial indexing/API design task and should be scoped on its own. For present
needs, tagging the specific knobs you expect to query on is the simple, sufficient workaround.
