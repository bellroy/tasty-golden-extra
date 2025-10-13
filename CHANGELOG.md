# Changes

## Version 0.1.0.1

_2025-10-13_

- `GoldenVsToYAML` was using `fail` instead of `error` for reporting
  mismatches, which caused behavioural inconsistencies (such as not
  creating new files when golden files do not exist). This has been
  fixed to use `error` for consistency with `GoldenVsToJSON`.

## Version 0.1.0.0

_2024-12-17_

- Introducing `GoldenVsShow`, `GoldenVsString`, `GoldenVsToJSON`, `GoldenVsToYAML`
