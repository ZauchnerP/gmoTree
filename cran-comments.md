# Version 0.0.1 

```
❯ checking for portable file names ... WARNING
  Found the following files with non-portable file names:
    inst/extdata/exp_data_2.1.0/Chat messages (accessed 2023-05-16).csv
    inst/extdata/exp_data_2.1.0/TimeSpent (accessed 2023-05-16).csv
    inst/extdata/exp_data_2.2.4/Chat messages (accessed 2023-05-13).csv
    inst/extdata/exp_data_2.2.4/TimeSpent (accessed 2023-05-13).csv
  These are not fully portable file names.
  See section 'Package structure' in the 'Writing R Extensions' manual.
```

This warning is not circumventable since my package is intended to import
data that may have this pattern (i.e., may include whitespaces).