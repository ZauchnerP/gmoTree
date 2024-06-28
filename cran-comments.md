# Version 1.1.0

[x] oTree is not misspelled.

```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```
[x] Can be ignored according to https://github.com/r-hub/rhub/issues/503.




# Version 1.0.1 (Resubmission)

[x] make_ids() function accidentally left the # in the code although putting a try() around the functions.
Now the try() function is replaced by \dontrun{} and the # are removed.

[x] oTree in the title and description of the DESCRIPTION file is now in single quotations.

[] There should be examples in comments in oTree.Rd; however, there are no examples in this file.


# Version 1.0.0 (Resubmission)

[x] Portable file names warning is now circumvented.

Now, there are no ERRORs or WARNINGs. But there are some notes.


## Notes

```
Notes:

* checking CRAN incoming feasibility ... [6s/16s] NOTE
Maintainer: anonymized

New submission

Possibly misspelled words in DESCRIPTION:
  oTree (2:23, 8:50)

```
[x] oTree is not misspelled.

[x] The feasibility-note can be ignored according to https://mailman.stat.ethz.ch/pipermail/r-devel/2014-March/068497.html.

```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```
[x] Can be ignored according to https://github.com/r-hub/rhub/issues/503.

```
* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''
```
[x] Can be ignored according to https://github.com/r-hub/rhub/issues/560.

```
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
```
[x] A bug in rhub? https://github.com/r-hub/rhub/issues/548

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
