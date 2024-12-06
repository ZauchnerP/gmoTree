**Changes in CITATION file,  NEWS file are not documented.**

# gmoTree 1.3.1 

* Updated figure in vignette.

# gmoTree 1.3.0

* Changes in ```codebook()``` 
  * Bug fixes
    * Resolved an issue with ```Constants``` vector presentations in the output files
    * Fixed incorrect handling of equal signs (```=```) in documentation 
    and choices.
  * New arguments
      * ```splitvarname``` to deal with long variable names. Caveat: In some cases, columns may still overlap. Control your output carefully.
      * ```sep_list``` to decide on list presentation or newline presentation in the output file
      * ```initial``` to include the initial values in the codebook
  * Enhancements
    * New preamble text 
    * ```output_format```
      * Expanded the input possibilities for greater flexibility
      * Changed default of ```output_format``` to ```pdf_document_simple```
    * Improved handling of quotations
      * Better presentations of quotations inside strings
      * Quotation marks are now presented as straight and not smart form
      * Removed non-escaped quotation marks from the output
    * Improved list handling
      * Values are now saved in list format and not as vectors
      * Types of variables now correspond better to the types of the original variables
      * Improved list presentation in output files for better readability
    * Enhanced error and warning messages
    * The function can now better deal better with empty arguments in the oTree code
* Linted older functions (more readable)
* Website changes

# gmoTree 1.2.1

* New function
  * ```codebook()```: Create a codebook for the oTree code
* Linted code (more readable)
* Minor changes in warning messages
* More tests and examples

# gmoTree 1.1.0

* Changes in functions
  * ```extime()```: Enhanced efficiency and extended functionality: Introduced the capability to perform calculations using only the 'seconds_on_page2' variable.
  * ```apptime()```: Enhanced efficiency and extended functionality: Introduced the capability to perform calculations using only the 'seconds_on_page2' variable.
* Linted code (more efficient & readable)
* Minor changes in tests.R
* Minor changes in website, new logo

# gmoTree 1.0.1

* Minor changes in examples and DESCRIPTION file. 
* Changes in functions: NONE

# gmoTree 1.0.0

File names of the examples are now portable.

* Changes in functions
  * ```import_otree()```: Minor changes. Can also import portable file names now.

# gmoTree 0.0.3

* Minor improvement of the documentation & website.
* Changes in functions: NONE

# gmoTree 0.0.2 

* Minor improvement of the documentation & website. (Sent to CRAN and omitted here.)
* Changes in functions: NONE

# gmoTree 0.0.1

* Beta version of gmoTree published (formerly known as "ioTree").
