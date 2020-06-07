lice.el - License And Header Template
=====================================

Overview
--------

`lice.el` provides following features:

- License template management.
- File header insertion.

Usage
-----

Usage is very easy, put `lice.el` in your Emacs system, and open a new
file, and run:

    M-x lice

Then, `lice.el` tell to use which license (default is gpl-3.0). You
can select license on minibuffer completion.

When you select license, and enter the `RET`, license and copyright is
putted into a text.

Use dir-locals
--------------

You can use `.dir-locals.el` for your project. Put `.dir-locals.el` in
your project root directory and write follows:

    ((nil
      (lice:default-license . "mit")))

That means use `mit' for default license template.

You can also set the copyright holder in `.dir-locals.el`. For example the
folowing will put "John Doe" as the copyright holder:

    ((nil
      (lice:copyright-holder . "John Doe")))

The default is to use `(user-full-name)`.

Custom license headers
----------------------

You could add custom license headers to `lice.el`. Creates a directory like that :

	$HOME/.emacs.d/lice

and put yours license headers into this directory.
