
-------------------------------------------------------------------------------
COMP3109 Assignment 2. Compiler Front End

Name

  [Your name here]


What tasks did you complete?

  [Describe which tasks you completed]


Did you get stuck, or couldn't work something out?

  [I just can't stop updating]


-------------------------------------------------------------------------------
1. Install the Haskell Platform.
   Current version is 7.10.2
   https://www.haskell.org/platform/

   If this works you should now have the 'ghc', 'ghci' and 'cabal' commands.

   Cabal is the Haskell package manager and build tool.

   You can check the current install ghc version:

   $ ghc --version


2. Update the cabal package directory.
   This gets the list of available libraries from the online Hackage database.
   https://hackage.haskell.org/

   $ cabal update


3. Install some extra libraries.
   The template code uses these.
   The following command downloads them from Hackage.

   $ cabal install pretty-show
   $ cabal install Diff


4. Build the template code.

   $ cabal configure
   $ cabal build


5. Run some tests.

   The compiled executable is written to dist/build/imp/imp

   $ dist/build/imp/imp -lex test/works/factorial.imp


6. If you like, install your compiler.

   $ cabal install

   The executable will be installed in your system, 
   though the exact place depends on your OS.

   On OSX:   /Users/USER/Library/Haskell/bin
   On Linux: ~/.cabal/bin

   Add the correct directory to your $PATH.
