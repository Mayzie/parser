#!/bin/bash

## COMP3109 Test Suite files
## Written by Daniel Collis -- 430133523

if [[ $# -eq 0 ]] ; then
  echo "COMP3109 Run Tests Help"
  echo "$0 <executable> [<option> [<test_dir>]]"
  echo "    <executable>     - Location of 'Imp' (e.g. dist/build/imp/imp)"
  echo "    <option>         - Examination option {lex, parse, convert, execute. Default: parse}"
  echo "    <test_dir>       - Directory that contains 'fails' and 'works' directries (Default: test/)"

  exit 1
else
  if [ ! -e $1 ]; then
    echo $"Executable file '$1' does not exist"
    exit 1
  else
    EXEC_PATH=$1
    if [ ! -z $2 ]; then
      case "$2" in
        lex)
          OPTION="lex"
          ;;
        parse)
          OPTION="check"
          ;;
        convert)
          OPTION="convert"
          ;;
        execute)
          OPTION="execute"
          ;;
        *)
          echo $"Usage: $0 $1 [{lex|parse|convert|execute}] [<test_dir>]"
          exit 1
      esac
  
      if [ ! -z $3 ]; then
        if [ -d $3 ]; then
          DIRTESTS=$3
        else
          echo $"Test directory '$3' does not exist"
          exit 2
        fi
      else
        if [ ! -d "test" ]; then
          echo $"Test directory 'test'"
          exit 2
        else
          DIRTESTS="test"
        fi
      fi
    else
      DIRTESTS="test"
      OPTION="check"
    fi

    _TESTFAIL="/fails"
    _TESTWORK="/works"

    echo $DIRTESTS

    DIRFAILS="${DIRTESTS}${_TESTFAIL}"
    DIRWORKS="${DIRTESTS}${_TESTWORK}"

    if [ ! -d $DIRFAILS ]; then
      echo $"Directory '$DIRFAILS' does not exist"
      exit 3
    fi
    
    if [ ! -d $DIRWORKS ]; then
      echo $"Directory '$DIRWORKS' does not exist"
      exit 3
    fi

    case "$OPTION" in
      lex)
        echo $"$0 Lexing all files"
        echo $"==== Working:"
        for i in $DIRWORKS/*.imp; do
          echo $i
          $EXEC_PATH "-"$OPTION $i
          echo ""
        done

        echo $"==== Fails:"
        for i in $DIRFAILS/*.imp; do
          echo $i
          $EXEC_PATH "-"$OPTION $i
        done
        ;;
      check)
        echo $"$0 Checking all files"
        echo $"==== Working:"
        for i in $DIRWORKS/*.imp; do
          echo $i
          $EXEC_PATH "-"$OPTION $i
          echo ""
        done

        echo $"==== Fails:"
        for i in $DIRFAILS/*.imp; do
          echo $i
          $EXEC_PATH "-"$OPTION $i
          echo ""
        done
        ;;
      convert)
        echo $"$0 Converting all files to IR"
        echo $"==== Working:"
        for i in $DIRWORKS/*.imp; do
          echo $i
          $EXEC_PATH "-"$OPTION $i
          echo ""
        done

        echo $"==== Fails:"
        for i in $DIRFAILS/*.imp; do
          echo $i
          $EXEC_PATH "-"$OPTION $i
          echo ""
        done
        ;;
      execute)
        echo $"$0 Executing all files"
        echo $"==== Working:"
        for i in $DIRWORKS/*.imp; do
          echo $i
          $EXEC_PATH "-"$OPTION $i
          echo ""
        done

        echo $"==== Fails:"
        for i in $DIRFAILS/*.imp; do
          echo $i
          $EXEC_PATH "-"$OPTION $i
          echo ""
        done
    esac
  fi
fi
