(**
 *  This module translates the symbols into a text representation which fits
 * within the specified column width.
 * @author YAMATODANI Kiyoshi
 * @version $Id: PrettyPrinter.sml,v 1.4 2007/06/18 13:30:43 kiyoshiy Exp $
 *)
structure PrettyPrinter :> PRETTYPRINTER =
struct

  (***************************************************************************)

  structure FE = FormatExpression
  structure PP = PrinterParameter

  (***************************************************************************)

  type environmentEntry =
       {
         requiredColumns : int, 
         newline : bool ref,
         priority : FE.priority
       }

  type environment = environmentEntry list

  datatype symbol =
           Term of (int * string)
         | List of
           {
             symbols : symbol list,
             environment : environment
           }
         | Indicator of {space : bool, newline : bool ref}
         | DeferredIndicator of {space : bool, requiredColumns : int ref}
         | StartOfIndent of int
         | EndOfIndent

  (***************************************************************************)

  (** the exception raised when the EndOfIndent with no matched
   * FormatIndicator is found.
   *)
  exception UnMatchEndOfIndent

  (**
   * the exception raised when the specified indent offset plus the current
   * indent is less than 0.
   * @params offset
   * @param offset the indent offset which causes the indent underflow.
   *)
  exception IndentUnderFlow of int

  (***************************************************************************)

  (**
   * sorts a list
   * @params comparator list
   * @param comparator a function which compares two elements in the list.
   *       When applied to (left, right), it must return true if left < right.
   * @param list the list to be sorted.
   * @return the sorted list
   *)
  fun sort isBefore list =
    let
      fun insert (element, []) = [element]
        | insert (element, (head :: tail)) =
          if isBefore (element, head)
          then (element :: head :: tail)
          else head :: (insert (element, tail))
    in
      foldl insert [] list
    end

  (****************************************)

  (**
   *  translates the symbol into a text representation which fits within the
   * specified column width.
   * <p>
   *  This function tries to insert newline characters so that the text can
   * fit within the specified column width, but it may exceed the specified
   * column width if the column width is too small.
   * </p>
   * @params parameter symbol
   * @param parameter parameters which control the printer
   * @param symbol the symbol to be translated.
   * @return the text representation of the symbol.
   *)
  fun format parameters =
    let
      val (initialCols, spaceString, newlineString, cutOverTail) =
          List.foldl
              (fn (param, (cols, space, newline, cuttail)) =>
                  case param
                   of PP.Newline s => (cols, space, s, cuttail)
                    | PP.Space s => (cols, s, newline, cuttail)
                    | PP.Columns n => (n, space, newline, cuttail)
                    | PP.CutOverTail b => (cols, space, newline, b)
                    | _ => (cols, space, newline, cuttail))
              (
                PP.defaultColumns,
                PP.defaultSpace,
                PP.defaultNewline,
                PP.defaultCutOverTail
              )
              parameters

      datatype line =
               Unclosed of string
             | Closed of string
             | Truncated of string

      type context =
           {
             (** the number of remaining columns. *)
             cols : int,
             (** line list in reversed order. *)
             lines : line list,
             indentString : string,
             indentWidth : int,
             indentStack : int list
           }

      (**
       * extends or shrink a indent
       * @params indentString currentIndentWidth diff
       * @param indentString a string of seqeunce of whitespaces.
       * @param currentIndentWidth the current indent width
       * @param diff the number of charactes by which extend or shrink
       *          the indent.
       * @return the indent text which is extended by diff characters
       *    if diff > 0, or shrinked by diff characters if diff < 0.
       * @exception IndentUnderFlow when addition of the size of indent and
       *         the diff is less than 0.
       *)
      fun extendIndent (indentString : string) currentIndentWidth diff =
          if 0 = diff
          then indentString
          else
            let val newIndentSize = currentIndentWidth + diff
            in
              if newIndentSize < 0 then
                raise IndentUnderFlow diff
              else
                String.concat
                (List.tabulate (newIndentSize, fn _ => spaceString))
            end

      (** creates a string of specified number of whitespaces.
       * @params size
       * @param size the number of whitespace characters
       * @return a string consisted of the <code>size</code> whitespaces.
       *)
      fun makeIndent size =
          String.concat (List.tabulate (size, fn _ => spaceString))

      fun checkOverTail ifNotOver string =
          let
            val sizeOfString = size string
          in
            if cutOverTail andalso initialCols < sizeOfString
            then
              Truncated
                  (if initialCols < 2 orelse sizeOfString < 2
                   then ".."
                   else String.substring (string, 0, initialCols - 2) ^ "..")
            else ifNotOver string
          end
      fun appendToLine text (Unclosed line :: lines') =
          let val str = line ^ text
          in (checkOverTail Unclosed str) :: lines'
          end
        | appendToLine text (lines as (Truncated _ :: _)) = lines
        | appendToLine text lines = (Unclosed text) :: lines
      fun closeLine (Unclosed line :: lines) =
          (checkOverTail Closed line) :: lines
        | closeLine (Truncated line :: lines) = (Closed line) :: lines
        | closeLine lines = (Closed "") :: lines
      fun linesToString ({lines, ...} : context) =
          let
            val strings =
                map
                    (fn (Closed s) => s ^ newlineString
                      | (Truncated s) => s ^ newlineString
                      | (Unclosed s) => s)
                    (List.rev lines) (* lines are in reversed order. *)
          in
            String.concat strings
          end

      fun visit canMultiline (context : context) (Term (columns, text)) =
          {
            cols = (#cols context) - columns,
            lines = appendToLine text (#lines context),
            indentString = #indentString context,
            indentWidth = #indentWidth context,
            indentStack = #indentStack context
          }

        | visit
          canMultiline
          context
          (List
           {
             symbols,
             environment = unsortedEnvironment
           }) =
          let

            (*
              sort environment entries in descending order of the priority
            *)
            val environment =
                sort
                (fn (left, right) =>
                    FE.isHigherThan (#priority left, #priority right))
                unsortedEnvironment

            (*
                Decide whether to begin a new line at preferred indicators.
                Decisions are made for the higher priority before for the
               lower priority. ( The 'environment' has been sorted in
               descending order by the above code. )
                The result 'allPreferredMultiLined' is true if newlines begin
               at the all preferred indicators.
             *)
            val allPreferredMultiLined =
                foldl
                (fn ({requiredColumns, newline, priority, ...}, multilined) =>
                    (* the 'multilined' is true if newlines begin at the all
                      preferred indicators in the enclosing guards and the
                      higher preferred indicators in this guard. *)
                    (
                      newline :=
                      (multilined andalso (#cols context) < requiredColumns);
                      ! newline
                    ))
                canMultiline environment

            val newContext =
                foldl
                    (fn (symbol, context) =>
                        visit allPreferredMultiLined context symbol)
                    {
                      cols = #cols context,
                      lines = #lines context,
                      indentString =
                          makeIndent (initialCols - (#cols context)),
                      indentWidth = (initialCols - (#cols context)),
                      indentStack = []
                    }
                    symbols

          in
            {
              cols = #cols newContext, (* from newContext *)
              lines = #lines newContext,
              indentString = #indentString context,
              indentWidth = #indentWidth context,
              indentStack = #indentStack context
            }
          end

        | visit canMultiline context (StartOfIndent indent) =
          let
            val newIndentStack = indent :: (#indentStack context)
            val newIndentWidth = #indentWidth context + indent
            val newIndentString =
                extendIndent
                (#indentString context) (#indentWidth context) indent
          in
            {
              cols = #cols context,
              lines = #lines context,
              indentString = newIndentString,
              indentWidth = newIndentWidth,
              indentStack = newIndentStack
            }
          end

        | visit canMultiline context (Indicator {space, newline}) =
          if ! newline
          then
            let
              val newCols = initialCols - (#indentWidth context)
            in
              {
                cols = newCols,
                lines =
                    appendToLine
                        (#indentString context) (closeLine (#lines context)),
                indentString = #indentString context,
                indentWidth = #indentWidth context,
                indentStack = #indentStack context
              }
            end
          else
            if space
            then visit canMultiline context (Term (1, spaceString))
            else context

        | visit
          canMultiline context (DeferredIndicator {space, requiredColumns}) =
          if canMultiline andalso (#cols context) < (!requiredColumns) 
          then
            let
              val newCols = initialCols - (#indentWidth context)
            in
              {
                cols = newCols,
                lines =
                    appendToLine
                        (#indentString context) (closeLine (#lines context)),
                indentString = #indentString context,
                indentWidth = #indentWidth context,
                indentStack = #indentStack context
              }
            end
          else
            if space
            then visit canMultiline context (Term (1, spaceString))
            else context

        | visit _ context EndOfIndent =
          case #indentStack context
           of [] => raise UnMatchEndOfIndent
            | (indent :: newIndentStack) =>
              let
                val newIndentString =
                    extendIndent
                    (#indentString context) (#indentWidth context) (~indent)
                val newIndentWidth = #indentWidth context - indent
              in
                {
                  cols = #cols context,
                  lines = #lines context,
                  indentString = newIndentString,
                  indentWidth = newIndentWidth,
                  indentStack = newIndentStack
                }
              end
    in
      linesToString
      o (visit
             true
             {
               cols = initialCols,
               lines = [],
               indentString = "",
               indentWidth = 0,
               indentStack = []
            })
    end

end