(**
 * @author YAMATODANI Kiyoshi
 * @copyright (c) 2006, Tohoku University.
 * @version $Id: Script.sml,v 1.12 2007/04/01 02:30:42 kiyoshiy Exp $
 *)
structure Script : SCRIPT =
struct

  (***************************************************************************)

  structure MT = MatchTree

  (***************************************************************************)

  fun notImplemented () = raise Fail "not implemented"

  (***************************************************************************)
  (* Text *)

  type pattern = string

  fun chop string =
      case size string of
        0 => ""
      | 1 => ""
      | n =>
        let
          val chopLen = 
              if
                #"\r" = String.sub (string, n - 2)
                andalso #"\n" = String.sub (string, n - 1)
              then 2
              else 1
        in
          String.substring (string, 0, n - chopLen)
        end

  fun itoa int = Int.toString int

  fun atoi string =
      case Int.fromString string of
        SOME int => int
      | NONE => 0

  local

    fun makeStringReader string =
        let val len = size string
        in
          fn nextPos =>
             if len <= nextPos
             then NONE
             else SOME(String.sub (string, nextPos), nextPos + 1)
        end

    fun getMatches (mt as MT.Match(SOME{pos, len}, groups)) =
        SOME(pos, len) :: (List.concat(map getMatches groups))
      | getMatches _ = [NONE]

    fun getMatchedStrings string (mt as MT.Match(SOME{pos, len}, groups)) =
        map
            (fn NONE => ""
              | SOME(pos, len) => String.substring (string, pos, len))
            (getMatches mt)
      | getMatchedStrings _ _ = raise Fail "getMatchedStrings: unexpected NONE"

    fun getStringOfPosLenOpt string (SOME(pos, len)) =
        String.substring (string, pos, len)
      | getStringOfPosLenOpt string NONE = ""

    fun match pattern =
        let val re = RegExp.compileString pattern
        in
          fn string =>
             let
               val reader = makeStringReader string
               val matchReader = RegExp.find re reader
             in
               fn start => matchReader start
             end
        end

  in

  fun find_group pattern =
      let val f = match pattern
      in
        fn string =>
           case f string 0
            of SOME(mt, _) => SOME(getMatches mt)
             | NONE => NONE
      end

  fun find pattern =
      let val f = find_group pattern
      in
        fn string =>
           case f string
            of SOME(wholeMatch :: _) => wholeMatch
             | _ => NONE
      end

  fun global_find_group pattern =
      let val f = match pattern
      in
        fn string =>
           let
             val stringLen = size string
             fun loop founds index =
                 if stringLen = index
                 then List.rev founds
                 else
                   case f string index of
                     SOME(mt as MT.Match(SOME {len = 0, ...}, _), nextIndex) =>
                     loop founds (nextIndex + 1)
                   | SOME(mt as MT.Match(SOME _, _), nextIndex) =>
                     loop ((getMatches mt) :: founds) nextIndex
                   | _ => List.rev founds
           in
             loop [] 0
           end
      end

  fun global_find pattern =
      let val f = global_find_group pattern
      in
        fn string =>
           map
               (fn (SOME wholeMatch :: _) => wholeMatch
                 | _ => raise Fail "global_find unexpected nil.")
               (f string)
      end

  fun slice_group pattern =
      let val f = find_group pattern
      in
        fn string =>
           Option.map (map (getStringOfPosLenOpt string)) (f string)
      end

  fun slice pattern =
      let val f = slice_group pattern
      in fn string => hd (Option.getOpt (f string, [""]))
      end

  fun global_slice_group pattern =
      let val f = global_find_group pattern
      in fn string => map (map (getStringOfPosLenOpt string)) (f string)
      end

  fun global_slice pattern =
      let val f = global_slice_group pattern
      in fn string => map hd (f string)
      end

  fun replace pattern replacer =
      let 
        val f = match pattern
      in
        fn string =>
           case f string 0 of
             SOME(mt as MT.Match(SOME{pos, len}, _), nextPos) =>
             Substring.concat
                 [
                   Substring.substring (string, 0, pos),
                   Substring.full (replacer (getMatchedStrings string mt)),
                   Substring.substring (string, nextPos, size string - nextPos)
                 ]
           | _ => string
      end

  fun global_replace pattern replacer =
      let
        val f = match pattern
      in
        fn string =>
           let
             val len = size string
             val f = f string

             fun onEOS substrings lastPos =
                 let
                   val tail =
                       Substring.substring (string, lastPos, len - lastPos)
                 in
                   Substring.concat (rev (tail :: substrings))
                 end

             fun loop substrings nextPos =
                 case f nextPos of
                   SOME(mt as MT.Match(SOME{pos, len = 0}, _), newNextPos) =>
                   (* For example, a pattern "x?" mathces with any string.
                    * To avoid infinite loop, advance the cursor one character.
                    *)
                   if len = newNextPos
                   then onEOS substrings nextPos
                   else
                     let
                       val unmatched = Substring.substring (string, nextPos, 1)
                       val matched =
                           Substring.full
                               (replacer (getMatchedStrings string mt))
                     in
                       loop
                           (matched :: unmatched :: substrings)
                           (newNextPos + 1)
                     end
                 
                 | SOME(mt as MT.Match(SOME{pos, len}, _), newNextPos) =>
                   let
                     val unmatched =
                         Substring.substring
                             (string, nextPos, newNextPos - nextPos - len)
                     val matched =
                         Substring.full
                             (replacer (getMatchedStrings string mt))
                   in
                     loop (matched :: unmatched :: substrings) newNextPos
                   end
                 | SOME(MT.Match(NONE, _), _) =>
                   raise Fail "global_replace.loop unexpected match tree."
                 | NONE => onEOS substrings nextPos
           in
             loop [] 0
           end
      end

  fun subst pattern replaceString =
      replace
          pattern
          (fn (matched :: _) => replaceString
            | [] => raise Fail "subst unexpected nil.")

  fun global_subst pattern replaceString =
      global_replace
          pattern
          (fn (matched :: _) => replaceString
            | [] => raise Fail "global_subst unexpected nil.")

  infix =~
  fun string =~ pattern = isSome(find pattern string)

  fun fields pattern =
      let
        val f = match pattern
      in
        fn string =>
           let
             val len = size string
             val f = f string

             fun onEOS substrings lastPos =
                 let
                   val tail =
                       Substring.substring (string, lastPos, len - lastPos)
                 in
                   map Substring.string (rev (tail :: substrings))
                 end

             fun loop substrings nextPos =
                 case f nextPos of
                   SOME(mt as MT.Match(SOME{pos, len = 0}, _), newNextPos) =>
                   (* For example, a pattern "x?" mathces with any string.
                    * To avoid infinite loop, advance the cursor one character.
                    *)
                   if len = newNextPos
                   then onEOS substrings nextPos
                   else
                     let
                       val unmatched = Substring.substring (string, nextPos, 1)
                     in
                       loop (unmatched :: substrings) (newNextPos + 1)
                     end
                 
                 | SOME(mt as MT.Match(SOME{pos, len}, _), newNextPos) =>
                   let
                     val unmatched =
                         Substring.substring
                             (string, nextPos, newNextPos - nextPos - len)
                   in
                     loop (unmatched :: substrings) newNextPos
                   end
                 | SOME(MT.Match(NONE, _), _) =>
                   raise Fail "global_replace.loop unexpected match tree."
                 | NONE => onEOS substrings nextPos
           in
             loop [] 0
           end
      end

  fun tokens pattern =
      let val f = fields pattern
      in
        fn string => List.filter (fn field => 0 < size field) (f string)
      end

  end

  (****************************************)
  (* (imperative) association list *)

  type ('key, 'value) assoc_list = ('key * 'value) list ref

  fun assoc (ref list) key =
      List.find (fn (k, _) => k = key) list

  fun add_assoc listRef (key, value) = listRef := (key, value) :: (!listRef)

  fun del_assoc listRef key =
      listRef := List.filter (fn (k, _) => k <> key) (!listRef)

  (****************************************)
  (* list manipulation *)

  fun sort compare list =
      let
        val array = Array.fromList list
        fun sub index = Array.sub (array, index)
        fun update (index, value) = Array.update (array, index, value)
        fun swap (i, j) =
            let val tmp = sub i in update (i, sub j); update (j, tmp) end
        fun quickSort (left, right) =
	    if left < right
            then
              let
	        val center = (left + right) div 2
	        val pivotValue = sub center

	        val _ = update (center, sub left)
                (* ensure that values left to pivot are less than pivotValue,
                 * and value to rigth to pivot are greater than or equal to
                 * pivotValue. *)
                fun scan (cursor, pivot) =
                    if cursor <= right
                    then
                      if LESS = compare(sub cursor, pivotValue)
                      then
                        (
                          swap (pivot + 1, cursor);
                          scan (cursor + 1, pivot + 1)
                        )
                      else scan (cursor + 1, pivot)
                    else pivot
                val pivot = scan (left + 1, left)
              in
                update (left, sub pivot);
                update (pivot, pivotValue);
                (* now, array is partitioned at pivot. *)

	        quickSort(left, pivot - 1);
	        quickSort(pivot + 1, right)
              end
            else ()
      in
        quickSort (0, Array.length array - 1);
        Array.foldr (fn (value, list) => value :: list) [] array
      end

  (****************************************)
  (* Process *)

  fun exec (commandName, args) = notImplemented ()

  fun system (commandName, args) =
      let
        val command =
            foldl (fn (arg, command) => command ^ " " ^ arg) commandName args
      in OS.Process.system command
      end

  val exit = ignore o OS.Process.exit

  val sleep = OS.Process.sleep o Time.fromSeconds o LargeInt.fromInt

  fun env name =
      case OS.Process.getEnv name
       of SOME value => value
        | NONE => ""

  val argv = CommandLine.arguments ()

  val cd = OS.FileSys.chDir

  val pwd = OS.FileSys.getDir

  (****************************************)
  (* File system *)

  type glob_pattern = string

  fun listDir path =
      let
        val dirStream = OS.FileSys.openDir path
        fun collect list =
            case OS.FileSys.readDir dirStream
             of NONE => list
              | SOME "." => collect list
              | SOME ".." => collect list
              | SOME entity => collect (entity :: list)
      in
        collect [] before OS.FileSys.closeDir dirStream
      end

  local
    (* ToDo : translate to regular expression according to Posix strictly
     *)
    val translators =
        map
            (fn (pat, rep) => global_subst pat rep)
            [("\\[!", "[^"), ("\\*", ".*"), ("\\?", ".")]
    fun transPattern globPattern =
        "^" 
        ^ (foldl
               (fn (translator, string) => translator string)
               globPattern
               translators)
        ^ "$"
    (* ToDo : compile pattern to RE to improve efficiency. *)
    fun isMatch pattern name = name =~ pattern
  in
  fun glob pattern =
      let
        val {isAbs, vol, arcs} = OS.Path.fromString pattern
        val start = if "" <> vol then vol else if isAbs then "/" else "."
        val patterns = map transPattern (List.filter (fn s => s <> "") arcs)
        fun collect [] path = [path]
          | collect (pattern :: patterns) path =
            let
              val elements = listDir path
              val matchedElements = List.filter (isMatch pattern) elements
            in
              List.concat
                  (map
                       (fn arc =>
                           collect patterns (OS.Path.concat (path, arc)))
                       matchedElements)
            end
      in
        collect patterns start
      end
  end

  fun exists path = (OS.FileSys.fullPath path; true) handle OS.SysErr _ => true

  fun isLink path = OS.FileSys.isLink path handle OS.SysErr _ => false

  fun isDir path = OS.FileSys.isDir path handle OS.SysErr _ => false

  (****************************************)
  (* IO *)

  datatype file = Instream of TextIO.instream | Outstream of TextIO.outstream

  exception EOF

  val stdIn = Instream TextIO.stdIn
  val stdOut = Outstream TextIO.stdOut
  val stdErr = Outstream TextIO.stdErr

  fun fopen fileName mode =
      case mode of
        "r" => Instream(TextIO.openIn fileName)
      | "w" => Outstream(TextIO.openOut fileName)
      | "a" => Outstream(TextIO.openAppend fileName)
      | _ => raise Fail ("fopen got invalid mode: " ^ mode)

  fun fclose (Instream stream) = TextIO.closeIn stream
    | fclose (Outstream stream) = TextIO.closeOut stream

  fun feof (Instream stream) = TextIO.endOfStream stream
    | feof _ = false

  fun fflush (Outstream stream) = TextIO.flushOut stream
    | fflush _ = ()

  fun fpeek (Instream stream) = TextIO.lookahead stream
    | fpeek _ = raise Fail "fpeek expectes instram"

  local
    fun onFinish chars = implode (rev chars)
    fun skipNewlines stream =
        case TextIO.lookahead stream of
          SOME #"\n" => (TextIO.input1 stream; skipNewlines stream)
        | SOME #"\r" => (TextIO.input1 stream; skipNewlines stream)
        | SOME _ => ()
        | NONE => ()
    fun untilNewlines stream =
        let
          fun loop chars =
              case TextIO.lookahead stream of
                SOME #"\n" => onFinish chars
              | SOME #"\r" => onFinish chars
              | SOME c => (TextIO.input1 stream; loop (c :: chars))
              | NONE => onFinish chars
        in
          loop []
        end
    fun untilLS stream ls =
        let
          val lsChars = explode ls
          fun loop chars [] = onFinish chars
            | loop chars (ls1 :: ls) =
              case TextIO.input1 stream of
                SOME c =>
                if ls1 = c
                then loop (c :: chars) ls
                else loop (c :: chars) lsChars
              | NONE => onFinish chars
        in
          loop [] lsChars
        end
  in
  fun fgets (Instream stream) lsOpt =
      (case lsOpt of
         NONE => TextIO.inputAll stream
       | SOME "" => (skipNewlines stream; untilNewlines stream)
       | SOME ls => untilLS stream ls)
    | fgets _ _ = raise Fail "fgets expects instream."

  fun readline (s as Instream stream) lsOpt =
      if TextIO.endOfStream stream
      then raise EOF
      else fgets s lsOpt
    | readline _ _ = raise Fail "readline expects instream."
  end (* local *)

  fun readlines (stream as Instream _) lsOpt =
      let
        exception Result of string list
        fun loop lines =
            let
              val line =
                  (readline stream lsOpt)
                  handle EOF => raise Result lines
            in loop (line :: lines)
            end
      in
        (loop [])
        handle Result lines => rev lines
      end
    | readlines _ _ = raise Fail "readlines expects instream."

  fun fputc (Outstream stream) char = TextIO.output1 (stream, char)
    | fputc _ _ = raise Fail "fputc expects outstream."

  fun fputs (Outstream stream) string = TextIO.output (stream, string)
    | fputs _ _ = raise Fail "fputs expects outstream."

  fun prints strings = app print strings

  fun println string = (print string; print "\n")

  (***************************************************************************)

end
