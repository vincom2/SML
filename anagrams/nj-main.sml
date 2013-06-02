signature ANAGRAMFINDER = sig
  structure A : ANAGRAMDICT
  type filename = string

  val runWithDict : filename -> unit
end

functor Anagram (An : ANAGRAMDICT) : ANAGRAMFINDER = struct
  open TextIO
  structure A = An
  type filename = string
  val d = ref A.empty

  fun chomp' [] = []
      | chomp' (c::cs) = if (Char.isSpace c) then chomp' cs else (c::cs)
  fun chomp NONE = NONE
    | chomp (SOME w) =
      SOME ((String.implode o List.rev o chomp' o List.rev o chomp' o String.explode) w)

  fun readInput file =
            case chomp (inputLine file) of
            SOME line => (d := A.add(!d,line); readInput file)
          | NONE => ()

  fun printOutput NONE = print "no anagrams found, sorry!\n"
    | printOutput (SOME L) = List.app (fn s => print (s ^ "\n")) L

  fun run () =
      (print "enter a word: ";
      case chomp (inputLine stdIn) of
      SOME w => (printOutput (A.find (!d,w)); run())
    | NONE => ())

  fun runWithDict filename = let val file = openIn filename in
        readInput file;
        run()
      end
end
