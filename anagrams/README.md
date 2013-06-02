## Contents:<br/>
trie.sig/trie.sml - code for terrible, highly memory-inefficient tries<br/>
anagram.sig/anagram.sml - uses the trie stuff to store anagrams<br/>
nj-main.sml - contains code to read in a dictionary file and take user input for SML/NJ use<br/>

---
## Usage:<br/>
CM.make "sources.cm"<br/>
This adds a structure WordDict : TRIE (the trie implementation), a functor AnagramDict : ANAGRAMDICT (it takes a trie implementation and yields an anagram dictionary thingy), and a functor Anagram : ANAGRAMFINDER (it takes an anagram dictionary thingy and yields stuff) to the environment.<br/>
To actually do something, put a word list (text, words separated by newlines) in the same directory, then<br/>
structure T = Anagram(AnagramDict(WordDict))<br/>
T.runWithDict "dictionary_file_name.txt"