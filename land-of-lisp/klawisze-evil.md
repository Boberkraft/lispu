




**k** góra  **j** dół **h** lewo **l** prawo 


**:wqa <ENTER>** zapisz i wyjdź
**:qa! <ENTER>** wyjdź bez zapisywanie

**x** usuń znak

**u** undo

**CTRL-R** redo



i oraz **a** # INSERT MODE #

**[number] d object** lub **d [number] object**

- <d> (delete) lub <c> (change)
  -- <d> (linia)
      usuwa całą linię
  -- <w> (word)
      usuwa wyraz wraz z spacja
  -- <e> (end)
      usuwa wyraz bez spacji 
  -- <$> (end of line**
      usuwa do końca
**p** wklej, put
**r** zamień znak replace

**gg** - beggining of buffer
**G** - end of buffer
: <number linii> przenosi na linie
/ <slowo> wyszukuje slowo
      + teraz **n** wyszukuje nastepne, a **N** poprzednie (albo ? zamiast /)
% przenosi do pasujacego nawiasu
:s/old/new/g zamienia old na new
:?,?s/old/new/g zamienia pomiedzy liniami
:%s/old/new/g zamienia w calym bufferze
:%s/old/new/gc prosi o potwierdzenie
:!<COMMAND> tak jakbys w cmd zrobil COMMAND




 
