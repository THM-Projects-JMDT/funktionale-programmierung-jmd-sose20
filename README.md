# SPL Formatierungstool – Gruppe 8

## Mitglieder

- Jannik Lapp
- Max Stephan
- David Martschenko

## Information

Das **SPL Formatierungstool** ist es ein Programm zur [Quelltextformatierung](https://de.wikipedia.org/wiki/Quelltextformatierung) der Programiersprache **SPL** (Simple Programming Languange). **SPL** ist eine Programiersprache die an der [THM](https://www.thm.de/site/) für das Module [Compillerbau](https://www.thm.de/organizer/index.php?option=com_organizer&view=subject_item&id=9) Entwickelt wurde.

## Verwendung

### Benötigte Software:

  - `GHC 8.0.*`
  - `cabal >= 3.0.*`

> Es kann auch eine andere `GHC` version verwendet werden, damit dies Funktioniert muss allerdings die `base` version in der datei `SPL-Formatierungstool.cabal` angepasst werden. 

<!--TODO vtl. allow more base Versions in Cabal file--->

### Programm Starten:

_Mit Cabal Starten_: 
  
  `cabal v2-run SPL-Formatierungstool -- "spl-code"` 
   
  > `Spl-code` muss durch den Source Code des SPL Programms ersetzt werden.

_Programm Bauen und Starten_:

  1. `cabal v2-build`
  2. In dem `dist-newstyle/build` Odner befindet sich nun das gebaut Programm.
   
   > Das Programm befindet sich in einigen unter Odnern, diese sind je nach Betriebsystem und ghci version unterschiedlich
  3. In den in Schritt `2` beschriebenen Ordner wechseln.
  4. Starten:
     - Windows: `SPL-Formatierungstool "spl-code"`
     - Linux: `./SPL-Formatierungstool "spl-code"`
  
  > `Spl-code` muss durch den Source Code des SPL Programms ersetzt werden.


_Tests Ausführen_: 
  
  `cabal v2-test`