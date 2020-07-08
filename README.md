# SPL Formatierungstool – Gruppe 8

## Mitglieder

- Jannik Lapp
- Max Stephan
- David Martschenko

## Information

Das **SPL Formatierungstool** ist es ein Programm zur [Quelltextformatierung](https://de.wikipedia.org/wiki/Quelltextformatierung) der Programmiersprache **SPL** (Simple Programming Languange). **SPL** ist eine Programmiersprache, die an der [THM](https://www.thm.de/site/) für das Modul [Compillerbau](https://www.thm.de/organizer/index.php?option=com_organizer&view=subject_item&id=9) entwickelt wurde.

## Verwendung

### Benötigte Software:

- `GHC 8.0.*`
- `cabal >= 3.0.*`

> Es kann auch eine andere `GHC` version verwendet werden - damit dies funktioniert, muss allerdings die `base` version in der Datei `SPL-Formatierungstool.cabal` angepasst werden.

<!--TODO vtl. allow more base Versions in Cabal file--->

### Programm starten:

_Mit cabal starten_:

`cabal v2-run splf -- [args] input`

> - Die Beschreibung für die Argumente (`args`) befindet sich [hier](#programm-einstellungen)
> - `input` ist standardmäßig der Quellcode des SPL-Programms - es kann aber auch eine Datei eingelesen werden, indem ein Flag gesetzt wird (siehe [hier](#programm-einstellungen)).

_Programm bauen und starten_:

1. `cabal v2-build`
2. In dem Ordner `dist-newstyle/build` befindet sich nun das gebaute Programm.

> Das Programm befindet sich in einigen Unterordnern, diese sind je nach Betriebssystem und `GHC`-Version unterschiedlich.

3. In den in Schritt `2` beschriebenen Ordner wechseln.
4. Starten:
   - Windows: `splf [args] input`
   - Linux: `./splf [args] input`

> - Die Beschreibung für die Argumente(`args`) befindet sich [hier](#programm-einstellungen)
> - `input` ist standardmäßig der Quellcode des SPL-Programms - es kann aber auch eine Datei eingelesen werden, indem ein Flag gesetzt wird (siehe [hier](#programm-einstellungen)).

_Tests ausführen_:

`cabal v2-test`

### Programm Einstellungen

Folgende **Kommandozeilenargumente** stehen zur Konfiguration des Formatierers bereit:

**`--itype ARG`**:

Bestimmt, welches Zeichen für **Einrückungen** verwendet wird.

- Mögliche Werte für `ARG`:
  - `s`: für Leerzeichen (Default)
  - `t`: für Tabulator

**`--inum INT`**:

**Anzahl** der **Leerzeichen/Tabulatoren** pro Einrückung.

- `INT` kann eine beliebige Zahl sein (Default: `2`).

**`--rms`**:

Wenn dieses Flag gesetzt ist, werden alle **nicht benötigten Vorzeichen entfernt**. (z.B.: `+1` -> `1`)

**`--kc`**:

Wenn dieses Flag gesetzt ist, werden alle "**fehlplatzierten" Kommentare** **beibehalten** .
Ansonsten werden diese einfach entfernt.

**`--nls ARG`**:

Bestimmt welche/s Zeichen für den **Zeilenumbruch** Verwendet wird/werden.

- Mögliche werte für `ARG`:
  - `linux`: `\n` (Default)
  - `win`: `\r\n`
  - `mac`: `\r`

**`-f`**:

Wenn dieses Flag gesetzt ist, wird der Inhalt der Datei, deren Pfad bei `input` angegeben ist, eingelesen und formattiert.
Ansonsten wird `input` direkt als SPL-Code interpretiert (damit Leerzeichen verwendet werden können, muss `ìnput` hierbei  
in Anführungszeichen gesetzt werden).

**`-h, --help`**:

Hilfetext anzeigen und Programm beenden.
