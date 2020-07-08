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

`cabal v2-run splf -- [args] "spl-code"`

> - Die Beschreibung für die Argumente(`args`) befindet sich [hier](#programm-einstellungen)
> - `spl-code` muss durch den Quellcode des SPL-Programms ersetzt werden.

_Programm bauen und starten_:

1. `cabal v2-build`
2. In dem Ordner `dist-newstyle/build` befindet sich nun das gebaute Programm.

> Das Programm befindet sich in einigen Unterordnern, diese sind je nach Betriebssystem und `GHC`-Version unterschiedlich.

3. In den in Schritt `2` beschriebenen Ordner wechseln.
4. Starten:
   - Windows: `splf [args] "spl-code"`
   - Linux: `./splf [args] "spl-code"`

> - Die Beschreibung für die Argumente(`args`) befindet sich [hier](#programm-einstellungen)
> - `spl-code` muss durch den Quellcode des SPL-Programms ersetzt werden.

_Tests ausführen_:

`cabal v2-test`

### Programm Einstellungen

Folgende Einstellungen Können per **Kommandozeilenargument** eingestellt werden:

**`--itype ARG`**:

Bestimmt welches Zeichen zur **Einrückung** Verwendet wird.

- Mögliche werte für `ARG`:
  - `s`: für Leerzeichen (Standart)
  - `t`: für Tabulator

**`--inum INT`**:

**Anzahl** der **Leerzeichen/Tabs** pro Einrückung.

- `INT` kann eine belibige Zahl sein (Standart `2`).

**`--rms`**:

Wenn dieses Argument angegeben ist werden alle **nicht Benötigten Vorzeichen entfernt**. (z.B.: `+1` -> `1`)

**`--kc`**:

Wenn dieses Argument angegeben ist werden alle "**fehlplatzierten Kommentare**" **beibehalten** (Ohne diese Option werden diese Einfach entfernt).

**`--nls ARG`**:

Bestimmt welche/s Zeichen für den **Zeilenumbruch** Verwendet wird/werden.

- Mögliche werte für `ARG`:
  - `linux`: `\n` (Standart)
  - `win`: `\r\n`
  - `mac`: `\r`
