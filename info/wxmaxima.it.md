# Il manuale utente di wxMaxima {-}

wxMaxima è un'interfaccia utente grafica (GUI) per il sistema di algebra
computazionale (CAS) _Maxima_. wxMaxima consente di utilizzare tutte le
funzioni di _Maxima_, inoltre, fornisce comode procedure guidate per
accedere alle funzionalità più comunemente utilizzate. Questo manuale
descrive alcune delle funzionalità che rendono wxMaxima una delle GUI più
popolari per _Maxima_.

![logo wxMaxima](./wxMaximaLogo.png){ id=img_wxMaximaLogo }

______________________________________________________________________

# Introduzione a wxMaxima

## _Maxima_ e wxMaxima

Nel dominio open source, i grandi sistemi sono normalmente suddivisi in
progetti più piccoli che sono più facili da gestire per piccoli gruppi di
sviluppatori. Ad esempio, un programma di masterizzazione di CD sarà
costituito da uno strumento a riga di comando che effettivamente masterizza
il CD e da un'interfaccia utente grafica che consente agli utenti di
utilizzarlo senza dover conoscere tutte le opzioni della riga di comando, e
di fatto senza utilizzare affatto la riga di comando. Un vantaggio di questo
approccio è che il lavoro di sviluppo che è stato investito nel programma a
riga di comando può essere condiviso da molti programmi: lo stesso programma
a riga di comando del masterizzatore CD può essere utilizzato come plug-in
"invia-al-CD" per un'applicazione di gestione file, per la funzione
"masterizza su CD" di un lettore musicale e come masterizzatore di CD per
uno strumento di backup su DVD. Un altro vantaggio è che suddividere una
grande attività in parti più piccole consente agli sviluppatori di fornire
diverse interfacce utente per lo stesso programma.

Un sistema di algebra computazionale (CAS) come _Maxima_ si inserisce in
questo quadro. Un CAS può fornire la logica dietro un'applicazione di
calcolatrice di precisione arbitraria o può fare trasformazioni automatiche
di formule sullo sfondo di un sistema più grande (ad esempio, [Sage]
(https://www.sagemath.org/)). In alternativa, può essere utilizzato
direttamente come sistema indipendente. _Maxima_ è utilizzabile tramite riga
di comando. Spesso, tuttavia, un'interfaccia come _wxMaxima_ si rivela un
modo più efficiente per accedere al software, in particolare per i nuovi
utenti.

### _Maxima_

_Maxima_ è un sistema di algebra computazionale (CAS) completo. Un CAS è un
programma in grado di risolvere problemi matematici riorganizzando le
formule e trovando una formula che risolve il problema invece di emettere
semplicemente il valore numerico del risultato. In altre parole, _Maxima_
può fungere da calcolatrice che fornisce rappresentazioni numeriche di
variabili e può anche fornire soluzioni analitiche. Inoltre, offre una gamma
di metodi numerici di analisi per equazioni o sistemi di equazioni che non
possono essere risolti analiticamente.

![schermata di Maxima, riga di comando](./maxima_screenshot.png){
id=img_maxima_screenshot }

Ampia documentazione per _Maxima_ è [disponibile su Internet]
(https://maxima.sourceforge.io/documentation.html). Parte di questa
documentazione è disponibile anche nel menu di aiuto di wxMaxima. Premendo
il tasto Guida (sulla maggior parte dei sistemi il tasto  <kbd>F1</kbd>), la
funzione di guida sensibile al contesto di _wxMaxima_ passa automaticamente
alla pagina di manuale di _Maxima_ per il comando sotto il cursore.

### wxMaxima

_wxMaxima_ è un'interfaccia utente grafica che fornisce flessibilità e tutte
le funzionalità di _Maxima_. wxMaxima offre agli utenti una visualizzazione
grafica e molte caratteristiche che rendono più facile lavorare con
_Maxima_. Ad esempio _wxMaxima_ consente di esportare il contenuto di una
qualsiasi cella (o, se necessario, anche qualsiasi parte di una formula)
come testo, come LaTeX o come specifica MathML con un semplice clic
destro. In effetti, è possibile esportare un'intera cartella di lavoro, sia
come file HTML che come file LaTeX. La documentazione per _wxMaxima_,
comprese le cartelle di lavoro per illustrare gli aspetti del suo utilizzo,
è online su _wxMaxima_ [sito della
guida](https://wxMaxima-developers.github.io/wxmaxima/help.html), nonché
tramite il menu della guida.

![wxMaxima window](./wxMaximaWindow.png){ id=img_wxMaximaWindow }

I calcoli immessi in _wxMaxima_ vengono eseguiti dallo strumento della riga
di comando _Maxima_ in background.

## Nozioni di base sulle cartelle di lavoro

Gran parte di _wxMaxima_ si spiega da sé, ma alcuni dettagli richiedono
attenzione. [Questo
sito](https://wxMaxima-developers.github.io/wxmaxima/help.html) contiene una
serie di cartelle di lavoro che affrontano vari aspetti di
_wxMaxima_. Lavorare attraverso alcuni di questi (in particolare il
"tutorial _(wx)Maxima_ di 10 minuti") può essere utile ad aumentare la
propria familiarità sia con il contenuto di _Maxima_ sia con l'uso di
_wxMaxima_ per interagire con _Maxima_. Questo manuale si concentra sulla
descrizione degli aspetti di _wxMaxima_ che probabilmente non sono evidenti
e che potrebbero non essere trattati nel materiale online.

### L'approccio della cartella di lavoro

Una delle pochissime cose che non sono standard in _wxMaxima_ è che
organizza i dati per _Maxima_ in celle che vengono elaborate (ovvero:
inviate a _Maxima_) solo quando l'utente lo richiede. Quando viene elaborata
una cella, tutti i comandi in quella cella, e solo quella cella, vengono
valutati in modalità 'batch'. (L'affermazione precedente non è del tutto
accurata: si può selezionare un insieme di celle adiacenti e valutarle tutte
assieme. Inoltre, si può istruire _Maxima_ a valutare tutte le celle in una
cartella di lavoro in un solo passaggio.) L'approccio di _wxMaxima_
all'invio di comandi per l'esecuzione potrebbe sembrare poco familiare a
prima vista. Tuttavia, facilita drasticamente il lavoro con documenti di
grandi dimensioni (dove l'utente potrebbe non desiderare che ogni modifica
attivi automaticamente una rivalutazione completa dell'intero
documento). Inoltre, questo approccio è molto utile per la ricerca difetti.

Se il testo viene digitato in _wxMaxima_, si crea automaticamente una nuova
cella del foglio di lavoro. Il tipo di questa cella può essere selezionato
nella barra degli strumenti. Se viene creata una cella di codice, la cella
può essere inviata a _Maxima_, il che fa sì che il risultato del calcolo
venga visualizzato sotto il codice. Di seguito viene mostrata una coppia di
tali comandi.

![Input/output cell](./InputCell.png){ id=img_InputCell }

Sulla valutazione del contenuto di una cella di ingresso, la cella di
ingresso _Maxima_ assegna un'etichetta all'ingresso (per impostazione
predefinita mostrata in rosso e riconoscibile da `%i`) con la quale può
essere referenziata successivamente durante la sessione. L'uscita generata
da _Maxima_ ottiene anch'essa un'etichetta, la quale inizia con `%o` e, per
impostazione predefinita, è nascosta, tranne se l'utente assegna un nome
all'uscita. In questo caso, ancora per impostazione predefinita, viene
visualizzata un'etichetta definita dall'utente. Anche l'etichetta in stile
`%o`\ _Maxima_ e generata automaticamente, sarà accessibile.

Oltre alle celle di ingresso, _wxMaxima_ consente l'uso di celle di testo
per la documentazione, celle immagine, celle titolo, celle capitolo e celle
sezione. Ogni cella ha una propria memoria degli annullamenti, quindi
eseguire la correzione modificando i valori di più celle e quindi
ripristinare gradualmente le modifiche non necessarie, è piuttosto
semplice. Inoltre, il foglio di lavoro stesso ha una memoria degli
annullamenti globale che può annullare le modifiche, le aggiunte e le
eliminazioni delle celle.

La figura seguente mostra diversi tipi di celle (celle titolo, celle
sezione, celle sottosezione, celle testo, celle ingresso/uscita e celle
immagine).

![Esempio di celle wxMaxima diverse](./cell-example.png){
id=img_cell-example }

### Celle

Il foglio di lavoro è organizzato in celle. wxMaxima riconosce i seguenti
tipi di celle:

- Celle matematiche, contenenti una o più righe di ingresso _Maxima_.
- Uscita di, o una domanda da parte di, _Maxima_.
- Celle immagine.
- Celle di testo, che per esempio possono essere usate per documentazione.
- Un titolo, sezione o sottosezione. Sono possibili 6 livelli diversi di
  intestazione.
- Interruzioni di pagina.

Il comportamento predefinito di _wxMaxima_ quando viene inserito del testo è
creare automaticamente una cella matematica. È possibile creare celle di
altro tipo utilizzando il menu Cella, utilizzando i tasti di scelta rapida
visualizzati nel menu o utilizzando l'elenco a discesa nella barra degli
strumenti. Una volta creata la cella non-matematica, tutto ciò che viene
digitato nel file viene interpretato come testo.

Un testo di commento (in stile linguaggio C) può fare parte di una cella
matematica come di seguito: `/* Questo commento verrà ignorato da Maxima */`

"`/*`" marca l'inizio del commento, mentre "`*/`" la sua fine.

### Cursori orizzontali e verticali

Se l'utente tenta di selezionare una frase completa, un elaboratore di testi
tenterà di estendere la selezione per iniziare e terminare automaticamente
con un limite di parola. Allo stesso modo _wxMaxima_ se viene selezionata
più di una cella estenderà la selezione alle celle intere.

Ciò che non è standard è che _wxMaxima_ fornisce flessibilità di
trascinamento della selezione definendo due tipi di cursori. _wxMaxima_
passerà da uno all'altro automaticamente alla bisogna:

- Il cursore viene disegnato orizzontalmente se viene spostato nello spazio
  tra due celle o facendo clic lì.
- Un cursore verticale che funziona all'interno di una cella. Questo cursore
  viene attivato spostando il cursore all'interno di una cella utilizzando
  il puntatore del mouse o i tasti cursore e funziona in modo molto simile
  al cursore in un editor di testo.

Quando si avvia wxMaxima, è visibile solo un cursore orizzontale
lampeggiante. Se si inizia a digitare, viene creata automaticamente una
cella matematica e il cursore diventa verticale (si vedrà una freccia destra
come "prompt", dopo l'elaborazione della cella matematica
(<kbd>CTRL+INVIO</kbd >), si noteranno, ad esempio, le etichette `(%i1)` e
`(%o1)`).

![(blinking) cursore orizzontale dopo l'avvio di
wxMaxima](./horizontal-cursor-only.png){ id=img_horizontal_cursor_only }

Si potrebbe voler creare un altro tipo di cella (usando il menu "Cella"),
magari una cella titolo o una cella di testo, che descriva cosa verrà fatto
quando si inizi a creare il foglio di lavoro.

Se si naviga tra le diverse celle, si vedrà anche il cursore orizzontale
(lampeggiante), col quale si può inserire una cella nel foglio di lavoro
(una cella matematica, semplicemente iniziando a digitare la formula, o un
altro tipo di cella utilizzando il menu).

![(blinking) cursore orizzontale tra
celle](./horizontal-cursor-between-cells.png){
id=img_horizontal_cursor_between_cells }

### Invio delle celle a Maxima

Il comando in una cella di codice viene eseguito una volta premuti i tasti
<kbd>CTRL</kbd>+<kbd>INVIO</kbd>, <kbd>MAIUSC</kbd>+<kbd>INVIO</kbd> o
<kbd>INVIO</kbd> sulla tastiera. L'impostazione predefinita di _wxMaxima_
prevede l'immissione di comandi quando viene inserito
<kbd>CTRL</kbd>+<kbd>INVIO</kbd> o <kbd>MAIUSC</kbd>+<kbd>INVIO</kbd>, ma
_wxMaxima_ può essere configurato per eseguire comandi in risposta anche a
solo <kbd>INVIO</kbd>.

### Autocompletamento comandi

_wxMaxima_ contiene una funzione di completamento automatico che viene
attivata tramite il menu (Cella/Parola completa) o in alternativa premendo
la combinazione di tasti <kbd>CTRL</kbd>+<kbd>SPAZIO</kbd>. Il completamento
automatico è sensibile al contesto. Ad esempio, se attivato all'interno di
una specifica di unità di ezUnits, offrirà un elenco di unità applicabili.

![ezUnits](./ezUnits.png){ id=img_ezUnits }

Oltre a completare un nome di file, un nome di unità o il nome del comando o
della variabile corrente, l'autocompletamento è in grado di mostrare un
modello per la maggior parte dei comandi indicando il tipo (e il
significato) dei parametri che questo programma si aspetta. Per attivare
questa funzione premere <kbd>MAIUSC</kbd>+<kbd>CTRL</kbd>+<kbd>SPAZIO</kbd>
o selezionare la rispettiva voce di menu (Cella/Mostra modello).

#### Caratteri greci

I computer tradizionalmente memorizzavano i caratteri in valori a 8 bit. Ciò
consente un massimo di 256 caratteri diversi. Tutte le lettere, i numeri e i
simboli di controllo (fine trasmissione, fine stringa, linee e bordi per
disegnare rettangoli per i menu, _ecc._ .) di quasi tutte le lingue possono
essere compresi in tale limitazione.

Per la maggior parte dei paesi, la codifica di 256 caratteri che è stata
scelta non include per esempio le lettere greche, che sono usate
frequentemente in matematica. Per superare questo tipo di limitazione è
stato inventato [Unicode](https://home.unicode.org/): una codifica che fa
funzionare normalmente il testo inglese, ma può andare molto oltre il limite
dei 256 caratteri.

_Maxima_ consente Unicode, se è stato compilato utilizzando un compilatore
Lisp che supporta Unicode o che non si preoccupa della codifica dei
caratteri. Dato che è molto probabile che almeno una di questa coppia di
condizioni sia vera, _wxMaxima_ fornisce un metodo per inserire caratteri
greci usando la tastiera:

- È possibile inserire una lettera greca premendo il tasto <kbd>ESC</kbd> e
  quindi iniziare a digitare il nome del carattere greco.
- In alternativa può essere inserito premendo <kbd>ESC</kbd>, una lettera (o
  due per la lettera greca omicron) e <kbd>ESC</kbd> nuovamente. In questo
  caso sono supportate le seguenti lettere:

|tasto|Lettera greca |tasto|Lettera greca |Tasto|Lettera greca |
|:---:|:------------:|:---:|:------------:|:---:|:------------:|
|  a  |     alpha    |  i  |     iota     |  r  |      rho     |
|  b  |     beta     |  k  |     kappa    |  s  |     sigma    |
|  g  |     gamma    |  l  |    lambda    |  t  |      tau     |
|  d  |     delta    |  m  |      mu      |  u  |    upsilon   |
|  e  |    epsilon   |  n  |      nu      |  f  |      phi     |
|  z  |     zeta     |  x  |      xi      |  c  |      chi     |
|  h  |      eta     |  om |    omicron   |  y  |      psi     |
|  q  |     theta    |  p  |      pi      |  o  |     omega    |
|  A  |     Alpha    |  I  |     Iota     |  R  |      Rho     |
|  B  |     Beta     |  K  |     Kappa    |  S  |     Sigma    |
|  G  |     Gamma    |  L  |    Lambda    |  T  |      Tau     |
|  D  |     Delta    |  M  |      Mu      |  U  |    Upsilon   |
|  E  |    Epsilon   |  N  |      Nu      |  P  |      Phi     |
|  Z  |     Zeta     |  X  |      Xi      |  C  |      Chi     |
|  H  |      Eta     |  Om |    Omicron   |  Y  |      Psi     |
|  T  |     Theta    |  P  |      Pi      |  O  |     Omega    |

You can also use the "Greek letters"-sidebar to enter the Greek letters.

##### Attention: Lookalike characters

Several Latin letters look like the Greek letters, e.g. the Latin letter "A"
and the Greek letter "Alpha". Although they look identical, they are two
different Unicode characters, represented by different Unicode code points
(numbers).

This might be problematic, if you assign a value to the variable A and later
use the Greek letter Alpha to do something with this variable, especially on
printouts.  For the Greek letter my (which is also used as prefix for micro)
there are also two different Unicode code points.

The "Greek letters"-sidebar therefore has the option, that lookalike
characters are not available (which can be changed using a right-click
menu).

Lo stesso meccanismo permette anche di inserire alcuni simboli matematici
vari:

| keys to enter  | mathematical symbol                                   |
| -------------- | ----------------------------------------------------- |
| hbar           | Planck constant: a h with a horizontal bar above it   |
| Hbar           | a H with a horizontal bar above it                    |
| 2              | squared                                               |
| 3              | to the power of three                                 |
| /2             | 1/2                                                   |
| partial        | partial sign (the d of dx/dt)                         |
| integral       | integral sign                                         |
| sq             | square root                                           |
| ii             | imaginary                                             |
| ee             | element                                               |
| in             | in                                                    |
| impl implies   | implies                                               |
| inf            | infinity                                              |
| empty          | empty                                                 |
| TB             | big triangle right                                    |
| tb             | small triangle right                                  |
| and            | and                                                   |
| or             | or                                                    |
| xor            | xor                                                   |
| nand           | nand                                                  |
| nor            | nor                                                   |
| equiv          | equivalent to                                         |
| not            | not                                                   |
| union          | union                                                 |
| inter          | intersection                                          |
| subseteq       | subset or equal                                       |
| subset         | subset                                                |
| notsubseteq    | not subset or equal                                   |
| notsubset      | not subset                                            |
| approx         | approximately                                         |
| propto         | proportional to                                       |
| neq != /= or # | not equal to                                          |
| +/- or pm      | a plus/minus sign                                     |
| \<= or leq     | equal or less than                                    |
| >= or geq      | equal or greater than                                 |
| \<\< or ll     | much less than                                        |
| >> or gg       | much greater than                                     |
| qed            | end of proof                                          |
| nabla          | a nabla operator                                      |
| sum            | sum sign                                              |
| prod           | product sign                                          |
| exists         | there exists sign                                     |
| nexists        | there is no sign                                      |
| parallel       | a parallel sign                                       |
| perp           | a perpendicular sign                                  |
| leadsto        | a leads to sign                                       |
| ->             | a right arrow                                         |
| -->            | a long right arrow                                    |

You can also use the "Symbols"-sidebar to enter these Mathematical symbols.

If a special symbol isn’t in the list, it is possible to input arbitrary
Unicode characters by pressing <kbd>ESC</kbd> \[number of the character
(hexadecimal)\] <kbd>ESC</kbd>. Additionally the "symbols" sidebar has a
right-click menu that allow to display a list of all available Unicode
symbols one can add to this toolbar or to the worksheet.

<kbd>ESC</kbd><kbd>6</kbd><kbd>1</kbd><kbd>ESC</kbd> therefore results in an
`a`.

Please note that most of these symbols (notable exceptions are the logic
symbols) do not have a special meaning in _Maxima_ and therefore will be
interpreted as ordinary characters. If _Maxima_ is compiled using a Lisp
that doesn’t support Unicode characters they might cause an error message.

Potrebbe capitare che, ad es. caratteri greci o simboli matematici non siano
stati inclusi nel font selezionato, e quindi non possano essere
visualizzati.
Per risolvere questo problema, provare a selezionare un altro font (tramite:
Modifica -> Configura -> Stile).

### Unicode replacement

wxMaxima will replace several Unicode characters with their respective
Maxima expressions, e.g. `²` with `^2`, `³` with `^3`, the square root sign
with the function `sqrt()`, the (mathematical) Sigma sign (which is not the
same Unicode character as the corresponding Greek letter) with `sum()`, etc.

Unicode has several "common" fractions encoded as one Unicode code point:
`¼, ½, ¾, ⅐, ⅑, ⅒, ⅓, ⅔, ⅕, ⅖, ⅗, ⅘, ⅙, ⅚, ⅛, ⅜, ⅝, ⅞`

wxMaxima will replace them with their Maxima representations, e.g `(1/4)`
before the input is sent do Maxima. There are also `⅟`, which will be
replaced by `1/` and `↉` (used in baseball), which will be replaced by
`(0/3)`.

It is recommended to use **Maxima code** (not these Unicode code points) in
input cells (Rationale: (a) it might be possible, that the used font for
math input does not contain them; (b) if you save the document as
`wxm`-file, it is usually readable by (command line) Maxima, but these
changes will of course not work in command line Maxima); but they may occur,
if you cut&paste a formula from another document.


### Pannelli laterali

È possibile accedere più velocemente ai comandi _Maxima_ più importanti o a
cose come il sommario, finestre con messaggi diagnostici o la cronologia
degli ultimi comandi emessi utilizzando i riquadri laterali. Questi possono
essere abilitati utilizzando il menu "Visualizza". Tutti possono essere
spostati in altre posizioni all'interno o all'esterno della finestra
_wxMaxima_. Altro riquadro utile è quello che permette di inserire le
lettere greche utilizzando il mouse.

![Esempio di diversi pannelli laterali](./SidePanes.png){ id=img_SidePanes }

Nel riquadro laterale "sommario" è possibile elevare o abbassare di livello
un'intestazione; è sufficiente fare clic sull'intestazione con il tasto
destro del mouse e selezionare il tipo di intestazione successiva superiore
o inferiore.

![Eleva o abbassa le intestazioni nel riquadro laterale del
sommario](./Sidepane-TOC-convert-headings.png){
id=Sidepane-TOC-convert-headings }

### Uscita MathML

Several word processors and similar programs either recognize
[MathML](https://www.w3.org/Math/) input and automatically insert it as an
editable 2D equation - or (like LibreOffice) have an equation editor that
offers an “import MathML from clipboard” feature. Others support RTF
maths. _WxMaxima_, therefore, offers several entries in the right-click
menu.

### Supporto Markdown

_WxMaxima_ offers a set of standard
[Markdown](https://en.wikipedia.org/wiki/Markdown) conventions that don’t
collide with mathematical notation. One of these elements is bullet lists.

Testo normale
 * Un elemento, livello indentazione 1
 * Un altro elemento a livello indentazione 1
   * Un elemento al secondo livello di indentazione
   * Un secondo elemento al secondo livello di indentazione
 * Un terzo elemento al primo livello di indentazione
Testo normale

_wxMaxima_ riconoscerà il testo che inizia con i caratteri `>` come
virgolette di blocco:

``` Ordinary text > quote quote quote quote > quote quote quote quote >
quote quote quote quote Ordinary text ```

Anche l'uscita TeX e HTML di _wxMaxima_ riconoscerà `=>` e lo sostituirà con
il corrispondente simbolo Unicode:

``` cogito => sum.  ```

Altri simboli che l'esportazione HTML e TeX riconoscerà sono `<=` e `>=` per
i confronti, una doppia freccia a doppia punta (`<=>`), frecce a punta
singola (`<->`, `-> ` e `<-`) e `+/-` come simbolo. Per l'uscita TeX sono
riconosciuti anche `<<` e `>>`.

### Scorciatoie da tastiera

La maggior parte dei tasti di scelta rapida si trova nel testo dei
rispettivi menu. Dal momento che sono effettivamente presi dal testo del
menu e quindi possono essere personalizzati dalle traduzioni di _wxMaxima_
per soddisfare le esigenze degli utenti della tastiera locale, non vengono
documentati quì. Alcuni tasti di scelta rapida o alias di tasti di scelta
rapida, tuttavia, non sono documentati neanche nei menu:

- <kbd>CTRL</kbd>+<kbd>MAIUSC</kbd>+<kbd>CANC</kbd> elimina una cella
  intera.
- <kbd>CTRL</kbd>+<kbd>TAB</kbd> or
  <kbd>CTRL</kbd>+<kbd>SHIFT</kbd>+<kbd>TAB</kbd> triggers the
  auto-completion mechanism.
- <kbd>MAIUSC</kbd>+<kbd>SPAZIO</kbd> inserisce uno spazio non divisibile.

### TeX grezzo nell'esportazione TeX

Se una cella di testo inizia con `TeX:`, l'esportazione TeX contiene il
testo letterale che segue l'indicatore `TeX:`. L'utilizzo di questa funzione
consente l'inserimento del marcatore TeX all'interno della cartella di
lavoro _wxMaxima_.

## Formati di file

Il materiale sviluppato in una sessione _wxMaxima_ può essere archiviato per
un uso successivo in uno dei seguenti tre modi:

### .mac

I file `.mac` sono normali file di testo che contengono comandi
_Maxima_. Possono essere letti usando il comando `batch()` o `load()` di
_Maxima_ o la voce di menu File/File batch di _wxMaxima_.

Un esempio viene mostrato di seguito. `Quadratic.mac` definisce una funzione
e successivamente genera un grafico con `wxdraw2d()`. Successivamente viene
stampato il contenuto del file `Quadratic.mac` e viene valutata la nuova
funzione definita `f()`.

![Loading a file with `batch()`](./BatchImage.png){ id=img_BatchImage }

Attention: Although the file `Quadratic.mac` has a usual _Maxima_ extension
(`.mac`), it can only be read by _wxMaxima_, since the command `wxdraw2d()`
is a wxMaxima-extension to _Maxima_. (Command line) Maxima will ignore the
unknown command `wxdraw2d()` and print it as output again.

Si può usare i file `.mac` per scrivere la propria libreria di macro. Ma dal
momento che non contengono informazioni strutturali sufficienti, non possono
essere rilette come sessioni _wxMaxima_.

### .wxm

`.wxm` files contain the worksheet except for _Maxima_’s output. On Maxima
versions >5.38 they can be read using _Maxima_’s `load()` function just as
.mac files can be. With this plain-text format, it sometimes is unavoidable
that worksheets that use new features are not downwards-compatible with
older versions of _wxMaxima_.

#### File format of wxm files

This is just a plain text file (you can open it with a text editor),
containing the cell contents as some special Maxima comments.

It starts with the following comment:

``` /* [wxMaxima batch file version 1] [ DO NOT EDIT BY HAND! ]*/ /* [
Created with wxMaxima version 24.02.2_DevelopmentSnapshot ] */ ```

And then the cells follow, encoded as Maxima comments, e.g. a section cell:

```
/* [wxMaxima: section start ]
Title of the section
   [wxMaxima: section end   ] */
```

or (in a Math cell the input is of course *not* commented out (the output is
not saved in a `wxm` file)):

```
/* [wxMaxima: input   start ] */
f(x):=x^2+1$
f(2);
/* [wxMaxima: input   end   ] */
```

Images are [Base64 encoded](https://en.wikipedia.org/wiki/Base64) with the
image type as first line):

```
/* [wxMaxima: image   start ]
jpg
[very chaotic looking character sequence]
   [wxMaxima: image   end   ] */
```

A page break is just one line containing:

```
/* [wxMaxima: page break    ] */
```

And folded cells marked by:

```
/* [wxMaxima: fold    start ] */
...
/* [wxMaxima: fold    end   ] */
```

### .wxmx

Questo formato di file basato su XML salva il foglio di lavoro completo,
inclusi elementi come il fattore di zoom e la watchlist. È il formato di
file preferito.

#### File format of wxmx files

A `wxmx`-file seems to be a binary format, but one can handle it with tools,
which are included in your OS. It is a zip file, one can decompress it with
`unzip` (maybe rename it before, so that is recognized by the unzip program
of your OS).  We do not use the compression function, just the possibility
to merge several files into one file - images are already compressed and the
rest is simple text (probably much smaller, than huge images, which are
included).

It does contain the following files:

- `mimetype`: this file does contain the mimetype of wxMaxima files:
  `text/x-wxmathml`
- `format.txt`: a short description about wxMaxima and the wxmx file format
- Images (e.g. png, jpeg): inline plots which were produced in the wxMaxima
  session and included images.
- `content.xml`: a XML document, which contains the various cells of your
  document in XML format.

So, if something goes wrong, you can unzip a wxMaxima document (maybe rename
it before to a `zip`-file), maybe make changes in the `content.xml` file
with a text editor, or replace an broken image, zip the files again,
probably rename the `zip` to a `wxmx`-file - and you get another modified
`wxmx`-file.

## Opzioni di configurazione

Per alcune variabili di configurazione comuni _wxMaxima_ offre due modalità
di configurazione:

- La finestra di dialogo di configurazione sottostante consente di
  modificare i valori predefiniti per le sessioni correnti e successive.
- Inoltre, i valori per la maggior parte delle variabili di configurazione
  possono essere modificati per la sessione corrente solo sovrascrivendo i
  loro valori dal foglio di lavoro, come mostrato di seguito.

![Configurazione wxMaxima 1](./wxMaxima_configuration_001.png){
id=img_wxMaxima_configuration_001 }

### Frequenza di quadro animazioni predefinita

La frequenza di quadro dell'animazione usata per le nuove animazioni è
conservata nella variabile `wxanimate_framerate`. Il valore iniziale che
questa variabile conterrà in un nuovo foglio di lavoro può essere modificato
utilizzando la finestra di configurazione.

### Dimensione predefinita dei grafici per le nuove sessioni _maxima_

Al prossimo riavvio, i grafici incorporati nel foglio di lavoro verranno
creati con questa dimensione se il valore di `wxplot_size` non viene
modificato da _maxima_.

Per impostare la dimensione del grafico di un singolo grafico è possibile
utilizzare solo la seguente notazione che imposta il valore di una variabile
per un solo comando:

```maxima
wxdraw2d(
   explicit(
       x^2,
       x,-5,5
   )
), wxplot_size=[480,480]$
```

### Abbina parentesi nei controlli di testo

Questa opzione abilita due cose:

- Se viene inserita una apertura parentesi tonda, quadra o virgolette doppie
  _wxMaxima_ ne inserirà una di chiusura dopo di essa.
- Se il testo è selezionato e viene premuto uno di questi tasti, questo
  verrà inserito in una coppia di segni corrispondenti.

### Non salvare il foglio di lavoro automaticamente

Se questa opzione è impostata, il file in cui si trova il foglio di lavoro
verrà sovrascritto solo su richiesta dell'utente. Tuttavia, in caso di
arresto anomalo/interruzione dell'alimentazione/... una copia di backup
recente sarà ancora disponibile nella directory temp.

Se questa opzione non è impostata, _wxMaxima_ si comporta più come una
moderna app per cellulari:

- I file vengono salvati automaticamente all'uscita
- E il file verrà automaticamente salvato ogni 3 minuti.

### Dove viene salvata la configurazione?

If you are using Unix/Linux, the configuration information will be saved in
a file `.wxMaxima` in your home directory (if you are using wxWidgets \<
3.1.1), or `.config/wxMaxima.conf` ((XDG-Standard) if wxWidgets >= 3.1.1 is
used). You can retrieve the wxWidgets version from the command
`wxbuild_info();` or by using the menu option
Help->About. [wxWidgets](https://www.wxwidgets.org/) is the cross-platform
GUI library, which is the base for _wxMaxima_ (therefore the `wx` in the
name).  (Since the filename starts with a dot, `.wxMaxima` or `.config` will
be hidden).

Se si sta usando Windows, la configurazione verrà memorizzata nel registro
di sistema. Le voci per _wxMaxima_ sono nella seguente posizione nel
registro: `HKEY_CURRENT_USER\Software\wxMaxima`

______________________________________________________________________

# Estensioni a _Maxima_

_wxMaxima_ è principalmente un'interfaccia utente grafica per _Maxima_. In
quanto tale, il suo scopo principale è passare i comandi a _Maxima_ e
riportare i risultati dell'esecuzione di tali comandi. In alcuni casi,
tuttavia, _wxMaxima_ aggiunge funzionalità a _Maxima_. È stata menzionata la
capacità di _wxMaxima_ di generare report esportando il contenuto di una
cartella di lavoro in file HTML e LaTeX. Questa sezione descrive alcuni modi
in cui _wxMaxima_ migliora l'inclusione della grafica in una sessione.

## Variabili pedice

`wxsubscripts` specifies, if (and how) _wxMaxima_ will autosubscript
variable names:

If it is `false`, the functionality is off, wxMaxima will not autosubscript
part of variable names after an underscore.

If it is set to `'all`, everything after an underscore will be subscripted.

se `wxsubscripts` è impostato su vera i nomi delle variabili nel formato
`x_y` vengono visualizzati utilizzando un pedice se

- Either `x` or `y` is a single letter or
- `y` is an integer (can include more than one character).

![How variables are autosubscripted using wxsubscripts](./wxsubscripts.png){
id=img_wxsubscripts }

Se il nome della variabile non soddisfa questi requisiti, può comunque
essere dichiarato come "da rendere pedice" usando il comando
`wxdeclare_subscript(nome_variabile);` o
`wxdeclare_subscript([nome_variabile1,nome_variabile2,...]);` Dichiarare una
variabile pedice può essere ripristinata usando il seguente comando:
`wxdeclare_subscript(nome_variabile,false);`

You can use the menu "View->Autosubscript" to set these values.

## Feedback utente nella barra di stato

I comandi a esecuzione prolungata possono fornire feedback all'utente nella
barra di stato. Questo feedback all'utente viene sostituito da qualsiasi
nuovo feedback inserito lì (consentendo di utilizzarlo come indicatore di
avanzamento) e viene eliminato non appena il comando corrente inviato a
_Maxima_ viene terminato. È sicuro usare `wxstatusbar()` anche nelle
librerie che potrebbero essere usate semplicemente solo con _Maxima_ (invece
che con _wxMaxima_): Se _wxMaxima_ non è presente, il comando
`wxstatusbar()` non viene valutato.

```maxima
for i:1 thru 10 do (
    /* Tell the user how far we got */
    wxstatusbar(concat("Pass ",i)),
    /* (sleep n) is a Lisp function, which can be used */
    /* with the character "?" before. It delays the */
    /* program execution (here: for 3 seconds) */
    ?sleep(3)
)$
```

## Diagrammi

Fare diagrammi (che ha fondamentalmente a che fare con la grafica) è un
luogo in cui un'interfaccia utente grafica dovrà fornire alcune estensioni
al programma originale.

### Incorporare un diagramma nel foglio di lavoro

_Maxima_ normally instructs the external program _Gnuplot_ to open a
separate window for every diagram it creates. Since many times it is
convenient to embed graphs into the worksheet instead _wxMaxima_ provides
its own set of plot functions that don’t differ from the corresponding
_maxima_ functions save in their name: They are all prefixed by a “wx”.

The following plotting functions have wx-counterparts:

| wxMaxima’s plot function | Maxima’s plot function                                                                          |
| ------------------------ | ----------------------------------------------------------------------------------------------- |
| `wxplot2d()`             | [plot2d](https://maxima.sourceforge.io/docs/manual/maxima_singlepage.html#plot2d)               |
| `wxplot3d()`             | [plot3d](https://maxima.sourceforge.io/docs/manual/maxima_singlepage.html#plot3d)               |
| `wxdraw2d()`             | [draw2d](https://maxima.sourceforge.io/docs/manual/maxima_singlepage.html#draw2d)               |
| `wxdraw3d()`             | [draw2d](https://maxima.sourceforge.io/docs/manual/maxima_singlepage.html#draw3d)               |
| `wxdraw()`               | [draw](https://maxima.sourceforge.io/docs/manual/maxima_singlepage.html#draw)                   |
| `wximplicit_plot()`      | [implicit_plot](https://maxima.sourceforge.io/docs/manual/maxima_singlepage.html#implicit_plot) |
| `wxhistogram()`          | [histogram](https://maxima.sourceforge.io/docs/manual/maxima_singlepage.html#histogram)         |
| `wxscatterplot()`        | [scatterplot](https://maxima.sourceforge.io/docs/manual/maxima_singlepage.html#scatterplot)     |
| `wxbarsplot()`           | [barsplot](https://maxima.sourceforge.io/docs/manual/maxima_singlepage.html#barsplot)           |
| `wxpiechart()`           | [piechart](https://maxima.sourceforge.io/docs/manual/maxima_singlepage.html#piechart)           |
| `wxboxplot()`            | [boxplot](https://maxima.sourceforge.io/docs/manual/maxima_singlepage.html#boxplot)             |

If a `wxm`-file is read by (console) Maxima, these functions are ignored
(and printed as output, as other unknown functions in Maxima).

If you got problems with one of these functions, please check, if the
problem exists in the the Maxima function too (e.g. you got an error with
`wxplot2d()`, check the same plot in the Maxima command `plot2d()` (which
opens the plot in a separate Window)). If the problem does not disappear, it
is most likely a Maxima issue and should be reported in the [Maxima
bugtracker](https://sourceforge.net/p/maxima/bugs/). Or maybe a Gnuplot
issue.

### Rendere i diagrammi incorporati più grandi o più piccoli

Come evidenziato sopra, la finestra di dialogo di configurazione fornisce un
modo per modificare le dimensioni predefinite dei grafici creati impostando
il valore iniziale di `wxplot_size`. Le routine di graficazione di
_wxMaxima_ rispettano questa variabile che specifica la dimensione di un
grafico in pixel. Può sempre essere interrogata o utilizzata per impostare
la dimensione dei seguenti grafici:

```maxima
wxplot_size:[1200,800]$
wxdraw2d(
    explicit(
        sin(x),
        x,1,10
    )
)$
```

Se è necessario modificare la dimensione di un solo grafico, _Maxima_
fornisce un modo canonico per modificare un attributo solo per la cella
corrente. Con questo metodo di utilizzo la specifica `wxplot_size = [value1,
value2]` viene aggiunta al comando `wxdraw2d()` e non fa parte del comando
`wxdraw2d`.

wxdraw2d(
    explicit(
        sin(x),
        x,1,10
    )
),wxplot_size=[1600,800]$

Setting the size of embedded plot with `wxplot_size` works for embedded
plots using e.g. `wxplot`, `wxdraw`, `wxcontour_plot` and `wximplicit_plot`
commands and for embedded animations with `with_slider_draw` and `wxanimate`
commands.

### Grafici di qualità superiore

_Gnuplot_ doesn’t seem to provide a portable way of determining whether it
supports the high-quality bitmap output that the Cairo library provides. On
systems where _Gnuplot_ is compiled to use this library the pngCairo option
from the configuration menu (that can be overridden by the variable
`wxplot_pngcairo`) enables support for antialiasing and additional line
styles. If `wxplot_pngCairo` is set without _Gnuplot_ supporting this the
result will be error messages instead of graphics.

### Aprire grafici incorporati in finestre interattive _gnuplot_

If a plot was generated using the `wxdraw`-type commands (`wxplot2d` and
`wxplot3d` isn’t supported by this feature) and the file size of the
underlying _Gnuplot_ project isn’t way too high _wxMaxima_ offers a
right-click menu that allows to open the plot in an interactive _Gnuplot_
window.

### Apertura della console dei comandi di gnuplot in finestra `plot`

On MS Windows, there are two Gnuplot programs, `gnuplot.exe` and
`wgnuplot.exe`.  You can configure, which command should be used using the
configuration menu. `wgnuplot.exe` offers the possibility to open a console
window, where _gnuplot_ commands can be entered into, `gnuplot.exe` does not
offer this possibility. Unfortunately, `wgnuplot.exe` causes _Gnuplot_ to
"steal" the keyboard focus for a short time every time a plot is prepared.

### Incorporamento di animazioni nel foglio di calcolo

I diagrammi 3D tendono a rendere difficile la lettura dei dati
quantitativi. Una valida alternativa potrebbe essere quella di assegnare il
3° parametro alla rotellina del mouse. Il comando `with_slider_draw` è una
versione di `wxdraw2d` che prepara più grafici e consente di passare da uno
all'altro muovendo il cursore nella parte superiore dello
schermo. _wxMaxima_ consente di esportare questa animazione come gif
animata.

I primi due argomenti per `with_slider_draw` sono il nome della variabile
che si trova tra i grafici e un elenco dei valori di queste variabili. Gli
argomenti che seguono sono gli argomenti ordinari per `wxdraw2d`:

```maxima
with_slider_draw(
    f,[1,2,3,4,5,6,7,10],
    title=concat("f=",f,"Hz"),
    explicit(
        sin(2*%pi*f*x),
        x,0,1
    ),grid=true
);
```

La stessa funzionalità per i grafici 3D è accessibile come
`with_slider_draw3d`, che consente di ruotare i grafici 3D:

```maxima
wxanimate_autoplay:true;
wxanimate_framerate:20;
with_slider_draw3d(
    α,makelist(i,i,1,360,3),
    title=sconcat("α=",α),
    surface_hide=true,
    contour=both,
    view=[60,α],
    explicit(
        sin(x)*sin(y),
        x,-π,π,
        y,-π,π
    )
)$
```

Se ciò che conta è la forma generale del grafico, potrebbe bastare spostare
leggermente il grafico per rendere intuibile la sua natura 3D:

```maxima
wxanimate_autoplay:true;
wxanimate_framerate:20;
with_slider_draw3d(
    t,makelist(i,i,0,2*π,.05*π),
    title=sconcat("α=",α),
    surface_hide=true,
    contour=both,
    view=[60,30+5*sin(t)],
    explicit(
        sin(x)*y^2,
        x,-2*π,2*π,
        y,-2*π,2*π
    )
)$
```

Per coloro che hanno più familiarità con `plot` che con `draw` c'è un
secondo insieme di funzioni:

- `with_slider` e
- `wxanimate`.

Normalmente le animazioni vengono riprodotte o esportate con la frequenza di
quadro scelta nella configurazione di _wxMaxima_. Per impostare la velocità
di riproduzione di una singola animazione è possibile utilizzare la
variabile `wxanimate_framerate`:

```maxima
wxanimate(a, 10,
    sin(a*x), [x,-5,5]), wxanimate_framerate=6$
```

Le funzioni di animazione usano il comando `makelist` di _Maxima_ e quindi
condividono il problema che il valore della variabile slider viene
sostituito nell'espressione solo se la variabile è direttamente visibile
nell'espressione. Pertanto il seguente esempio fallirà:

```maxima
f:sin(a*x);
with_slider_draw(
    a,makelist(i/2,i,1,10),
    title=concat("a=",float(a)),
    grid=true,
    explicit(f,x,0,10)
)$
```

Se a _Maxima_ viene chiesto esplicitamente di sostituire il valore del
dispositivo di scorrimento, il tracciato funziona invece correttamente:

```maxima
f:sin(a*x);
with_slider_draw(
    b,makelist(i/2,i,1,10),
    title=concat("a=",float(b)),
    grid=true,
    explicit(
        subst(a=b,f),
        x,0,10
    )
)$
```

### Apertura di più grafici in più finestre contemporaneamente

Sebbene non sia fornita da _wxMaxima_, questa funzionalità di _Maxima_ (nei
setup che la supportano) a volte è utile. L'esempio seguente viene da un
post di Mario Rodriguez alla mailing list _Maxima_:

```maxima load(draw);

/* Parabola in window #1 */ draw2d(terminal=[wxt,1],explicit(x^2,x,-1,1));

/* Parabola in window #2 */ draw2d(terminal=[wxt,2],explicit(x^2,x,-1,1));

/* Paraboloid in window #3 */
draw3d(terminal=[wxt,3],explicit(x^2+y^2,x,-1,1,y,-1,1)); ```

Plotting multiple plots in the same window is possible, too (the same is
possible in command line Maxima with the standard `draw()` command):

```maxima
wxdraw(
  gr2d(
    key="sin (x)",grid=[2,2],
    explicit(sin(x),x,0,2*%pi)),
  gr2d(
    key="cos (x)",grid=[2,2],
    explicit(cos(x),x,0,2*%pi))
);
```

### The "Plot using draw" side pane

The "Plot using draw" sidebar hides a simple code generator that allows
generating scenes that make use of some of the flexibility of the _draw_
package _maxima_ comes with.

#### 2D

Generates the skeleton of a `draw()` command that draws a 2D scene. This
scene later has to be filled with commands that generate the scene’s
contents, for example by using the buttons in the rows below the "2D"
button.

One helpful feature of the 2D button is that it allows to set up the scene
as an animation in which a variable (by default it is _t_) has a different
value in each frame: Often a moving 2D plot allows easier interpretation
than the same data in a non-moving 3D one.

#### 3D

Generates the skeleton of a `draw()` command that draws a 3D scene. If
neither a 2D nor a 3D scene is set up, all of the other buttons set up a 2D
scene that contains the command the button generates.

#### Expression

Appends a standard plot of an expression like `sin(x)`, `x*sin(x)` or
`x^2+2*x-4` to the `draw()` command the cursor currently is in. If there is
no draw command a 2D scene with the plot is generated. Each scene can be
filled with any number of plots.

#### Implicit plot

Tries to find all points an expression like `y=sin(x)`, `y*sin(x)=3` or
`x^2+y^2=4` is true at and plots the resulting curve in the `draw()` command
the cursor currently is in. If there is no draw command a 2D scene with the
plot is generated.

#### Parametric plot

Steps a variable from a lower limit to an upper limit and uses two
expressions like `t*sin(t)` and `t*cos(t)` for generating the x, y (and in
3D plots also z) coordinates of a curve that is put into the current draw
command.

#### Punti

Draws many points that can optionally be joined. The coordinates of the
points are taken from a list of lists, a 2D array or one list or array for
each axis.

#### Diagram title

Draws a title on the upper end of the diagram,

#### Assi

Sets up the axis.

#### Contour

(Only for 3D plots): Adds contour lines similar to the ones one can find in
a map of a mountain to the plot commands that follow in the current `draw()`
command and/or to the ground plane of the diagram. Alternatively, this
wizard allows skipping drawing the curves entirely only showing the contour
plot.

#### Plot name

Adds a legend entry showing the next plot’s name to the legend of the
diagram. An empty name disables generating legend entries for the following
plots.

#### Line colour

Sets the line colour for the following plots the current draw command
contains.

#### Colore riempimento

Sets the fill colour for the following plots the current draw command
contains.

#### Griglia

Pops up a wizard that allows to set up grid lines.

#### Accuracy

Allows to select an adequate point in the speed vs. accuracy tradeoff that
is part of any plot program.

### Modify font and font size for plots

Especially when you use a high resolution display, the default font size
might be very small. For the `draw`-based commands, you can set the font /
font size using options like `font=...`, `font_size=...`, e.g.:

~~~maxima
wxdraw2d(
     font="Helvetica",
     font_size=30,
     explicit(sin(x),x,1,10));
~~~

For the `plot`-commands (e.g. `wxplot2d`, `wxplot3d`) font sizes and fonts
can be set using the `gnuplot_preamble` command, e.g.:

~~~maxima
wxplot2d(sin(x),[x,1,10],
         [gnuplot_preamble, "set tics font \"Arial, 30\"; set xlabel font \",20\"; set ylabel font \",20\";"]);
~~~

This sets the font for the numbers to Arial with size 30, the size for the
xlabel and ylabel font to 20 (with the default font).

Read the Maxima and Gnuplot documentation for further information.  Note:
Gnuplot seems to have issues with larger font sizes, see [wxMaxima issue
1966](https://github.com/wxMaxima-developers/wxmaxima/issues/1966).

## Embedding graphics

If the `.wxmx` file format is being used embedding files in a _wxMaxima_
project can be done as easily as per drag-and-drop. But sometimes (for
example if an image’s contents might change later on in a session) it is
better to tell the file to load the image on evaluation:

```maxima show_image("man.png"); ```

## Startup files

The config dialogue of _wxMaxima_ offers to edit two files with commands
that are executed on startup:

- A file that contains commands that are executed on starting up _Maxima_:
  `maxima-init.mac`
- one file of additional commands that are executed if _wxMaxima_ is
  starting _Maxima_: `wxmaxima-init.mac`

For example, if Gnuplot is installed in `/opt` (maybe on MacOS), you can add
`gnuplot_command:"/opt/local/bin/gnuplot"$` (or `/opt/gnuplot/bin/gnuplot`
or any other path) to these files.

These files are in the Maxima user directory (usually `%USERPROFILE%/maxima`
in Windows, `$HOME/.maxima` otherwise). The location can be found out with
the command: `maxima_userdir;`

## Special variables wx...

- `wxsubscripts` tells _Maxima_ if it should convert variable names that
  contain an underscore (`R_150` or the like) into subscripted
  variables. See `wxdeclare_subscript` for details which variable names are
  automatically converted.
- `wxfilename`: This variable contains the name of the file currently opened
  in _wxMaxima_.
- `wxdirname`: This variable contains the name the directory, in which the
  file currently opened in _wxMaxima_ is.
- `wxplot_pngcairo` tells whether _wxMaxima_ tries to use _Gnuplot_’s
  pngcairo terminal that provides more line styles and a better overall
  graphics quality.
- `wxplot_size` defines the resolution of embedded plots.
- `wxchangedir`: On most operating systems _wxMaxima_ automatically sets
  _Maxima_’s working directory to the directory of the current file. This
  allows file I/O (e.g. by `read_matrix`) to work without specifying the
  whole path to the file that has to be read or written. On Windows this
  feature sometimes causes error messages and therefore can be set to
  `false` from the config dialogue.
- `wxanimate_framerate`: The number of frames per second the following
  animations have to be played back with.
- `wxanimate_autoplay`: Automatically play animations by default?
- `wxmaximaversion`: Returns the version number of _wxMaxima_.
- `wxwidgetsversion`: Returns the wxWidgets version _wxMaxima_ is using.

## Pretty-printing 2D output

The function `table_form()` displays a 2D list in a form that is more
readable than the output from _Maxima_’s default output routine. The input
is a list of one or more lists. Like the "print" command, this command
displays output even when ended with a dollar sign. Ending the command with
a semicolon results in the same table along with a "done" statement.

```maxima
table_form(
    [
        [1,2],
        [3,4]
    ]
)$
```

As the next example shows, the lists that are assembled by the `table_form`
command can be created before the command is executed.

![A third table example](./MatrixTableExample.png){
id=img_MatrixTableExample }

Also, because a matrix is a list of lists, matrices can be converted to
tables in a similar fashion.

![Another table_form example](./SecondTableExample.png){
id=img_SecondTableExample }

## Bug reporting

_WxMaxima_ provides a few functions that gather bug reporting information
about the current system:

- `wxbuild_info()` gathers information about the currently running version
  of _wxMaxima_
- `wxbug_report()` tells how and where to file bugs


## Marking output being drawn in red

_Maxima_’s `box()` command causes _wxMaxima_ to print its argument with a
red foreground, if the second argument to the command is the text
`highlight`.

## Output rendering.

With `set_display()` one can set, how wxMaxima will render the output.

`set_display('xml)` is the default value. Here Maxima speaks to wxMaxima
using an (machine readable) XML-dialect (can be seen in the "Raw XML
sidebar") and outputs the resulting formulas nicely rendered, e.g. pretty
Matrices, Square root signs, fractions, etc.

<!--- Currently that does not work as it should, the line with the output
label is shifted right (issue: #2006) --> `set_display('ascii)` causes
wxMaxima to output formulas as in command line Maxima - as ASCII-Art.

`set_display('none)` causes 'one-line' ASCII results - the same as the
command line Maxima command `display2d:false;` does.

# Help menu

WxMaxima’s help menu provides access to the Maxima and wxMaxima manual,
tips, some example worksheets and in command line Maxima included demos (the
`demo()` command).

Please notice, that the demos write:

~~~ At the ’_’ prompt, type ’;’ and <enter> to proceed with the
demonstration.  ~~~

That is valid for command-line Maxima, however in wxMaxima by default it is
necessary to continue the demonstration with:
<kbd>CTRL</kbd>+<kbd>ENTER</kbd>

(That can be configured in the Configure->Worksheet->"Hotkeys for sending
commands to Maxima" menu.)

______________________________________________________________________

# Risoluzione dei problemi

## Estensioni a _Maxima_

Since _Maxima_ (the program that does the actual mathematics) and _wxMaxima_
(providing the easy-to-use user interface) are separate programs that
communicate by the means of a local network connection. Therefore the most
probable cause is that this connection is somehow not working. For example,
a firewall could be set up in a way that it doesn’t just prevent
unauthorized connections from the internet (and perhaps intercept some
connections to the internet, too), but also blocks
inter-process-communication inside the same computer. Note that since
_Maxima_ is being run by a Lisp processor the process communication that is
blocked does not necessarily have to be named "maxima". Common names of the
program that opens the network connection would be sbcl, gcl, ccl, lisp.exe,
or similar names.

On Unix computers another possible reason would be that the loopback network
that provides network connections between two programs in the same computer
isn’t properly configured.

## How to save data from a broken .wxmx file

Internally most modern XML-based formats are ordinary zip files. _WxMaxima_
doesn’t turn on compression, so the contents of `.wxmx` files can be viewed
in any text editor.

If the zip signature at the end of the file is still intact after renaming a
broken `.wxmx` file to `.zip` most operating systems will provide a way to
extract any portion of the information that is stored inside it. This can be
done when there is a need of recovering the original image files from a text
processor document. If the zip signature isn’t intact that does not need to
be the end of the world: If _wxMaxima_ during saving detected that something
went wrong there will be a `.wxmx~` file whose contents might help.

And even if there isn’t such a file: The `.wxmx` file is a container format
and the XML portion is stored uncompressed. It it is possible to rename the
`.wxmx` file to a `.txt` file and to use a text editor to recover the XML
portion of the file’s contents (it starts with `<?xml version="1.0"
encoding="UTF-8"?>` and ends with `</wxMaximaDocument>`. Before and after
that text you will see some unreadable binary contents in the text editor).

If a text file containing only these contents (e.g. copy and paste this text
into a new file) is saved as a file ending in `.xml`, _wxMaxima_ will know
how to recover the text from the document.

## I want some debug info to be displayed on the screen before my command
has finished

Normally _wxMaxima_ waits for the whole 2D formula to be transferred before
it begins to typeset. This saves time for making many attempts to typeset a
only partially completed equation. There is a `disp` command, though, that
will provide debug output immediately and without waiting for the current
_Maxima_ command to finish:

```maxima
for i:1 thru 10 do (
   disp(i),
   /* (sleep n) is a Lisp function, which can be used */
   /* with the character "?" before. It delays the */
   /* program execution (here: for 3 seconds) */
   ?sleep(3)
)$
```

Alternatively one can look for the `wxstatusbar()` command above.

## Plotting only shows a closed empty envelope with an error message

This means that _wxMaxima_ could not read the file _Maxima_ that was
supposed to instruct _Gnuplot_ to create.

Possible reasons for this error are:

- The plotting command is part of a third-party package like `implicit_plot`
  but this package was not loaded by _Maxima_’s `load()` command before
  trying to plot.
- _Maxima_ tried to do something the currently installed version of
  _Gnuplot_ isn’t able to understand. In this case, a file ending in
  `.gnuplot` located in the directory, which _Maxima_’s variable
  `maxima_userdir` is pointing, contains the instructions from _Maxima_ to
  _Gnuplot_. Most of the time, this file’s contents therefore are helpful
  when debugging the problem.
- Gnuplot was instructed to use the pngCairo library that provides
  antialiasing and additional line styles, but it was not compiled to
  support this possibility. Solution: Uncheck the "Use the Cairo terminal
  for the plot" checkbox in the configuration dialog and don’t set
  `wxplot_pngcairo` to true from _Maxima_.
- Gnuplot didn’t output a valid `.png` file.

## Plotting an animation results in “error: undefined variable”

The value of the slider variable by default is only substituted into the
expression that is to be plotted if it is visible there. Using a `subst`
command that substitutes the slider variable into the equation to plot
resolves this problem. At the end of section [Embedding animations into the
spreadsheet](#embedding-animations-into-the-spreadsheet), you can see an
example.

## I lost cell content and undo doesn’t remember

There are separate undo functions for cell operations and for changes inside
of cells so chances are low that this ever happens. If it does there are
several methods to recover data:

- _WxMaxima_ actually has two undo features: The global undo buffer that is
  active if no cell is selected and a per-cell undo buffer that is active if
  the cursor is inside a cell. It is worth trying to use both undo options
  in order to see if an old value can still be accessed.
- If you still have a way to find out what label _Maxima_ has assigned to
  the cell just type in the cell’s label and its contents will reappear.
- If you don’t: Don’t panic. In the “View” menu there is a way to show a
  history pane that shows all _Maxima_ commands that have been issued
  recently.
- If nothing else helps _Maxima_ contains a replay feature:

```maxima playback(); ```

## _WxMaxima_ starts up with the message “Maxima process terminated.”

One possible reason is that _Maxima_ cannot be found in the location that is
set in the “Maxima” tab of _wxMaxima_’s configuration dialog and therefore
won’t run at all. Setting the path to a working _Maxima_ binary should fix
this problem.

## Maxima is forever calculating and not responding to input

It is theoretically possible that _wxMaxima_ doesn’t realize that _Maxima_
has finished calculating and therefore never gets informed it can send new
data to _Maxima_. If this is the case “Trigger evaluation” might
resynchronize the two programs.

## My SBCL-based _Maxima_ runs out of memory

The Lisp compiler SBCL by default comes with a memory limit that allows it
to run even on low-end computers. When compiling a big software package like
Lapack or dealing with extremely big lists of equations this limit might be
too low. In order to extend the limits, SBCL can be provided with the
command line parameter `--dynamic-space-size` that tells SBCL how many
megabytes it should reserve. A 32bit Windows-SBCL can reserve up to 999
Megabytes. A 64-bit SBCL version running on Windows can be instructed to use
more than the about 1280 Megabytes compiling Lapack needs.

One way to provide _Maxima_ (and thus SBCL) with command line parameters is
the "Additional parameters for Maxima" field of _wxMaxima_’s configuration
dialogue.

![sbcl memory](./sbclMemory.png){ id=img_sbclMemory }

## Input sometimes is sluggish/ignoring keys on Ubuntu

Installing the package `ibus-gtk` should resolve this issue. See
([https://bugs.launchpad.net/ubuntu/+source/wxwidgets3.0/+bug/1421558](https://bugs.launchpad.net/ubuntu/+source/wxwidgets3.0/+bug/1421558))
for details.

## _WxMaxima_ halts when _Maxima_ processes Greek characters or Umlauts

If your _Maxima_ is based on SBCL the following lines have to be added to
your `.sbclrc`:

```commonlisp (setf sb-impl::*default-external-format* :utf-8)  ```

The folder where this file has to be placed is system- and
installation-specific. But any SBCL-based _Maxima_ that already has
evaluated a cell in the current session will happily tell where it can be
found after getting the following command:

``` :lisp (sb-impl::userinit-pathname)  ```

## Note concerning Wayland (recent Linux/BSD distributions)

There seem to be issues with the Wayland Display Server and wxWidgets.
WxMaxima may be affected, e.g. that sidebars are not moveable.

You can either disable Wayland and use X11 instead (globally)  or just tell,
that wxMaxima should use the X Window System by setting: `GDK_BACKEND=x11`

E.g. start wxMaxima with:

`GDK_BACKEND=x11 wxmaxima`

## Why is the integrated manual browser not offered on my Windows PC?

Either wxWidgets wasn’t compiled with support for Microsoft’s webview2 or
Microsoft’s webview2 isn’t installed.

## Why is the external manual browser not working on my Linux box?

The HTML browser might be a snap, flatpack or appimage version. All of these
typically cannot access files that are installed on your local
system. Another reason might be that maxima or wxMaxima is installed as a
snap, flatpack or something else that doesn’t give the host system access to
its contents. A third reason might be that the maxima HTML manual isn’t
installed and the online one cannot be accessed.

## Can I make _wxMaxima_ output both image files and embedded plots at once?

The worksheet embeds `.png files. _WxMaxima_ allows the user to specify
where they should be generated:

```maxima
wxdraw2d(
    file_name="test",  /* extension .png automatically added */
    explicit(sin(x),x,1,10)
);
```

If a different format is to be used, it is easier to generate the images and
then import them into the worksheet again:

```maxima
load("draw");
pngdraw(name,[contents]):=
(
    draw(
        append(
            [
                terminal=pngcairo,
                dimensions=wxplot_size,
                file_name=name
            ],
            contents
        )
    ),
    show_image(printf(false,"~a.png",name))
);
pngdraw2d(name,[contents]):=
    pngdraw(name,gr2d(contents));

pngdraw2d("Test",
        explicit(sin(x),x,1,10)
);
```

## Can I set the aspect ratio of an embedded plot?

Use the variable `wxplot_size`:

```maxima
wxdraw2d(
    explicit(sin(x),x,1,10)
),wxplot_size=[1000,1000];
```

## After upgrading to MacOS 13.1 plot and/or draw commands output error
messages like

``` 1 HIToolbox 0x00007ff80cd91726
_ZN15MenuBarInstance22EnsureAutoShowObserverEv + 102 2 HIToolbox
0x00007ff80cd912b8 _ZN15MenuBarInstance14EnableAutoShowEv + 52 3 HIToolbox
0x00007ff80cd35908 SetMenuBarObscured + 408 ...  ```

This might be an issue with the operating system. Disable the hiding of the
menu bar (SystemSettings => Desktop & Dock => Menu Bar) might solve the
issue. See [wxMaxima issue
#1746](https://github.com/wxMaxima-developers/wxmaxima/issues/1746) for more
information.

## Logging

Log messages might be helpful to debug problems. WxMaxima can log many
events. Most log entries will be helpful for developers, especially in case
of problems or bugs. If you run a "Release"-Build, the log windows is not
shown by default, if you run a development version, it is shown by default
as a second window. You can enable and disable this window using the
"View->Toggle log window" menu entry.

Messages are not 'lost', if the log window is not shown, if you select to
show the log window later, you will see past log messages (if you did not
clear the messages).

Such messages may be helpful, when you create bug reports (or trying to find
a bug by yourself).

Log messages can (additionally) be printed to STDERR, when using the command
line option "--logtostderr". On Windows a separate text console will be
opened, as a Windows GUI application does not have the standard IO
connected.

______________________________________________________________________

# FAQ

## Is there a way to make more text fit on a LaTeX page?

Yes. Use the [LaTeX package "geometry"](https://ctan.org/pkg/geometry) to
specify the size of the borders.

You can add the following line to the LaTeX preamble (for example by using
the respective field in the config dialogue ("Export"->"Additional lines for
the TeX preamble"), to set borders of 1cm):

```latex \usepackage[left=1cm,right=1cm,top=1cm,bottom=1cm]{geometry} ```

## Is there a dark mode?

If wxWidgets is new enough, _wxMaxima_ will automatically be in dark mode if
the rest of the operating system is. The worksheet itself is by default
equipped with a bright background. But it can be configured
otherwise. Alternatively, there is a `View/Invert worksheet brightness` menu
entry that allows to quickly convert the worksheet from dark to bright and
vice versa.

## _WxMaxima_ sometimes hangs for several seconds once in the first minute

_WxMaxima_ delegates some big tasks like parsing _Maxima_’s
>1000-page-manual to background tasks, which normally goes totally
unnoticed. At the moment the result of such a task is needed, though, it is
possible that _wxMaxima_ needs to wait a couple of seconds before it can
continue its work.

## Especially when testing new locale settings, a message box "locale
’xx_YY’ can not be set" occurs

![Locale warning](./locale-warning.png){ id=img_locale_warning}

(The same problem can occur with other applications too). The translations
seem okay after you click on ’OK’. WxMaxima does not only use its own
translations but the translations of the wxWidgets framework too.

These locales maybe not present in the system. On Ubuntu/Debian systems they
can be generated using: `dpkg-reconfigure locales`

## How can I use symbols for real numbers, natural numbers (ℝ, ℕ), etc.?

You can find these symbols in the Unicode sidebar (search for ’double-struck
capital’). But the selected font must also support these symbols. If they do
not display properly, select another font.

## How can a Maxima script determine, if it is running under wxMaxima or
command line Maxima?

If wxMaxima is used, the Maxima variable `maxima_frontend` is set to
`wxmaxima`. The Maxima variable `maxima_frontend_version` contains the
wxMaxima version in this case.

If no frontend is used (you are using command line Maxima), these variables
are `false`.

______________________________________________________________________

# Argomenti della riga di comando

Usually you can start programs with a graphical user interface just by
clicking on a desktop icon or desktop menu entry. WxMaxima - if started from
the command line - still provides some command-line switches, though.

- `-v` or `--version`: Output the version information
- `-h` or `--help`: Output a short help text
- `-o` or `--open=<str>`: Open the filename given as an argument to this
  command-line switch
- `-e` or `--eval`: Evaluate the file after opening it.
- `-b` or `--batch`: If the command-line opens a file all cells in this file
  are evaluated and the file is saved afterward. This is for example useful
  if the session described in the file makes _Maxima_ generate output
  files. Batch-processing will be stopped if _wxMaxima_ detects that
  _Maxima_ has output an error and will pause if _Maxima_ has a question:
  Mathematics is somewhat interactive by nature so a completely
  interaction-free batch processing cannot always be guaranteed.
- `--logtostderr`:                 Log all "debug messages" sidebar messages to stderr, too.
- `--pipe`:                        Pipe messages from Maxima to stdout.
- `--exit-on-error`:               Close the program on any maxima error.
- `-f` or `--ini=<str>`: Use the init file that was given as an argument to this command-line switch
- `-u`, `--use-version=<str>`:     Use maxima version `<str>`.
- `-l`, `--lisp=<str>`:              Use a Maxima compiled with Lisp compiler `<str>`.
- `-X`, `--extra-args=<str>`:        Allows to specify extra Maxima arguments
- `-m` or `--maxima=<str>`:    allows specifying the location of the _maxima_ binary
- `--enableipc`: Lets Maxima control wxMaxima via interprocess communications. Use this option with care.
- `--wxmathml-lisp=<str>`:   Location of wxMathML.lisp (if not the built-in should be used, mainly for developers).

Instead of a minus, some operating systems might use a dash in front of the
command-line switches.

______________________________________________________________________

# About the program, contributing to wxMaxima

wxMaxima is mainly developed using the programming language C++ using the
[wxWidgets framework](https://www.wxwidgets.org), as build system we use
[CMake](https://www.cmake.org), a small part is written in Lisp. You can
contribute to wxMaxima, join the wxMaxima project at
<https://github.com/wxMaxima-developers/wxmaxima>, if you have knowledge of
these programming languages and want to help and contribute to the open
source project wxMaxima.

But not only programmers are necessary! You can also contribute to wxMaxima,
if you help to improve the documentation, find and report bugs (and maybe
bugfixes), suggest new features, help to translate wxMaxima or the manual to
your language (read the README.md in the [locale
subdirectory](https://github.com/wxMaxima-developers/wxmaxima/tree/main/locales)
how wxMaxima and the manual can be translated).

Or answer questions of other users in the discussion forum.

The source code of wxMaxima is documented using Doxygen
[here](https://wxmaxima-developers.github.io/wxmaxima/Doxygen-documentation/).

The program is nearly self-contained, so except for system libraries (and
the wxWidgets library), no external dependencies (like graphic files or the
Lisp part (the `wxmathML.lisp`-file) is needed, these files are included in
the executable.

If you are a developer, you might want to try out a modified
`wxmathML.lisp`-file without recompiling everything, one can use the command
line option `--wxmathml-lisp=<str>` to use another Lisp file, not the
included one.
