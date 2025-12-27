# Manuel de l'utilisateur de wxMaxima

WxMaxima est une interface graphique (GUI) pour le système de calcul formel
(CAS) _Maxima_.  WxMaxima permet d’utiliser toutes les fonctions de
_Maxima_. De plus, il fournit des assistants pratiques pour accéder aux
fonctionnalités les plus couramment utilisées. Ce manuel décrit certaines
des caractéristiques qui font de wxMaxima l’une des interfaces graphiques
les plus populaires pour _Maxima_.

![wxMaxima logo](./wxMaximaLogo.png){ id=img_wxMaximaLogo }

______________________________________________________________________

# Présentation de wxMaxima

## _Maxima_ et wxMaxima

Dans le domaine open-source, les grands systèmes sont généralement divisés
en projets plus petits, plus faciles à gérer pour de petits groupes de
développeurs. Par exemple, un programme de gravure de CD peut se composer
d’un outil en ligne de commande qui effectue réellement la gravure du CD, et
d’une interface graphique permettant aux utilisateurs de l’utiliser sans
avoir à apprendre toutes les options de la ligne de commande, voire sans
jamais y recourir. Un avantage de cette méthode est que le travail de
développement réalisé dans le programme en ligne de commande peut être
partagé par de nombreuses applications : le même programme de gravure en
ligne de commande peut servir de module "Envoyer vers un CD",  pour un
gestionnaire de fichiers, de fonction "Graver sur CD" pour un lecteur
multimédia, ou encore de graveur de CD pour un outil de sauvegarde sur
DVD. Un autre avantage est que la division d’une tâche complexe en parties
plus réduites permet aux développeurs de proposer plusieurs interfaces
utilisateur pour un même programme.

Un système de calcul formel (CAS) comme _Maxima_ s'intègre dans ce cadre. Un
CAS peut fournir la logique derrière une application de type calculatrice à
précision arbitraire ou effectuer des transformations automatiques de
formules en arrière-plan d'un système plus large (par exemple,
[Sage](https://www.sagemath.org/)). Alternativement, il peut être utilisé
directement comme un logiciel autonome. _Maxima_ peut être accessible via
une ligne de commande. Cependant, une interface comme _wxMaxima_ s'avère
souvent un moyen plus efficace d'accéder au logiciel, surtout pour les
nouveaux utilisateurs.

### _Maxima_

_Maxima_ est un système complet de calcul formel (CAS). Un CAS est un
programme capable de résoudre des problèmes mathématiques en manipulant des
formules et en retournant des expressions algébriques qui résolvent le
problème, plutôt que de simplement fournir une valeur numérique du
résultat. Autrement dit, _Maxima_ peut fonctionner comme une calculatrice
qui donne  des valeurs numériques des variables, mais il peut aussi fournir
des solutions analytiques formelles. De plus, il propose une gamme de
méthodes numériques pour analyser des équations ou des systèmes d’équations
qui ne peuvent pas être résolus de manière formelle.

![Copie d'écran Maxima, ligne de commande](./maxima_screenshot.png){
id=img_maxima_screenshot }

Une documentation complète pour _Maxima_ est [disponible en
ligne](https://maxima.sourceforge.io/documentation.html). Une partie de
cette documentation est également accessible via le menu d’aide de
wxMaxima. En appuyant sur la touche d’aide (généralement la touche
<kbd>F1</kbd> sur la plupart des systèmes), la fonction d’aide contextuelle
de _wxMaxima_ ouvre automatiquement la page du manuel de _Maxima_
correspondant à la commande située sous le curseur.

### WxMaxima

_WxMaxima_ est une interface graphique qui offre toute les fonctionnalités
et la flexibilité de _Maxima_. WxMaxima propose aux utilisateurs un
affichage graphique ainsi que de nombreuses fonctionnalités qui facilitent
l’utilisation de _Maxima_. Par exemple, _wxMaxima_ permet d’exporter le
contenu de n’importe quelle cellule (ou, si nécessaire, une partie d’une
formule) sous forme de texte, de code LaTeX ou de codage MathML, simplement
en effectuant un clic droit. En outre, un notebook entier peut être exporté,
soit en tant que fichier HTML, soit en tant que fichier LaTeX. La
documentation de _wxMaxima_, incluant des notebooks d’exemples illustrant
ses différentes fonctionnalités, est disponible en ligne sur le [site d’aide
de wxMaxima](https://wxMaxima-developers.github.io/wxmaxima/help.html),
ainsi que via le menu d’aide.

![fenêtre de wxMaxima](./wxMaximaWindow.png){ id=img_wxMaximaWindow }

Les calculs saisis dans _wxMaxima_ sont exécutés en arrière-plan par l'outil
en ligne de commande _Maxima_.

## Notions de base d'un notebook

Une grande partie de wxMaxima est intuitive, mais certains détails
nécessitent une attention particulière. [Ce
site](https://wxMaxima-developers.github.io/wxmaxima/help.html) contient
plusieurs notebooks qui abordent divers aspects de _wxMaxima_. Parcourir
certains d’entre eux (notamment le « tutoriel de 10 minutes sur
(wx)Maxima ») permettra de mieux maîtriser à la fois le contenu de _Maxima_
et l’utilisation de _wxMaxima_ pour interagir avec _Maxima_. Ce manuel se
concentre sur la description des aspects de _wxMaxima_ qui ne sont pas
nécessairement évidents et qui pourraient ne pas être présentés dans les
ressources en ligne.

### Fondamentaux du notebook

L'une des très rares choses qui ne sont pas standard avec _wxMaxima_ est
qu'il organise les données pour _Maxima_ en cellules qui ne sont évaluées
(c'est-à-dire envoyées à _Maxima_) que lorsque l'utilisateur le
demande. Lorsqu'une cellule est évaluée, toutes les commandes de cette
cellule, et uniquement de cette cellule, sont évaluées en bloc. (La phrase
précédente n'est pas tout à fait exacte : on peut sélectionner un ensemble
de cellules adjacentes et les évaluer ensemble. On peut aussi demander à
_Maxima_ d'évaluer toutes les cellules d'un notebook  en une seule fois.)
L'approche de _wxMaxima_ pour exécuter les commandes peut sembler
inhabituelle au premier abord. Elle facilite cependant grandement le travail
avec de grands documents (où l'utilisateur ne souhaite pas que chaque
modification déclenche automatiquement une réévaluation complète de
l'ensemble du document). De plus, cette approche est très pratique pour le
débogage.

Si du texte est saisi dans _wxMaxima_, une nouvelle cellule est
automatiquement créée dans le notebook. Le type de cette cellule peut être
sélectionné dans la barre d'outils. Si une cellule de code est créée, elle
peut être envoyée à _Maxima_, ce qui affiche le résultat du calcul en
dessous du code. Une paire de ce type de commandes est présentée ci-dessous.

![Cellules entrée/sortie cell](./InputCell.png){ id=img_InputCell }

Lors de l’évaluation du contenu d’une cellule d’entrée, _Maxima_ attribue un
identifiant à l’entrée (affiché par défaut en rouge et reconnaissable par le
`%i`), ce qui permet d’y faire référence plus tard dans la session
_wxMaxima_. La sortie générée par _Maxima_ reçoit également un identifiant
commençant par `%o`, masqué par défaut, sauf si l’utilisateur attribue un
nom à cette sortie. Dans ce cas, l’identifiant défini par l’utilisateur
s’affiche par défaut, mais l’identifiant automatique de type `%o` reste
accessible.

Outre les cellules d’entrée, _wxMaxima_ permet d’insérer des cellules de
texte pour la documentation, des cellules d’image, des cellules de titre, de
chapitre et de section. Chaque cellule dispose de son propre historique
d’annulation, ce qui facilite le débogage en modifiant les valeurs de
plusieurs cellules puis en annulant progressivement les changements
inutiles. De plus, le notebook en lui-même possède un historique
d’annulation global qui permet d’annuler les modifications, ajouts et
suppressions de cellules.

La figure ci-dessous montre différents types de cellules (cellules de titre,
cellules de section, cellules de sous-section, cellules de texte, cellules
d’entrée/sortie et cellules d’image).

![Exemple de cellules différentes de wxMaxima](./cell-example.png){
id=img_cell-example }

### Cellules

Le notebook est organisé en cellules. WxMaxima distingue les types de
cellules suivants  :

- Math cells, containing one or more lines of _Maxima_ input.
- Output of, or a question from, _Maxima_.
- Image cells.
- Text cells, that can for example be used for documentation.
- A title, section or a subsection. 6 levels of different headings are
  possible.
- Page breaks.

Par défaut, _wxMaxima_ crée automatiquement une cellule de calcul lorsque du
texte est saisi. Pour créer des cellules d’un autre type, utilisez le menu
Cellule, les raccourcis clavier indiqués dans ce menu ou la liste déroulante
de la barre d’outils. Une fois qu’une cellule non mathématique est créée,
tout ce qui y est saisi est interprété comme du texte.

Un [commentaire (style
C)](https://maxima.sourceforge.io/docs/manual/maxima_singlepage.html#Comments)
peut être inséré dans une cellule de calcul comme suit : `/* Ce commentaire
sera ignoré par Maxima */`

"`/*`" indique le début du commentaire, "`*/`" la fin.

### curseurs horizontaux et verticaux

Si l’utilisateur tente de sélectionner une phrase complète, un traitement de
texte étendra automatiquement la sélection pour qu’elle commence et se
termine aux limites d’un mot. De même, si plusieurs cellules sont
sélectionnées, _wxMaxima_ étendra la sélection aux cellules entières.

Ce qui est moins conventionnel, c’est que _wxMaxima_ offre une facilité de
glisser-déposer en définissant deux types de curseurs. _wxMaxima_ basculera
automatiquement entre eux selon le besoin :

- The cursor is drawn horizontally if it is moved in the space between two
  cells or by clicking there.
- A vertical cursor that works inside a cell. This cursor is activated by
  moving the cursor inside a cell using the mouse pointer or the cursor keys
  and works much like the cursor in a text editor.

Quand vous lancez wxMaxima, vous ne verrez que le curseur horizontal
clignotant. Si vous commencez à taper, une cellule mathématique sera
automatiquement créée et le curseur deviendra un curseur vertical classique
(vous verrez une flèche vers la droite comme "invite", et après avoir évalué
la cellule mathématique (<kbd>CTRL</kbd>+<kbd>ENTRÉE</kbd>), les étiquettes
apparaîtront, par exemple `(%i1)`, `(%o1)`.

![curseur horizontal (clignotant) après le démarrage de
wxMaxima](./horizontal-cursor-only.png){ id=img_horizontal_cursor_only }

Vous pourrez peut-être vouloir créer un type de cellule différent (via le
menu "Cellule"), comme une cellule de titre ou une cellule de texte, pour
décrire ce que vous allez faire lorsque vous commencerez à créer votre
notebook.

Si vous naviguez entre les différentes cellules, vous verrez également le
curseur horizontal (clignotant), indiquant l'endroit où vous pouvez insérer
une nouvelle cellule dans votre notebook (soit une cellule mathématique, en
commençant simplement à saisir votre formule, soit un autre type de cellule
en utilisant le menu).

![curseur horizontal (clignotant) entre deux
cellules](./horizontal-cursor-between-cells.png){
id=img_horizontal_cursor_between_cells }

### Envoyer des cellules à Maxima

La commande dans une cellule de code est exécutée en appuyant une fois sur
<kbd>CTRL</kbd>+<kbd>ENTRÉE</kbd>, <kbd>MAJ</kbd>+<kbd>ENTRÉE</kbd> ou la
touche <kbd>ENTRÉE</kbd> du pavé numérique. Par défaut, _wxMaxima_ exécute
les commandes lorsque <kbd>CTRL</kbd>+<kbd>ENTRÉE</kbd> ou
<kbd>MAJ</kbd>+<kbd>ENTRÉE</kbd> est pressé, mais il est possible de
configurer _wxMaxima_ pour qu’il exécute les commandes en réponse à la
touche <kbd>ENTRÉE</kbd>.

### Complétion automatique des commandes

_wxMaxima_ dispose d’une fonction de complétion automatique, accessible via
le menu (Cellule/Compléter le mot) ou en appuyant sur la combinaison de
touches <kbd>CTRL</kbd>+<kbd>ESPACE</kbd>. Cette complétion est sensible au
contexte : par exemple, si elle est activée lors d'une spécification d’unité
pour ezUnits, elle proposera une liste des unités applicables.

![ezUnits](./ezUnits.png){ id=img_ezUnits }

Outre la complétion des noms de fichiers, d’unités, de commandes ou de
variables, la complétion automatique peut aussi afficher un modèle pour la
plupart des commandes, indiquant le type (et la signification) des
paramètres attendus par le programme. Pour activer cette fonctionnalité,
appuyez sur <kbd>MAJ</kbd>+<kbd>CTRL</kbd>+<kbd>ESPACE</kbd> ou sélectionnez
l’option correspondante dans le menu (Cellule/Afficher le modèle).

#### Caractères grecs

Traditionnellement, les ordinateurs stockaient les caractères sur 8 bits, ce
qui permet un maximum de 256 caractères différents. Toutes les lettres,
chiffres et symboles de contrôle (fin de transmission, fin de chaîne, lignes
et bordures pour dessiner des rectangles autour des menus, etc.) de presque
n'importe quelle langue pouvaient tenir avec cette limite.

Pour la plupart des pays, la page de codes de 256 caractères choisie
n'inclut cependant pas des éléments comme les lettres grecques, pourtant
fréquemment utilisées en mathématiques. Pour surmonter ce type de
limitation, [Unicode](https://home.unicode.org/) a été inventé : il s'agit
d'un encodage qui permet aux textes en anglais de s'afficher normalement,
tout en utilisant bien plus que 256 caractères.

_Maxima_ prend en charge Unicode s'il a été compilé avec un compilateur Lisp
qui supporte Unicode ou qui ne tient pas compte de l'encodage de la
police. Comme au moins une de ces deux conditions est probablement remplie,
_wxMaxima_ fournit une méthode pour saisir des caractères grecs à l'aide du
clavier :

- A Greek letter can be entered by pressing the <kbd>ESC</kbd> key and then
  starting to type the Greek character’s name.
- Alternatively it can be entered by pressing <kbd>ESC</kbd>, one letter (or
  two for the Greek letter omicron) and <kbd>ESC</kbd> again. In this case
  the following letters are supported:

| touche | Lettre grecque | touche | Lettre grecque | touche | Lettre grecque |
| :-: | :----------: | :-: | :----------: | :-: | :----------: |
|  a  |    alpha     |  i  |     iota     |  r  |     rho      |
|  b  |     beta     |  k  |    kappa     |  s  |    sigma     |
|  g  |    gamma     |  l  |    lambda    |  t  |     tau      |
|  d  |    delta     |  m  |      mu      |  u  |   upsilon    |
|  e  |   epsilon    |  n  |      nu      |  f  |     phi      |
|  z  |     zeta     |  x  |      xi      |  c  |     chi      |
|  h  |     eta      | om  |   omicron    |  y  |     psi      |
|  q  |    theta     |  p  |      pi      |  o  |    omega     |
|  A  |    Alpha     |  I  |     Iota     |  R  |     Rho      |
|  B  |     Beta     |  K  |    Kappa     |  S  |    Sigma     |
|  G  |    Gamma     |  L  |    Lambda    |  T  |     Tau      |
|  D  |    Delta     |  M  |      Mu      |  U  |   Upsilon    |
|  E  |   Epsilon    |  N  |      Nu      |  P  |     Phi      |
|  Z  |     Zeta     |  X  |      Xi      |  C  |     Chi      |
|  H  |     Eta      | Om  |   Omicron    |  Y  |     Psi      |
|  T  |    Theta     |  P  |      Pi      |  O  |    Omega     |

Vous pouvez également utiliser la barre latérale « Lettres grecques » pour
insérer les lettres grecques.

##### Attention aux caractères similaires

Plusieurs lettres latines ressemblent aux lettres grecques, par exemple la
lettre latine "A" et la lettre grecque "Alpha". Bien qu'elles aient la même
apparence, ce sont deux caractères Unicode distincts, représentés par des
codages Unicode (nombres) différents.

Cela peut poser problème si vous attribuez une valeur à la variable A et que
vous utilisez ensuite la lettre grecque Alpha pour faire quelque chose avec
cette variable, notamment sur les impressions. Pour la lettre grecque mu
(utilisée aussi comme préfixe pour micro), il existe également deux codages
Unicode différents.

La barre latérale « Lettres grecques » offre donc l’option de désactiver les
caractères similaires (ce paramètre peut être modifié via un menu accessible
par un clic droit).

Le même mécanisme permet également de saisir divers symboles mathématiques :

Touches à saisir | symbole mathématique                                            |
|---------------|-----------------------------------------------------------------|
| hbar           | Constante de Planck : un h avec une barre horizontale au-dessus |
| Hbar           | un H avec une barre horizontale au-dessus                       |
| 2              | au carré                                                        |
| 3              | à la puissance trois                                            |
| /2             | 1/2                                                             |
| partial        | symbole de dérivation partielle (le d de dx/dt)                 |
| integral       | symbole intégrale                                               |
| sq             | racine carrée                                                   |
| ii             | imaginaire                                                      |
| ee             | appartient à                                                    |
| in             | dans                                                            |
| impl implies   | implique                                                        |
| inf            | infini                                                          |
| empty          | ensemble vide                                                   |
| TB             | grand triangle vers la droite                                   |
| tb             | petit triangle vers la droite                                   |
| and            | et                                                              |
| or             | ou                                                              |
| xor            | ou exclusif                                                     |
| nand           | et non                                                          |
| nor            | ni                                                              |
| equiv          | équivalent à                                                    |
| not            | non                                                             |
| union          | union                                                           |
| inter          | intersection                                                    |
| subseteq       | sous-ensemble ou égal                                           |
| subset         | sous-ensemble                                                   |
| notsubseteq    | n'est pas un  sous-ensemble ou égal                             |
| notsubset      | n'est pas un sous-ensemble                                      |
| approx         | approximativement                                               |
| propto         | proportionnel à                                                 |
| neq != /= ou # | différent de                                                    |
| +/- ou pm      | signe plus/moins                                                |
| \<= ou leq     | inférieur ou égal à                                             |
| >= ou geq      | supérieur ou égal à                                             |
| \<\< ou ll     | beaucoup plus petit que                                         |
| >> ou gg       | beaucoup plus grand que                                         |
| qed            | fin de preuve                                                   |
| nabla          | opérateur nabla                                                 |
| sum            | symbole somme                                                   |
| prod           | symbole produit                                                 |
| exists         | il existe                                                       |
| nexists        | il n'existe pas                                                 |
| parallel       | symbole parallèle                                               |
| perp           | symbole perpendiculaire                                         |
| leadsto        | symbole mène à                                                  |
| ->             | flèche vers la droite                                           |
| -->            | longue flèche vers la droite                                    |

Vous pouvez aussi utiliser la barre latérale « Symboles » pour saisir ces
symboles mathématiques.

Si un symbole spécial ne figure pas dans la liste, il est possible de saisir
des caractères Unicode arbitraires en appuyant sur <kbd>ÉCHAP</kbd> \[numéro
du caractère (en hexadécimal)\] <kbd>ÉCHAP</kbd>. De plus, la barre latérale
« Symboles » dispose d’un menu accessible par un clic droit, qui permet
d’afficher une liste de tous les symboles Unicode disponibles que l’on peut
ajouter à cette barre d’outils ou insérer dans le notebook.

<kbd>ESC</kbd><kbd>6</kbd><kbd>1</kbd><kbd>ESC</kbd>  est une séquence qui
produit un `a`.

Veuillez noter que la plupart de ces symboles (à l'exception notable des
symboles logiques) n'ont pas de signification particulière dans _Maxima_ et
seront donc interprétés comme des caractères ordinaires. Si _Maxima_ est
compilé avec un Lisp qui ne supporte pas les caractères Unicode, ceux-ci
peuvent provoquer un message d'erreur.

Il est possible que, par exemple, les caractères grecs ou les symboles
mathématiques ne soient pas inclus dans la police sélectionnée et ne
puissent donc pas s'afficher. Pour résoudre ce problème, choisissez une
autre police (via : Édition -> Configurer -> Style).

### Remplacement de caractères Unicode

wxMaxima remplacera plusieurs caractères Unicode par leurs expressions
Maxima correspondantes, par exemple « ² » par « ^2 », « ³ » par « ^3 », le
symbole de la racine carrée par la fonction `sqrt()`, le symbole Sigma
mathématique (qui n’est pas le même caractère Unicode que la lettre grecque
correspondante) par `sum()`, etc.

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


### Les panneaux latéraux

Les raccourcis vers les commandes les plus importantes de _Maxima_, comme
une table des matières, des fenêtres avec des messages de débogage ou un
historique des dernières commandes exécutées, peuvent être accessibles via
les panneaux latéraux. Ceux-ci peuvent être activés depuis le menu
« Affichage ». Ils peuvent tous être déplacés vers d’autres emplacements, à
l’intérieur ou à l’extérieur de la fenêtre de _wxMaxima_. Un autre volet
utile est celui qui permet de saisir des lettres grecques avec la souris.

![Illustration de différents volets latéraux](./SidePanes.png){
id=img_SidePanes }

Dans le volet latéral « table des matières », vous pouvez augmenter ou
diminuer le niveau d’un titre en cliquant simplement dessus avec le bouton
droit de la souris et en sélectionnant le niveau de titre supérieur ou
inférieur.

![Augmenter ou diminuer le niveau de titres dans le panneau latéral Table
des Matières](./Sidepane-TOC-convert-headings.png){
id=Sidepane-TOC-convert-headings }

### Export MathML

Plusieurs logiciels de traitement de texte et programmes similaires
reconnaissent soit  une entrée  [MathML](https://www.w3.org/Math/)  et
l'insère automatiquement sous forme d'équation 2D modifiable, soit (comme
LibreOffice) disposent d'un éditeur d'équations proposant une fonction
« Importer du MathML depuis le presse-papiers ». D'autres prennent en charge
les équations en RTF. _WxMaxima_ offre donc plusieurs options dans le menu
contextuel (clic droit).

### Prise en charge de Markdown

_WxMaxima_ propose un ensemble de balises
[Markdown](https://en.wikipedia.org/wiki/Markdown) standard qui n’entrent
pas en conflit avec la notation mathématique. L’un de ces éléments est les
listes à puces.

```
Texte ordinaire
* Un élément, niveau d'indentation 1
* Un autre élément au niveau d'indentation 1
  * Un élément au deuxième niveau d'indentation
  * Un deuxième élément au deuxième niveau d'indentation
* Un troisième élément au premier niveau d'indentation
Texte ordinaire
```

_WxMaxima_ reconnaîtra le texte commençant par le caractère `>` comme des
blocs de citations  :

``` Texte ordinaire > citation citation citation citation > citation
citation citation citation > citation citation citation citation Texte
ordinaire```

_WxMaxima_ reconnaîtra `=>` dans ses sorties TeX et HTML et le remplacera
par le symbole Unicode correspondant :

``` cogito => sum.  ```

D’autres symboles reconnus par les exports HTML et TeX sont `<=` et `>=`
pour les comparaisons, une double flèche à double pointe (`<=>`), des
flèches à une seule direction (`<->`, `->` et `<-`) ainsi que `+/-` pour le
symbole correspondant. Pour la sortie TeX, les symboles `<<` et `>>` sont
également reconnus.

### Raccourcis clavier

La plupart des raccourcis clavier peuvent être trouvés dans le texte des
menus associés. Comme ils sont effectivement tirés du texte des menus, ils
peuvent ainsi être personnalisés par les traductions de _wxMaxima_ pour
correspondre aux besoins des utilisateurs des claviers locaux, nous ne les
documentons pas ici. Cependant, quelques raccourcis clavier ou alias de
raccourcis ne sont pas documentés dans les menus :

- <kbd>CTRL</kbd>+<kbd>SHIFT</kbd>+<kbd>DELETE</kbd> deletes a complete
  cell.
- <kbd>CTRL</kbd>+<kbd>TAB</kbd> or
  <kbd>CTRL</kbd>+<kbd>SHIFT</kbd>+<kbd>TAB</kbd> triggers the
  auto-completion mechanism.
- <kbd>SHIFT</kbd>+<kbd>SPACE</kbd> inserts a non-breaking space.

### TeX brut dans l'export TeX

Si une cellule de texte commence par `TeX:`, l'export TeX inclut le texte
littéral qui suit le marqueur `TeX:`. Cette fonctionnalité permet d'insérer
du code TeX directement dans le classeur _wxMaxima_.

## Formats de fichiers

Le contenu développé lors d'une session _wxMaxima_ peut être enregistré pour
une utilisation ultérieure de trois manières différentes :

### .mac

Les fichiers `.mac` sont des fichiers texte ordinaires qui contiennent des
commandes _Maxima_. Ils peuvent être lus à l'aide de la commande `batch()`
ou `load()` de _Maxima_, ou via l'entrée de menu Fichier/Charger un
packetage de _wxMaxima_.

Un exemple est présenté ci-dessous. `Quadratic.mac` définit une fonction,
puis génère un tracé avec `wxdraw2d()`. Ensuite, le contenu du fichier
`Quadratic.mac` est affiché et la fonction nouvellement définie `f()` est
évaluée.

![Chargement d'un fichier avec `batch()`](./BatchImage.png){
id=img_BatchImage }

Attention : Bien que le fichier `Quadratic.mac` ait l'extension habituelle
de _Maxima_ (`.mac`), il ne peut être lu que par _wxMaxima_, car la commande
`wxdraw2d()` est une extension de wxMaxima à _Maxima_. _Maxima_ en ligne de
commande ignorera la commande inconnue `wxdraw2d()` et l'affichera à
l'identique en sortie.

Vous pouvez utiliser les fichiers `.mac` pour écrire votre propre
bibliothèque de macros. Cependant, comme ils ne contiennent pas assez
d’informations structurelles, ils ne peuvent pas être relus en tant que
session _wxMaxima_.

### .wxm

Les fichiers `.wxm` contiennent la feuille de travail, à l’exception des
sorties de _Maxima_. À partir des versions de _Maxima_ >5.38, ils peuvent
être lus avec la fonction `load()` de _Maxima_, tout comme les fichiers
`.mac`. Avec ce format en texte brut, il est parfois inévitable que les
feuilles de travail utilisant de nouvelles fonctionnalités ne soient pas
rétrocompatibles avec les anciennes versions de _wxMaxima_.

#### Format de fichier des fichiers wxm

Il s'agit simplement d'un fichier texte brut (vous pouvez l'ouvrir avec un
éditeur de texte), contenant le contenu des cellules sous forme de
commentaires spéciaux Maxima.

Cela commence par le commentaire suivant :

``` /* [wxMaxima batch file version 1] [ DO NOT EDIT BY HAND! ]*/ /* [
Created with wxMaxima version 24.02.2_DevelopmentSnapshot ] */ ```

Puis les cellules suivent, encodées sous forme de commentaires Maxima, par
exemple une cellule de section :

```
/* [wxMaxima: section start ]
Titre de la section
   [wxMaxima: section end   ] */
```

ou (dans une cellule Math, l'entrée n'est bien sûr *pas* mise en commentaire
(la sortie n'est pas enregistrée dans un fichier `wxm`)) :

```
/* [wxMaxima: input   start ] */
f(x):=x^2+1$
f(2);
/* [wxMaxima: input   end   ] */
```

Les images sont [encodées en Base64](https://en.wikipedia.org/wiki/Base64)
avec l'indication du type de l'image sur la première ligne):

```
/* [wxMaxima: image   start ]
jpg
[séquence de caractères très chaotique à première vue]
   [wxMaxima: image   end   ] */
```

Une saut de page est simplement une ligne contenant :

```
/* [wxMaxima: page break    ] */
```

Et les cellules repliées sont marquées par :

```
/* [wxMaxima: fold    start ] */
...
/* [wxMaxima: fold    end   ] */
```

### .wxmx

Ce format de fichier basé sur le XML enregistre la feuille de travail
complète, y compris des éléments comme le facteur de zoom et la liste de
suivi du notebook. C'est le format de fichier recommandé.

#### Format de fichier des fichiers wxmx

Un fichier `wxmx` semble être un format binaire, mais on peut le gérer avec
des outils inclus dans votre système d'exploitation. Il s'agit d'un fichier
zip, que l'on peut décompresser avec `unzip` (peut-être faut-il le renommer
au préalable pour qu'il soit reconnu par le programme de décompression de
votre OS). Nous n'utilisons pas la fonction de compression, seulement la
possibilité de regrouper plusieurs fichiers en un seul — les images sont
déjà compressées et le reste est du texte simple (probablement bien plus
petit que les images volumineuses qu'il contient).

Il contient les fichiers suivants :

- `mimetype`: this file does contain the mimetype of wxMaxima files:
  `text/x-wxmathml`
- `format.txt`: a short description about wxMaxima and the wxmx file format
- Images (e.g. png, jpeg): inline plots which were produced in the wxMaxima
  session and included images.
- `content.xml`: a XML document, which contains the various cells of your
  document in XML format.

Donc, si quelque chose ne va pas, vous pouvez décompresser un document
wxMaxima (peut-être en le renommant au préalable en `.zip`), éventuellement
modifier le fichier `content.xml` avec un éditeur de texte ou remplacer une
image corrompue, recompresser les fichiers, probablement renommer le `.zip`
en `.wxmx` — et vous obtiendrez un autre fichier `wxmx` modifié.

## Options pour la configuration

Pour certaines variables courantes liées à la configuration, _wxMaxima_
propose deux méthodes de configuration :

- The configuration dialog box below lets you change their default values
  for the current and subsequent sessions.
- Also, the values for most configuration variables can be changed for the
  current session only by overwriting their values from the worksheet, as
  shown below.

![configuration 1 de wxMaxima](./wxMaxima_configuration_001.png){
id=img_wxMaxima_configuration_001 }

### Fréquence par défaut des images pour les animations

La fréquence des images utilisée pour les nouvelles animations est stockée
dans la variable `wxanimate_framerate`. La valeur initiale de cette variable
dans un nouveau notebook peut être modifiée via la boîte de dialogue de
configuration.

### Dimensions par défaut des graphiques pour les nouvelles sessions de
_maxima_

Lors du prochain démarrage, les graphiques intégrés au notebook seront créés
avec ces dimensions, sauf si la valeur de `wxplot_size` est modifiée par
_Maxima_.

Afin de définir la taille d'un seul graphique, la notation suivante peut
être utilisée afin d'affecter une valeur à une variable pour une seule
commande :

```maxima
wxdraw2d(
   explicit(
       x^2,
       x,-5,5
   )
), wxplot_size=[480,480]$
```

### Correspondance des parenthèses dans les zones de texte

Cette option active deux fonctionnalités :

- If an opening parenthesis, bracket, or double quote is entered _wxMaxima_
  will insert a closing one after it.
- If text is selected if any of these keys is pressed the selected text will
  be put between the matched signs.

### Ne pas enregistrer automatiquement le notebook

Si cette option est activée, le fichier du notebook ne sera écrasé que sur
demande de l'utilisateur. En cas de plantage, de coupure de courant ou autre
incident, une copie de sauvegarde récente reste cependant disponible dans le
dossier temporaire.

Si cette option n'est pas activée, _wxMaxima_ se comporte davantage comme
une application moderne de smartphone :

- Files are saved automatically on exit
- And the file will automatically be saved every 3 minutes.

### Où est enregistrée la configuration  ?

Si vous utilisez Unix/Linux, les informations de configuration sont
enregistrées dans un fichier `.wxMaxima` de votre répertoire personnel (si
vous utilisez wxWidgets < 3.1.1), ou dans `.config/wxMaxima.conf` (norme
XDG, si wxWidgets >= 3.1.1 est utilisé). Vous pouvez connaître la version de
wxWidgets avec la commande `wxbuild_info();` ou via le menu Aide → À
propos. [wxWidgets](https://www.wxwidgets.org/) est la bibliothèque
d'interface graphique multiplateforme sur laquelle repose _wxMaxima_ (d'où
le `wx` dans le nom). (Ces fichiers ou dossiers, commençant par un point,
`.wxMaxima` ou `.config`, sont donc des fichiers cachés).

Si vous utilisez Windows, la configuration est stockée dans le
registre. Vous trouverez les entrées pour _wxMaxima_ à l'emplacement suivant
du registre : `HKEY_CURRENT_USER\Software\wxMaxima`

______________________________________________________________________

# Extensions pour _Maxima_

_WxMaxima_ est avant tout une interface graphique pour _Maxima_. Ainsi, son
rôle principal consiste à transmettre les commandes à _Maxima_ et à afficher
les résultats de leur exécution. Cependant, _WxMaxima_ ajoute également
certaines fonctionnalités à _Maxima_. La capacité de _WxMaxima_ à générer
des rapports en exportant le contenu d'un notebook vers des fichiers HTML et
LaTeX a déjà été évoquée. Cette section aborde certaines améliorations
apportées par _WxMaxima_ pour l'inclusion de graphiques dans une session.

## Variables en indice

`wxsubscripts` définit si (et comment) _wxMaxima appliquera automatiquement
des indices aux noms de variables :

Si sa valeur est `false`, cette fonctionnalité est désactivée : wxMaxima ne
transformera pas automatiquement en indices les parties des noms de
variables suivant un tiret bas.

Si elle est définie sur `'all`, tout ce qui suit un tiret bas sera converti
en indice.

Si elle est définie sur `true`, les noms de variables au format `x_y`
s'affichent avec un indice uniquement si

- Either `x` or `y` is a single letter or
- `y` is an integer (can include more than one character).

![Comment les variables sont automatiquement mises en indices en utilisant
wxsubscripts](./wxsubscripts.png){ id=img_wxsubscripts }

Si le nom de la variable ne répond pas à ces exigences, il peut toujours
être déclaré comme « à mettre en indice » en utilisant la commande
`wxdeclare_subscript(nom_variable);` ou `wxdeclare_subscript([nom_variable1,
nom_variable2, ...]);`Une déclaration de variable en tant qu’indice peut
être annulée avec la commande suivante : `wxdeclare_subscript(nom_variable,
false);`

Vous pouvez utiliser le menu "Affichage->Mise en indice automatique" pour
définir ces valeurs.

## Message utilisateur dans la barre d'état

Les commandes qui durent longtemps peuvent générer un message pour
l'utilisateur dans la barre d'état. Ce dernier est remplacé par tout nouveau
message qui apparait dans cette barre (ce qui permet de l'utiliser comme
indicateur de progression) et est supprimé dès que la commande actuelle
envoyée à _Maxima_ est terminée. Il n'y a pas de risque à utiliser
`wxstatusbar()` même avec des bibliothèques qui pourraient être utilisées
avec _Maxima_ standard (par opposition à _wxMaxima_) : si _wxMaxima_ n'est
pas présent, la commande `wxstatusbar()` restera simplement non évaluée.

```maxima
for i:1 thru 10 do (
    /* Informe l'utilisateur de l'avancement. */
    wxstatusbar(concat("Pass ",i)),
    /* (sleep n) est une fonction Lisp, qui peut être utilisée */
    /* avec le caractère "?" avant. Cela retarde */
    /* l'exécution du programme (dans ce cas : pour 3 secondes) */
    ?sleep(3)
)$
```

## Graphiques

La création de courbes (qui repose fondamentalement sur des éléments
graphiques) est un domaine où une interface utilisateur graphique devra
apporter certaines extensions au programme d'origine.

### Intégrer un graphique dans le notebook

Normalement, _Maxima_ demande au programme externe _Gnuplot_ d'ouvrir une
fenêtre séparée pour chaque graphique qu'il génère. Comme il est souvent
plus pratique d'intégrer les graphiques directement dans le notebook,
_wxMaxima_ propose son propre ensemble de fonctions graphiques. Celles-ci ne
diffèrent des fonctions _Maxima_ correspondantes que par leur nom : elles
sont toutes préfixées par « wx ».

Les fonctions graphiques suivantes ont leurs équivalents préfixés par « wx »
:

| wxMaximaâ fonction graphique | Maxima fonction graphique                                                                          |
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

Si un fichier `.wxm` est lu par _Maxima_ (en mode console), ces fonctions
sont ignorées (et affichées à l'identique en sortie, comme toute autre
fonction inconnue dans _Maxima_).

If you got problems with one of these functions, please check, if the
problem exists in the the Maxima function too (e.g. you got an error with
`wxplot2d()`, check the same plot in the Maxima command `plot2d()` (which
opens the plot in a separate Window)). If the problem does not disappear, it
is most likely a Maxima issue and should be reported in the [Maxima
bugtracker](https://sourceforge.net/p/maxima/bugs/). Or maybe a Gnuplot
issue.

### Agrandir ou diminuer les graphiques intégrés

Comme indiqué précédemment, la boîte de dialogue de configuration permet de
modifier la taille par défaut des graphiques créés, en définissant la valeur
initiale de `wxplot_size`. Les routines graphiques de _wxMaxima_ tiennent
compte de cette variable, qui spécifie la taille d'un graphique en
pixels. Il est toujours possible d'interroger ou de définir cette variable
pour ajuster la taille des graphiques suivants :

```maxima
wxplot_size:[1200,800]$
wxdraw2d(
    explicit(
        sin(x),
        x,1,10
    )
)$
```

Si la dimension d'un seul graphique doit être modifiée, _Maxima_ offre une
méthode standard pour changer l'attribut de taille uniquement pour la
cellule actuelle. Dans ce cas, la spécification `wxplot_size = [valeur1,
valeur2]` est ajoutée à la commande `wxdraw2d()`, sans faire partie
intégrante de cette commande.

```maxima
wxdraw2d(
    explicit(
        sin(x),
        x,1,10
    )
),wxplot_size=[1600,800]$
```

Le réglage de la taille des graphiques intégrés avec `wxplot_size`
fonctionne pour les graphiques en ligne utilisant par exemple les commandes
`wxplot`, `wxdraw`, `wxcontour_plot` et `wximplicit_plot`, ainsi que pour
les animations intégrées avec les commandes `with_slider_draw` et
`wxanimate`.

### Des graphiques de meilleur qualité

Il semble que _Gnuplot_ ne propose pas de méthode portable pour déterminer
s'il prend en charge la sortie bitmap haute qualité fournie par la
bibliothèque Cairo. Sur les systèmes où _Gnuplot_ est compilé pour utiliser
cette bibliothèque, l'option pngCairo du menu de configuration (qui peut
être écrasée par la variable `wxplot_pngcairo`) active la prise en charge de
l'antialiasing et des styles de ligne supplémentaires. Si `wxplot_pngcairo`
est définie alors que _Gnuplot_ ne supporte pas cette fonctionnalité, le
résultat résultera en des messages d'erreur au lieu des graphiques.

### Ouvrir les graphiques intégrés dans les fenêtres interactives de
_Gnuplot_

Si un graphique a été généré avec des commandes de type `wxdraw` (`wxplot2d`
et `wxplot3d` ne sont pas pris en charge par cette fonctionnalité) et que la
taille du fichier du projet _Gnuplot_ correspondant n'est pas trop élevée,
_wxMaxima_ propose un menu contextuel (clic droit) permettant d'ouvrir le
graphique dans une fenêtre interactive _Gnuplot_.

### Ouvrir la console de commandes de Gnuplot dans les fenêtres `plot`

Sous MS Windows, il existe deux programmes Gnuplot : `gnuplot.exe` et
`wgnuplot.exe`. Vous pouvez configurer lequel utiliser via le menu de
configuration. `wgnuplot.exe` permet d'ouvrir une fenêtre de console où les
commandes _Gnuplot_ peuvent être saisies, ce que `gnuplot.exe` ne propose
pas. Malheureusement, `wgnuplot.exe` provoque un bref "décrochage" du focus
clavier à chaque génération d'un graphique.

### Intégrer des animations dans le notebook

Les diagrammes en 3D impliquent souvent que la lecture des données
quantitatives soit difficile. Une alternative intéressante consiste à
associer le 3ᵉ paramètre à la molette de la souris. La commande
`with_slider_draw` est une version de `wxdraw2d` qui génère plusieurs
graphiques et permet de basculer entre eux à l'aide d'un curseur situé en
haut de l'écran. _WxMaxima_ permet d'exporter cette animation au format GIF
animé.

Les deux premiers arguments de `with_slider_draw` sont le nom du paramètre
qui varie entre les graphiques et une liste des valeurs prises par ce
paramètre. Les arguments suivants sont les arguments classiques de la
commande `wxdraw2d` :

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

La même fonctionnalité pour les graphiques 3D est accessible avec
`with_slider_draw3d`, qui permet de faire tourner les graphiques 3D :

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

Si c'est plutôt la forme générale du graphique qui importe, un léger
déplacement peut suffire pour rendre la visualisation 3D plus intuitive :

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

Pour ceux qui maîtrisent mieux la commande `plot` que `draw`, il existe un
second ensemble de fonctions :

- `with_slider` and
- `wxanimate`.

Normalement, les animations sont lues ou exportées avec le taux de
rafraîchissement défini dans la configuration de _wxMaxima_. Pour régler la
vitesse de lecture d'une animation donnée, on peut utiliser la variable
`wxanimate_framerate` :

```maxima
wxanimate(a, 10,
    sin(a*x), [x,-5,5]), wxanimate_framerate=6$
```

Les fonctions d'animation utilisent la commande `makelist` de _Maxima_ et
partagent donc le même écueil : la valeur de la variable du curseur n'est
substituée dans l'expression que si cette variable y apparaît
explicitement. Par conséquent, l'exemple suivant échouera :

``maxima
f:sin(a*x);
with_slider_draw(
    a,makelist(i/2,i,1,10),
    title=concat("a=",float(a)),
    grid=true,
    explicit(f,x,0,10)
)$

Si _Maxima_ est explicitement invité à substituer la valeur du curseur, le
tracé fonctionne correctement :

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

### Ouvrir plusieurs graphiques dans des fenêtres de manière simultanée

Cette fonctionnalité de _Maxima_ (sur les configurations qui la supportent),
bien qu'elle ne soit pas disponible via  _wxMaxima_, s'avère parfois bien
pratique. L'exemple suivant provient d'un message de Mario Rodriguez sur la
_liste de diffusion de Maxima_ :

```maxima load(draw);

/* Une parabole dans une fenêtre #1 */
draw2d(terminal=[wxt,1],explicit(x^2,x,-1,1));

/* Une parabole dans une fenêtre #2 */
draw2d(terminal=[wxt,2],explicit(x^2,x,-1,1));

/* Un paraboloide dans une fenêtre #3 */
draw3d(terminal=[wxt,3],explicit(x^2+y^2,x,-1,1,y,-1,1)); ```

Il est également possible de tracer plusieurs graphiques dans une même
fenêtre (la même chose est réalisable avec la commande standard `draw()` de
Maxima en ligne de commande) :

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

### Le panneau latéral "Graphiques avec draw"

Le panneau latéral "Graphiques avec draw" cache un générateur simple de code
qui permet de créer des représentations graphiques exploitant une partie des
fonctionnalités du package _draw_ intégré à _Maxima_.

#### 2D

Génère le squelette d'une commande `draw()` pour dessiner une scène
2D. Cette scène doit ensuite être complétée avec des commandes qui génèrent
son contenu, par exemple en utilisant les boutons situés dans les lignes en
dessous du bouton "2D".

Une fonctionnalité utile du bouton 2D est qu'il permet de configurer la
scène comme une animation dans laquelle un paramètre (par défaut, _t_) prend
une valeur différente à chaque image : une courbe 2D animée facilite souvent
l'interprétation des données plus qu'une représentation 3D statique des
mêmes informations.

#### 3D

Génère le squelette d'une commande `draw()` pour tracer une scène 3D. Si
aucune scène 2D ou 3D n'est définie, tous les autres boutons configurent
automatiquement une scène 2D contenant la commande générée par le bouton.

#### Expression

Ajoute un tracé standard d'une expression comme `sin(x)`, `x*sin(x)` ou
`x^2+2*x-4` à la commande `draw()` où se trouve actuellement le curseur. Si
aucune commande `draw()` n'existe, une scène 2D avec ce tracé est
générée. Chaque scène peut contenir un nombre illimité de graphiques.

#### Courbe définie de manière implicite

Tente de trouver tous les points où une expression comme `y=sin(x)`,
`y*sin(x)=3` ou `x^2+y^2=4` est vérifiée et trace la courbe résultante dans
la commande `draw()` où se trouve le curseur. Si aucune commande `draw()`
n'existe, une scène 2D avec ce tracé est générée.

#### Courbe paramétrique

Fait varier une variable entre une borne inférieure et une borne supérieure,
puis utilise deux expressions comme `t*sin(t)` et `t*cos(t)` pour générer
les coordonnées x, y (et z dans le cas des tracés 3D) d'une courbe, qui est
ensuite insérée dans la commande `draw()` actuelle.

#### Points

Trace plusieurs points, éventuellement reliés entre eux. Les coordonnées des
points peuvent provenir d'une liste de listes, d'un tableau 2D, ou d'une
liste ou d'un tableau pour chaque axe.

#### Titre du graphique

Ajoute un titre en haut du graphique,

#### Axes

Configurer l'axe.

#### Lignes de niveau

(Uniquement pour les tracés 3D) : Ajoute des lignes de niveau similaires à
celles que l'on trouve sur une carte de montagne aux commandes du tracé qui
suivent dans la commande `draw()` actuelle et/ou au plan de base du
graphique. Alternativement, cet assistant permet de sauter entièrement le
tracé des courbes et d'afficher uniquement le tracé des lignes de niveau.

#### Légende du graphique

Ajoute une nouvelle entrée pour la légende affichant le nom du prochain
tracé à la légende actuelle du graphique. Un nom vide désactive la
génération d'entrées des légende pour les tracés suivants.

#### Couleur de la ligne du tracé

Définit la couleur de ligne pour les tracés suivants contenus dans la
commande `draw()` actuelle.

#### Couleur de remplissage

Définit la couleur de remplissage pour les tracés suivants contenus dans la
commande `draw()` actuelle.

#### Quadrillage

Ouvre un assistant permettant de configurer les lignes du quadrillage.

#### Précision

Permet de choisir un compromis adapté entre vitesse et précision, inhérent à
tout programme de tracé graphique.

### Modifier la police et la taille de police pour les graphiques.

Surtout lorsque vous utilisez un affichage haute résolution, la taille de
police par défaut peut être très petite. Pour les commandes basées sur
`draw`, vous pouvez définir la police et la taille de police à l'aide
d'options comme `font=...`, `font_size=...`, par exemple :

~~~maxima
wxdraw2d(
     font="Helvetica",
     font_size=30,
     explicit(sin(x),x,1,10));
~~~

Pour les commandes de type `plot` (par exemple `wxplot2d`, `wxplot3d`), les
tailles et polices de caractères peuvent être définies à l'aide de la
commande `gnuplot_preamble`, par exemple :

~~~maxima
wxplot2d(sin(x),[x,1,10],
         [gnuplot_preamble, "set tics font \"Arial, 30\"; set xlabel font \",20\"; set ylabel font \",20\";"]);
~~~

Cela définit la police pour les nombres en Arial avec une taille de 30, et
la taille de la police des étiquettes xlabel et ylabel à 20 (avec la police
par défaut).

Consultez la documentation de Maxima et Gnuplot pour plus
d'informations. Remarque : Gnuplot semble rencontrer des problèmes avec les
polices de grande taille, voir [wxMaxima issue
1966](https://github.com/wxMaxima-developers/wxmaxima/issues/1966).

## Graphiques intégrés

Si le format de fichier `.wxmx` est utilisé, l'intégration de fichiers dans
un notebook _wxMaxima_ peut se faire simplement par glisser-déposer. Mais
parfois (par exemple si le contenu d'une image est susceptible de changer
plus tard dans une session), il est préférable d'indiquer au fichier de
charger l'image lors de l'évaluation :

```maxima show_image("man.png"); ```

## Fichiers au démarrage

La boîte de dialogue de configuration de _wxMaxima_ permet de modifier deux
fichiers contenant des commandes exécutées au démarrage :

- A file that contains commands that are executed on starting up _Maxima_:
  `maxima-init.mac`
- one file of additional commands that are executed if _wxMaxima_ is
  starting _Maxima_: `wxmaxima-init.mac`

Par exemple, si Gnuplot est installé dans `/opt` (peut-être sur macOS), vous
pouvez ajouter `gnuplot_command:"/opt/local/bin/gnuplot"$` (ou
`/opt/gnuplot/bin/gnuplot` ou tout autre chemin) à ces fichiers.

Ces fichiers se trouvent dans le répertoire utilisateur de Maxima
(généralement `%USERPROFILE%/maxima` sous Windows, ou `$HOME/.maxima`
sinon). Vous pouvez connaître leur emplacement avec la commande :
`maxima_userdir;`

## Variables spéciales wx...

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

## Affichage formaté en 2D

La fonction `table_form()` affiche une liste 2D sous une forme plus lisible
que le résultat produit par la sortie par défaut de _Maxima_. L'argument
doit être une liste contenant une ou plusieurs listes. Comme la commande
"print", cette commande affiche le résultat même si elle se termine par un
dollar (`$`). Si elle se termine par un point-virgule (`;`), le tableau
s'affiche accompagné d'un message "done".

```maxima
table_form(
    [
        [1,2],
        [3,4]
    ]
)$
```

Comme le montre l'exemple suivant, les listes utilisées par la commande
`table_form` peuvent être créées avant son exécution.

![Un troisième exemple de tableau](./MatrixTableExample.png){
id=img_MatrixTableExample }

De même, comme une matrice est une liste de listes, elle peut être convertie
en tableau de manière similaire.

![Un autre exemple avec table_form](./SecondTableExample.png){
id=img_SecondTableExample }

## Signalement de bugs

_WxMaxima_ propose quelques fonctions qui collectent des informations utiles
pour signaler un bug concernant le système actuel :

- `wxbuild_info()` gathers information about the currently running version
  of _wxMaxima_
- `wxbug_report()` tells how and where to file bugs


## Ecrire le résultat de la sortie en rouge

La commande `box()` de _Maxima_ permet à _wxMaxima_ d'afficher son argument
en rouge, si le deuxième argument de la commande est le texte `highlight`.

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

# Menu d'aide

Le menu d'aide de _wxMaxima_ donne accès au manuel de Maxima et à celui de
wxMaxima, à des astuces, à des exemples de notebooks, ainsi qu'aux exemples
intégrés dans Maxima en ligne de commande (via la commande `demo()`).

Veuillez noter que les exemples indiquent :

~~~ À l’invite `’_’`, tapez `;` puis `<entrée>` pour poursuivre la
démonstration.  ~~~

Cela est valable pour Maxima en ligne de commande, mais dans wxMaxima, par
défaut, il est nécessaire de poursuivre la démonstration avec :
<kbd>CTRL</kbd> + <kbd>ENTRÉE</kbd>

(Ce comportement peut être configuré dans le menu Configurer → Notebook →
" Raccourcis pour envoyer des commandes à Maxima".)

______________________________________________________________________

# Résolution des problèmes

## Impossible de se connecter à _Maxima_

Puisque _Maxima_ (le programme qui effectue les calculs mathématiques) et
_wxMaxima_ (qui fournit l’interface utilisateur conviviale) sont deux
programmes distincts qui communiquent via une connexion réseau locale, la
cause la plus probable d'un dysfonctionnement est que cette connexion ne
s'établit pas correctement. Par exemple, un pare-feu peut être configuré non
seulement pour bloquer les connexions non autorisées depuis Internet (et
éventuellement intercepter certaines connexions sortantes), mais aussi pour
empêcher la communication entre processus sur un même ordinateur. Notez que,
comme Maxima est exécuté via un processeur Lisp, le processus bloqué ne
porte pas nécessairement le nom "maxima". Les noms usuels pour le programme
ouvrant la connexion réseau peuvent être sbcl, gcl, ccl, lisp.exe, ou des
noms similaires.

Sur les ordinateurs Unix, une autre cause possible serait que le réseau
loopback, qui permet les connexions entre deux programmes sur le même
ordinateur, ne soit pas correctement configuré.

## Comment récupérer les données d’un fichier .wxmx corrompu

En informatique, la plupart des formats modernes basés sur XML sont en
réalité des fichiers zip ordinaires. _WxMaxima_ n’active pas la compression,
donc le contenu des fichiers `.wxmx` peut être consulté avec n’importe quel
éditeur de texte.

Si la signature du zip à la fin du fichier est toujours intacte après avoir
renommé un fichier `.wxmx` corrompu en `.zip`, la plupart des systèmes
d’exploitation permettront d’extraire une partie des informations qu’il
contient. Cette méthode est souvent utilisée pour récupérer les fichiers
image originaux d’un document issu d'un traitement de texte. Si la signature
du zip n’est plus intacte, ce n’est pas forcément une impasse : si
_wxMaxima_ a détecté une erreur lors de l’enregistrement, un fichier
`.wxmx~` aura probablement été créé et son contenu pourrait s’avérer utile.

Et même s’il n’existe pas un tel fichier : le format `.wxmx` est un format
conteneur et la partie XML y est stockée sans compression. Il est possible
de renommer le fichier `.wxmx` en `.txt` et d’utiliser un éditeur de texte
pour récupérer la portion XML du contenu (elle commence par `<?xml
version="1.0" encoding="UTF-8"?>` et se termine par
`</wxMaximaDocument>`. Avant et après ce texte, vous verrez du contenu
binaire illisible dans l’éditeur).

Si vous enregistrez un fichier texte contenant uniquement ce contenu (par
exemple, en copiant et collant ce texte dans un nouveau fichier) avec une
extension `.xml`, _wxMaxima_ saura comment récupérer le texte du document.

## Je souhaite afficher des informations de débogage à l’écran avant que ma
commande ne soit terminée

Normalement, _wxMaxima_ attend que la formule 2D soit transférée en totalité
avant de commencer la composition typographique. Cela permet d’économiser du
temps en évitant de tenter d'écrire une équation incomplète. Cependant, il
existe une commande `disp` qui affiche immédiatement des informations de
débogage, sans attendre la fin de l’exécution de la commande _Maxima_ en
cours :

``maxima
for i:1 thru 10 do (
   disp(i),
   /* (sleep n) is a Lisp function, which can be used */
   /* with the character "?" before. It delays the */
   /* program execution (here: for 3 seconds) */
   ?sleep(3)
)$
```

Sinon, on peut utiliser la commande `wxstatusbar()` mentionnée ci-dessus.

## La création d'un graphique n'affiche qu'une enveloppe vide encadrée avec
un message d'erreur

Cela signifie que _wxMaxima_ n'a pas pu lire le fichier généré par _Maxima_,
qui était censé donner les instructions à _Gnuplot_ pour créer le graphique.

Les causes possibles de cette erreur sont :

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

## Le tracé d'une animation génère l'erreur "error: undefined variable"

Par défaut, la valeur du paramètre pour le curseur n'est substituée dans
l'expression à tracer que si elle y est visible. L'utilisation d'une
commande `subst` qui substitue la variable du curseur dans l'équation à
tracer résout ce problème. À la fin de la section [Intégrer des animations
dans le notebook](#embedding-animations-into-the-spreadsheet), vous pouvez
voir un exemple.

## J’ai perdu le contenu d’une cellule et la fonction Annuler ne le restaure
pas

Il existe des fonctions Annuler distinctes pour les opérations sur les
cellules et pour les modifications à l’intérieur des cellules, donc il est
peu probable que cela arrive. Si cela se produit, plusieurs méthodes
permettent de récupérer les données :

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

## _WxMaxima_ démarre avec le message  Maxima process terminated

Une cause possible est que le programme _Maxima_ n’est pas trouvé à
l’emplacement indiqué dans l’onglet Maxima de la boîte de dialogue de
configuration de _wxMaxima_, ce qui l’empêche de démarrer. Vérifiez et
corrigez le chemin vers l’exécutable _Maxima_ pour résoudre ce problème.

## Maxima calcule indéfiniment et ne répond plus aux commandes entrées

Il est théoriquement possible que _wxMaxima_ ne détecte pas que _Maxima_ a
terminé ses calculs et, de ce fait, n’autorise pas l’envoi de nouvelles
données. Dans ce cas, l’option Déclencher l’évaluation (« Trigger
evaluation ») peut resynchroniser les deux programmes.

## Mon _Maxima_ basé sur SBCL manque de mémoire

Le compilateur Lisp SBCL est configuré par défaut avec une limite de mémoire
qui lui permet de fonctionner même sur des ordinateurs peu
puissants. Cependant, lors de la compilation de gros packages logiciels
comme Lapack ou du traitement de listes d'équations extrêmement longues,
cette limite peut s'avérer insuffisante. Pour l'augmenter, vous pouvez
utiliser le paramètre en ligne de commande `--dynamic-space-size` lors du
lancement de SBCL, qui indique combien de mégaoctets doivent être
réservés. - Une version 32 bits de SBCL sous Windows peut réserver jusqu'à
999 Mo. - Une version 64 bits sous Windows peut aller au-delà des 1280 Mo
nécessaires pour compiler Lapack.

Une façon de fournir des paramètres en ligne de commande à _Maxima_ (et donc
à SBCL) est d’utiliser le champ "Paramètres supplémentaires pour Maxima"
dans la boîte de dialogue de configuration de _wxMaxima_.

![mémoire pour sbcl](./sbclMemory.png){ id=img_sbclMemory }

## Sous Ubuntu, la saisie est parfois lente ou certaines touches sont
ignorées

L'installation du paquet 'ibus-gtk' devrait résoudre ce problème. Pour plus
de détails, voir :
[https://bugs.launchpad.net/ubuntu/+source/wxwidgets3.0/+bug/1421558](https://bugs.launchpad.net/ubuntu/+source/wxwidgets3.0/+bug/1421558).

## _WxMaxima_ se fige lorsque _Maxima_ traite des caractères grecs ou des
lettres accentuées (comme les umlauts)

Si votre _Maxima_ est basé sur SBCL, ajoutez les lignes suivantes à votre
fichier `.sbclrc` :

```commonlisp (setf sb-impl::*default-external-format* :utf-8)  ```

Le dossier où ce fichier doit être placé dépend du système et de
l’installation. Cependant, toute version de _Maxima_ basée sur SBCL, ayant
déjà évalué une cellule durant la session en cours, peut gentiment indiquer
son emplacement après exécution de la commande suivante :

``` :lisp (sb-impl::userinit-pathname)  ```

## Remarque concernant Wayland (distributions Linux/BSD récentes)

Il semble y avoir des problèmes de compatibilité entre le serveur
d'affichage Wayland et wxWidgets. WxMaxima peut en être affecté, par exemple
avec des barres latérales qui ne peuvent pas être déplacées.

Vous pouvez soit : - désactiver Wayland et utiliser X11 à la place (de
manière globale), - ou forcer WxMaxima à utiliser le système X Window en
définissant la variable d’environnement :  `GDK_BACKEND=x11`

Par exemple, lancez WxMaxima avec la commande :

`GDK_BACKEND=x11 wxmaxima`

## Pourquoi le navigateur pour le manuel intégré n’est-il pas disponible sur
mon PC Windows ?

Either wxWidgets wasn’t compiled with support for Microsoft’s webview2 or
Microsoft’s webview2 isn’t installed.

## Pourquoi le navigateur pour le manuel externe ne fonctionne-t-il pas sur
ma machine Linux ?

The HTML browser might be a snap, flatpack or appimage version. All of these
typically cannot access files that are installed on your local
system. Another reason might be that maxima or wxMaxima is installed as a
snap, flatpack or something else that doesn’t give the host system access to
its contents. A third reason might be that the maxima HTML manual isn’t
installed and the online one cannot be accessed.

## Puis-je faire en sorte que _wxMaxima_ génère à la fois des fichiers image
et des graphiques en ligne en même temps ?

Le notebook intègre les fichiers `.png`. _WxMaxima_ permet à l'utilisateur
de spécifier où ces fichiers doivent être générés :

```maxima
wxdraw2d(
    file_name="test",  /* extension .png automatically added */
    explicit(sin(x),x,1,10)
);
```

Si vous souhaitez utiliser un format différent, il est plus simple de
générer les images séparément, puis de les réimporter dans le notebook :

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

## Puis-je définir le rapport hauteur/largeur (aspect ratio) d’un graphique
en ligne ?

Utilisez la variable `wxplot_size` :

```maxima
wxdraw2d(
    explicit(sin(x),x,1,10)
),wxplot_size=[1000,1000];
```

## Après la mise à niveau vers macOS 13.1, les commandes plot et/ou draw
affichent des messages d'erreur du type

``` 1 HIToolbox 0x00007ff80cd91726
_ZN15MenuBarInstance22EnsureAutoShowObserverEv + 102 2 HIToolbox
0x00007ff80cd912b8 _ZN15MenuBarInstance14EnableAutoShowEv + 52 3 HIToolbox
0x00007ff80cd35908 SetMenuBarObscured + 408 ...  ```

Ce problème pourrait être lié au système d'exploitation. Désactiver le
masquage automatique de la barre de menus (via Réglages Système => Bureau et
Dock => Barre des menus) pourrait résoudre le problème. Pour plus
d'informations, voir [l'issue wxMaxima
#1746](https://github.com/wxMaxima-developers/wxmaxima/issues/1746).

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

## Comment faire tenir plus de texte sur une page LaTeX ?

Oui. Utilisez le [package LaTeX "geometry"](https://ctan.org/pkg/geometry)
pour définir la taille des marges.

Vous pouvez ajouter la ligne suivante dans le préambule LaTeX (par exemple
via le champ dédié dans la fenêtre de configuration : ("Export" → "Lignes
supplémentaires pour le préambule TeX") pour régler les marges à 1 cm :

```latex \usepackage[left=1cm,right=1cm,top=1cm,bottom=1cm]{geometry} ```

## Un mode sombre existe-t-il ?

Si wxWidgets est suffisamment récent, _wxMaxima_ passera automatiquement en
mode sombre si le système d'exploitation l'est également. Par défaut, la
feuille de travail a un fond clair, mais cela peut être personnalisé. Sinon,
il existe une option 'Affichage/Inverser la luminosité de la feuille' qui
permet de basculer rapidement entre un fond clair et un fond sombre.

## _WxMaxima_ se fige parfois pendant plusieurs secondes lors de la première
minute d'utilisation

_WxMaxima_ delegates some big tasks like parsing _Maxima_’s
>1000-page-manual to background tasks, which normally goes totally
unnoticed. At the moment the result of such a task is needed, though, it is
possible that _wxMaxima_ needs to wait a couple of seconds before it can
continue its work.

## Especially when testing new locale settings, a message box "locale
’xx_YY’ can not be set" occurs

![Avertissement sur la langue locale](./locale-warning.png){
id=img_locale_warning}

(The same problem can occur with other applications too). The translations
seem okay after you click on ’OK’. WxMaxima does not only use its own
translations but the translations of the wxWidgets framework too.

Ces langues locales peuvent ne pas être présentes dans le système. Sur les
systèmes Ubuntu/Debian, elles peuvent être générées avec : `dpkg-reconfigure
locales`

## How can I use symbols for real numbers, natural numbers (ℝ, ℕ), etc.?

You can find these symbols in the Unicode sidebar (search for ’double-struck
capital’). But the selected font must also support these symbols. If they do
not display properly, select another font.

## Comment un script Maxima peut-il déterminer s'il s'exécute sous wxMaxima
ou en ligne de commande ?

Si wxMaxima est utilisé, la variable Maxima `maxima_frontend` est définie
sur `wxmaxima`. Dans ce cas, la variable Maxima `maxima_frontend_version`
contient la version de wxMaxima.

Si aucun programme pour le frontend n'est utilisé (si vous utilisez Maxima
en ligne de commande), ces variables valent `false`.

______________________________________________________________________

# Arguments en ligne de commande

Généralement, les programmes dotés d'une interface graphique se lancent
simplement en cliquant sur une icône ou une entrée de menu. WxMaxima,
lorsqu'il est démarré depuis la ligne de commande, accepte néanmoins
certains arguments.

- `-v` ou `--version` : affiche les informations de version
- `-h` ou `--help` : affiche un texte d'aide succinct
- `-o` ou `--open=<fichier>` : ouvre le fichier spécifié en argument de
  cette option de la ligne de commande
- `-e` ou `--eval` : évalue le fichier après l'avoir ouvert.
- `-b` ou `--batch` : si un fichier est ouvert en ligne de commande, toutes
  ses cellules sont évaluées et le fichier est ensuite enregistré. Cela peut
  être utile, par exemple, si la session décrite dans le fichier amène
  _Maxima_ à générer des fichiers de sortie. Le traitement en mode batch
  s'arrête si _wxMaxima_ détecte une erreur émise par _Maxima_, et
  s'interrompt au cas où _Maxima_ pose une question : la nature interactive
  des mathématiques ne permet pas toujours un traitement entièrement
  automatisé.
- `--logtostderr` : redirige aussi les messages de débogage (barre latérale) vers stderr.
- `--pipe` : transmet les messages de Maxima vers stdout.
- `--exit-on-error` : ferme le programme en cas d’erreur Maxima.
- `-f` ou `--ini=<fichier>` : utilise le fichier d’initialisation spécifié en argument.
- `-u`, `--use-version=<version>` : utilise la version `<version>` de Maxima.
- `-l`, `--lisp=<compilateur>` : utilise une version de Maxima compilée avec le compilateur Lisp `<compilateur>`.
- `-X`, `--extra-args=<args>` : permet de passer des arguments supplémentaires à Maxima.
- `-m` ou `--maxima=<chemin>` : spécifie l’emplacement du binaire maxima.
- `--enableipc` : permet à Maxima de contrôler wxMaxima via une communication inter-processus (à utiliser avec prudence).
- `--wxmathml-lisp=<fichier>` : indique l’emplacement de `wxMathML.lisp` (pour remplacer la version intégrée, principalement pour les développeur·ses).

Certains systèmes d'exploitation peuvent utiliser un tiret long (—) au lieu
d'un tiret court (-) devant les options en ligne de commande.

______________________________________________________________________

# À propos du logiciiel et des contributions à wxMaxima

wxMaxima est principalement développé en C++ avec le framework
[wxWidgets](https://www.wxwidgets.org), et utilise
[CMake](https://www.cmake.org) comme système de build. Une petite partie est
écrite en Lisp. Si vous maîtrisez ces langages et souhaitez contribuer à ce
projet open source, vous pouvez rejoindre l’équipe sur
[GitHub](https://github.com/wxMaxima-developers/wxmaxima).

Mais les programmeurs ne sont pas les seul·es à pouvoir contribuer ! Vous
pouvez aussi aider à améliorer wxMaxima en améliorant la documentation, en
signalant (ou corrigeant) des bugs, en proposant de nouvelles
fonctionnalités, en traduisant l’interface ou le manuel dans votre langue
(consultez le fichier README.md dans le [dossier
locales](https://github.com/wxMaxima-developers/wxmaxima/tree/main/locales)
pour savoir comment procéder).

Ou répondre aux questions d'autres utilisateurs sur le forum de discussion.

Le code source de wxMaxima est documenté avec Doxygen, disponible
[ici](https://wxmaxima-developers.github.io/wxmaxima/Doxygen-documentation/).

Le programme est quasi complet : hormis les bibliothèques système (et la
bibliothèque wxWidgets), il ne nécessite aucune dépendance externe (comme
des fichiers graphiques ou le logiciel Lisp.   Les éléments comme le fichier
( `wxmathML.lisp`) sont intégrés directement dans l'exécutable.

Si vous êtes développeur et que vous souhaitez tester une version modifiée
du fichier `wxmathML.lisp`  sans tout recompiler, vous pouvez utiliser
l'option en ligne de commande : `--wxmathml-lisp=<chemin>` pour spécifier un
autre fichier Lisp à la place de celui intégré à la version actuelle.
