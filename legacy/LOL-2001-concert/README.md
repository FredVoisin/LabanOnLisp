# LabanOnLisp — version du concert *L'Écarlate* (2001)

**Archive historique, non maintenue.** Ces fichiers sont conservés tels
quels, comme référence. Ils ne sont pas destinés à être corrigés ni
chargés avec une version actuelle de LabanOnLisp.

## Contexte

LOL (*Laban On Lisp*) est un environnement de composition chorégraphique
assistée par ordinateur, développé en Common Lisp par Frédéric Voisin
de 1999 à 2001, pour Myriam Gourfink et l'association LOL (Paris).
Il décrit le corps dansant par des *situations* : pour chaque partie du
corps (tête, bras, jambes, tronc, bassin, respiration…), un ensemble de
dimensions inspirées de la notation Laban (appui, niveau, direction,
rotation, distance, flexion, courbure, contact, adresse, temps…), que
l'on peut éditer, combiner, filtrer et sauvegarder.

Cette version est celle utilisée pour *L'Écarlate*, de Kasper T.
Toeplitz et Myriam Gourfink, créée le 7 juin 2001 à l'Ircam (Espace de
projection, Paris), dans le cadre du festival Agora. Elle est
contemporaine de la version de neuromuse utilisée pour le même concert :
https://github.com/FredVoisin/neuromuse/tree/master-archive/legacy/neuromuse-2001-concert

Rôle de LOL dans l'œuvre : [à compléter]

Fiche du concert :
https://ressources.ircam.fr/fr/media/x218df0_lecarlate-myriam-gourfink-kasper-t-toeplit

## Environnement d'origine

- Macintosh Common Lisp 4.x (CLOS, QuickDraw), Mac OS 9
- Chargement : évaluer `LOL3.0.lisp`, qui demande le dossier des
  sources puis charge les modules par `require` ; le package `LOL` et
  un menu « LOL » sont créés dans MCL

## Contenu

Noyau
- `LOL3.0.lisp` : loader et noyau (package `LOL`, recherche
  d'instances par distance d'édition, aide en ligne) ; porte la notice
  de copyright d'origine
- `LOL-environnement.lisp` : package `DATABASE` et fichiers
  d'environnement `.LOL`
- `LOL-Classes.lisp`, `LOL-define.lisp` : classes (situations, parties
  du corps, dimensions) et macro `define` pour créer de nouvelles
  catégories
- `LOL-default.lisp` : ensembles de valeurs par défaut de chaque
  dimension (`w!` = valeur libre)
- `LOL-edit.lisp`, `LOL-IO.lisp` : édition, renommage, sauvegarde et
  chargement des situations
- `LOL-combinatoire.lisp`, `LOL-maths.lisp`, `LOL-Tools.lisp`,
  `arithmetique.lisp` : génération combinatoire et aléatoire de
  situations, utilitaires
- `fuzz.lisp`, `fuzz1.0.lisp`, `fuzz2.0.lisp` : valeurs floues et
  infinies (`fuzz2.0.lisp` est la version chargée)
- `condition.lisp`, `equivalence.lisp`, `error.lisp` : conditions,
  classes d'équivalence, gestion d'erreurs

Interface (MCL)
- `LOL-windows.lisp`, `LOL-Menus.lisp` : fenêtres d'édition des
  situations (avec bouton d'envoi MIDI) et menu LOL
- `Laban.lisp` : fenêtre de notation Laban (portée)
- `chrono.lisp` : chronomètre (windoid)
- `cryspy.lisp`, `tempview.lisp`, `Temp2.lisp` : éléments d'interface
  en cours de développement

Concert *L'Écarlate*
- `ecarlate.lisp` : dimensions et parties du corps propres à la pièce,
  classes pour chaque danseuse ; `ecarlate2.lisp` en est une copie
  identique
- `ecarlate-score.lol` : partition de la pièce, sauvegarde des
  situations `A-2` à `A-8`, `B-1` à `B-7`, `C1` à `C6`, `D1` à `D3`,
  `E1`, `E2` (format `save-object`)
- `DEFAULT-ENV.LOL` : environnement par défaut

Antécédents et expérimentations
- `GESTES.lisp` (23 avril 1999) : fonctions de gestes et de MIDI pour
  OpenMusic (package `om`)
- `Fredgram.lisp`, `lolgram.lisp`, `music2.lisp` (1999) : traduction de
  langage naturel infixe vers Lisp, d'après *Paradigms of AI
  Programming* de Peter Norvig, pour piloter LOL ou OpenMusic ; non
  chargés dans LOL 3.0
- `dimanche 23 avril` : transcription d'une session du Listener MCL
  4.2 avec LOL 2.2 (probablement 23 avril 2000)
- `Temp.lisp`, `Temp-macro.lisp`, `def.lisp`, `make-listener.lisp`,
  `user-interaction.lisp` : essais et brouillons
- `old/` : versions antérieures de l'interface

Code de tiers (voir [../LICENSE](../LICENSE))
- `save-object.lisp` : SAVE-OBJECT 10A, Kerry V. Koitzsch / Kevin
  Thompson (NASA Ames), 1995
- `drag-and-drop.lisp` : Macintosh Drag and Drop for MCL 1.5.5, Dan S.
  Camper, 1996
- `dynamic-views.lisp`, `scrollers.lisp`, `scrolling-windows.lisp`,
  `picture-files.lisp`, `view-example.lisp`, `windoid-key-events.lisp` :
  exemples MCL (Apple Computer / Digitool)
- `simple-edit-value.lisp` : extrait de la liste info-mcl (Steve
  Strassmann, 1994)

## Notes de conservation

- Fichiers en fins de ligne CR (Mac classique) et en encodage
  MacRoman ; pour les lire sur un système actuel :
  `tr '\r' '\n' < fichier | iconv -f MACINTOSH -t UTF-8`
- La situation `A-2` apparaît deux fois dans `ecarlate-score.lol` ; au
  chargement, la seconde remplace la première.
- Les dates de modification d'origine des fichiers ont été perdues lors
  de copies successives.
