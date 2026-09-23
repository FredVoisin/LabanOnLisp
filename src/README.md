# Sources de travail

Copie de travail des sources de LOL 3.0 (1999-2001), prise dans
[`../legacy/LOL-2001-concert/`](../legacy/LOL-2001-concert/) et
convertie pour être lisible sur un système actuel :

- fins de ligne CR (Mac classique) remplacées par LF ;
- encodage MacRoman converti en UTF-8.

Le contenu n'est pas modifié par ailleurs, à l'exception de l'en-tête
de licence de `LOL3.0.lisp`. Le code dépend encore de Macintosh Common
Lisp (fenêtres, menus, QuickDraw) et ne se charge pas tel quel dans
une implémentation actuelle.

Point d'entrée d'origine : `LOL3.0.lisp`. La description détaillée des
fichiers se trouve dans
[`../legacy/LOL-2001-concert/README.md`](../legacy/LOL-2001-concert/README.md).

## Licence

PolyForm Noncommercial 1.0.0 (voir [`../LICENSE`](../LICENSE)), sauf
pour le code de tiers (`drag-and-drop.lisp`, les
exemples MCL d'Apple et Digitool, `simple-edit-value.lisp`,
`Fredgram.lisp`), qui reste soumis à ses propres notices : voir
[`../legacy/LICENSE`](../legacy/LICENSE).

`save-object.lisp` (Koitzsch/Thompson), chargé par `LOL3.0.lisp` pour
la sauvegarde des situations, n'est pas repris ici : il reste
disponible dans `legacy/LOL-2001-concert/` et sera remplacé lors du
portage.
