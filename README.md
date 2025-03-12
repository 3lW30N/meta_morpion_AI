# IA pour le Méta-Morpion
## Application de modèles d'IA en OCaml pour déterminer une stratégie gagnante au jeu du Méta-Morpion

Ce projet est mon TIPE (Travail d'Initiative Personnelle Encadré) que j'ai présenté lors des oraux des concours d'admission en école d'ingénieur à la fin de ma classe préparatoire.

L'objectif était de déterminer une stratégie gagnante au Méta-Morpion à l'image de celle du morpion.
J'ai pu implémenter deux modèles d'IA dans ce programme : l'algorithme de MinMax (avec élagage $\alpha - \beta$) et l'algorithme de recherche arborescente de Monte-Carlo

## Les règles du jeu

Tout d'abord, le Méta-Morpion est une variante du jeu du morpion dans laquelle chacune des cases de la grille de jeu classique contient elle-même une autre grille de morpion.

Chaque coup joué dans un petit morpion impose à l’adversaire de jouer dans le morpion correspondant à la position du coup précédent, ajoutant une dimension tactique importante.

Pour gagner, un joueur doit aligner trois petits morpions remportés sur la grande grille, ce qui oblige à penser à plusieurs échelles simultanément. Cette mécanique crée un équilibre subtil entre l’attaque et la défense, où un mauvais coup peut non seulement compromettre une victoire locale, mais aussi offrir un avantage global à l’adversaire.

Il est important d'analyser non seulement l’état local de chaque morpion, mais aussi la dynamique globale de la partie, en intégrant différentes heuristiques.

![image](https://github.com/user-attachments/assets/af0d93e3-777f-4a12-9bfb-8b9669309067)

## Stratégies gagnantes

Si le premier joueur commence par jouer dans la case centrale de la grille centrale, une stratégie gagnante existe déjà.

J'ai essayé de voir si mon programme pouvait retrouver cette stratégie gagnante, ce qui pourrait valider son efficacité, mais j'ai aussi essayé d'empêcher ce coup pour voir si de nouvelles stratégies émergeaient.

Il n'y a pas encore de statistiques sur taux les victoires de mes deux modèles car beaucoup de paramètres différents peuvent être choisis et avoir une influence sur le résultat.

## To do

Implémenter la stratégie gagnante existante pourrait me permettre d'y confronter mon programme en faisant jouer les deux programmes en parallèle. Je pourrai ainsi évaluer la qualité des choix de mon programme.

Je pourrai aussi implémenter de nouvelles heuristiques pour essayer d'améliorer mes résultats.