# Calculette pour Indexage Composé #

Ceci est un "hack" écrit en scheme, pour calculer les mouvements d'indexage simples et composées pour un diviseur spécifique.

Pour l'utiliser, il vous faut :

- Un implementation de scheme r6rs.  J'ai utilisé Chez Scheme, https://www.scheme.com, mais des autres devrait marcher aussi.

- Une installation des bibliothèques r6rs SRFI, au moins SRFI-1 et SRFI-26.  Pour Chez Scheme, https://github.com/arcfide/chez-srfi marchent bien.  Cette installation doit etre trouvable par scheme, ou spécifié sur la ligne de commande. 

- Une installation de TeX.  C'est horrible a faire manuellement, essayer https://miktex.org/

Puis pour executer l'application, on inclut les definitions pour la diviseur voulu avant la programme.  Ce n'est pas facile, mais bon, c'est un hack.

```bash
$ scheme [parametres de scheme] hbm.scm compound-division.scm
```

Apres un petit moment, ca sortira un PDF dans la dossier "output".  Celles déja defini sont precalculé pour les divisions 1-400 et une parametre de precision plutot utilisable, ces pdfs vous sera peut-etre utilisable sans executer l'application.  Si vous avez besoin d'un pdf speciale - diviseur non-repertorié, disque(s) manquant, precision plus élévé, ect, copie ou modifie un des definitions existants. 

Ces definitions couvre les diviseurs suivants :

![liste des diviseurs](https://raw.githubusercontent.com/tufty/compound-division/master/dividing-heads.jpg)


Dans ces definitions, le plus critique est `*tolerated-error-percentage*`, qui defini la percentage erreur par rapport au theorique pour chaque indexage.  Celle de defaut donne des resultats plutot respectable, mais ca se peut que vous en avez besoin de plus.  Modifiant cette parametre pour etre plus petit donnera plus de precision, mais aux prix d'un temps d'execution plus longue.  Si vous voulez la precision ultime, editer compound-division.scm, fonction acceptable-solutions-all-targets-for, et supprimer / commenter la ligne suivant

```scheme
((>= (length results) 3) (take results 3)) ;; Return top 3 approximations
```

Sans cela, l'application traversera tout le "search space" et retournera *tous* les solutions passant la critere de precision.  Donc ca donnera les mieux possible approximations pour division composé additif, mais le cout en temps est considerable - sur mon X220, calculer les divisions 1 a 400 pour Browne & Sharpe prends ~10s de temps CPU.  Avec cette ligne supprimé, plus de 2 heures de CPU.

Ca augmentera la taille resultant du pdf aussi (pas vraiment un problem), mais les "meilleur" resultats en terme de precision sont des fois "farfelus".

En gros, si vous supprimez cette ligne, il est fortement conseillé de modifier aussi `*first-division*` et  `*last-division*`, pour reduire le champs de recherche.
