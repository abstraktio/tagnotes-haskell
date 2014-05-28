how to compile SQLite3 to be used with tagnotes:
---

- http://www.sqlite.org/download.html
- sqlite-autoconf-3071601.tar.gz (1.77 MiB) 	
- CFLAGS="-Os -DSQLITE_ENABLE_FTS3=1 -DSQLITE_ENABLE_FTS3_PARENTHESIS=1 -DSQLITE_ENABLE_FTS4=1" ./configure && sudo make install


```
runhaskell App.hs

Notes Table:
1	|	cake	|	this note will contain a cake recipe	|	2014-05-28 08:55:42	|	2014-05-28 08:55:42
2	|	myths	|	some say the number three is lucky	|	2014-05-28 08:55:42	|	2014-05-28 08:55:42
3	|	eggs	|	the secret to perfect soft-boiled eggs	|	2014-05-28 08:55:42	|	2014-05-28 08:55:42
4	|	mousse	|	steps for a delicious chocolate mousse	|	2014-05-28 08:55:42	|	2014-05-28 08:55:42
5	|	chicken	|	cordon-bleu chicken, deep fried...	|	2014-05-28 08:55:42	|	2014-05-28 08:55:42

Tags Table:
1	|	cake
2	|	recipe
3	|	dessert
4	|	number
5	|	essay
6	|	eggs
7	|	entree
8	|	mousse
9	|	chicken

Join Table:
1	|	1
1	|	2
1	|	3
2	|	4
2	|	5
3	|	2
3	|	6
3	|	7
4	|	8
4	|	2
4	|	3
5	|	2
5	|	7
5	|	9

Tag search: (dessert)
1	|	cake
4	|	mousse

Full-text search:
4	|	mousse

Let's edit note 1 and its tags
format: (deletedTags,keptTags,addedTags)
([2,3],[1],[10])

Join Table:
1	|	1
2	|	4
2	|	5
3	|	2
3	|	6
3	|	7
4	|	8
4	|	2
4	|	3
5	|	2
5	|	7
5	|	9
1	|	10

Tags Table:
1	|	cake
2	|	recipe
3	|	dessert
4	|	number
5	|	essay
6	|	eggs
7	|	entree
8	|	mousse
9	|	chicken
10	|	not-recipe!
```